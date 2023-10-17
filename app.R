#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyjson) # BEWARE its filter() masks the tidyverse one!
library(curl)
library(shiny)
library(DT)
library(readr)
library(readxl)
library(dplyr)
library(forcats) # for as_factor
library(plotly) # for interactive graphics
library(scales) # for nicer scales
library(magrittr) # for %<>% only

## HARDCODED PARAMS ##
IACUC_of_interest <- "IACUC2022-46-III" #(will actually only be the default one on startup)
ona_API_server_prefix <- "https://ona.ilri.org/api/v1"
ona_form_ID <- 1273 # this is actually https://ona.ilri.org/erutto/465/1273
my_key <- readLines("ONA_API_key") # my personal API key (as a simple character variable)

# the temperature is missing from the scoring_columns because it is the result
# of a transformation (computed variable)
scoring_columns <- c("inappetence", "recumbency", "haemorrhage", "jntswelling",
                           "breathing", "odschrge", "diarrhoea", "urine", "vomit")

## END HARDCODED PARAMS ##

## USEFUL FUNCTIONS ##

# the following function is used mainly to trim prefixes like "grp1_sec1b/"
# from column names (will be used as a function called by dplyr::rename())
remove_group_prefix <- function (s) {
  # input is a mere string: we trim the prefix upto and including the **first** occurrence (scanning left to right) of a slash (/)
  s %>% stringr::str_replace("[^/]*/", "")
}


# According to https://stackoverflow.com/questions/74562346/prevent-ggplotly-to-change-the-legends-style-created-in-ggplot,
# we have to fix plotly's handling of legends for geom_line(),
# because it ignores its show.legend option
solid_lines_legend <- function(plotly_obj) {
  # the input to this function is a plotly output.
  # this fix borrows heavily from the one by https://stackoverflow.com/users/5329073/kat
  # here: https://stackoverflow.com/questions/74562346/prevent-ggplotly-to-change-the-legends-style-created-in-ggplot
  # BEWARE: lines that are dash-only WILL NOT appear in the legend
  lapply(1:length(plotly_obj$x$data),
         function(j) {
           if(plotly_obj$x$data[[j]]$mode == "lines") {
             if(plotly_obj$x$data[[j]]$line$dash == "dash" |
                nchar(plotly_obj$x$data[[j]]$name) == 0) # anonymous line: do not legend
               plotly_obj$x$data[[j]]$showlegend <<- F
             else
               plotly_obj$x$data[[j]]$showlegend <<- T
           } # endif
         }) #endfunction j #end lapply
  plotly_obj
} #endfunction solid_lines_legend

## END USEFUL FUNCTIONS ##

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("ASF clinical assessment: King scores"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "experiment",
        "IACUC number (with timespan of observations recorded)",
        choices = character(),
        multiple = F
      ),
      radioButtons(
        "displayType",
        "Table display",
        choices = c(
          `Condensed (clinical observations only)` = "Condensed",
          `Full (all columns, including calculated variables)` = "Full"),
        selected = "Condensed"
      ),
      radioButtons(
        "colourMapping",
        "Colour mapping in graphs",
        choices = c(`Individual animals` = "animalID", `Experimental groups` = "group"),
        selected = "animalID"
      ),
      #textOutput("warningOnGroups"),
      #br(),
      
      selectInput(
        "whichAnimal",
        "Which animal(s)?",
        choices = c("(all)"),
        multiple = T
      ),
      numericInput(
        "RI_threshold",
        "King score threshold to display on graphs (e.g. for humane endpoint)",
        value = 6,
        min = 0,
        max = 10,
        step = 0.1
      ),
      selectInput(
        "additionalVar",
        "What to visualize apart from King scores?",
        choices = character(),
        multiple = F
      ),
      width = 3
    ),
    
    
    # Show the main plot: King scores for animals over time
    mainPanel(dataTableOutput("mainDataTable"),
              plotlyOutput("mainPlot"),
              plotlyOutput("additionalPlot"), # and the additional plot
              width = 9)
  )
)

# Define server logic
server <- function(input, output, session) {
  
  
  output$warningOnGroups <- renderText({
    "(the above will trigger an error when \"Groups\" is selected but the input\
    data doesn't have the information on experimental groups: \ 
    please check your input file)"
  })
  
  # the following filters trim the table display when
  # input$displayType == "Condensed"
  data_cols_to_display <- c("iacuc", "date", "animalID", "temp", "temp_score", scoring_columns, "needvetexam", "total_King")
  
  # we first instantiate the all_dataset variable, as a reactive (tibble) object:
  all_datasets <- reactive({
    
    # building the curl request to get the data
    h <- new_handle(verbose = TRUE) # you don't need to keep this verbose option, it's just for debugging/curiosity
    handle_setheaders(h,
                      "Content-Type" = "application/json",
                      "Authorization" = paste("Token", my_key))
    
    # launching the API call
    response <- curl_fetch_memory(paste0(ona_API_server_prefix, "/data/", ona_form_ID), handle = h)
    
    # the JSON object from the ONA API, when converted ToChar, is a character vector of length **1** (a single array),
    # so we first make a call to gather_array().
    response$content %>% rawToChar() %>% as.tbl_json() %>%
      gather_array() %>% spread_all() %>% as_tibble() %>% select(-document.id) -> df
    
    # we drop all the "grp_sec1b/" prefixes
    df %<>% rename_with(.cols = starts_with("grp_sec1b/"), .fn = remove_group_prefix) %>% 
      filter(comments != 'Test form by Erick')
    
    # TODO: merge potentially multiple records corresponding to the same animal, same day (several successive submissions)
    # and containing partial recordings

    # all columns above have to be mutated to numeric:
    df %<>% mutate(across(any_of(scoring_columns), as.numeric))
    
    # then we will add a column with the King score for each animal, for each day.
    df %<>% mutate(temp_score = case_when(
      temp < 39.0 ~ 0L,
      temp >= 39.0 & temp < 39.5 ~ 1L,
      temp >= 39.5 & temp < 40.0 ~ 2L,
      temp >= 40.0 & temp <= 40.5 ~ 3L,
      temp > 40.5 & temp <= 41.0 ~ 4L,
      .default = 5L), .after = temp) %>% rowwise() %>% 
      mutate(total_King = sum(c_across(all_of(c("temp_score", scoring_columns)))), .before = needvetexam)
      
    # BEWARE: hardcoded "iacuc" and "date" column names
    df %<>% mutate(iacuc = factor(`grp_id/iacuc`), .keep = "unused")
    # lubrify the observation date:
    df %<>% mutate(date = lubridate::date(`grp_id/obdate`), .keep = "unused")
    # rename and factor animal IDs a,d groups:
    df %<>% mutate(animalID = factor(`grp_id/animid`), group = factor(`grp_id/expgroup`), .keep = "unused")
    
    return(df)
  })
  
  # we dynamically recompute the list of experiments to pick from:
  observe({
    all_datasets() %>% group_by(iacuc) %>% summarize(
      first_date = min(date, na.rm = T),
      last_date = max(date, na.rm = T)) %>%
    mutate(
        fullname = paste0(iacuc, " (", first_date, " – ", last_date, ")"),
        .keep = "all") -> temp_table
    temp_table %>% pull(iacuc) -> named_vec
    temp_table %>% pull(fullname) -> names(named_vec)
    # the display values will contain the timespan of recordings
    updateSelectInput(session, "experiment", choices = named_vec, selected = IACUC_of_interest)
  })
  
  # reactively set the dataset
  dataset <- reactive({
    req(input$experiment)
    return(all_datasets() %>% filter(iacuc == input$experiment))
  })
  
  # and set the list of columns to pick from, to filter on a column
  observe({
    req(input$experiment)
    updateSelectInput(session, "additionalVar", choices = setdiff(colnames(dataset()), c("total_King", "animalID")), selected = "temp")
  })
  
  # we dynamically recompute the list of animal IDs to pick from:
  observe({
    req(input$experiment)
    updateSelectInput(session, "whichAnimal",
                      choices = c("(all)", unique(as.character(
      `[[`(dataset(), "animalID")
    ))))
  })
  
  
  # main data table output
  output$mainDataTable <- renderDataTable({
    req(input$experiment)
    
    d <- dataset()
    if(input$displayType == "Condensed")
      d %<>% select(any_of(data_cols_to_display))

    if(is.null(input$whichAnimal) | "(all)" %in% input$whichAnimal)
        d
    else
        d %>%
        filter(as.character(.data[["animalID"]]) %in% input$whichAnimal)
  
  }, options = list(pageLength = 5))
  
  
  # careful in the following: we use hardcoded column names, including animalID
  # TRICK: there is a computed column experimentDuration, NA only for the virtual
  # observations. We use that to filter the stuff we are plotting.
  # We plot dashed lines underneath, and then solid lines on top.
  # In all the graphs, setting group = animalID makes sure we keep one line per animal
  # no matter what the other aesthetics are.
  output$mainPlot <- renderPlotly({
    req(input$experiment)
    validate(
      need(input$colourMapping == "animalID" | ("group" %in% colnames(dataset()) & any(!is.na(dataset() %>% pull(group)))),
           "You asked for a colour mapping on groups, but your input table doesn't contain \
           any information on groups for this trial.")
    )
    
    if (is.null(input$whichAnimal) | "(all)" %in% input$whichAnimal)
      { dataset() %>%
        ggplot(aes(group = animalID, x = date, y = total_King, color = !!sym(input$colourMapping))) +
        geom_line() +
        labs(title = paste0("King scores for all animals"),
                            x = "Observation date", y = "King score") +
        scale_x_continuous(breaks = breaks_width(1), labels = lubridate::as_date) -> p
      if(isTruthy(input$RI_threshold)) p + geom_hline(yintercept = input$RI_threshold) -> p
      ggplotly(p) #%>% solid_lines_legend()
    } else {
      dataset() %>% 
        filter(animalID %in% input$whichAnimal) %>%
        ggplot(aes(group = animalID, x = date, y = total_King, color = !!sym(input$colourMapping))) +
        geom_line() +
        labs(
        title = paste0("King scores for animal(s) ", paste(input$whichAnimal, collapse = ", ")),
        x = "Observation date",
        y = "King score") +
        scale_x_continuous(breaks = breaks_width(1), labels = lubridate::as_date) -> p
      if(isTruthy(input$RI_threshold)) p + geom_hline(yintercept = input$RI_threshold) -> p
      ggplotly(p) #%>% solid_lines_legend()
    }
  })
  
  # careful in the following: we use hardcoded column names, including animalID
  output$additionalPlot <- renderPlotly({
    req(input$experiment, input$additionalVar)
    validate(
      need(input$colourMapping == "animalID" | ("group" %in% colnames(dataset()) & any(!is.na(dataset() %>% pull(group)))),
           "You asked for a colour mapping on groups, but your input table doesn't contain \
           any information on groups for this trial.")
    )
    
    if (is.null(input$whichAnimal) | "(all)" %in% input$whichAnimal)
    { dataset() %>%
        ggplot(aes(group = animalID, x = date, y = !!sym(input$additionalVar), color = !!sym(input$colourMapping))) +
        geom_line() +
        labs(title = paste(input$additionalVar, "for all animals"), x = "Observation date", y = input$additionalVar) +
        scale_x_continuous(breaks = breaks_width(1), labels = lubridate::as_date) -> p
      ggplotly(p) %>% solid_lines_legend()
    } else {
      dataset() %>%
        filter(animalID %in% input$whichAnimal) %>%
        ggplot(aes(group = animalID, x = date, y = !!sym(input$additionalVar), color = !!sym(input$colourMapping))) +
        geom_line() + 
        labs(
        title = paste(input$additionalVar, "for animal(s)", paste(input$whichAnimal, collapse = ", ")),
        x = "Observation date",
        y = input$additionalVar) +
        scale_x_continuous(breaks = breaks_width(1), labels = lubridate::as_date) -> p
      ggplotly(p) %>% solid_lines_legend()
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
