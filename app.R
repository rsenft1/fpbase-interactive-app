rm(list = ls())

library(shiny)
library(vroom)
library(ghql)
library(tidyverse)
library(jsonlite)
library(data.table)

data <- vroom("/Users/rsenft/Documents/GitHub/JUMP_ORF_features.csv", delim=",")
my_autocomplete_list <- data

ui <- fluidPage(
    fluidRow(
        selectizeInput(
            inputId = 'search',
            label = 'Search',
            choices = my_autocomplete_list,
            selected = NULL,
            multiple = FALSE, # allow for multiple inputs
            options = list(create = FALSE) # if TRUE, allows newly created inputs
        ),
        textOutput("feature"),
        DT::dataTableOutput("table"),
        uiOutput("moreControls"),
        uiOutput("fpbase_page")
    ),
    tags$head(tags$style(HTML(".selectize-input {width: 500px; max-height: 1000 px;}"))),
    tags$style(type='text/css', ".selectize-dropdown-content {max-height: 1000px; }"), 
    
)

server <- function(input, output, session) {
    output$feature <- renderText({ input$search })
    
    link <<- 'https://www.fpbase.org/graphql/'
    conn <<- GraphqlClient$new(url = link)
    
    # Function to run a GraphQL query
    execute_query = function(query) {
        result <- conn$exec(Query$new()$query('link', query)$link)
        flat_result <- result %>% fromJSON(flatten = F)
        # print(flat_result)
        result.df <- as.data.frame(flat_result[[1]])
        # print(result.df)
        output$table <- DT::renderDataTable({result.df}, rownames = TRUE, filter = 'top', selection = 'single', extensions = 'Buttons')
    }
    
    # Function to run a GraphQL query and return df
    execute_query_df = function(query) {
        result <- conn$exec(Query$new()$query('link', query)$link)
        flat_result <- result %>% fromJSON(flatten = F)
        # print(flat_result)
        result.df <- as.data.frame(flat_result[[1]])
        return(result.df)
    }
    
    execute_query('
    query SampleQuery {
    microscopes {
        name 
        description
        id
    }
    }
    ')
    microscopes <- reactive({
        execute_query_df('
        query SampleQuery {
        microscopes {
            name
            id
        }
        }
        ')
    })
    ########## TIDY UP AT END OF SESSION ##########
    
    session$onSessionEnded(function() {
        removeModal()
        link <<- NULL
        conn <<- NULL
    })
    
    ## controls ##
    output$moreControls <- renderUI({
        tagList(
            selectizeInput(
                inputId = 'search',
                label = 'Search',
                choices = microscopes()$name,
                selected = NULL,
                multiple = FALSE, # allow for multiple inputs
                options = list(create = FALSE) # if TRUE, allows newly created inputs
            ),
        )
    })
    
    ## Selecting data table ##
    observe({
        req(input$table_rows_selected)
        selRow <- microscopes()[input$table_rows_selected,]
        print(selRow[[1]])
        print(selRow[[2]])
        my_id <- selRow[[2]]
        url <- a("Link to FPbase", href=paste0("https://www.fpbase.org/microscope/",my_id))
        output$fpbase_page <- renderUI({
            tagList("URL link:", url)
        })
    })
    
    ## link to fpbase page for selected microscope ##
    
}
shinyApp(ui = ui, server = server)