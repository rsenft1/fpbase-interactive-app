rm(list = ls())

library(shiny)
library(vroom)
library(ghql)
library(tidyverse)
library(jsonlite)
library(data.table)


ui <- fluidPage(
    fluidRow(
        DT::dataTableOutput("table"),
        # uiOutput("moreControls"),
        selectizeInput('microscope_name', 
                       choices = NULL,
                       label = "Search FPbase microscopes:",
                       selected = NULL,
                       multiple = FALSE, # allow for multiple inputs
                       options = list(create = FALSE)) # if TRUE, allows newly created inputs)
    ),
    uiOutput("fpbase_page"),
    tags$head(tags$style(HTML(".selectize-input {width: 500px; max-height: 1000 px;}"))),
    tags$style(type='text/css', ".selectize-dropdown-content {max-height: 1000px; }"), 
    
)

server <- function(input, output, session) {
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
    }
    }
    ')
    microscopes <- reactive({
        execute_query_df('
        query SampleQuery {
        microscopes {
            name
            id
            description
        }
        }
        ')
    })
    ## non-reactive microscopes
    microscopes_nr <-
      execute_query_df('
        query SampleQuery {
        microscopes {
            name
            id
            description
        }
        }
        ')
    ########## TIDY UP AT END OF SESSION ##########
    
    session$onSessionEnded(function() {
        removeModal()
        link <<- NULL
        conn <<- NULL
    })
    
    # ## ui output - obsolete##
    # output$moreControls <- renderUI({
    #     tagList(
    #         selectizeInput(
    #             inputId = "search",
    #             label = 'Search microscopes.:',
    #             choices = paste(microscopes()[,1], microscopes()[,3]),
    #             selected = NULL,
    #             multiple = FALSE, # allow for multiple inputs
    #             options = list(create = FALSE) # if TRUE, allows newly created inputs
    #         )
    #     )
    # })
    
    ## Selecting data table ##
    observe({
      req(input$table_rows_selected)
      selRow <- microscopes()[input$table_rows_selected,]
      # print(selRow[[1]])
      # print(selRow[[2]])
      my_id <- selRow[[2]]
      url <- a("Link to FPbase", href=paste0("https://www.fpbase.org/microscope/",my_id))
      output$fpbase_page <- renderUI({
          tagList("URL link:", url)
      })
    })
      
    ## Selecting dropdown ##
    updateSelectizeInput(session, 'microscope_name', choices = microscopes_nr[,1], server = TRUE)
    
    observe({
      req(input$microscope_name)
      # print(input$microscope_name)
      names <- microscopes()[,"microscopes.name"]
      selRow <- microscopes()[names==input$microscope_name,]
      my_id <- selRow[[2]]
      url <- a("Link to FPbase", href=paste0("https://www.fpbase.org/microscope/",my_id))
      output$fpbase_page <- renderUI({
        tagList("URL link:", url)
      })
    })  
    
    ## link to fpbase page for selected microscope ##
    
}
shinyApp(ui = ui, server = server)