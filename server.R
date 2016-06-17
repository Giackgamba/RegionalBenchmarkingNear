
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(rCharts)
library(DT)
library(dplyr)
library(shinyjs)
source("global.R")

options(
    DT.options = list(
        server = FALSE,
        pageLength = 14,
        processing = TRUE,
        escape = FALSE,
        rownames = FALSE,
        language = list(
            info = ""
        ),
        dom = "t"
    )
)

shinyServer(function(input, output, clientData, session) {
    
    sectors <- getSectors()
    geoFilter <- getGEOFilters()
    indicators <- reactive({ 
        getIndicatorsList(input$settore) 
    })
    
    
    output$NUTS <- renderDataTable(
        datatable(getGEO() %>%
                      mutate(
                          id,
                          Stato,
                          Regione =  paste0("<a href=\"", 
                                            link, "\">", 
                                            Regione, "</a>")) %>%
                      select(-link),
                  escape = FALSE,
                  rownames = FALSE
        )  %>%
            formatStyle(
                columns = 1:3, 
                backgroundColor = "#222D32"
            )
    )
    
    observeEvent(
        input$settore, 
        ({ 
            updateSelectInput(session, 
                              "indd", 
                              choices = indicators()) 
        })
    )
    
    
    data <- reactive({
        getData(input$ind)
    })
    
    selected <- reactive({
        sel <- input$table_rows_selected
    })
    
    observeEvent(input$switch, {
        if(!is.null(input$switch)){
            if(input$switch%%2 == 0) html("switch", "Vedi graduatoria")
            else html("switch", "Vedi valori")
        }
    })
    
    observeEvent(input$indd,
                 {updateSelectInput(session, "ind", selected = input$indd)})
    
    observeEvent(
        input$ind,
        output$table <- DT::renderDataTable(
            ({
                tab <- data() %>%
                    pivotData() %>%
                    datatable(
                        rownames = FALSE,
                        escape = FALSE,
                        selection = list(
                            mode = "multiple",
                            selected = 6),
                        extensions = list(
                            FixedColumns = list(
                                leftColumns = 2)
                        ),
                        options = list(
                            scrollX = TRUE,
                            columnDefs = list(
                                list(orderable = FALSE,
                                     title = "*",
                                     targets = -1),
                                list(orderable = FALSE,
                                     title = " ",
                                     width = "20px",
                                     targets = 1
                                )
                            )
                        )
                    ) %>%
                    formatRound(2:10, 1)
            })
        )
    )
    
    observeEvent(
        input$ind,
        output$rankTable <- DT::renderDataTable(
            ({
                data <- data()
                tab <- rankData(data)
                tab <- datatable(
                    tab,
                    rownames = FALSE,
                    escape = FALSE,
                    extensions = list(
                        FixedColumns = list(
                            leftColumns = 2)
                    ),
                    selection = list(
                        mode = "multiple",
                        selected = 6),
                    options = list(
                        scrollX = TRUE,
                        columnDefs = list(
                            list(orderable = FALSE, 
                                 title = "*",
                                 targets = -1),
                            list(orderable = FALSE,
                                 title = " ",
                                 width = "20px",
                                 targets = 1
                            )
                        )
                    )
                )
            })
        )
    )
    observeEvent(
        input$ind,
        output$title <- renderText(
            name <- getIndName(input$ind)
        )
    )
    
    observeEvent(
        input$ind,
        output$valuePlot <- renderChart2(
            ({
                data <- data()
                p <- valuePlot(data, input$table_rows_selected)
                p$addParams(dom = "valuePlot")
                
                return(p)
            })
        )
    )
    
    observeEvent(
        input$ind,
        output$rankPlot <- renderChart2(
            ({
                data <- data()
                p <- rankPlot(data, input$rankTable_rows_selected)
                p$addParams(dom = "rankPlot")
                
                return(p)
            })
        )
    )
    
    output$textBest <- renderTable({
        res <- getBestTN(input$anno) %>%
            select(Indicatore = ind,
                   Valore = obsValue,
                   Rank = rank,
                   Anno = obsTime) %>%
            arrange(Rank)
    },
    include.rownames = F,
    digits = c(0,0,1,0,0))
    
    output$textWorst <- renderTable({
        res <- getWorstTN(input$anno) %>%
            select(Indicatore = ind,
                   Valore = obsValue,
                   Rank = rank,
                   Anno = obsTime) %>%
            arrange(desc(Rank))
    },
    include.rownames = F,
    digits = c(0,0,1,0,0))  
    
    output$map <- renderLeaflet(makeMap())
    
    
    ## Function to create multiple "observeEvent"
    
    updInd <- function(ind) {
        assign(paste0("obs2",ind),
               observeEvent(input[[paste0("box_", ind, "-sec")]], {
                   updateSelectInput(session, 
                                     "ind", 
                                     selected = as.character(ind))
                   updateTabItems(session, "sidebarmenu", "indicatori")
                   
               })  
        )
    }
    
    # For loop to create multiple box each with its observeEvent
    for (a in 1:8){
        box_output_list <- lapply(getIndicators(a)$idDataFlow,
                                  function(i) {
                                      callModule(comparison, 
                                                 paste0("box_", i), 
                                                 i)
                                  })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, box_output_list)
        lapply(getIndicators(a)$idDataFlow, updInd) 
    }
    
    
})


