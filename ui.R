
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinydashboard)
library(shinyjs)
require(rCharts)
library(DT)
source("global.R")

sectors <- getSectors()

shinyUI(
    dashboardPage(
        title = "Benchmarking regionale",
        
        # Application title
        dashboardHeader(title = HTML("<img src=\"logoISPAT.svg\" width=70 heigth=35 />Benchmarking regionale"),
                        titleWidth = 380
        ),
        
        # Sidebar with a select input for indicator
        dashboardSidebar(
            width = 380,
            sidebarMenu(
                id = "sidebarmenu",
                menuItem("Cruscotto generale",
                         tabName = "dashboard",
                         icon = icon("dashboard")
                ),
                menuItem("Indicatori",
                         tabName = "indicatori",
                         icon = icon("line-chart")
                ),
                conditionalPanel("input.sidebarmenu == 'indicatori'",
                                 selectInput(
                                     "settore",
                                     "Settore:",
                                     sectors, 
                                     selected = 1
                                 ),
                                 selectInput(
                                     "indd",
                                     "Indicatore:",
                                     c("optionA" = "A")
                                 )
                ),
                conditionalPanel("1 == 2",
                                 selectInput("ind",
                                             "asd",
                                             getIndicators())),
                dataTableOutput("NUTS"),
                leafletOutput("map")
            )
        ),
        
        dashboardBody(
            useShinyjs(),
            tags$head(
                tags$link(rel = "stylesheet", 
                          type = "text/css", 
                          href = "custom.css")
            ),
            tabItems(
                tabItem(
                    tabName = "indicatori",
                    h1(textOutput("title")),
                    tabBox(
                        width = 12,
                        height = 650,
                        tabPanel(
                            "Tabella",
                            conditionalPanel("input.switch % 2 == 0",
                                             box(title = "Serie Storica",
                                                 collapsible = F,
                                                 width = 12,
                                                 #height = "100%",
                                                 DT::dataTableOutput("table")
                                             )
                            ),
                            conditionalPanel("input.switch % 2 == 1",
                                             box(title = "Serie Storica",
                                                 collapsible = F,
                                                 width = 12,
                                                 #height = "100%",
                                                 DT::dataTableOutput("rankTable")
                                             )
                            ),
                            "*: la variazione fa riferimento all'anno precedente"
                        ),
                        tabPanel(
                            "Grafico",
                            conditionalPanel("input.switch % 2 == 0",
                                             box(title = "Grafico",
                                                 width = 12,
                                                 height = "50%",
                                                 collapsible = F,
                                                 showOutput("valuePlot", "highcharts")
                                             )
                            ),
                            conditionalPanel("input.switch % 2 == 1",
                                             box(title = "Grafico",
                                                 width = 12,
                                                 height = "50%",
                                                 collapsible = F,
                                                 showOutput("rankPlot", "highcharts")
                                             )
                            )
                        )
                    ),
                actionButton("switch", "Vedi graduatoria")
                ),
                tabItem(
                    tabName = "dashboard",
                    h1("Cruscotto generale"),
                        selectInput("anno",
                                    "Scegli anno:",
                                    choices = c("Ultimo anno disponibile",
                                                2015, 2014, 
                                                2013, 2012, 
                                                2011, 2010)
                                    ),
                    height = 1500,
                    fluidRow(
                        box(id = "good",
                            width = 6,
                            title = "Dove andiamo bene:",
                            solidHeader = T,
                            tableOutput("textBest")
                        ),
                        box(id = "bad",
                            width = 6,
                            title = "Dove andiamo male:",
                            solidHeader = T,
                            tableOutput("textWorst")
                        )
                    ),                
                    
                    h1("Il benchmarking per settore"),
                    tabBox(
                        id = "sectors",
                        width = 12,
                        height = 850, color = "grey",
                        tabPanel(id = "demo",
                                 width = 12,
                                 title = "Demografia",
                                 lapply(getIndicators(1)$idDataFlow, 
                                        function(x) {
                                            comparisonUi(paste0("box_", x), x)
                                            })
                        ),
                        tabPanel(id = "salu",
                                 width = 12,
                                 title = "Salute",
                                 lapply(getIndicators(2)$idDataFlow, 
                                        function(x) {
                                            comparisonUi(paste0("box_", x), x)
                                            })
                        ),
                        tabPanel(id = "ist",
                                 width = 12,
                                 title = "Istruzione",
                                 lapply(getIndicators(3)$idDataFlow, 
                                        function(x) {
                                            comparisonUi(paste0("box_", x), x)
                                            })
                        ),
                        tabPanel(id = "eco",
                                 width = 12,
                                 title = "Economia",
                                 lapply(getIndicators(4)$idDataFlow, 
                                        function(x) {
                                            comparisonUi(paste0("box_", x), x)
                                            })
                        ),
                        tabPanel(id = "lav",
                                 width = 12,
                                 title = "Lavoro",
                                 lapply(getIndicators(5)$idDataFlow, 
                                        function(x) {
                                            comparisonUi(paste0("box_", x), x)
                                            })
                        ),
                        tabPanel(id = "tur",
                                 width = 12,
                                 title = "Turismo",
                                 lapply(getIndicators(6)$idDataFlow, 
                                        function(x) {
                                            comparisonUi(paste0("box_", x), x)
                                        })
                        ),
                        tabPanel(id = "inn",
                                 width = 12,
                                 title = "Innovazione",
                                 lapply(getIndicators(7)$idDataFlow, 
                                        function(x) {
                                            comparisonUi(paste0("box_", x), x)
                                        })
                        ),
                        tabPanel(id = "amb",
                                 width = 12,
                                 title = "Ambiente",
                                 lapply(getIndicators(8)$idDataFlow, 
                                        function(x) {
                                            comparisonUi(paste0("box_", x), x)
                                        })
                        )
                    )
                )
            )
        )
    )
)