library(shinydashboard)
#library(shinyIncubator)
library(shiny)
library(shinyBS)
library(plotly)
library(d3heatmap)
library(shinyjs)
library(rglwidget)

ui <- dashboardPage(
  dashboardHeader(title = "Ligand-Receptor Pairs",titleWidth = 300),
  dashboardSidebar(width = 300,
                   div(style="overflow-y: scroll"),
                   tags$head(tags$style(HTML(".sidebar { height: 250vh; overflow-y: auto; }" ))),
                   sidebarMenu(
                     menuItem("scRNA data", tabName = "dashboard", icon = icon("hand-o-right"))
                   )#end of sidebar menu
                   
  ),#end dashboardSidebar
  
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    useShinyjs(),
    tabItems(
      tabItem(tabName = "dashboard",
              box(width = 6, status = "primary",solidHeader = TRUE,title = "Ligand Selection Panel",
                    fileInput('liggeneli', 'Upload Receptor Genelist',accept=c('text/csv','text/comma-separated-values,text/plain','.txt'))
                  ),
              box(width = 6, status = "primary",solidHeader = TRUE,title = "Receptor Selection Panel",
                    fileInput('recgeneli', 'Upload Receptor Genelist',accept=c('text/csv','text/comma-separated-values,text/plain','.txt'))
                  ),
              box(width = 12, status = "primary",solidHeader = TRUE,title = "Ligand-Receptor pairs",
                  DT::dataTableOutput('ligrecpairs'),uiOutput("dwldtab"))
      )#end of tabitem
    )#end of tabitems
  )#end of dashboardbosy
)#end of dashboard page

