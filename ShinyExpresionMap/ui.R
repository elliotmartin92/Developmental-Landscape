library(shinycssloaders)
library(shinydashboard)
library(plotly)

ui = dashboardPage(skin = "purple", 
  dashboardHeader(title = "Oogenesis Viz"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
    menuItem("Developmental Progression", tabName = "DevProg", icon = icon("dashboard")),
    menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
    menuItem("Generate report", icon = icon("fas fa-file-download"), tabName = "DownloadTab",
             # Input directly under menuItem
             radioButtons("reportPage", "Generate Report for:",
                         choices = c("All Pages", "Current Page"), selected = "All Pages",
                         width = '98%'),
             downloadButton("report"))
  )),
    dashboardBody(
      # Include the custom styling
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
      tabItems(
      # First tab content
      tabItem(tabName = "DevProg",
              fluidRow(
                box(
                  width = 12,
                  plotOutput("legend", height = "40px"),
                  withSpinner(plotOutput("distPlot", width = "auto"))
                ),
                box(
                  title = "Controls",
                  withSpinner(uiOutput("choose_dataset")),
                  uiOutput("choose_columns"),
                  checkboxInput("displayTPM", "Display TPMs", FALSE),
                  br()
              )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "heatmap", 
              fluidRow(
                box(
                  width = 12, height = 6,
                  withSpinner(plotlyOutput("heatPlot", width = "auto")))))
    ))
)
