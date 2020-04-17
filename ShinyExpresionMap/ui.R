library(shinycssloaders)
library(shinydashboard)
library(plotly)

ui = dashboardPage(
  dashboardHeader(title = "Oogenesis Viz"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Developmental Progression",
      tabName = "DevProg",
      icon = icon("dashboard")
    ),
    menuItem("Heatmap", tabName = "heatmap", icon = icon("th"))
  )),
    dashboardBody(tabItems(
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
                  width = 12,
                  withSpinner(plotlyOutput("heatPlot", width = "auto"))
                )
              ))
    ))
)
