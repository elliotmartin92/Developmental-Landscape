library(shinycssloaders)

ui <- fluidPage(
  sidebarPanel(
    withSpinner(uiOutput("choose_dataset")),
                uiOutput("choose_columns"),
                checkboxInput("displayTPM", "Display TPMs", FALSE),
                br()),
  
  mainPanel(
    align="center",
    plotOutput("legend", height = "40px"),
    withSpinner(plotOutput("distPlot", width = "auto")))
)