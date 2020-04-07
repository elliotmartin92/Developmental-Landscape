library(shinycssloaders)
shinyUI(pageWithSidebar(
  
  headerPanel(uiOutput("info")),
  
  sidebarPanel(
    withSpinner(uiOutput("choose_dataset")),
    uiOutput("choose_columns"),
    checkboxInput("displayTPM", "Display TPMs", FALSE),
    br()
  ),
  
  
  mainPanel(align="center",
    tableOutput("data_table"),
    hr(),
    uiOutput('myPanel'),
    plotOutput("legend", height = "20px"),
    withSpinner(plotOutput("distPlot", width = "100%"))
  )
))

