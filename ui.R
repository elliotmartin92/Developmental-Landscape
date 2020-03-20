shinyUI(pageWithSidebar(
  
  headerPanel(uiOutput("info")),
  
  sidebarPanel(
    withSpinner(uiOutput("choose_dataset")),
    uiOutput("choose_columns"),
    checkboxInput("displayTPM", "Display TPMs", FALSE),
    br()
  ),
  
  
  mainPanel(
    tableOutput("data_table"),
    hr(),
    uiOutput('myPanel'),
    withSpinner(plotOutput("distPlot"))
  )
))

