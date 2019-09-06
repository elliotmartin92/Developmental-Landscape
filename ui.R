shinyUI(pageWithSidebar(
  
  headerPanel(uiOutput("info")),
  
  sidebarPanel(
    uiOutput("choose_dataset"),
    
    uiOutput("choose_columns"),
    br()
  ),
  
  
  mainPanel(
    tableOutput("data_table"),
    hr(),
    uiOutput('myPanel'),
    plotOutput("distPlot")
  )
))

