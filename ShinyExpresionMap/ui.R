library(shinycssloaders)
library(shinydashboard)
library(plotly)

ui = dashboardPage(skin = "purple", 
  dashboardHeader(title = "Oogenesis Viz"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
    menuItem("Developmental Progression", tabName = "DevProg", icon = icon("dashboard")),
    menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
    menuItem("Gene Groups", tabName = "violin", icon = icon("far fa-chart-bar")),
    menuItem("Generate report", icon = icon("fas fa-file-download"), tabName = "DownloadTab",
             # Input directly under menuItem
             radioButtons("reportPage", "Generate Report for:",
                         choices = c("All Pages", "Current Page"), selected = "All Pages",
                         width = '98%'),
             downloadButton("report"))
  )),
    dashboardBody(
      # Include the custom styling (sidebar color)
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
      tabItems(
      # First tab content
      tabItem(tabName = "DevProg",
              fluidRow(
                box(
                  width = 12,
                  plotOutput("legend", height = "40px"),
                  withSpinner(plotOutput("ovary_map", width = "auto"))),
                box(
                  title = "Controls",
                  withSpinner(uiOutput("choose_dataset")),
                  selectizeInput('variable', label = "Gene of Interest", choices = NULL),
                  checkboxInput("displayTPM", "Display TPMs", TRUE),
                  br()))),
      
      # Second tab content
      tabItem(tabName = "heatmap", 
              fluidRow(
                box(
                  width = 12,
                  withSpinner(plotlyOutput("heatPlot", width = "auto"))))),
      
      # third tab content
      tabItem(tabName = "violin",
              fluidRow(
                box(
                  width = 12,
                  withSpinner(plotOutput("violinPlot", width = "auto"))),
              box(
                title = "Controls",
                radioButtons("violin_geneList_option", label = "Radio buttons",
                             choices = list("Select genes by GO term" = "GO_term_selection", 
                                            "Enter custom list of genes" = "Custom_selection"), 
                             selected = "GO_term_selection"),
                selectizeInput('GO_term', label = "choose_GO_term", choices = NULL),
                textInput("Gene_interest_list", "Genes of Interest List", "Enter genes"),
                verbatimTextOutput("value"),
                radioButtons("violin_normalization_option", label = "Radio buttons",
                             choices = list("Normalize each gene to 1 in UAS-TKV" = "each_gene", 
                                            "Normalize mean to 1 in UAS-TKV" = "mean_gene"), 
                             selected = "each_gene"),
                br())))
    ))
)
