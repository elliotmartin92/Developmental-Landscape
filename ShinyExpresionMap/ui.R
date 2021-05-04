library(shinycssloaders)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(rintrojs)

# adds a class to a shiny element
add_class <- function(x, class) {
  x$attribs <- append(x$attribs, list(class = class))
  x
}

ui = dashboardPage(skin = "purple",
  dashboardHeader(title = "Oogenesis Viz"),
  dashboardSidebar(
    introjsUI(),
    sidebarMenu(id = "tabs",
      menuItem("Select a dataset", icon = icon("fas fa-table"), tabName = "DatasetTab", startExpanded = TRUE,
              # Input directly under menuItem
              radioButtons("SeqDataset", "View dataset:",
                choices = list("Input mRNAseq" = "Input_seq", 
                               "Polysome-seq"= "Polysome_seq",
                               "Single Cell-seq"= "Single_cell_seq"), 
                selected = "Input_seq",
                width = '98%')) %>% add_class("view_DatasetTab"), 
      menuItem("Developmental Progression", tabName = "DevProg", icon = icon("dashboard")) %>% 
        add_class("view_DevProg"),
      menuItem("Heatmap", tabName = "heatmap", icon = icon("th")) %>% 
        add_class("view_heatmap"),
      menuItem("Gene Groups", tabName = "violin", icon = icon("far fa-chart-bar")) %>% 
        add_class("view_violin"),
      menuItem("Generate report", icon = icon("fas fa-file-download"), tabName = "DownloadTab",
             # Input directly under menuItem
             radioButtons("reportPage", "Generate Report for:",
                         choices = list("Current Page", "All Pages"), selected = "Current Page",
                         width = '98%'),
             downloadButton("report")) %>% 
               add_class("view_report"),
      actionButton("help_btn","Help", icon = icon("far fa-question-circle"))
  )),
    dashboardBody(
      shinyjs::useShinyjs(),
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
                  checkboxInput("displayTPM", "Display Expression Values", TRUE),
                  checkboxInput("display_stage_labels", "Display Stage Labels", TRUE),
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
                radioButtons("violin_geneList_option", label = "Genes by:",
                             choices = list("Select genes by GO term" = "GO_term_selection", 
                                            "Enter custom list of genes" = "Custom_selection"), 
                             selected = "GO_term_selection"),
                selectizeInput('GO_term', label = "choose_GO_term", choices = NULL),
                textInput("Gene_interest_list", "Genes of Interest List", "Enter a list of FBids"),
                verbatimTextOutput("value"),
                radioButtons("violin_normalization_option", label = "Normalization",
                             choices = list("Normalize each gene" = "each_gene", 
                                            "Log normalized Expression" = "unNorm"), 
                             selected = "each_gene"),
                br())))
    ))
)
