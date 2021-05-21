library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
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
                prettyToggle(inputId = "cartoon_toggle", label_on = "Display Cartoon", label_off = "Display Seq Data", value = TRUE,
                             shape = "round", fill = FALSE, outline = FALSE, icon_on = icon("fas fa-toggle-on"), icon_off = icon("fas fa-toggle-off"),
                             animation = "smooth") %>% add_class("view_Cartoon"),
      menuItem("Select a dataset", icon = icon("fas fa-table"), tabName = "DatasetTab", startExpanded = TRUE,
              # Input directly under menuItem
              awesomeRadio("SeqDataset", "View dataset:",
                choices = list("Input mRNAseq" = "Input_seq", 
                               "Polysome-seq"= "Polysome_seq",
                               "Single Cell-seq: Germline"= "Single_cell_seq_germline",
                               "Single Cell-seq: Soma"= "Single_cell_seq_soma"), 
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
             awesomeRadio("reportPage", "Generate Report for:",
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
                  plotOutput("legend", height = "auto"),
                  withSpinner(plotOutput("ovary_map", height = "auto")))
              ),
                
                fluidRow( id = "DevProg_control_box",
                           style="margin-bottom:50px;",
                box(
                  title = "Controls",
                  withSpinner(uiOutput("choose_dataset")),
                  selectizeInput('gene_of_interest', label = "Gene of Interest", choices = NULL),
                  prettyCheckbox("displayTPM", "Display Expression Values", TRUE, icon = icon("check"), status = "success"),
                  prettyCheckbox("display_stage_labels", "Display Stage Labels", TRUE, icon = icon("check"), status = "success")
                  )
                )
                ),
      
      # Second tab content
      tabItem(tabName = "heatmap", 
              fluidRow(
                box(
                  width = 12,
                  withSpinner(plotlyOutput("heatPlot", width = "auto"))),
                box(
                  title = "Controls",
                  checkboxInput("display_heatmap_row_labs", "Display Row Labels", FALSE),
                    )
                )),
      
      # third tab content
      tabItem(tabName = "violin",
              fluidRow(
                box(
                  width = 12,
                  withSpinner(plotOutput("violinPlot", width = "auto"))),
              box(
                title = "Controls",
                awesomeRadio("violin_geneList_option", label = "Genes by:",
                             choices = list("Select genes by GO term" = "GO_term_selection", 
                                            "Enter custom list of genes" = "Custom_selection"), 
                             selected = "GO_term_selection"),
                selectizeInput('GO_term', label = "choose_GO_term", choices = NULL),
                textInput("Gene_interest_list", "Genes of Interest List", "Enter a list of FBids"),
                verbatimTextOutput("value"),
                awesomeRadio("violin_normalization_option", label = "Normalization",
                             choices = list("Normalize each gene" = "each_gene", 
                                            "Log normalized Expression" = "unNorm"), 
                             selected = "each_gene"),
                br())))
    ))
)
