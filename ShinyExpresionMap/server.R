library(shiny)
# library(shinyWidgets)
library(dtplyr)
library(tidyverse)
library(heatmaply)
library(rgdal)
library(sf)
library(ggplot2)
library(plotly)
library(ggmap)
library(cowplot)
library(purrr)
library(tinytex)
library(quanteda)
library(ggpubr)
library(ggprism)
library(rstatix)
library(Cairo)
library(rlang)
library(latex2exp)
# source("server_modules/ovary_map.R")
source("server_modules/ggplotWhiteTheme.R")
options(shiny.usecairo=T)

# initialize shinyEnv and declare some variables that will be required in server/modules
shinyEnv = new.env()
ps = .libPaths()
data_sets <- c("FBID", "Symbol")
GO_term_tib <<-  read_tsv("Preprocessed_data/all_go_terms.tsv")
GO_term_description <<- sort(GO_term_tib$description)
figure_legends_table <<- read_csv("Figure_legends.csv")

####Shiny Server gene_of_interest initialization and housekeeping####
# server initialization and check to ensure server shuts down cleanly on tab closure
shinyServer(function(input, output, session) {
  session$onSessionEnded(function(){stopApp()})
  
  # implements logic for tutorial button
  steps <- reactive(
    data.frame(
      element=c(
        ".view_Cartoon", 
        ".view_DatasetTab", 
        ".view_DevProg", 
        ".view_heatmap",
        ".view_violin",
        ".view_report"),
      intro=c(
        "View a color-coded cartoon illustrating early oogenesis stages",
        "You can select a seq dataset",
        "You can view several vizualiations/tools such as: Developmental Progression",
        "Heatmaps",
        "Or Gene Groups",
        "You can download vizualizations you've made using the report tool"
      ),
      position=c("auto", "auto", "auto", "auto", "auto", "auto")
    )
  )
  
  # observes help button press, initiates tutorial on press
  observeEvent(input$help_btn,
               introjs(session, options = list(steps=steps(), "nextLabel"="Next", "nextToDone"="true")))
  # keeping track of the page being viewed for report output
  observe({
  
  if(input$reportPage == "All Pages"){
    pages_to_report <<- "all"
  }
    else if(input$reportPage == "Current Page"){
      pages_to_report <<- input$tabs
    }
    else{
      print("No report selection made")
      }
  })
  
  # Drop-down selection box for which data set
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Data set", as.list(data_sets), selected = "Symbol")
  })
  
  # Selction for FBGNs or Gene Symbols
  observeEvent(input$dataset, {
    # Get the data set with the appropriate name
    dat <- get(input$dataset)
    updateSelectizeInput(session = session, inputId = "gene_of_interest", selected = "RpS19b",
                    label = "Gene of Interest", choices = dat, server = TRUE)
  })
  observe({
    if (input$violin_geneList_option == "GO_term_selection") {
      shinyjs::show("GO_term")
      shinyjs::hide("Gene_interest_list", anim = TRUE, animType = "slide")
    } else {
      shinyjs::show("Gene_interest_list", anim = TRUE, animType = "slide")
      shinyjs::hide("GO_term")
    }
    if (input$tabs == "DevProg") {
      shinyjs::show("cartoon_toggle", anim = TRUE, animType = "slide")
      shinyjs::toggle(id = c("DevProg_control_box"), condition = {input$cartoon_toggle == TRUE})
      # shinyjs::toggle(id = c("DevProg_legend_box"), condition = {input$cartoon_toggle == TRUE}) 
    } else {
      shinyjs::hide("cartoon_toggle", anim = TRUE, animType = "slide")
    }
  })
  # Output the data as a table for GO selection
  updateSelectizeInput(session = session, inputId = "GO_term", selected = "cytosolic ribosome",
                        label = "GO Term to Plot", choices = GO_term_description, server = TRUE)
  
####Plotting ovary_map####
  output$ovary_map = renderPlot({
    #scale text off of tab size
    plotwidth = round(session$clientData[["output_ovary_map_width"]], -2)
    text_scale_map = plotwidth/330
    if (input$cartoon_toggle == FALSE) {
      source("server_modules/ovary_map_cartoon.R")
      cartoon_toggle_global <<- FALSE
        ovary_cartoon_plot <<- ovary_map_cartoon(text_scale = text_scale_map)
        ovary_cartoon_plot
    }else{
      source("server_modules/ovary_map.R")
      cartoon_toggle_global <<- TRUE
      if(is.null(input$gene_of_interest) | is.null(input$dataset)) {
        return()
      }
      validate(
        need(input$gene_of_interest != "",  "Please select a gene")
      )
      
      assign("input", input, envir = shinyEnv)
      assign("ovary_map_SeqDataset", input$SeqDataset, envir = shinyEnv)
    
      ovary_map_plot <<- ovary_map(data_set_to_plot = input$SeqDataset,
                                   gene_name_format = input$dataset, 
                                   displayTPM = input$displayTPM, 
                                   display_stage_labels = input$display_stage_labels,
                                   gene_of_interest = input$gene_of_interest, 
                                   text_scale = text_scale_map, 
                                   graphic_to_generate = "map")
      ovary_map_plot
    } #sets aspect ratio of plot, in conjunction height=auto in UI, rounding hack prevents infinite rendering loop between textsize and height
  }, height = function() { round(session$clientData$output_ovary_map_width, -2)*0.28} 
  )
  
    # Dont render legend in cartoon view
  output$legend = renderPlot({
    if (input$cartoon_toggle == FALSE) {
    # Adding separate legend so that all legend values can always be displayed
    }else{
      plotwidth = round(session$clientData[["output_ovary_map_width"]], -2)
      text_scale_legend = plotwidth/100
      ovary_map_legend <<- ovary_map(graphic_to_generate = "legend", text_scale = text_scale_legend)
      ovary_map_legend
    }
  }, height = function() { round(session$clientData$output_legend_width, -2)*0.025} )
  
#### Plotting of heatmap ####
  output$heatPlot = renderPlotly({
    source("server_modules/heat_map.R")
    DE_heatmap(data_set_to_plot = input$SeqDataset, row_labels = input$display_heatmap_row_labs)
    heat_map_global
  })

#### Plotting of violinplot ####
output$violinPlot = renderPlot({
  source("server_modules/violin_genes.R")
  if (is.null(input$GO_term)) {
    return()
  }
  validate(
    need(input$GO_term != "" | input$violin_geneList_option != "GO_term_selection", "Please select a GO term")
  )
  validate(
    need(input$Gene_interest_list != "" | input$violin_geneList_option != "Custom_selection", "Please enter a list of FBids")
  )
  
  plotwidth = round(session$clientData[["output_violinPlot_width"]], -2)
  text_scale_violin = plotwidth/90
  assign("input", input, envir = shinyEnv)
  assign("violinPlot_SeqDataset", input$SeqDataset, envir = shinyEnv)
  gene_violin_plot_global <<- gene_violin(data_set_to_plot = input$SeqDataset,
                                          genes_by_GO = input$violin_geneList_option, 
                                          GO_term = input$GO_term,
                                          gene_of_interest = input$Gene_interest_list,
                                          normalization = input$violin_normalization_option,
                                          text_scale = text_scale_violin)
  gene_violin_plot_global
}, height = function() { round(session$clientData$output_violinPlot_width, -2)*.3})

  # report function calls report.Rmd to knit an rmarkdown file to save data analysis
  # Filenames don't work when app is run locally, but work when deployed
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Ovary_App_Report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed)
      # writing locally can break report on deployment, but otherwise how to access modules w/o file structure?
      # Maybe run .rmd locally, but export to temp dir via YAML?
      # tempReport = file.path(getwd(), "report.Rmd") 
      # file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Rather than pass data via params, assign data to a shinyEnv environment for the report code
      rmarkdown::render("report.Rmd", output_file = file, envir = shinyEnv)
    })
    
  # subset data and serve csv with subsetted data for dl
  output$violin_data_download <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      if (input$violin_geneList_option == "GO_term_selection") {
        paste0("Selected_gene_expression_from_", input$SeqDataset, "_of_GO_term_", input$GO_term, ".csv")
      }else if (input$violin_geneList_option == "Custom_selection") {
        paste0("Selected_gene_expression_from_", input$SeqDataset, "_of_custom_list_of_genes.csv")
      }
    },
    content = function(file) {
      # selected data passed from violin_genes
      
      # write .csv of selected data
      write.csv(selected_gene_data_norm_global, file, row.names = FALSE)
  })
})