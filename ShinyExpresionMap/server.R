library(shiny)
library(shinyWidgets)
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
library(rstatix)
source("server_modules/ovary_map.R")
source("server_modules/ggplotWhiteTheme.R")

ps = .libPaths()
data_sets <- c("FBID", "Symbol")
GO_term_tib <<-  read_tsv("Preprocessed_data/all_go_terms.tsv")
GO_term_description <<- GO_term_tib$description

####Shiny Server variable initialization and housekeeping####
#server initialization and check to ensure server shutsdown cleanly on tab closure
shinyServer(function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
    })
  
  #keeping track of the page being viewed for report output
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
    updateSelectizeInput(session = session, inputId = "variable", selected = "RpS19b",
                    label = "Gene of Interest", choices = dat, server = TRUE)
  })
  observe({
    if (input$violin_geneList_option == "GO_term_selection") {
      shinyjs::show("GO_term")
      shinyjs::hide("Gene_interest_list")
    } else {
      shinyjs::show("Gene_interest_list")
      shinyjs::hide("GO_term")
    }
  })
  # Output the data as a table for GO selection
  updateSelectizeInput(session = session, inputId = "GO_term", selected = "large ribosomal subunit",
                        label = "GO Term to Plot", choices = GO_term_description, server = TRUE, )
  
####Plotting ovary_map####
  output$ovary_map <- renderPlot({
    if(is.null(input$variable) | is.null(input$dataset)) {
      return()
    }
    #scale text off of tab size
    plotwidth <- session$clientData[["output_ovary_map_width"]]
    text_scale = plotwidth/375
    
    ovary_map_plot <<- ovary_map(data_set_to_plot = input$SeqDataset,
                                 gene_name_format = input$dataset, 
                                 displayTPM = input$displayTPM, 
                                 gene_of_interest = input$variable, 
                                 text_scale = text_scale, 
                                 graphic_to_generate = "map")
    ovary_map_plot
  })
  
  #Adding separate legend so that all legend values can always be displayed
  output$legend <- renderPlot({
    ovary_map_legend <<- ovary_map(graphic_to_generate = "legend")
    ovary_map_legend
  })
  
#### Plotting of heatmap ####
  output$heatPlot <- renderPlotly({
    source("server_modules/heat_map.R")
    DE_heatmap(data_set_to_plot = input$SeqDataset)
    heat_map_global
  })

#### Plotting of violinplot ####
output$violinPlot <- renderPlot({
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
  gene_violin_plot_global <<- gene_violin(data_set_to_plot = input$SeqDataset,
                                          genes_by_GO = input$violin_geneList_option, 
                                          GO_term = input$GO_term,
                                          gene_of_interest = input$Gene_interest_list,
                                          normalization = input$violin_normalization_option)
  gene_violin_plot_global
})
  
  # report function calls report.Rmd to knit an rmarkdown file to save data analysis
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Ovary_App_Report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport = file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file)
      })
})