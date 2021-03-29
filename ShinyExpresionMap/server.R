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
source("server_modules/ovary_map.R")

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
    selectInput("dataset", "Data set", as.list(data_sets))
  })
  
  # Selction for FBGNs or Gene Symbols
  observeEvent(input$dataset, {
    # Get the data set with the appropriate name
    dat <- get(input$dataset)
    updateSelectizeInput(session = session, inputId = "variable",
                    label = "Gene of Interest", choices = dat, server = TRUE)
  })
  
  # Output the data as a table for GO selection
  updateSelectizeInput(session = session, inputId = "GO_term",
                        label = "GO Term to Plot", choices = GO_term_description, server = TRUE)
  
####Plotting ovary_map####
  output$ovary_map <- renderPlot({
    if (is.null(input$variable)) {
      return()
    }
    #scale text off of tab size
    plotwidth <- session$clientData[["output_ovary_map_width"]]
    text_scale = plotwidth/260
    
    ovary_map_plot <<- ovary_map(gene_name_format = input$dataset, 
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
      heat
  })

#### Plotting of heatmap ####
output$violinPlot <- renderPlot({
  source("server_modules/violin_genes.R")
  if (is.null(input$GO_term)) {
    return()
  }
  gene_violin_plot_global <<- gene_violin(genes_by_GO = TRUE, GO_term = input$GO_term)
  gene_violin_plot_global
})
  
  # report function calls report.Rmd to knit an rmarkdown file to save data analysis
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
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