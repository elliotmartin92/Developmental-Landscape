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

ps = .libPaths()
data.seq = readRDS("preprocessed_seq_data.RDS") #data from preprocessed tpms (binned/organized)
shape.plot = readRDS("preprocessed_sf.RDS")
shape = readRDS("preloaded_shape.RDS")

FBID = data.seq$FBGN
Symbol = data.seq$symbol
data_sets <- c("FBID", "Symbol")

pal <- c(
  "Black" = "Black",
  "Very High" = "#FF0000",
  "High" = "#FF2B2B",
  "Med" = "#FF5656", 
  "Low" = "#FF8181", 
  "Very Low" = "#FFACAC",
  "None" = "#D6D6D6"
)


shinyServer(function(input, output, session, width) {
  width <- session$client_data$output_ap_plot_width
  session$onSessionEnded(function() {
    stopApp()
    })
  
  # Drop-down selection box for which data set
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Data set", as.list(data_sets))
  })
  
  # Check boxes
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
    
    # Get the data set with the appropriate name
    dat <- get(input$dataset)
    
    # Create the checkboxes and select them all by default
      selectInput("variable", "Gene of Interest #1", dat)
  })
  
  # Output the data
  output$data_table <- renderTable({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
    
    # Get the data set
    dat <- get(input$dataset)
    
    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    if (is.null(input$columns) || !(input$columns %in% names(dat)))
      return()
    
    # Keep the selected columns
    dat <- dat[, input$columns, drop = FALSE]
    
    # Return first 20 rows
    head(dat, 20)
    
  })
  output$distPlot <- renderPlot({
    if (is.null(input$variable)) {
      return()
    }
    if (input$dataset == "FBID") {
      all.colors = data.seq[data.seq$FBGN %in% input$variable, 13:17]
    }
    else{
      all.colors = data.seq[data.seq$symbol %in% input$variable, 13:17]
    }
    
    plotwidth <- session$clientData[["output_distPlot_width"]]
    text_scale = plotwidth/260
    
    shape.plot$FID_[c(18,25)] = all.colors[[1]]
    shape.plot$FID_[c(2,19)] = all.colors[[2]]
    shape.plot$FID_[c(20:23)] = all.colors[[2]]
    shape.plot$FID_[c(33)] = all.colors[[4]]

    dist_pl=ggplot(data = shape.plot)+
      geom_sf(aes(geometry=geometry, fill=`FID_`), color = "black")+
      scale_fill_manual(values = pal, name="Binned Expression")+
      theme_void()+
      theme(panel.grid.major = element_line(colour = "transparent"),
            panel.background = element_rect(fill = "transparent", colour = "transparent"),
            panel.border = element_rect(fill = "transparent", colour = "transparent"),
            legend.position = "none")
    
    if (input$displayTPM==FALSE){
      dist_pl <<- dist_pl
      dist_pl
      }
    else{
      if (input$dataset == "FBID") {
        TPMs = data.seq[data.seq$FBGN %in% input$variable, 7:11][1,]
      }
      else{
        TPMs = data.seq[data.seq$symbol %in% input$variable, 7:11][1,]
      }
      shape_centroids = st_centroid(shape)
      shape.x.y = data.frame(x=map_dbl(shape_centroids$geometry, 1), y=map_dbl(shape_centroids$geometry, 2))
      
      dist_pl = dist_pl+
        annotate("text", label=paste0(TPMs[1], "\nTPM"), x=shape.x.y[18,1], y=shape.x.y[18,2], size=text_scale)+
        annotate("text", label=paste0(TPMs[2], "\nTPM"), x=shape.x.y[19,1], y=shape.x.y[19,2], size=text_scale)+
        annotate("text", label=paste0(TPMs[3], " TPM"), x=shape.x.y[22,1]+.1, y=shape.x.y[22,2]-.5, size=text_scale)+
        annotate("segment", x=shape.x.y[22,1]-.5, xend=shape.x.y[22,1]+.7, y=shape.x.y[22,2]-.35, yend=shape.x.y[22,2]-.35)+
        annotate("text", label=paste0(TPMs[4], " TPM"), x=shape.x.y[33,1], y=shape.x.y[33,2]+.25, size=text_scale)
      dist_pl <<- dist_pl
      dist_pl
    }
  })
  legend.data = data.frame(Name = names(pal), Color = pal)
  legend.data.cull = legend.data[-1,]
  legend.data.cull$Name = factor(legend.data.cull$Name, 
                            levels = c("None", "Very Low", "Low", "Med", "High", "Very High")) 
  
  output$legend <- renderPlot({
    dist_leg = ggplot(legend.data.cull)+
      geom_area(aes(x=1, y=1, fill=Name))+
      scale_fill_manual(values = pal, name="Binned\nExpression")+
      theme_void()+
      guides(fill = guide_legend(nrow = 1))+
      theme(legend.position = "top",
            legend.text = element_text(size=13),
            legend.title = element_text(size=16))
    dist_leg
    dist_leg <<- dist_leg
  })
  output$heatPlot <- renderPlotly({
    changing_genes = readRDS("developmentally_regulated_gene_list.RDS")
    modls = function(x){log2(x+1)}
    heat.data = data.seq %>%
      filter(FBGN %in% changing_genes) %>%
      dplyr::select(MeanTPM_TKV_input,
             MeanTPM_BamRNAi_input,
             MeanTPM_BamHSbam_input,
             MeanTPM_youngWT_input,
             MeanTPM_pelo_cyo_input) %>% 
      modls() %>%
      data.frame()
      rownames(heat.data) = data.seq %>% filter(FBGN %in% changing_genes) %>% pull(FBGN)
      heat = heatmaply(heat.data,
                showticklabels = c(TRUE, FALSE),
                seriate = "none")
      heat
      heat <<- heat
  })
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file)
      })
})