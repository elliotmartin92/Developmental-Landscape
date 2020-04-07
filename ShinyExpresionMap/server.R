library(shiny)
library(shinyWidgets)
library(rgdal)
library(sf)
library(ggplot2)
library(ggmap)
library(cowplot)
library(purrr)

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

shinyServer(function(input, output, session) {
  
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
    shape.plot$FID_[c(18,25)] = all.colors[[1]]
    shape.plot$FID_[c(2,19)] = all.colors[[2]]
    shape.plot$FID_[c(20:23)] = all.colors[[2]]
    shape.plot$FID_[c(33)] = all.colors[[4]]

    p=ggplot(data = shape.plot)+
      geom_sf(aes(geometry=geometry, fill=`FID_`), color = "black")+
      scale_fill_manual(values = pal, name="Binned Expression")+
      theme_void()+
      theme(panel.grid.major = element_line(colour = "transparent"),
            panel.background = element_rect(fill = "transparent"),
            legend.position = "none")
    
    if (input$displayTPM==FALSE){p}
    else{
      if (input$dataset == "FBID") {
        TPMs = data.seq[data.seq$FBGN %in% input$variable, 7:11][1,]
      }
      else{
        TPMs = data.seq[data.seq$symbol %in% input$variable, 7:11][1,]
      }
      shape_centroids = st_centroid(shape)
      shape.x.y = data.frame(x=map_dbl(shape_centroids$geometry, 1), y=map_dbl(shape_centroids$geometry, 2))
      
      p+
        annotate("text", label=paste0(TPMs[1], "\nTPM"), x=shape.x.y[18,1], y=shape.x.y[18,2], size=4)+
        annotate("text", label=paste0(TPMs[2], "\nTPM"), x=shape.x.y[19,1], y=shape.x.y[19,2], size=4)+
        annotate("text", label=paste0(TPMs[3], " TPM"), x=shape.x.y[22,1]+.1, y=shape.x.y[22,2]-.5, size=4)+
        annotate("segment", x=shape.x.y[22,1]-.5, xend=shape.x.y[22,1]+.7, y=shape.x.y[22,2]-.35, yend=shape.x.y[22,2]-.35)+
        annotate("text", label=paste0(TPMs[4], " TPM"), x=shape.x.y[33,1], y=shape.x.y[33,2]+.25, size=4)
    }
  }, height = 200, width = 600)
  legend.data = data.frame(Name = names(pal), Color = pal)
  legend.data.cull = legend.data[-1,]
  legend.data.cull$Name = factor(legend.data.cull$Name, 
                            levels = c("None", "Very Low", "Low", "Med", "High", "Very High")) 
  output$legend <- renderPlot({
    l = ggplot(legend.data.cull)+
      geom_area(aes(x=1, y=1, fill=Name))+
      scale_fill_manual(values = pal, name="Binned Expression")+
      theme_void()+
      theme(legend.position = "top") +
      guides(fill = guide_legend(nrow = 1))
    l
  })
})