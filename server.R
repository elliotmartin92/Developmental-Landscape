library(shiny)
library(shinyWidgets)
library(rgdal)
library(sf)
library(ggplot2)
library(ggmap)
library(plotly)

data.seq = read.csv("www/Dev_TPMS.csv", stringsAsFactors = FALSE)
FBID = data.seq$V1

Symbol = data.seq$symbol
data.seq = data.seq[-6]
data_sets <- c("FBID", "Symbol")

shape <- read_sf(dsn = "www/germPoly/.", layer = "germPoly")
data.seq$test = 1:length(data.seq$V1)
data.seq$TKVbin1 = cut(as.numeric(data.seq$TKV), breaks = c(0,10,100,250,1000,2500,100000), 
                       labels=c("NA","VeryLow","Low","Med","High","VeryHigh"))

data.seq$Bambin1 = cut(as.numeric(data.seq$Bam), breaks = c(0,10,100,250,1000,2500,100000), 
                       labels=c("NA","VeryLow","Low","Med","High","VeryHigh"))

data.seq$Cystbin1 = cut(as.numeric(data.seq$bamhs.bam1), breaks = c(0,10,100,250,1000,2500,100000), 
                        labels=c("NA","VeryLow","Low","Med","High","VeryHigh"))

data.seq$Virginbin1 = cut(as.numeric(data.seq$VirginNG4), breaks = c(0,10,100,250,1000,2500,100000), 
                          labels=c("NA","VeryLow","Low","Med","High","VeryHigh"))

# shape$FID_ = seq(from = 1, to = 340, by = 10)
shape$FID_[1] = "NR"
shape$FID_[shape$LineWt == 35] = "NA"
shape$FID_[shape$LineWt == 106] = "NA"
shape$FID_[shape$Color == 7] = "Black"
shape$FID_[shape$FID_ == 0] = "NA"
# shape$FID_[2] = "Med"
shape$FID_ = factor(shape$FID_, c("NR", "NA", "VeryLow", "Low", "Med", "High", "VeryHigh", "Black"))
pal <- c(
  "Black" = "Black",
  "VeryHigh" = "#FF0000",
  "High" = "#FF2B2B",
  "Med" = "#FF5656", 
  "Low" = "#FF8181", 
  "VeryLow" = "#FFACAC",
  "NA" = "#D6D6D6",
  "NR" = "#FFFFFF"
)

shinyServer(function(input, output) {
  
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
    if (is.null(input$variable))
      return()
    if (input$dataset == "FBID") {
    color.GSC = as.character(data.seq[data.seq$V1 %in% input$variable, 9])
    color.CB = as.character(data.seq[data.seq$V1 %in% input$variable, 10])
    color.Cyst = as.character(data.seq[data.seq$V1 %in% input$variable, 11])
    color.Virgin = as.character(data.seq[data.seq$V1 %in% input$variable, 12])
    
    TPM.GSC = as.character(data.seq[data.seq$V1 %in% input$variable, 3])
    TPMs = c(1,2,3,4)
    }
    else{
      color.GSC = as.character(data.seq[data.seq$symbol %in% input$variable, 9])
      color.CB = as.character(data.seq[data.seq$symbol %in% input$variable, 10])
      color.Cyst = as.character(data.seq[data.seq$symbol %in% input$variable, 11])
      color.Virgin = as.character(data.seq[data.seq$symbol %in% input$variable, 12]) 
    }
    shape.plot = data.frame(shape)
    shape.plot$TPMs = seq(1:34)
    shape.plot$FID_[c(18,25)] = color.GSC
    shape.plot$FID_[c(2,19)] = color.CB
    shape.plot$FID_[c(20:23)] = color.Cyst
    shape.plot$FID_[c(33)] = color.Virgin
    ggplot(data = shape.plot)+
      geom_sf(aes(geometry=geometry, fill=`FID_`, alpha=TPMs), color = "black")+
      scale_fill_manual(values = pal)+
      theme_void()+
      theme(panel.grid.major = element_line(colour = "transparent"))
  })
  output$sel = renderText(as.character(input$plot_hover))
})