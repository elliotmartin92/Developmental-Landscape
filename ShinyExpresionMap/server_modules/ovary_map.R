data.seq = readRDS("preprocessed_seq_data.RDS") #data from preprocessed tpms (binned/organized)
shape.plot = readRDS("preprocessed_sf.RDS") #data to populate shape file for distPlot
shape = readRDS("preloaded_shape.RDS") #shape file for distPlot

#setting some variables for distPlot that must be declared outside of the server function

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

ovary_map = function(gene_name_format, displayTPM, gene_of_interest, text_scale){
  
  if (gene_name_format == "FBID") {
    all.colors = data.seq[data.seq$FBGN %in% gene_of_interest, 13:17]
  }else{
    all.colors = data.seq[data.seq$symbol %in% gene_of_interest, 13:17]
  }
  
  #mapping different features in shape to have proper base colors
  shape.plot$FID_[c(18,25)] = all.colors[[1]]
  shape.plot$FID_[c(2,19)] = all.colors[[2]]
  shape.plot$FID_[c(20:23)] = all.colors[[2]]
  shape.plot$FID_[c(33)] = all.colors[[4]]
  
  #plotting distplot
  dist_pl=ggplot(data = shape.plot)+
    geom_sf(aes(geometry=geometry, fill=`FID_`), color = "black")+
    scale_fill_manual(values = pal, name="Binned Expression")+
    theme_void()+
    theme(panel.grid.major = element_line(colour = "transparent"),
          panel.background = element_rect(fill = "transparent", colour = "transparent"),
          panel.border = element_rect(fill = "transparent", colour = "transparent"),
          legend.position = "none")
  dist_pl_rmd <<- dist_pl
  if (displayTPM==FALSE){ #switch for TPM display
  }
  else{
    if (gene_name_format == "FBID") {
      TPMs = data.seq[data.seq$FBGN %in% gene_of_interest, 7:11][1,]
    }
    else{
      TPMs = data.seq[data.seq$symbol %in% gene_of_interest, 7:11][1,]
    }
    
    #adding TPM values to the proper place on the shape
    shape_centroids = st_centroid(shape)
    shape.x.y = data.frame(x=map_dbl(shape_centroids$geometry, 1), y=map_dbl(shape_centroids$geometry, 2))
    
    dist_pl = dist_pl+
      annotate("text", label=paste0(TPMs[1], "\nTPM"), x=shape.x.y[18,1], y=shape.x.y[18,2], size=text_scale)+
      annotate("text", label=paste0(TPMs[2], "\nTPM"), x=shape.x.y[19,1], y=shape.x.y[19,2], size=text_scale)+
      annotate("text", label=paste0(TPMs[3], " TPM"), x=shape.x.y[22,1]+.1, y=shape.x.y[22,2]-.5, size=text_scale)+
      annotate("segment", x=shape.x.y[22,1]-.5, xend=shape.x.y[22,1]+.7, y=shape.x.y[22,2]-.35, yend=shape.x.y[22,2]-.35)+
      annotate("text", label=paste0(TPMs[4], " TPM"), x=shape.x.y[33,1], y=shape.x.y[33,2]+.25, size=text_scale)
  }
  #Adding seperate legend so that all legend values can always be displayed
  legend.data = data.frame(Name = names(pal), Color = pal)
  legend.data.cull = legend.data[-1,]
  legend.data.cull$Name = factor(legend.data.cull$Name, 
                                 levels = c("None", "Very Low", "Low", "Med", "High", "Very High")) 
  
    dist_leg = ggplot(legend.data.cull)+
      geom_area(aes(x=1, y=1, fill=Name))+
      scale_fill_manual(values = pal, name="Binned\nExpression")+
      theme_void()+
      guides(fill = guide_legend(nrow = 1))+
      theme(legend.position = "top",
            legend.text = element_text(size=13),
            legend.title = element_text(size=16))
    
 return(list(dist_pl, dist_leg))
}