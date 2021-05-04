merge_plot = readRDS("Preprocessed_data/preprocessed_sf.RDS") #data to populate shape file for distPlot
shape = readRDS("Preprocessed_data/preloaded_shape.RDS") #shape file for distPlot
data.seq = readRDS("Preprocessed_data/preprocessed_RNA_seq_data.RDS")

#setting some variables for distPlot that must be declared outside of the server function
FBID = data.seq$FBGN
Symbol = data.seq$symbol

pal <- c(
  "Black" = "Black",
  "Very High" = "#00ffff",
  "High" = "#66ffff",
  "Med" = "#99ffff", 
  "Low" = "#ccffff", 
  "Very Low" = "#d9ffff",
  "None" = "#bdbdbd",
  "White" = "White",
  "line" = "Black")

bulk_bins = c("TKVbin1", "Bambin1", "Cystbin1", "Virginbin1")
sc_seq_bins = c("bin_GSC/CB/2-cc",
                "bin_4-cc",
                "bin_8-cc",
                "bin_16-cc.2a.I",
                "bin_16-cc.2a.II",
                "bin_16-cc.2ab",
                "bin_16-cc.2b",
                "bin_16-cc.3",
                "bin_St2")

ovary_map = function(data_set_to_plot="Input_seq", gene_name_format="Symbol", displayTPM=TRUE, display_stage_labels=TRUE,
                     gene_of_interest="RpS19b", 
                     text_scale=10, graphic_to_generate){
  if(data_set_to_plot=="Input_seq"){
      data.seq = readRDS("Preprocessed_data/preprocessed_RNA_seq_data.RDS") #data from preprocessed tpms (binned/organized)
      expression_unit = "TPM"
    }else if(data_set_to_plot=="Polysome_seq"){
      data.seq = readRDS("Preprocessed_data/preprocessed_polysome_seq_data.RDS") #data from preprocessed TE (binned/organized)
      expression_unit = "TE"
    }else if(data_set_to_plot=="Single_cell_seq"){
      data.seq = readRDS("Preprocessed_data/preprocessed_single_cell_seq_data.RDS") #data from preprocessed SC-seq (binned/organized)
      expression_unit = "NC"
    }else{
      return("Missing data_set_to_plot")
    }
  if (graphic_to_generate == "legend") {
    #Adding separate legend so that all legend values can always be displayed
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
    return(dist_leg)
  }else if (graphic_to_generate == "map"){
    if(data_set_to_plot=="Input_seq" | data_set_to_plot=="Polysome_seq"){
      if (gene_name_format == "FBID") {
        all.colors = data.seq[data.seq$FBGN %in% gene_of_interest, names(data.seq) %in% bulk_bins]
      }else{
        all.colors = data.seq[data.seq$symbol %in% gene_of_interest, names(data.seq) %in% bulk_bins]
      }
    }else if (data_set_to_plot=="Single_cell_seq") {
        all.colors = data.seq[data.seq$symbol %in% gene_of_interest, names(data.seq) %in% sc_seq_bins]
    }
    #mapping different features in shape to have proper base colors
    if (data_set_to_plot == "Input_seq" | data_set_to_plot == "Polysome_seq") {
      cysts_stages = c("2CC", "4CC", "8CC", "16CC_2A1", "16CC_2A2", "16CC_2AB", "16CC_2B", "16CC_3")
      merge_plot$color[merge_plot$cell_type=="GSC"] = all.colors[[1]]
      merge_plot$color[merge_plot$cell_type=="CB"] = all.colors[[2]]
      merge_plot$color[merge_plot$cell_type %in% cysts_stages] = all.colors[[3]]
      merge_plot$color[merge_plot$cell_type=="ST2"] = all.colors[[4]]
    }else if (data_set_to_plot=="Single_cell_seq"){
      merge_plot$color[merge_plot$cell_type=="GSC"] = all.colors[[1]]
      merge_plot$color[merge_plot$cell_type=="CB"] = all.colors[[1]]
      merge_plot$color[merge_plot$cell_type=="2CC"] = all.colors[[1]]
      merge_plot$color[merge_plot$cell_type=="4CC"] = all.colors[[2]]
      merge_plot$color[merge_plot$cell_type=="8CC"] = all.colors[[3]]
      merge_plot$color[merge_plot$cell_type=="16CC_2A1"] = all.colors[[4]]
      merge_plot$color[merge_plot$cell_type=="16CC_2A2"] = all.colors[[5]]
      merge_plot$color[merge_plot$cell_type=="16CC_2AB"] = all.colors[[6]]
      merge_plot$color[merge_plot$cell_type=="16CC_2B"] = all.colors[[7]]
      merge_plot$color[merge_plot$cell_type=="16CC_3"] = all.colors[[8]]
      merge_plot$color[merge_plot$cell_type=="ST2"] = all.colors[[9]]
    }

    #plotting distplot
    dist_pl = merge_plot %>%
      st_as_sf() %>% 
      mutate(region_index = row_number()) %>%
      mutate(color = color %>% forcats::fct_reorder(-region_index)) %>%
      ggplot()+
      geom_sf(aes(geometry=geometry, fill=color), color = "grey50")+
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
      if (data_set_to_plot=="Input_seq"){
        if (gene_name_format == "FBID") {
          TPMs = data.seq[data.seq$FBGN %in% gene_of_interest, 6:9][1,]
        }
        else{
          TPMs = data.seq[data.seq$symbol %in% gene_of_interest, 6:9][1,]
        }
      }else if(data_set_to_plot=="Polysome_seq"){
        if (gene_name_format == "FBID") {
          TPMs = data.seq[data.seq$FBGN %in% gene_of_interest, 19:22][1,]
        }
        else{
          TPMs = data.seq[data.seq$symbol %in% gene_of_interest, 19:22][1,]
        }
      }else if(data_set_to_plot=="Single_cell_seq"){
        if (gene_name_format == "FBID") {
          TPMs = signif(data.seq[data.seq$FBGN %in% gene_of_interest, 19:22][1,], 2)
        }else{
          TPMs = signif(data.seq[data.seq$symbol %in% gene_of_interest, 2:10][1,], 2)
        }
      }
      #adding TPM values to the proper place on the shape
      shape_centroids = st_centroid(shape)
      shape_ymin = st_bbox(shape$geometry)[[2]]
      shape_ymax = st_bbox(shape$geometry)[[4]]
      shape.x.y = data.frame(x=map_dbl(shape_centroids$geometry, 1), y=map_dbl(shape_centroids$geometry, 2))
      if (data_set_to_plot == "Input_seq" | data_set_to_plot == "Polysome_seq") {
      dist_pl = dist_pl+
        annotate("text", label=paste0(TPMs[1], "\n", expression_unit), x=shape.x.y[24,1], y=shape.x.y[24,2], size=text_scale)+
        annotate("text", label=paste0(TPMs[2], "\n", expression_unit), x=shape.x.y[26,1], y=shape.x.y[26,2], size=text_scale)+
        annotate("text", label=paste0(TPMs[3], " ", expression_unit), x=shape.x.y[27,1]+2.34,  y=shape_ymin-.1, size=text_scale)+
        annotate("segment", x=shape.x.y[27,1], xend=shape.x.y[33,1]+.7, y=shape_ymin-.17, yend=shape_ymin-0.17)+
        annotate("text", label=paste0(TPMs[4], " ", expression_unit), x=shape.x.y[23,1], y=shape.x.y[23,2]+.25, size=text_scale)
      }else if (data_set_to_plot=="Single_cell_seq"){
        dist_pl = dist_pl+
          annotate("text", label=paste0(TPMs[1], " ", expression_unit), 
                   x=st_bbox(shape[24,])[[1]]+(st_bbox(shape[27,])[[3]]-st_bbox(shape[24,])[[1]])/2,
                   y=shape_ymin+0.27, size=text_scale)+
          annotate("segment", x=st_bbox(shape[24,])[[1]], xend=st_bbox(shape[27,])[[3]], y=shape_ymin+0.2, yend=shape_ymin+0.2)+
          annotate("text", label=paste0(TPMs[2], "\n", expression_unit), x=shape.x.y[28,1], y=shape.x.y[28,2], size=text_scale)+
          annotate("text", label=paste0(TPMs[3], "\n", expression_unit), x=shape.x.y[29,1], y=shape.x.y[29,2], size=text_scale)+
          annotate("text", label=paste0(TPMs[4], "\n", expression_unit), x=shape.x.y[30,1], y=shape.x.y[30,2], size=text_scale)+
          annotate("text", label=paste0(TPMs[5], "\n", expression_unit), x=shape.x.y[31,1], y=shape.x.y[31,2], size=text_scale)+
          annotate("text", label=paste0(TPMs[6], "\n", expression_unit), x=shape.x.y[32,1], y=shape.x.y[32,2], size=text_scale)+
          annotate("text", label=paste0(TPMs[7], "\n", expression_unit), x=shape.x.y[34,1], y=shape.x.y[34,2], size=text_scale)+
          annotate("text", label=paste0(TPMs[8], "\n", expression_unit), x=shape.x.y[33,1], y=shape.x.y[33,2], size=text_scale)+
          annotate("text", label=paste0(TPMs[9], "\n", expression_unit), x=shape.x.y[23,1], y=shape.x.y[23,2], size=text_scale)
      }
    }
    if (display_stage_labels==FALSE){ #switch for label display
    }else{
      if (data_set_to_plot == "Input_seq" | data_set_to_plot == "Polysome_seq") {
        dist_pl = dist_pl+
          # TKV cell label
          annotate("text", label="UAS-Tkv", x=shape.x.y[24,1]-0.24, y=shape_ymin-0.24, size=text_scale)+
          annotate("segment", x=shape.x.y[24,1], xend=shape.x.y[24,1]-0.24, 
                 y=st_bbox(shape$geometry[24])[[2]], yend=shape_ymin-0.17)+
          # bamRNAi cell label
          annotate("text", label="bam RNAi", x=shape.x.y[26,1]-0.24, y=shape_ymax+0.24, size=text_scale)+
          annotate("segment", x=shape.x.y[26,1], xend=shape.x.y[26,1]-0.24, 
                   y=st_bbox(shape$geometry[26])[[4]], yend=shape_ymax+0.17)+
          # bamHSbam line label (redraws line in case)
          annotate("text", label="bamRNAi HS-bam", x=shape.x.y[27,1]+2.34, y=shape_ymin-0.24, size=text_scale)+
          annotate("segment", x=shape.x.y[27,1], xend=shape.x.y[33,1]+0.7, y=shape_ymin-0.17, yend=shape_ymin-0.17)+
          # youngWT cell label 
          annotate("text", label="young WT", x=shape.x.y[23,1]+0.24, y=shape_ymax+0.24, size=text_scale)+
          annotate("segment", x=shape.x.y[23,1], xend=shape.x.y[23,1]+0.24, y=st_bbox(shape$geometry[23,1])[[4]], yend=shape_ymax+0.17)

        
      }else if (data_set_to_plot=="Single_cell_seq"){
        dist_pl = dist_pl+
          annotate("text", label="GSC/CB/2CC", 
                   x=st_bbox(shape[24,])[[1]]+(st_bbox(shape[27,])[[3]]-st_bbox(shape[24,])[[1]])/2,
                   y=shape_ymin+0.13, size=text_scale)+
          annotate("segment", x=st_bbox(shape[24,])[[1]], xend=st_bbox(shape[27,])[[3]], y=shape_ymin+0.2, yend=shape_ymin+0.2)+
          annotate("text", label="4-CC", x=shape.x.y[28,1], y=shape_ymax+0.24, size=text_scale)+
          annotate("text", label="8-CC", x=shape.x.y[29,1], y=shape_ymin-0.24, size=text_scale)+
          annotate("text", label="16-CC 2aI", x=shape.x.y[30,1], y=shape_ymax+0.24, size=text_scale)+
          annotate("text", label="16-CC 2aII", x=shape.x.y[31,1], y=shape_ymin-0.24, size=text_scale)+
          annotate("text", label="16-CC 2ab", x=shape.x.y[32,1], y=shape_ymax+0.24, size=text_scale)+
          annotate("text", label="16-CC 2b", x=shape.x.y[34,1], y=shape_ymin-0.24, size=text_scale)+
          annotate("text", label="16-CC 3", x=shape.x.y[33,1], y=shape_ymax+0.24, size=text_scale)+
          annotate("text", label="ST2", x=shape.x.y[23,1], y=shape_ymin-0.24, size=text_scale)+
          
          annotate("segment", x=shape.x.y[28,1], xend=shape.x.y[28,1],
                   y=st_bbox(shape$geometry[28])[[4]], yend=shape_ymax+0.17)+
          annotate("segment", x=shape.x.y[29,1], xend=shape.x.y[29,1],
                   y=st_bbox(shape$geometry[29])[[2]], yend=shape_ymin-0.17)+
          annotate("segment", x=shape.x.y[30,1], xend=shape.x.y[30,1],
                   y=st_bbox(shape$geometry[30])[[4]], yend=shape_ymax+0.17)+
          annotate("segment", x=shape.x.y[31,1], xend=shape.x.y[31,1],
                   y=st_bbox(shape$geometry[31])[[2]], yend=shape_ymin-0.17)+
          annotate("segment", x=shape.x.y[32,1], xend=shape.x.y[32,1],
                   y=st_bbox(shape$geometry[32])[[4]], yend=shape_ymax+0.17)+
          annotate("segment", x=shape.x.y[34,1], xend=shape.x.y[34,1],
                   y=st_bbox(shape$geometry[33])[[2]], yend=shape_ymin-0.17)+
          annotate("segment", x=shape.x.y[33,1], xend=shape.x.y[33,1],
                   y=st_bbox(shape$geometry[34])[[4]], yend=shape_ymax+0.17)+
          annotate("segment", x=shape.x.y[23,1], xend=shape.x.y[23,1],
                   y=st_bbox(shape$geometry[23])[[2]], yend=shape_ymin-0.17)
          
      }
    }
   return(dist_pl)
    }else{
      message("graphic_to_generate should be of type 'map', or 'legend'")
  }
}