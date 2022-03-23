merge_plot = readRDS("Preprocessed_data/preprocessed_sf.RDS") #data to populate shape file for distPlot
shape = readRDS("Preprocessed_data/preloaded_shape.RDS") #shape file for distPlot
data.seq = readRDS("Preprocessed_data/preprocessed_RNA_seq_data.RDS")

#setting some variables for distPlot that must be declared outside of the server function
FBID = data.seq$FBGN
Symbol = sort(data.seq$Symbol)

pal <- c(
  "Black" = "Black",
  "Very High" = "#00ffff",
  "High" = "#66ffff",
  "Med" = "#99ffff", 
  "Low" = "#ccffff", 
  "Very Low" = "#e6ffff",
  "None" = "#e3e3e3",
  "White" = "White",
  "line" = "Black")

group_geometry = shape %>%  
  group_by(cell_type) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  mutate(centroid = st_centroid(geometry))

st_bbox_by_feature = function(x) {
  x = st_geometry(x)
  f <- function(y) (st_bbox(y))
  lapply(x, f)
}

group_geometry_bounding = tibble(cell_type=group_geometry$cell_type, 
                                 x=map_dbl(group_geometry$centroid, 1), 
                                 y=map_dbl(group_geometry$centroid, 2),
                                 bbox=st_bbox_by_feature(group_geometry$geometry))

ovary_map = function(data_set_to_plot="Input_seq", 
                     gene_name_format="Symbol", 
                     displayTPM=TRUE, 
                     display_stage_labels=TRUE,
                     display_title=FALSE,
                     gene_of_interest="RpS19b", 
                     text_scale=10, 
                     map_line_width=0.5,
                     graphic_to_generate){
  if(data_set_to_plot=="Input_seq"){
    data.seq = readRDS("Preprocessed_data/preprocessed_RNA_seq_data.RDS") #data from preprocessed tpms (binned/organized)
    expression_unit = "TPM"
    bins = c("TKVbin1", "Bambin1", "Cystbin1", "Virginbin1")
    title_plot = TeX(paste("$Bulk\\, mRNAseq:\\, \\textit{", gene_of_interest, "}$"))
  }else if(data_set_to_plot=="Polysome_seq"){
    data.seq = readRDS("Preprocessed_data/preprocessed_polysome_seq_data.RDS") #data from preprocessed TE (binned/organized)
    expression_unit = "TE"
    bins = c("TKVbin1", "Bambin1", "Cystbin1", "Virginbin1")
    title_plot = TeX(paste("$Bulk\\, Polysome-seq:\\, \\textit{", gene_of_interest, "}$"))
  }else if(data_set_to_plot=="Single_cell_seq_germline"){
    data.seq = readRDS("Preprocessed_data/preprocessed_single_cell_seq_data_GC.RDS") #data from preprocessed germline SC-seq (binned/organized)
    expression_unit = "NE"
    bins = c("bin_GSC/CB/2-cc",
             "bin_4-cc",
             "bin_8-cc",
             "bin_16-cc.2a.I",
             "bin_16-cc.2a.II",
             "bin_16-cc.2ab",
             "bin_16-cc.2b",
             "bin_16-cc.3",
             "bin_St2")
    title_plot = TeX(paste("$Germline\\, scRNA-seq:\\, \\textit{", gene_of_interest, "}$"))
  }else if(data_set_to_plot=="Single_cell_seq_soma"){
    data.seq = readRDS("Preprocessed_data/preprocessed_single_cell_seq_data_germarium_soma.RDS") #data from preprocessed somaSC-seq (binned/organized)
    expression_unit = "NE"
    bins = c("bin_TF/CC",
             "bin_aEC",
             "bin_cEC",
             "bin_pEC",
             "bin_FSC/pre-FC",
             "bin_pre-stalk",
             "bin_stalk",
             "bin_polar")
    title_plot = TeX(paste("$Soma\\, scRNA-seq:\\, \\textit{", gene_of_interest, "}$"))
    
  }else{
    return("Missing data_set_to_plot")
  }
  if (graphic_to_generate == "legend") {
    #Adding separate legend so that all legend values can always be displayed
    legend.data = data.frame(Name = names(pal), Color = pal)
    legend.data.cull = legend.data[-c(1,8,9), ]
    legend.data.cull$Name = factor(legend.data.cull$Name, 
                                   levels = c("None", "Very Low", "Low", "Med", "High", "Very High")) 
    
    dist_leg = ggplot(legend.data.cull)+
      geom_area(aes(x=1, y=1, fill=Name))+
      scale_fill_manual(values = pal, name="Binned Expression")+
      theme_void()+
      guides(fill = guide_legend(nrow = 1))+
      theme(legend.position = "top",
            legend.text = element_text(size=text_scale),
            legend.title = element_text(size=text_scale*1.2))
    return(dist_leg)
  }else if (graphic_to_generate == "map"){
    if (gene_name_format == "FBID") {
      all.colors = data.seq[data.seq$FBGN %in% gene_of_interest, names(data.seq) %in% bins]
    }else{
      all.colors = data.seq[data.seq$Symbol %in% gene_of_interest, names(data.seq) %in% bins]
    }
    #### mapping different features in shape to have proper base colors #### 
    if (data_set_to_plot == "Input_seq" | data_set_to_plot == "Polysome_seq") {
      cysts_stages = c("2CC", "4CC", "8CC", "16CC_2A1", "16CC_2A2", "16CC_2AB", "16CC_2B", "16CC_3")
      merge_plot$color[merge_plot$cell_type=="GSC"] = all.colors[[1]]
      merge_plot$color[merge_plot$cell_type=="CB"] = all.colors[[2]]
      merge_plot$color[merge_plot$cell_type %in% cysts_stages] = all.colors[[3]]
      merge_plot$color[merge_plot$cell_type=="ST2"] = all.colors[[4]]
    }else if (data_set_to_plot=="Single_cell_seq_germline"){
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
    }else if (data_set_to_plot=="Single_cell_seq_soma"){
      merge_plot$color[merge_plot$cell_type=="TF/CC"] = all.colors[[1]]
      merge_plot$color[merge_plot$cell_type=="EC_a"] = all.colors[[2]]
      merge_plot$color[merge_plot$cell_type=="EC_c"] = all.colors[[3]]
      merge_plot$color[merge_plot$cell_type=="EC_p"] = all.colors[[4]]
      merge_plot$color[merge_plot$cell_type=="FSC"] = all.colors[[5]]
      merge_plot$color[merge_plot$cell_type=="pre-stalk"] = all.colors[[6]]
      merge_plot$color[merge_plot$cell_type=="stalk"] = all.colors[[7]]
      merge_plot$color[merge_plot$cell_type=="polar"] = all.colors[[8]]
    }
    
    #### initial plotting distplot ####
    dist_pl = merge_plot %>%
      st_as_sf() %>% 
      arrange(region) %>% 
      mutate(region_index = row_number()) %>%
      mutate(color = color %>% forcats::fct_reorder(-region_index)) %>%
      filter(region != "background") %>%
      ggplot()+
      geom_sf(aes(geometry=geometry, fill=color), size = map_line_width, color = "grey50")+
      scale_fill_manual(values = pal, name="Binned Expression")+
      theme_void()+
      theme(panel.grid.major = element_line(colour = "transparent"),
            panel.background = element_rect(fill = "transparent", colour = "transparent"),
            panel.border = element_rect(fill = "transparent", colour = "transparent"),
            legend.position = "none")
    dist_pl_rmd <<- dist_pl
    
    #### add title ####
    if (display_title == TRUE) {
      dist_pl = dist_pl+
        ggtitle(title_plot)+
        theme(plot.title = element_text(size = text_scale*ggplot2::.pt, face = "bold", hjust = 0.5))
    }
    
    #### labeling map ####
    # fetch parameters required for either type of label
    if (displayTPM==TRUE | display_stage_labels==TRUE){
      shape_centroids = st_centroid(shape)
      shape_ymin = st_bbox(shape$geometry)[[2]]
      shape_ymax = st_bbox(shape$geometry)[[4]]
      shape.x.y = data.frame(x=map_dbl(shape_centroids$geometry, 1), y=map_dbl(shape_centroids$geometry, 2), shape$cell_type)
      
      # Segments needed for TPM or stage label
      if (data_set_to_plot == "Input_seq" | data_set_to_plot == "Polysome_seq"){
        dist_pl = dist_pl+
          # UAS-tkv cell line
          annotate("segment", x=shape.x.y[1,1], xend=shape.x.y[1,1]-0.47, 
                   y=st_bbox(shape$geometry[1])[[2]], yend=shape_ymin-0.17)+
          # bamRNAi cell line
          annotate("segment", x=shape.x.y[3,1], xend=shape.x.y[3,1]-0.30, 
                   y=st_bbox(shape$geometry[3])[[4]], yend=shape_ymax+0.17)+
          # bamHSbam  line
          annotate("segment", x=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="2CC"][[1]][1], 
                   xend=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="16CC_3"][[1]][3], 
                   y=shape_ymin-0.385, yend=shape_ymin-0.385)
          # youngWT cell line 
          annotate("segment", x=shape.x.y[12,1], xend=shape.x.y[12,1]+0.30, y=st_bbox(shape$geometry[12,1])[[4]], yend=shape_ymax+0.17)
      
      }else if (data_set_to_plot == "Single_cell_seq_germline"){
      # GSC/CB/2CC
        dist_pl = dist_pl+
        annotate("segment", x=st_bbox(shape[1,])[[1]], xend=st_bbox(shape[4,])[[3]], 
                 y=shape_ymin+0.06, yend=shape_ymin+0.06)
        
      }else if (data_set_to_plot == "Single_cell_seq_soma"){
        dist_pl = dist_pl+
        annotate("segment", x=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="TF/CC"][[1]][1], 
                 xend=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="TF/CC"][[1]][3], 
                 y=shape_ymin+0.45, yend=shape_ymin+0.45)+
          
          annotate("segment", x=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_a"][[1]][1], 
                   xend=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_a"][[1]][3], 
                   y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_a"][[1]][4]+.24, 
                   yend=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_a"][[1]][4]+.24)+
          
          annotate("segment", x=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_c"][[1]][1], 
                   xend=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_c"][[1]][3], 
                   y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_c"][[1]][2]-.24, 
                   yend=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_c"][[1]][2]-.24)+
          
          annotate("segment", x=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_p"][[1]][1], 
                   xend=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_p"][[1]][3], 
                   y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_p"][[1]][4]+.24, 
                   yend=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_p"][[1]][4]+.24)+
          
          annotate("segment", x=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="FSC"][[1]][1], 
                   xend=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="FSC"][[1]][3], 
                   y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="FSC"][[1]][2]-.24, 
                   yend=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="FSC"][[1]][2]-.24)+
          
          annotate("segment", x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="pre-stalk"], 
                   xend=group_geometry_bounding$x[group_geometry_bounding$cell_type=="pre-stalk"], 
                   y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="pre-stalk"][[1]][4], 
                   yend=shape_ymax+0.07)+
          
          annotate("segment", x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="polar"], 
                   xend=group_geometry_bounding$x[group_geometry_bounding$cell_type=="polar"], 
                   y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="polar"][[1]][4], 
                   yend=shape_ymax+0.07)+
          
          annotate("segment", x=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="stalk"][[1]][1], 
                   xend=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="stalk"][[1]][3], 
                   y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="stalk"][[1]][4]+.24, 
                   yend=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="stalk"][[1]][4]+.24)
        }
      
    }
    if (displayTPM==FALSE){ #switch for TPM display
    }else{
      if (data_set_to_plot=="Input_seq"){
        if (gene_name_format == "FBID") {
          TPMs = data.seq[data.seq$FBGN %in% gene_of_interest, 6:9][1,]
        }else{
          TPMs = data.seq[data.seq$Symbol %in% gene_of_interest, 6:9][1,]
        }
      }else if(data_set_to_plot=="Polysome_seq"){
        if (gene_name_format == "FBID") {
          TPMs = data.seq[data.seq$FBGN %in% gene_of_interest, 19:22][1,]
        }else{
          TPMs = data.seq[data.seq$Symbol %in% gene_of_interest, 19:22][1,]
        }
      }else if(data_set_to_plot=="Single_cell_seq_germline"){
        if (gene_name_format == "FBID") {
          TPMs = signif(data.seq[data.seq$FBGN %in% gene_of_interest, 3:11][1,], 2)
        }else{
          TPMs = signif(data.seq[data.seq$Symbol %in% gene_of_interest, 3:11][1,], 2)
        }
      }else if(data_set_to_plot=="Single_cell_seq_soma"){
        if (gene_name_format == "FBID") {
          TPMs = signif(data.seq[data.seq$FBGN %in% gene_of_interest, 3:10][1,], 2)
        }else{
          TPMs = signif(data.seq[data.seq$Symbol %in% gene_of_interest, 3:10][1,], 2)
        }
      }
        # adding TPM values to the proper place on the shape
        if (data_set_to_plot == "Input_seq" | data_set_to_plot == "Polysome_seq") {
          dist_pl = dist_pl+
            # UAS-tkv TPM value
            annotate("text", label=paste0(TPMs[1], " ", expression_unit), x=shape.x.y[1,1]-0.47, y=shape_ymin-0.30, size=text_scale)+
            # bamRNAi TPM value
            annotate("text", label=paste0(TPMs[2], " ", expression_unit), x=shape.x.y[3,1]-0.30, y=shape_ymax+0.30, size=text_scale)+
            # bamRNAi HS-bam TPM value
            annotate("text", label=paste0(TPMs[3], " ", expression_unit), 
                     x=((group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="2CC"][[1]][1]+
                          group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="16CC_3"][[1]][3])/2),
                     y=shape_ymin-0.24, size=text_scale)+
            # young WT TPM value
            annotate("text", label=paste0(TPMs[4], " ", expression_unit), x=shape.x.y[12,1]+0.30, y=shape_ymax+0.30, size=text_scale)
          
        }else if (data_set_to_plot=="Single_cell_seq_germline"){
          dist_pl = dist_pl+
            # GSC-2CC
            annotate("text", label=paste0(TPMs[1], " ", expression_unit), 
                     x=st_bbox(shape[1,])[[1]]+(st_bbox(shape[4,])[[3]]-st_bbox(shape[1,])[[1]])/2,
                     y=shape_ymin+0.16, size=text_scale)+
            # 4CC
            annotate("text", label=paste0(TPMs[2], "\n", expression_unit), x=shape.x.y[5,1], y=shape.x.y[5,2], size=text_scale)+
            # 8CC
            annotate("text", label=paste0(TPMs[3], "\n", expression_unit), x=shape.x.y[6,1], y=shape.x.y[6,2], size=text_scale)+
            # 16CC_2A1
            annotate("text", label=paste0(TPMs[4], "\n", expression_unit), x=shape.x.y[7,1], y=shape.x.y[7,2], size=text_scale)+
            # 16CC_2A2
            annotate("text", label=paste0(TPMs[5], "\n", expression_unit), x=shape.x.y[8,1], y=shape.x.y[8,2], size=text_scale)+
            # 16CC_2AB
            annotate("text", label=paste0(TPMs[6], "\n", expression_unit), x=shape.x.y[9,1], y=shape.x.y[9,2], size=text_scale)+
            # 16CC_2B
            annotate("text", label=paste0(TPMs[7], "\n", expression_unit), x=shape.x.y[10,1], y=shape.x.y[10,2], size=text_scale)+
            # 16CC_3
            annotate("text", label=paste0(TPMs[8], "\n", expression_unit), x=shape.x.y[11,1], y=shape.x.y[11,2], size=text_scale)+
            # ST2
            annotate("text", label=paste0(TPMs[9], "\n", expression_unit), x=shape.x.y[12,1], y=shape.x.y[12,2], size=text_scale)
        }else if (data_set_to_plot=="Single_cell_seq_soma"){
          dist_pl = dist_pl+
            # TF/CC
            annotate("text", label=paste0(TPMs[1], " ", expression_unit), 
                     x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="TF/CC"], 
                     y=shape_ymin+0.55, size=text_scale)+
            # EC_a
            annotate("text", label=paste0(TPMs[2], " ", expression_unit), 
                     x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="EC_a"], 
                     y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_a"][[1]][4]+0.13, size=text_scale)+
            # EC_c
            annotate("text", label=paste0(TPMs[3], " ", expression_unit), 
                     x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="EC_c"], 
                     y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_c"][[1]][2]-0.13, size=text_scale)+
            # EC_p
            annotate("text", label=paste0(TPMs[4], " ", expression_unit), 
                     x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="EC_p"], 
                     y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_p"][[1]][4]+0.13, size=text_scale)+
            # pre-FSC/FSC
            annotate("text", label=paste0(TPMs[5], " ", expression_unit), 
                     x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="FSC"], 
                     y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="FSC"][[1]][2]-0.13, size=text_scale)+
            # pre-stalk
            annotate("text", label=paste0(TPMs[6], " ", expression_unit),
                     x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="pre-stalk"], 
                     y=shape_ymax+0.17, size=text_scale)+
            # stalk
            annotate("text", label=paste0(TPMs[7], " ", expression_unit), 
                     x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="stalk"]+.15, 
                     y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="stalk"][[1]][4]+0.13, size=text_scale)+
            # polar
            annotate("text", label=paste0(TPMs[8], " ", expression_unit), 
                     x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="polar"], 
                     y=shape_ymax+0.17, size=text_scale)
        }
    }
        if (display_stage_labels==FALSE){ #switch for label display
        }else{
          if (data_set_to_plot == "Input_seq" | data_set_to_plot == "Polysome_seq") {
            dist_pl = dist_pl+
              # TKV cell label
              annotate("text", label=TeX(r'($> UAS- \textit{tkv}\, (GSCs)$)'), 
                       x=shape.x.y[1,1]-0.50, y=shape_ymin-0.50, size=text_scale)+
              annotate("segment", x=shape.x.y[1,1], xend=shape.x.y[1,1]-0.47, 
                       y=st_bbox(shape$geometry[1])[[2]], yend=shape_ymin-0.17)+
              # bamRNAi cell label
              annotate("text", label=TeX(r'($> \textit{bam}\, RNAi\, (CBs)$)'), 
                       x=shape.x.y[3,1]-0.30, y=shape_ymax+0.50, size=text_scale)+
              annotate("segment", x=shape.x.y[3,1], xend=shape.x.y[3,1]-0.30, 
                       y=st_bbox(shape$geometry[3])[[4]], yend=shape_ymax+0.17)+
              # bamHSbam line label
              annotate("text", label=TeX(r'($> \textit{bam}\, RNAi;\, hs-\textit{bam}\,(Cysts)$)'), 
                       x=((group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="2CC"][[1]][1]+
                             group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="16CC_3"][[1]][3])/2), 
                       y=shape_ymin-0.50, size=text_scale)+
              # youngWT cell label 
              annotate("text", label="young WT", 
                       x=shape.x.y[12,1]+0.30, y=shape_ymax+0.50, size=text_scale)+
              annotate("segment", x=shape.x.y[12,1], xend=shape.x.y[12,1]+0.30, y=st_bbox(shape$geometry[12,1])[[4]], yend=shape_ymax+0.17)
            
          }else if (data_set_to_plot=="Single_cell_seq_germline"){
            dist_pl = dist_pl+
              annotate("text", label="GSC/CB/2CC", 
                       x=st_bbox(shape[1,])[[1]]+(st_bbox(shape[4,])[[3]]-st_bbox(shape[1,])[[1]])/2,
                       y=shape_ymin-0.04, size=text_scale)+
              annotate("text", label="4-CC", x=shape.x.y[5,1], y=shape_ymax+0.30, size=text_scale)+
              annotate("text", label="8-CC", x=shape.x.y[6,1], y=shape_ymin-0.30, size=text_scale)+
              annotate("text", label="16-CC 2a I", x=shape.x.y[7,1], y=shape_ymax+0.30, size=text_scale)+
              annotate("text", label="16-CC 2a II", x=shape.x.y[8,1], y=shape_ymin-0.30, size=text_scale)+
              annotate("text", label="16-CC 2ab", x=shape.x.y[9,1], y=shape_ymax+0.30, size=text_scale)+
              annotate("text", label="16-CC 2b", x=shape.x.y[10,1], y=shape_ymin-0.30, size=text_scale)+
              annotate("text", label="16-CC 3", x=shape.x.y[11,1], y=shape_ymax+0.30, size=text_scale)+
              annotate("text", label="Stage 2 egg chamber", x=shape.x.y[12,1], y=shape_ymin-0.30, size=text_scale)+
              
              # GSC/CB/2CC rendered above
              
              # 4CC
              annotate("segment", x=shape.x.y[5,1], xend=shape.x.y[5,1],
                       y=st_bbox(shape$geometry[5])[[4]], yend=shape_ymax+0.17)+
              # 8CC
              annotate("segment", x=shape.x.y[6,1], xend=shape.x.y[6,1],
                       y=st_bbox(shape$geometry[6])[[2]], yend=shape_ymin-0.17)+
              # 16CC_2A1
              annotate("segment", x=shape.x.y[7,1], xend=shape.x.y[7,1],
                       y=st_bbox(shape$geometry[7])[[4]], yend=shape_ymax+0.17)+
              # 16CC_2A2
              annotate("segment", x=shape.x.y[8,1], xend=shape.x.y[8,1],
                       y=st_bbox(shape$geometry[8])[[2]], yend=shape_ymin-0.17)+
              # 16CC_2AB
              annotate("segment", x=shape.x.y[9,1], xend=shape.x.y[9,1],
                       y=st_bbox(shape$geometry[9])[[4]], yend=shape_ymax+0.17)+
              # 16CC_2B
              annotate("segment", x=shape.x.y[10,1], xend=shape.x.y[10,1],
                       y=st_bbox(shape$geometry[10])[[2]], yend=shape_ymin-0.17)+
              # 16CC_3
              annotate("segment", x=shape.x.y[11,1], xend=shape.x.y[11,1],
                       y=st_bbox(shape$geometry[11])[[4]], yend=shape_ymax+0.17)+
              # ST2
              annotate("segment", x=shape.x.y[12,1], xend=shape.x.y[12,1],
                       y=st_bbox(shape$geometry[12])[[2]], yend=shape_ymin-0.17)
            
          }else if (data_set_to_plot=="Single_cell_seq_soma"){
            dist_pl = dist_pl+
              
              annotate("text", label="Terminal filament cell", x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="TF/CC"], 
                       y=shape_ymin+0.35, size=text_scale)+
              
              annotate("text", label="/Cap cell", x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="TF/CC"], 
                       y=shape_ymin+0.18, size=text_scale)+
          
              annotate("text", label="Anterior escort cells", x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="EC_a"], 
                     y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_a"][[1]][4]+0.33, size=text_scale)+

              annotate("text", label="Central escort cells", x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="EC_c"], 
                     y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_c"][[1]][2]-0.33, size=text_scale)+
              
              annotate("text", label="Posterior escort cells", x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="EC_p"], 
                       y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="EC_p"][[1]][4]+0.33, size=text_scale)+

              annotate("text", label="Pre-follicle stem cell/Follicle stem cells", x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="FSC"], 
                       y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="FSC"][[1]][2]-0.33, size=text_scale)+
     
              annotate("text", label="Pre-stalk cells", x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="pre-stalk"], 
                       y=shape_ymax+0.33, size=text_scale)+

              annotate("text", label="Polar cells", x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="polar"], 
                       y=shape_ymax+0.33, size=text_scale)+
  
              annotate("text", label="Stalk", x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="stalk"], 
                       y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="stalk"][[1]][4]+0.5, size=text_scale)+
              
              annotate("text", label="cells", x=group_geometry_bounding$x[group_geometry_bounding$cell_type=="stalk"], 
                       y=group_geometry_bounding$bbox[group_geometry_bounding$cell_type=="stalk"][[1]][4]+0.33, size=text_scale)

          }
        }
    #### finish up ####
        .GlobalEnv$ovary_map_dataset_plotted = data_set_to_plot #keep track of plot plotted globally
        return(dist_pl)
      }else{
        message("graphic_to_generate should be of type 'map', or 'legend'")
      }
}
