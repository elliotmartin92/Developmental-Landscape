library(tidyverse)
library(sf)

ovary_map_cartoon <- function(text_scale) {
  merge_plot = readRDS("Preprocessed_data/preprocessed_sf_cartoon.RDS") #data to populate shape file for distPlot
  shape = readRDS("Preprocessed_data/preloaded_shape_cartoon.RDS") #shape file for distPlot
  
  pal <- c(
    "Black" = "Black",
    "1B1" = "darkred",
    "oocyte" = "grey30",
    "GSC" = "#7BC260",
    "CB" = "#83D2E7",
    "Early_cyst" = "#4E8CCA",
    "Late_cyst" = "#7BC260", 
    "soma" = "#FFD2ED", 
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
  
  bounding_16CC = 
  group_geometry %>% 
    filter(grepl(pattern = "16CC", cell_type)) %>% 
    summarise(geometry = st_union(geometry)) %>%
    summarise(bbox=st_bbox_by_feature(geometry)) %>% 
    pull(bbox) %>% 
    unlist()
  
  bounding_region_1 = 
    group_geometry %>% 
    filter(cell_type %in% c("GSC", "CB", "2CC", "4CC")) %>% 
    summarise(geometry = st_union(geometry)) %>%
    summarise(bbox=st_bbox_by_feature(geometry)) %>% 
    pull(bbox) %>% 
    unlist()
  
  bounding_region_2a = 
    group_geometry %>% 
    filter(cell_type %in% c("16CC_2A1", "16CC_2A2")) %>% 
    summarise(geometry = st_union(geometry)) %>%
    summarise(bbox=st_bbox_by_feature(geometry)) %>% 
    pull(bbox) %>% 
    unlist()
  
  bounding_region_2b = 
    group_geometry %>% 
    filter(cell_type %in% c("16CC_2B")) %>% 
    summarise(geometry = st_union(geometry)) %>%
    summarise(bbox=st_bbox_by_feature(geometry)) %>% 
    pull(bbox) %>% 
    unlist()
  
  bounding_region_3 = 
    group_geometry %>% 
    filter(cell_type %in% c("16CC_3")) %>% 
    summarise(geometry = st_union(geometry)) %>%
    summarise(bbox=st_bbox_by_feature(geometry)) %>% 
    pull(bbox) %>% 
    unlist()
  
  group_geometry_bounding = tibble(cell_type=group_geometry$cell_type, 
                                   x=map_dbl(group_geometry$centroid, 1), 
                                   y=map_dbl(group_geometry$centroid, 2),
                                   bbox=st_bbox_by_feature(group_geometry$geometry))
  
  merge_plot$color = factor(merge_plot$color, levels = c("1B1", "Black", "oocyte", "GSC", "CB", "Early_cyst", "Late_cyst", "soma", "White", "line"))
  
  # Toggle lines to display 1B1
  # merge_plot = merge_plot %>% filter(cell_type != "1B1")
  merge_plot$color[merge_plot$cell_type=="1B1"] = "1B1"
  
  merge_plot$color[merge_plot$cell_type=="oocyte"] = "oocyte"
  merge_plot$color[merge_plot$cell_type=="GSC"] = "GSC"
  merge_plot$color[merge_plot$cell_type=="CB"] = "CB"
  merge_plot$color[merge_plot$cell_type=="2CC"] = "Early_cyst"
  merge_plot$color[merge_plot$cell_type=="4CC"] = "Early_cyst"
  merge_plot$color[merge_plot$cell_type=="8CC"] = "Early_cyst"
  merge_plot$color[merge_plot$cell_type=="16CC_2A1"] = "Late_cyst"
  merge_plot$color[merge_plot$cell_type=="16CC_2A2"] = "Late_cyst"
  merge_plot$color[merge_plot$cell_type=="16CC_2AB"] = "Late_cyst"
  merge_plot$color[merge_plot$cell_type=="16CC_2B"] = "Late_cyst"
  merge_plot$color[merge_plot$cell_type=="16CC_3"] = "Late_cyst"
  merge_plot$color[merge_plot$cell_type=="ST2"] = "Late_cyst"
  merge_plot$color[merge_plot$region=="soma"] = "soma"
  
  dist_pl = merge_plot %>%
    st_as_sf() %>% 
    arrange(region) %>% 
    mutate(region_index = row_number()) %>%
    mutate(color = color %>% forcats::fct_reorder(-region_index)) %>%
    filter(region != "background") %>%
    ggplot()+
    geom_sf(aes(geometry=geometry, fill=color, color=region))+
    scale_fill_manual(values = pal, name="Binned Expression")+
    scale_color_manual(values = c("marker" = NA, "line" = "grey50", "germline" = "grey50", "soma" = "grey50"))+
    theme_void()+
    theme(panel.grid.major = element_line(colour = "transparent"),
          panel.background = element_rect(fill = "transparent", colour = "transparent"),
          panel.border = element_rect(fill = "transparent", colour = "transparent"),
          legend.position = "none")
  
  nonmarker_shape = shape %>% filter(region != "marker")
  nonmarker_centroids = st_centroid(nonmarker_shape)
  shape_ymin = st_bbox(shape$geometry)[[2]]
  shape_ymax = st_bbox(shape$geometry)[[4]]
  shape.x.y = data.frame(x=map_dbl(nonmarker_centroids$geometry, 1), y=map_dbl(nonmarker_centroids$geometry, 2), nonmarker_shape$cell_type)
  
  marker_shape = shape %>% filter(region == "marker")
  marker_centroids = st_centroid(marker_shape)
  marker_shape.x.y = data.frame(x=map_dbl(marker_centroids$geometry, 1), y=map_dbl(marker_centroids$geometry, 2), marker_centroids$cell_type)
  
  dist_pl = dist_pl+
    annotate("text", label="GSC", x=shape.x.y[16,1], y=shape_ymin-0.30, size=text_scale)+
    annotate("text", label="CB", x=shape.x.y[3,1], y=shape_ymax+0.16, size=text_scale)+
    annotate("text", label="2-CC", x=shape.x.y[23,1], y=shape_ymin-0.30, size=text_scale)+
    annotate("text", label="4-CC", x=shape.x.y[5,1], y=shape_ymax+0.16, size=text_scale)+
    annotate("text", label="8-CC", x=shape.x.y[6,1], y=shape_ymin-0.30, size=text_scale)+
    annotate("text", label="16-CC", x=(bounding_16CC[1]+bounding_16CC[3])/2, y=shape_ymin-0.30, size=text_scale)+
    annotate("text", label="Stage 2 Egg Chamber", x=shape.x.y[12,1], y=shape_ymin-0.30, size=text_scale)+
    annotate("text", label="Region 1", x=(bounding_region_1[1]+bounding_region_1[3])/2, y=shape_ymax+0.5, size=text_scale)+
    annotate("text", label="Region 2a", x=(bounding_region_2a[1]+bounding_region_2a[3])/2, y=shape_ymax+0.5, size=text_scale)+
    annotate("text", label="Region 2b", x=(bounding_region_2b[1]+bounding_region_2b[3])/2, y=shape_ymax+0.5, size=text_scale)+
    annotate("text", label="Region 3", x=(bounding_region_3[1]+bounding_region_3[3])/2, y=shape_ymax+0.5, size=text_scale)+
    
    # GSC
    annotate("segment", x=shape.x.y[1,1], xend=shape.x.y[16,1],
             y=st_bbox(nonmarker_shape$geometry[1])[[2]], yend=shape_ymin-0.17)+
    # CB
    annotate("segment", x=shape.x.y[3,1], xend=shape.x.y[3,1],
             y=st_bbox(nonmarker_shape$geometry[3])[[4]], yend=shape_ymax+0.03)+
    # 2CC
    annotate("segment", x=1.896065, xend=shape.x.y[23,1],
             y=5.716519, yend=shape_ymin-0.17)+
    # 4CC
    annotate("segment", x=shape.x.y[5,1], xend=shape.x.y[5,1],
             y=st_bbox(nonmarker_shape$geometry[5])[[4]], yend=shape_ymax+0.03)+
    # 8CC
    annotate("segment", x=shape.x.y[6,1], xend=shape.x.y[6,1],
             y=st_bbox(nonmarker_shape$geometry[6])[[2]], yend=shape_ymin-0.17)+
    # 16CC
    annotate("segment", x=bounding_16CC[1]*1.05, xend=bounding_16CC[3]*.95, y=shape_ymin-.17, yend=shape_ymin-0.17)+
    # ST2
    annotate("segment", x=shape.x.y[12,1], xend=shape.x.y[12,1],
             y=st_bbox(nonmarker_shape$geometry[12])[[2]], yend=shape_ymin-0.17)+
    # region 1
    annotate("segment", x=bounding_region_1[1], xend=bounding_region_1[3], y=shape_ymax+0.30, yend=shape_ymax+0.30)+
    # region 2a
    annotate("segment", x=bounding_region_2a[1], xend=bounding_region_2a[3], y=shape_ymax+0.30, yend=shape_ymax+0.30)+
    # region 2b
    annotate("segment", x=bounding_region_2b[1], xend=bounding_region_2b[3], y=shape_ymax+0.30, yend=shape_ymax+0.30)+
    # region 3
    annotate("segment", x=bounding_region_3[1], xend=bounding_region_3[3], y=shape_ymax+0.30, yend=shape_ymax+0.30)
  
  # markers
  dist_pl = dist_pl+
    # Oocyte
    annotate("text", label="Oocyte", x=shape.x.y[12,1], y=shape_ymax+.5, size=text_scale)+
    annotate("segment", x=marker_shape.x.y[11,1], xend=shape.x.y[12,1],
             y=st_bbox(marker_shape$geometry[10])[[4]], yend=shape_ymax+0.30)+
    annotate("segment", x=marker_shape.x.y[12,1], xend=shape.x.y[12,1],
             y=st_bbox(marker_shape$geometry[10])[[4]], yend=shape_ymax+0.30)+
  # Spectrosome
    annotate("text", label="Spectrosome", x=shape.x.y[18,1], y=shape_ymax+.5, size=text_scale)+
    annotate("segment", x=marker_shape.x.y[6,1], xend=shape.x.y[18,1],
             y=st_bbox(marker_shape$geometry[6])[[4]], yend=shape_ymax+0.30)+
  # Fusome
  annotate("text", label="Fusomes", x=shape.x.y[4,1]-.1, y=shape_ymin-0.30, size=text_scale)+
    # 2CC fusome
    annotate("segment", x=2.010876, xend=shape.x.y[4,1]-.1,
             y=st_bbox(marker_shape$geometry[1])[[2]], yend=shape_ymin-0.16)+
    # 8CC fusome
    annotate("segment", x=2.518981, xend=shape.x.y[4,1]-.1,
             y=st_bbox(marker_shape$geometry[8])[[2]], yend=shape_ymin-0.16)
  dist_pl
    
  dist_pl_rmd <<- dist_pl
  
}

