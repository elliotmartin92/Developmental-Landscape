DE_heatmap = function(data_set_to_plot="Input_seq", row_labels=FALSE){
  if (row_labels == FALSE) {
    if(data_set_to_plot == "Input_seq"){
      heatmap_local = readRDS("Preprocessed_data/Input_seq_plotly_heatmap.RDS")

    }else if (data_set_to_plot == "Polysome_seq"){
      heatmap_local = readRDS("Preprocessed_data/Polysome_seq_plotly_heatmap.RDS")
      
    }else if (data_set_to_plot == "Single_cell_seq_germline"){
      heatmap_local = readRDS("Preprocessed_data/Single_cell_seq_germline_plotly_heatmap.RDS")
      
    }else if (data_set_to_plot == "Single_cell_seq_soma"){
      heatmap_local = readRDS("Preprocessed_data/Single_cell_seq_soma_plotly_heatmap.RDS")
    }
  }else if (row_labels == TRUE) {
    if(data_set_to_plot == "Input_seq"){
      heatmap_local = readRDS("Preprocessed_data/Input_seq_row_labels_plotly_heatmap.RDS")
      
    }else if (data_set_to_plot == "Polysome_seq"){
      heatmap_local = readRDS("Preprocessed_data/Polysome_seq_row_labels_plotly_heatmap.RDS")
      
    }else if (data_set_to_plot == "Single_cell_seq_germline"){
      heatmap_local = readRDS("Preprocessed_data/Single_cell_seq_germline_row_labels_plotly_heatmap.RDS")
      
    }else if (data_set_to_plot == "Single_cell_seq_soma"){
      heatmap_local = readRDS("Preprocessed_data/Single_cell_seq_soma_row_labels_plotly_heatmap.RDS")
    }
  }
  
  .GlobalEnv$heat_map_dataset_plotted = data_set_to_plot
  heat_map_global <<- heatmap_local
}