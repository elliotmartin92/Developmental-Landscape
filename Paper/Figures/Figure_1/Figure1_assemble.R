source("Paper/Helper_functions/image_panel.R")


FigA = image_panel(path = "Paper/Figures/Figure_1/Control.Rps19b-GFP.40x.4_s3_5.tif", 
            colors_to_return = c("red", "blue"), 
            genotype_annotation = "RpS19b::GFP",
            red_annotation = "1B1", blue_annotation = "Vasa ",
            label_letters = c("A", "A'", "A''"))

FigB = image_panel(path = "Paper/Figures/Figure_1/Control.Rps19b-GFP.40x.4_s3_5.tif", 
            colors_to_return = c("red", "blue"), 
            genotype_annotation = "RpS19b::GFP",
            red_annotation = "1B1", blue_annotation = "Vasa ",
            label_letters = c("B", "B'", "B''"))

FigA / FigB + plot_layout(ncol = 1)

