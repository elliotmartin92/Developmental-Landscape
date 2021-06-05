here::i_am("Paper/figure_wrapper.R")
library(here)

source(here("Paper/Figures/Figure_1_assembly.R"))
source(here("Paper/Figures/Figure_2_assembly.R"))
source("Paper/Figures/Figure_3_assembly.R")
source("Paper/Figures/Figure_4_assembly.R")
source("Paper/Figures/Figure_1S_assembly.R")
source("Paper/Figures/Figure_2S_assembly.R")
source("Paper/Figures/Figure_3S_assembly.R")
source("Paper/Figures/Figure_4S_assembly.R")

rmarkdown::render("Paper/Figures/figure_wrapper.rmd")
