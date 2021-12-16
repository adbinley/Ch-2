#ebird data download

my_species <- read.csv("C:/Users/AllisonBinley/OneDrive - Carleton University/thesis/CH2_2021/data/bird_data_v4.csv")

#setdiff(my_species$species_code, bird_data_v5$species_code)

my_species1 <- my_species %>%
  filter(species_code == "orcori"|
           species_code == "grhowl"|
           species_code == "rebwoo")

#Boo's coding:
#qqqqq22d2irrrraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaOSUIIIIIIIII

setwd("E:/eBird/data/raw/STEM")

set_ebirdst_access_key("n9e2g499utv1", overwrite = T)

for(i in 1:length(my_species1$species_code)){

  spec <- my_species1$species_code[i]
  
ebirdst_download(
  species = "grhowl", #spec,
  path = getwd(),
  tifs_only = FALSE,
  force = TRUE,
  show_progress = TRUE
)

}