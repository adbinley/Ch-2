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

# set_ebirdst_access_key("tb7or2boq2lc", overwrite = T)
set_ebirdst_access_key("7329445pci6o", overwrite = T)

for(i in 1:length(my_species1$species_code)){

  spec <- my_species1$species_code[i]
  
ebirdst_download(
  species = "reevir1", #spec,
  path = getwd(),
  tifs_only = FALSE,
  force = TRUE,
  show_progress = TRUE
)

}
