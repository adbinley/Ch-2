#ebird data download

my_species1 <- read.csv("data/bird_data_v4.csv") #less clean version of table S4

#Boo's coding, left in for posterity:
#qqqqq22d2irrrraaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaOSUIIIIIIIII

setwd("E:/eBird/data/raw/STEM") #this is a drive where I downloaded the eBird data to since it is over 600 GB

set_ebirdst_access_key() #you will need to get your own access key from the eBird website. I've removed mine here as it is meant to be per user and confidential

#this takes DAYS
for(i in 1:length(my_species1$species_code)){

  spec <- my_species1$species_code[i]
  
ebirdst_download(
  species = spec,
  path = getwd(),
  tifs_only = FALSE, #this makes it so that you also get the PI and PD values needed for analysis, in addition to relative abundance rasters
  force = TRUE,
  show_progress = TRUE
)

}

#ebird data should all be in the designated folder now