# TAXA DISAGGREGATION - USA

# THESE ARE THE TOP 75% CATCH BY COUNTRY AND DISAGRREGATE TO TAXA LEVEL
# output needs to be a file of species you need to model in Access
# are there error files?

# load packages
library(tidyverse)
library(sqldf)

# import taxon table
taxonID <- read_csv("C:/Users/angmel/Documents/firstchapter/Reference tables/taxonID.csv")

# IN ORIGINAL SPLIT - IMPORT CSV FILES BY COUNTRIES
taxa_dis <- read_csv("split_catch/taxa_dis_1.csv")

taxa_dis_us <- taxa_dis %>% 
  filter(eez == "USA (West Coast)") 
# there are error files
error_taxa <- c("690658", "690115", "600752", "603310", "690440", "600098", "800012", "690088", "690053", "602540", "600382", "600136", "600006")
View(us_error)
us_error <- taxa_dis_us %>% 
  filter(suggested_taxaID %in% error_taxa) %>% 
  filter(Phylum == "Chordata") %>% 
  dplyr::select(suggested_taxaID)

# see if ssf 75 and lsf 75 match current split:
anti_join(sau_ssf_us, taxa_dis_us, by = "taxon_name") # sau_ssf_us from file sau_catch_inspection.R
aj_ls_us <- anti_join(sau_lsf_us, taxa_dis_us, by = "taxon_name")  # 19 species do not match
write_csv(aj_ls_us, "aj_ls_us") # split this in excel

# break down into species level
aj_ls_us_split <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/taxa_dis/aj_ls_us", encoding = "ASCII") %>% 
  dplyr::select(-X)
  #ASCII removes <u00a0>

# TODO
# split catch up
# merge LSF and SSF species list
# cross reference with DROBO and CYGWIN to generate list of species to model in ACCESS


