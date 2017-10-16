# TAXA DISAGGREGATION - CANADA

library(tidyverse)
library(sqldf)

# THESE ARE THE TOP 75% CATCH OF CANADA
# IN THIS FILE, I DISAGRREGATE HIGHER CLASSIFICATIONS TO TAXA LEVEL
# ALSO, SPLIT CATCH AMOUNT INTO SPECIES LEVEL
# RESULTS: split_ssf_ca - SAU catch 

# import taxonid table
taxonID <- read_csv("C:/Users/angmel/Documents/firstchapter/Reference tables/taxonID.csv")

# IN ORIGINAL SPLIT - IMPORT CSV FILES BY COUNTRIES
taxa_dis <- read_csv("split_catch/taxa_dis_1.csv")

# START WITH CANADA
# no error log from previous DBEM models

# previous split for Canada
taxa_dis_ca <- taxa_dis %>% 
  filter(eez == "Canada (Pacific)")

# SSF ----------------------------------------------------------------------- # NO CHANGES :)
# YOU ACCOUNTED FOR ALL OF THESE THE FIRST TIME AROUND
anti_join(sau_ssf_ca, taxa_dis_ca, by = "taxon_name") # perfect everything matches - DONE!
split_ssf_ca <- left_join(sau_ssf_ca, taxa_dis_ca, by = "taxon_name") %>% 
  select(eez, suggested_taxaID, Suggested, catch_sum)

# LSF -----------------------------------------------------------------------
dim(sau_lsf_ca) # 40 species in lsf
aj_lsf_ca <- anti_join(sau_lsf_ca, taxa_dis_ca, by = "taxon_name")  # 29 taxa do not match

# split within excel based on DFO, EcoTrust
write_csv(aj_lsf_ca, "aj_lsf_ca") 

# done split, import back csv
aj_lsf_ca_split <- read_csv(file = "taxa_dis/aj_lsf_ca.csv") %>% 
  dplyr::select(taxon_name, catch_sum, Suggested)

# merge lsf that already match
sau_lsf_tmp1 <- left_join(sau_lsf_ca, taxa_dis_ca, by = "taxon_name") %>% 
  dplyr::select(taxon_name, catch_sum, Suggested, suggested_taxaID) 

# merge newly split lsf
sau_lsf_tmp2 <- full_join(sau_lsf_tmp1, aj_lsf_ca_split, by = "taxon_name") %>% 
  dplyr:: select(-catch_sum.y)

# merge original and new suggested columns
sau_lsf_tmp2$suggested <- sau_lsf_tmp2$Suggested.x
sau_lsf_tmp2$suggested[!is.na(sau_lsf_tmp2$Suggested.y)] <- sau_lsf_tmp2$Suggested.y[!is.na(sau_lsf_tmp2$Suggested.y)] 
sau_lsf_tmp3 <- sau_lsf_tmp2 %>% 
  dplyr::select(taxon_name, catch_sum.x, suggested)

# split catch amount
Names <- toString(names(sau_lsf_tmp3["taxon_name"]))
df_count <- fn$sqldf("select $Names, count (*) count from sau_lsf_tmp3 group by $Names")
sau_lsf_tmp4 <- left_join(sau_lsf_tmp3, df_count, by = "taxon_name")

# divide original catch by count to get split catch
sau_lsf_tmp4$catch_split <- sau_lsf_tmp4$catch_sum.x/sau_lsf_tmp4$count
sau_lsf_canada <- sau_lsf_tmp4 %>% 
  dplyr::select(-count) %>% # take suggested, cross reference with DROBO
  rename(original_taxon_name = taxon_name) %>% 
  rename(taxon_name = suggested)

# CLEAN DATA - attempt to fix some of the naming
# sau_lsf_canada[8,3] <- "Lepidopsetta bilineata"
# sau_lsf_canada[10,3] <- "Parophrys vetulus"
# sau_lsf_canada[11,3] <- "Platichthys stellatus"
# sau_lsf_canada[12,3] <- "Reinhardtius hippoglossoides"
# sau_lsf_canada[29,3] <- "Eopsetta jordani"
# sau_lsf_canada[37,3] <- "Naucrates ductor"
# sau_lsf_canada[38,3] <- "Selar crumenophthalmus"
# sau_lsf_canada[52,3] <- "Lepidopsetta bilineata"
# sau_lsf_canada[54,3] <- "Parophrys vetulus"
# sau_lsf_canada[55,3] <- "Platichthys stellatus"
# sau_lsf_canada[56,3] <- "Reinhardtius hippoglossoides"
# sau_lsf_canada[91,3] <- "Eopsetta jordani"
# sau_lsf_canada[93,3] <- "Lepidopsetta bilineata"
# sau_lsf_canada[95,3] <- "Parophrys vetulus"
# sau_lsf_canada[96,3] <- "Platichthys stellatus"
# sau_lsf_canada[97,3] <- "Reinhardtius hippoglossoides"
# sau_lsf_canada[144,3] <- "Naucrates ductor"
# sau_lsf_canada[145,3] <- "Selar crumenophthalmus"
# sau_lsf_canada[124,3] <- "Thunnus alalunga"

# lsf_taxalist_unique[60,1] <- "Pandalus dispar"
# lsf_taxalist_unique[61,1] <- "Pandalus jordani"

sau_lsf_canada <- left_join(sau_lsf_canada, taxonID, by = "taxon_name")
lsf_taxalist <- sau_lsf_canada$taxon_name
lsf_taxalist <- unique(lsf_taxalist) %>% 
  as.data.frame()
colnames(lsf_taxalist) <- "taxon_name"
lsf_taxalist_unique <- left_join(lsf_taxalist_unique, taxonID, by = "taxon_name")

lsf_taxalist_unique$taxonID.x <- NULL
lsf_taxalist_unique %>% 
  arrange(taxonID.y)
write_csv(lsf_taxalist_unique, "canada_lsf_taxalist")

##################################################################################
# TO DO
# lsf_taxalist_unique
# these are the species I need to model (matched and not match with taxa ID)
# check if non-matches are because of error in names
# if not, reassign NEW taxaID!
# from here, remove duplicates within lsf - add to ssf
# compile one master taxa list and check against DROBO



