# TAXA DISAGGREGATION - USA

# RESULT
# SSF: View(sau_ssf_us)
# LSF: View(sau_lsf_us)

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
  filter(suggested_taxaID %in% error_taxa) 

# analyzed error individually, 800012 and 600136 to remove, the rest B/D species and has numbers - not sure why error log
# remove error species and resplit data
remove_us <- c("800012", "600136")
taxa_dis_us_2 <- taxa_dis_us %>% 
  filter(!suggested_taxaID %in% remove_us)

# see if ssf 75 and lsf 75 match current split:
anti_join(sau_ssf_us2, taxa_dis_us_2, by = "taxon_name") # sau_ssf_us from file sau_catch_inspection.R
aj_ls_us <- anti_join(sau_lsf_us2, taxa_dis_us_2, by = "taxon_name")  # 12 species do not match
write_csv(aj_ls_us, "aj_ls_us") # split this in excel
# realized after didn't take 75%
# 75 is sau_lsf_us2
# I removed the 25% from LSF excel and write file back in:
# break down into species level
aj_ls_us_split <- read.csv("~/MSc-small-scale-fisheries/taxa_dis/aj_lsf_us_75.csv")


# Join LSF split with original split
sau_lsf_tmp2 <- full_join(taxa_dis_us_2, aj_ls_us_split, by = "taxon_name")
# merge original and new suggested columns
sau_lsf_tmp2$suggested <- sau_lsf_tmp2$Suggested.x
sau_lsf_tmp2$suggested[!is.na(sau_lsf_tmp2$Suggested.y)] <- sau_lsf_tmp2$Suggested.y[!is.na(sau_lsf_tmp2$Suggested.y)] 
sau_lsf_tmp3 <- sau_lsf_tmp2 %>% 
  dplyr::select(taxon_name, catch_sum, suggested) %>% 
  rename(original_taxon_name = taxon_name) %>% 
  rename(taxon_name = suggested)
sau_lsf_tmp4 <- left_join(sau_lsf_tmp3, taxonID, by = "taxon_name")

#########################################################################################
# split catch up - SSF
us_ssf <- left_join(sau_ssf_us2, taxa_dis_us_2, by = "taxon_name")
# split catch by disaggregation
Names <- toString(names(taxa_dis_us_2["taxon_name"]))
df_count <- fn$sqldf("select $Names, count (*) count from taxa_dis_us_2 group by $Names")
sau_ssf_us_tmp1 <- left_join(us_ssf, df_count, by = "taxon_name")
# divide original catch by count to get split catch ----
sau_ssf_us_tmp1$catch_split <- sau_ssf_us_tmp1$catch_sum/sau_ssf_us_tmp1$count
sau_ssf_us <- sau_ssf_us_tmp1 %>% 
  dplyr::select(Suggested, suggested_taxaID, eez, catch_split)


# split catch up - LSF
sau_lsf_us3 <- sau_lsf_us2 %>% 
  rename(original_taxon_name = taxon_name)

us_lsf <- left_join(sau_lsf_us3, sau_lsf_tmp4, by = "original_taxon_name")
# split catch by disaggregation
Names <- toString(names(us_lsf["original_taxon_name"]))
df_count <- fn$sqldf("select $Names, count (*) count from us_lsf group by $Names")
sau_lsf_us_tmp1 <- left_join(us_lsf, df_count, by = "original_taxon_name")
# divide original catch by count to get split catch ----
sau_lsf_us_tmp1$catch_split <- sau_lsf_us_tmp1$catch_sum.x/sau_lsf_us_tmp1$count
sau_lsf_us <- sau_lsf_us_tmp1 %>% 
  dplyr::select(taxon_name, taxonID, catch_split) %>% 
  rename(Suggested = taxon_name) %>% 
  rename(suggested_taxaID = taxonID)

################################### COMPILE LSF AND SSF, FIND UNIQUE TAXA

uslist <- bind_rows(sau_ssf_us, sau_lsf_us) %>% 
  dplyr::select(Suggested) %>% 
  unique() %>% 
  rename(taxon_name = Suggested) %>% 
  left_join(taxonID)

#################################### SAVE SPLIT CATCHES

write.csv(sau_ssf_us, file = "sau_ssf_us_splitcatch.csv")
write.csv(sau_lsf_us, file = "sau_lsf_us_splitcatch.csv")
