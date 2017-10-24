# TAXA DISAGGREGATION - MEXICO

# RESULTS:
# ssf - View(sau_ssf_mexico)
# lsf - View(sau_lsf_mexico)

# THESE ARE THE TOP 75% CATCH BY COUNTRY AND DISAGRREGATE TO TAXA LEVEL
# output needs to be a file of species you need to model in Access

# load packages
library(tidyverse)
library(sqldf)

# import taxon table
taxonID <- read_csv("C:/Users/angmel/Documents/firstchapter/Reference tables/taxonID.csv")

# IN ORIGINAL SPLIT - IMPORT CSV FILES BY COUNTRIES
taxa_dis <- read_csv("split_catch/taxa_dis_1.csv")

taxa_dis_me <- taxa_dis %>% 
  filter(eez == "Mexico (Pacific)") 
# there are 20 error files
error_taxa <- c("690444", "602540", "601408", "600880", "600089", "690440", "600382", "600098", "690053", "690009", "603539", "603334", "602532", "600228", "600149", "600113", "800055", "800040", "800038", "800001")

me_error <- taxa_dis_me%>% 
  filter(suggested_taxaID %in% error_taxa) %>% 
  filter(Phylum == "Chordata") %>% 
  dplyr::select(suggested_taxaID)

View(me_error) # G species can be removed, B not sure

me_remove <- c("602540", "600880", "600089", "600098", "602532", "600228", "800055", "800040", "800038", "800001")
taxa_dis_me_2 <- taxa_dis_me %>% 
  filter(!suggested_taxaID %in% me_remove)

# see if ssf 75 and lsf 75 match current split:
anti_join(sau_ssf_me2, taxa_dis_me_2, by = "taxon_name") # sau_ssf_me from file sau_catch_inspection.R
aj_ls_me <- anti_join(sau_lsf_me2, taxa_dis_me, by = "taxon_name")  # 39 species categories do not match
write_csv(aj_ls_me, "aj_ls_me") # split this in excel

# break down into species level
# in Excel split species
aj_ls_me_split <- read.csv("~/MSc-small-scale-fisheries/taxa_dis/aj_ls_me.csv")

# Join LSF split with original split
sau_lsf_tmp2 <- full_join(taxa_dis_me_2, aj_ls_me_split, by = "taxon_name")
# merge original and new suggested columns
sau_lsf_tmp2$suggested <- sau_lsf_tmp2$Suggested.x
sau_lsf_tmp2$suggested[!is.na(sau_lsf_tmp2$Suggested.y)] <- sau_lsf_tmp2$Suggested.y[!is.na(sau_lsf_tmp2$Suggested.y)] 
sau_lsf_tmp3 <- sau_lsf_tmp2 %>% 
  dplyr::select(taxon_name, catch_sum, suggested) %>% 
  rename(original_taxon_name = taxon_name) %>% 
  rename(taxon_name = suggested)
sau_lsf_tmp4 <- left_join(sau_lsf_tmp3, taxonID, by = "taxon_name")


###########################################################################################
# split catch up - SSF
sau_ssf_me3 <- sau_ssf_me2 %>% 
  rename(original_taxon_name = taxon_name)

me_ssf <- left_join(sau_ssf_me2, taxa_dis_me_2, by = "taxon_name")
# split catch by disaggregation
Names <- toString(names(me_ssf["taxon_name"]))
df_count <- fn$sqldf("select $Names, count (*) count from me_ssf group by $Names")
sau_ssf_me_tmp1 <- left_join(me_ssf, df_count, by = "taxon_name")
# divide original catch by count to get split catch ----
sau_ssf_me_tmp1$catch_split <- sau_ssf_me_tmp1$catch_sum/sau_ssf_me_tmp1$count
sau_ssf_mexico <- sau_ssf_me_tmp1 %>% 
  dplyr::select(Suggested, suggested_taxaID, catch_split, eez)

# split catch up - LSF

sau_lsf_me3 <- sau_lsf_me2 %>% 
  rename(original_taxon_name = taxon_name)
me_lsf <- left_join(sau_lsf_me3, sau_lsf_tmp4, by = "original_taxon_name")
# split catch by disaggregation
Names <- toString(names(me_lsf["original_taxon_name"]))
df_count <- fn$sqldf("select $Names, count (*) count from me_lsf group by $Names")
sau_lsf_me_tmp1 <- left_join(me_lsf, df_count, by = "original_taxon_name")
# divide original catch by count to get split catch ----
sau_lsf_me_tmp1$catch_split <- sau_lsf_me_tmp1$catch_sum.x/sau_lsf_me_tmp1$count
sau_lsf_mexico <- sau_lsf_me_tmp1 %>% 
  dplyr::select(taxon_name, taxonID, catch_split) %>% 
  rename(Suggested = taxon_name) %>% 
  rename(suggested_taxaID = taxonID)

###########################################################################################

mexicolist <- bind_rows(sau_ssf_mexico, sau_lsf_mexico) %>% 
  dplyr::select(Suggested) %>% 
  rename(taxon_name = Suggested) %>% 
  unique() %>% 
  left_join(taxonID)

#################################### SAVE SPLIT CATCHES

write.csv(sau_ssf_mexico, file = "sau_ssf_me_splitcatch.csv")
write.csv(sau_lsf_mexico, file = "sau_lsf_me_splitcatch.csv")
