# TAXA DISAGGREGATION - CANADA

library(tidyverse)
library(sqldf)

# THESE ARE THE TOP 75% CATCH OF CANADA
# IN THIS FILE, I DISAGRREGATE HIGHER CLASSIFICATIONS TO TAXA LEVEL
# ALSO, SPLIT CATCH AMOUNT INTO SPECIES LEVEL
# RESULTS: 
# View(sau_ssf_canada) - SAU SSF catch already split 
# View(sau_lsf_canada3) - this contains LSF split catch

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
anti_join(sau_ssf_ca2, taxa_dis_ca, by = "taxon_name") # perfect everything matches - inverts!
split_ssf_ca <- left_join(sau_ssf_ca2, taxa_dis_ca, by = "taxon_name") %>% 
  dplyr::select(eez, suggested_taxaID, Suggested, catch_sum) %>% 
  rename(taxon_name = Suggested)

sau_ssf_canada <- left_join(split_ssf_ca, taxonID, by = "taxon_name")

# LSF -----------------------------------------------------------------------
dim(sau_lsf_ca) # 40 species in lsf
aj_lsf_ca2 <- anti_join(sau_lsf_ca2, taxa_dis_ca, by = "taxon_name")  # 22 taxa do not match

# split within excel based on DFO, EcoTrust
write_csv(aj_lsf_ca2, "aj_lsf_ca2") 

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
# sau_lsf_canada2[33,3] <- "Thunnus alalunga"
# sau_lsf_canada2[32,3] <- "Sardinops sagax"


# sau_lsf_canada2[130,3] <- "Pandalus dispar"
# sau_lsf_canada2[131,3] <- "Pandalus jordani"

sau_lsf_canada <- left_join(sau_lsf_canada, taxonID, by = "taxon_name")
sau_lsf_canada2 <- sau_lsf_canada[1:136,]
sau_lsf_canada2 <- sau_lsf_canada2[,1:4]
sau_lsf_canada3 <- left_join(sau_lsf_canada2, taxonID, by = "taxon_name")
View(sau_lsf_canada3) # THIS CONTAINS LSF SPLIT AND TAXON ID


########################################################################

# FIND WHAT I NEED TO MODEL
# UNIQUE CANADA LSF

lsf_taxalist <- sau_lsf_canada3$taxon_name
lsf_taxalist <- unique(lsf_taxalist) %>% 
  as.data.frame()
colnames(lsf_taxalist) <- "taxon_name"
lsf_taxalist_unique <- left_join(lsf_taxalist, taxonID, by = "taxon_name")

#SSF
ssf_taxalist <- sau_ssf_canada$taxon_name
ssf_taxalist <- unique(ssf_taxalist) %>% 
  as.data.frame()
colnames(ssf_taxalist) <- "taxon_name"
ssf_taxalist_unique <- left_join(ssf_taxalist, taxonID, by = "taxon_name")

# MERGE TO FIND UNIQUE TAXA

canadalist <- bind_rows(lsf_taxalist_unique, ssf_taxalist_unique) %>% 
  unique()

write_csv(canadalist, "canadalist") # CHECK AGAINST DROBO AND CYGWIN

##################################################################################
# check with list if modelled before (DROBO and cygwin)
# if not, collect info and model
# if exist, then no need!



