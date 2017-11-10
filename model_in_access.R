# Model in ACCESS

# load packages
library(tidyverse)

ACCESS_taxalist <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/ACCESS/Model_these_species.csv", encoding = "ASCII")

dim(ACCESS_taxalist) # 263 species over 4 days = 66 species per day

# Fix names
ACCESS_taxalist[236,1] <- "Selar crumenophthalmus"
ACCESS_taxalist[229,1] <- "Hypomesus pretiosus"
ACCESS_taxalist[180,1] <- "Citharichthys fragilis"

# extract scientific name, you can join with ID info after
sciname <- ACCESS_taxalist$taxon_name

# collect info from rfishbase etc and populate into excel table, then transfer into ACCESS table
install.packages("rfishbase", 
                 repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"), 
                 type="source")
library("rfishbase")

# validate names
ACCESS_taxalist$validate <- validate_names(ACCESS_taxalist$taxon_name)
    # assume that NAs are inverts, lets work with the validated names first

# select only validated names
fish_list <- ACCESS_taxalist %>% 
  dplyr::filter(!is.na(validate)) %>% 
  rename(sciname = validate)

invert_list <- ACCESS_taxalist %>% # model these after!########################################
  dplyr::filter(is.na(validate))

#########################
# Checks
View(fish_list)
View(meta_fish)

dim(fish_list)
dim(meta_fish)
214-199

####################################################################################
# Meta data search!
# for some reason, there are duplicates SpecCode
# duplicates were removed in Excel

meta_fish <- species(fish_list$sciname, fields = "FBname") %>% 
  dplyr::select(sciname, FBname, SpecCode) %>% 
  rename(CommonName = FBname) %>% 
  left_join(fish_list, by = "sciname") %>% 
  select(-DROBO) %>% 
left_join(ecology(fish_list$sciname, fields = c("FoodTroph", "FoodSeTroph")) %>% 
  dplyr::select(FoodTroph, FoodSeTroph, SpecCode) %>% 
  rename(TL = FoodTroph) %>% 
  rename(seTL = FoodSeTroph), by = "SpecCode") %>% 
left_join(species(fish_list$sciname, fields = c("Length", "MaxLengthRef", "LTypeMaxM")) %>% 
  select(SpecCode, Length) %>% 
  rename(MaxLength = Length), by = "SpecCode") %>% 
left_join(species(fish_list$sciname, fields = c("DepthRangeShallow", "DepthRangeDeep")) %>%
  dplyr::select(-sciname) %>% 
  rename(MinDepth = DepthRangeShallow) %>% 
  rename(MaxDepth = DepthRangeDeep), by = "SpecCode") %>% 
left_join(stocks(fish_list$sciname) %>% 
  select(SpecCode, Northernmost, NorthSouthN, Southermost, NorthSouthS), by = "SpecCode") %>% 
left_join(ecology(fish_list$sciname, fields = c("CoralReefs", "Pelagic", "Demersal")) %>% 
  dplyr::select(-StockCode, -sciname), by = "SpecCode") %>% 
left_join(species(fish_list$sciname, fields = c("Fresh", "Saltwater")) %>%  
  dplyr::select(-sciname), by = "SpecCode") %>% 
left_join(ecology(fish_list$sciname, fields = c("SeaGrassBeds", "Mangroves")) %>% 
  dplyr::select(-sciname, -StockCode), by = "SpecCode") %>% 
left_join(species(fish_list$sciname, fields = c("Brack")) %>% 
  dplyr::select(-sciname), by = "SpecCode") %>% 
left_join(ecology(fish_list$sciname, fields = c("Estuaries")) %>% 
  dplyr::select(-StockCode, -sciname), by = "SpecCode") 

# export as csv
write.csv(meta_fish, "meta_fish.csv")

#######################################################################
# Troubleshoot
# Most of the fields were collected using the code above, but some returned NA
# Attempt to troubleshoot and find values using other methods

# UPLOAD UPDATED DATA - remove duplicates, cleaned up data 
meta_fish_updated <- read_csv("~/MSc-small-scale-fisheries/meta_fish_excel_2.csv") # USE ME!

# Demersal-Pelagic
dempel <- species(fish_list$sciname, fields = c("DemersPelag")) # RERUN THIS WITH THIS FUNCTION!!!!!
meta_fish_dempel <- read_csv("~/MSc-small-scale-fisheries/meta_fish_excel_2.csv") %>% 
  left_join(dempel, by  = "SpecCode") %>% 
  select(sciname.x, CommonName, SpecCode, taxonID, DemersPelag)
write.csv(meta_fish_dempel, "meta_fish_dempel")

# Latitude Range
max(robis::occurrence(scientificname = "Kyphosus elegans", field = "decimalLatitude"))
min(robis::occurrence(scientificname = "Kyphosus elegans", field = "decimalLatitude"))

# Trophic Level - troubleshoot TL info

meta_fish_TL <- meta_fish_updated %>% 
  dplyr::select(sciname, CommonName, SpecCode, taxonID, TL, seTL) %>% 
  filter(is.na(TL))

meta_fish_TL_data <- diet(meta_fish_TL$sciname, fields = c("Troph", "seTroph")) %>% 
  dplyr::select(sciname, Troph, seTroph)

# Depth
# FishBase poplf not working, and REOL not working either!
# Check with Gabs - just looked up individually on Reol website
# library(Reol)
# getNamespaceExports("Reol")
# testdata <- DownloadSearchedTaxa(fish_list$sciname[1:2], to.file = FALSE, exact = TRUE)
# GatherDataObjectInformation(testdata)
# BestProvider(testdata)
# DOCount(testdata)
# CollectDataforWeb(testdata)
# DataProcessing(testdata)

# FAO AREAS
meta_FAO <- meta_fish_updated %>% 
  select(sciname, SpecCode, taxonID) %>% 
  left_join(faoareas(meta_fish_updated$sciname) %>%
  dplyr::select(AreaCode, SpecCode)) %>% 
  dplyr::select(sciname, AreaCode)
write.csv(meta_FAO, "meta_FAO.csv")

# Taxon Bio Info
meta_taxonbio <- popgrowth(meta_fish_updated$sciname, fields = c("K", "TLinfinity", "Winfinity", "b")) %>% 
  left_join(poplw(meta_fish_updated$sciname, fields = c("a", "b")), by = "SpecCode") %>% 
  dplyr::select(sciname.x, SpecCode, TLinfinity, K, Winfinity, a, b.x, b.y) %>% 
  rename(sciname = sciname.x) %>% 
  rename(Linf = TLinfinity) %>% 
  rename(Winf = Winfinity)
write.csv(meta_taxonbio, "meta_taxonbio.csv")


# Taxon Dist - Aspect Ratio
meta_aspect <- swimming(fish_list$sciname, fields = c("AspectRatio"))

# Taxon Habitat
meta_habitat <- ecology(fish_list$sciname, fields = c("Oceanic", "Sand", "Sloping"))
write.csv(meta_habitat, "meta_habitat.csv")


left_join(ecology(fish_list$sciname[1:2], fields = c("Demersal", "Pelagic")),
          dplyr::select(-sciname), by = "SpecCode")

#####################################################################################
# Inverts

# set API
options(FISHBASE_API = "https://fishbase.ropensci.org/sealifebase")

# list of invert species or species that got NA on fishbase search
invert_list

# validate name with sealifebase
invert_list$validate <- validate_names(invert_list$taxon_name)

# select only validated names
invert_taxalist <- invert_list %>% 
  dplyr::filter(!is.na(validate)) %>% 
  rename(sciname = validate)

NA_list <- invert_list %>% # CHECK THESE after!########################################
dplyr::filter(is.na(validate))

# Collect info for inverts
meta_invert <- species(invert_taxalist$sciname, fields = "FBname") %>% 
  dplyr::select(sciname, FBname, SpecCode) %>% 
  rename(CommonName = FBname) %>% 
  left_join(invert_taxalist, by = "sciname") %>% 
  select(-DROBO) %>% 
  left_join(ecology(invert_taxalist$sciname, fields = c("FoodTroph", "FoodSeTroph")) %>% 
              dplyr::select(FoodTroph, FoodSeTroph, SpecCode) %>% 
              rename(TL = FoodTroph) %>% 
              rename(seTL = FoodSeTroph), by = "SpecCode") %>% 
  left_join(species(invert_taxalist$sciname, fields = c("Length", "MaxLengthRef", "LTypeMaxM")) %>% 
              select(SpecCode, Length) %>% 
              rename(MaxLength = Length), by = "SpecCode") %>% 
  left_join(species(invert_taxalist$sciname, fields = c("DepthRangeShallow", "DepthRangeDeep")) %>%
              dplyr::select(-sciname) %>% 
              rename(MinDepth = DepthRangeShallow) %>% 
              rename(MaxDepth = DepthRangeDeep), by = "SpecCode") %>% 
  left_join(stocks(invert_taxalist$sciname) %>% 
              select(SpecCode, Northernmost, NorthSouthN, Southermost, NorthSouthS), by = "SpecCode") %>% 
  left_join(ecology(invert_taxalist$sciname, fields = c("CoralReefs", "Pelagic", "Demersal")) %>% 
              dplyr::select(-StockCode, -sciname), by = "SpecCode") %>% 
  left_join(species(invert_taxalist$sciname, fields = c("Fresh", "Saltwater")) %>%  
              dplyr::select(-sciname), by = "SpecCode") %>% 
  left_join(ecology(invert_taxalist$sciname, fields = c("SeaGrassBeds", "Mangroves")) %>% 
              dplyr::select(-sciname, -StockCode), by = "SpecCode") %>% 
  left_join(species(invert_taxalist$sciname, fields = c("Brack")) %>% 
              dplyr::select(-sciname), by = "SpecCode") %>% 
  left_join(ecology(invert_taxalist$sciname, fields = c("Estuaries")) %>% 
              dplyr::select(-StockCode, -sciname), by = "SpecCode") 
# export as csv
write.csv(meta_invert, "meta_invert.csv")


###################################################
# TROUBLESHOOT

# Trophic Level - troubleshoot TL info
meta_invert_TL <- invert_taxalist$sciname %>% 
  dplyr::select(sciname, CommonName, SpecCode, taxonID, TL, seTL) %>% 
  filter(is.na(TL))

meta_invert_TL_data <- diet(invert_taxalist$sciname, fields = c("Troph", "seTroph")) %>% 
  dplyr::select(sciname, Troph, seTroph)

ecology(invert_taxalist$sciname[1:2])

# Demersal-Pelagic
dempel <- species(invert_taxalist$sciname, fields = c("DemersPelag")) # RERUN THIS WITH THIS FUNCTION!!!!!
write.csv(dempel, "invert_dempel")

# Latitude Range

max(robis::occurrence(scientificname = "Haliotis kamtschatkana", field = "decimalLatitude"))
min(robis::occurrence(scientificname = "Haliotis kamtschatkana", field = "decimalLatitude"))

list_fields("increase")

# Demersal-Pelagic
dempel <- species(invert_taxalist$sciname, fields = c("DemersPelag")) # RERUN THIS WITH THIS FUNCTION!!!!!
write.csv(dempel, "invert_dempel")
