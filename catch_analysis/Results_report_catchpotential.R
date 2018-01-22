# For results in report

# 2.3.1 Species composition of LSF and SSF #####
# error Cancer magister in both LSF and SSF!

LSF_species3 %>%
  filter(ID %in% 'lsfnotssf') %>% 
  mutate(percentage = (LSF_totalcatch/LSF_spp)*100)

LSF_spp <- 129291403

SSF_species3 %>%
  filter(ID %in% 'ssfnotlsf') %>% 
  mutate(percentage = (SSF_totalcatch/SSF_spp)*100)

SSF_spp <- 28369215

# Mexico

LSF_species_me2 <- LSF_species_me[1:107,]
LSF_species_me2 <- LSF_species_me[1:93,] 
LSF_species_me_spp <- sum(LSF_species_me2$LSF_totalcatch_me)
LSF_species_me_3 <- LSF_species_me2 %>% 
  mutate(per = (LSF_totalcatch_me/LSF_species_me_spp)*100)

LSF_species_me_3 %>% 
  filter(ID %in% 'lsfnotssf') %>% 
  View()

SSF_species_me2 <- SSF_species_me[1:92,] 
SSF_species_me_spp <- sum(SSF_species_me2$SSF_totalcatch_me)
SSF_species_me_3 <- SSF_species_me2 %>% 
  mutate(per = (SSF_totalcatch_me/SSF_species_me_spp)*100)

SSF_species_me_3 %>% 
  filter(ID %in% 'ssfnotlsf') %>% 
  View()

# USA
LSF_species_us2 <- LSF_species_us[1:106,] 
LSF_species_us_spp <- sum(LSF_species_us2$LSF_totalcatch_us)
LSF_species_us_3 <- LSF_species_us2 %>% 
  mutate(per = (LSF_totalcatch_us/LSF_species_us_spp)*100)

LSF_species_us_3 %>% 
  View()

LSF_species_us_3 %>% 
  filter(ID %in% 'lsfnotssf') %>% 
  View()

SSF_species_us2 <- SSF_species_us[1:114,] 
SSF_species_us_spp <- sum(SSF_species_us2$SSF_totalcatch_us)
SSF_species_us_3 <- SSF_species_us2 %>% 
  mutate(per = (SSF_totalcatch_us/SSF_species_us_spp)*100)

SSF_species_us_3 %>% 
  View()

SSF_species_us_3 %>% 
  filter(ID %in% 'ssfnotlsf') %>% 
  View()

# Canada
LSF_species_ca2 <- LSF_species_ca[1:83,] 
LSF_species_ca_spp <- sum(LSF_species_ca2$LSF_totalcatch_ca)
LSF_species_ca_3 <- LSF_species_ca2 %>% 
  mutate(per = (LSF_totalcatch_ca/LSF_species_ca_spp)*100)

LSF_species_ca_3 %>% 
  View()

LSF_species_ca_3 %>% 
  filter(ID %in% 'lsfnotssf') %>% 
  View()

SSF_species_ca2 <- SSF_species_ca[1:99,] 
SSF_species_ca_spp <- sum(SSF_species_ca2$SSF_totalcatch_ca)
SSF_species_ca_3 <- SSF_species_ca2 %>% 
  mutate(per = (SSF_totalcatch_ca/SSF_species_ca_spp)*100)

SSF_species_ca_3 %>% 
  View()

SSF_species_ca_3 %>% 
  filter(ID %in% 'ssfnotlsf') %>% 
  View()

# Alaska
LSF_species_al2 <- LSF_species_al
LSF_species_al_spp <- sum(LSF_species_al2$LSF_totalcatch_al)
LSF_species_al_3 <- LSF_species_al2 %>% 
  mutate(per = (LSF_totalcatch_al/LSF_species_al_spp)*100)

LSF_species_al_3 %>% 
  View()

LSF_species_al_3 %>% 
  filter(ID %in% 'lsfnotssf') %>% 
  View()

SSF_species_al2 <- SSF_species_al
SSF_species_al_spp <- sum(SSF_species_al2$SSF_totalcatch_al)
SSF_species_al_3 <- SSF_species_al2 %>% 
  mutate(per = (SSF_totalcatch_al/SSF_species_al_spp)*100)

SSF_species_al_3 %>% 
  View()

SSF_species_al_3 %>% 
  filter(ID %in% 'ssfnotlsf') %>% 
  View()

# Overall changes in SSF and LSF catch potential - top 10 #############

# LSF [62.9% 8.5, 67.3% 2.6])
# GFDL_LSF_85-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(`2000`, `2087`) %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_85_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i)

GFDL_LSF_85_taxa[is.na(GFDL_LSF_85_taxa$change)] <- 0

GFDL_LSF_85_taxa$i <- as.numeric(as.character(GFDL_LSF_85_taxa$i))

GFDL_LSF_85_taxa_top10 <- GFDL_LSF_85_taxa %>% 
  rename(taxonID = i) %>% 
  right_join(top10LSF_taxa)

sum(GFDL_LSF_85_taxa$change < 0) # 102
sum(GFDL_LSF_85_taxa$change > 0) # 173

# GFDL_LSF_26 -------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(`2000`, `2087`) %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_26_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i)

GFDL_LSF_26_taxa[is.na(GFDL_LSF_26_taxa$change)] <- 0

GFDL_LSF_26_taxa$i <- as.numeric(as.character(GFDL_LSF_26_taxa$i))

GFDL_LSF_26_taxa_top10 <- GFDL_LSF_26_taxa %>% 
  rename(taxonID = i) %>% 
  right_join(top10LSF_taxa)


sum(GFDL_LSF_26_taxa$change < 0) # 90
sum(GFDL_LSF_26_taxa$change > 0) # 185


# figure
GFDL_LSF_26_taxa_top10$RCP <- "2.6"
GFDL_LSF_85_taxa_top10$RCP <- "8.5"

# maintain levels
GFDL_LSF_26_taxa_top10$taxon_name <- factor(GFDL_LSF_26_taxa_top10$taxon_name, levels = rev(GFDL_LSF_26_taxa_top10$taxon_name))
GFDL_LSF_85_taxa_top10$taxon_name <- factor(GFDL_LSF_85_taxa_top10$taxon_name, levels = rev(GFDL_LSF_85_taxa_top10$taxon_name))


png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/top10_lsf.png", width = 700, height = 463)

GFDL_LSF_85_taxa_top10 %>% 
  bind_rows(GFDL_LSF_26_taxa_top10) %>% 
  ggplot(aes(x=taxon_name,y=change,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Top 10 LSF Species by catch amount")+ylab("Percentage Change in Catch between 2087 and 2000") +
  theme_bw() +
  coord_flip()


dev.off()


# SSF 58.0% 8.5; 51.5% 2.6 
# GFDL_SSF_85 -------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
    p <- avg_catch %>% 
    dplyr::select(`2000`, `2087`) %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_SSF_85_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i)

GFDL_SSF_85_taxa$change[is.na(GFDL_SSF_85_taxa$change)] <- 0
GFDL_SSF_85_taxa[is.na(GFDL_SSF_85_taxa$change)] <- 0

GFDL_SSF_85_taxa$i <- as.numeric(as.character(GFDL_SSF_85_taxa$i))

GFDL_SSF_85_taxa_top10 <- GFDL_SSF_85_taxa %>% 
  rename(taxonID = i) %>% 
  right_join(top10SSF_taxa)

sum(GFDL_SSF_85_taxa$change < 0) # 110
sum(GFDL_SSF_85_taxa$change > 0) # 152


# GFDL_SSF_26 -------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(`2000`, `2087`) %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_SSF_26_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  filter(i == `690658`)

GFDL_SSF_26_taxa$change[is.na(GFDL_SSF_26_taxa$change)] <- 0
GFDL_SSF_26_taxa[is.na(GFDL_SSF_26_taxa$change)] <- 0

GFDL_SSF_26_taxa$i <- as.numeric(as.character(GFDL_SSF_26_taxa$i))

GFDL_SSF_26_taxa_top10 <- GFDL_SSF_26_taxa %>% 
  rename(taxonID = i) %>% 
  right_join(top10SSF_taxa)

sum(GFDL_SSF_26_taxa$change < 0) # 110
sum(GFDL_SSF_26_taxa$change > 0) # 152


# figure
GFDL_SSF_26_taxa_top10$RCP <- "2.6"
GFDL_SSF_85_taxa_top10$RCP <- "8.5"

# maintain levels
GFDL_SSF_26_taxa_top10$taxon_name <- factor(GFDL_SSF_26_taxa_top10$taxon_name, levels = rev(GFDL_SSF_26_taxa_top10$taxon_name))
GFDL_SSF_85_taxa_top10$taxon_name <- factor(GFDL_SSF_85_taxa_top10$taxon_name, levels = rev(GFDL_SSF_85_taxa_top10$taxon_name))


png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/top10_ssf.png", width = 700, height = 463)

GFDL_SSF_85_taxa_top10 %>% 
  bind_rows(GFDL_SSF_26_taxa_top10) %>% 
  ggplot(aes(x=taxon_name,y=change,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Top 10 SSF Species by catch amount")+ylab("Percentage Change in Catch between 2087 and 2000") +
  theme_bw() +
  coord_flip()


dev.off()

# Latitudes##############

eez_cellID <- read.csv(file = "C:/Users/angmel/Documents/MSc-small-scale-fisheries/reference_tables/eez_cellID.csv")

cellID_al <- eez_cellID %>% 
  filter(EEZID %in% "959") %>% 
  rename(INDEX = CellID.)

cellID_ca <- eez_cellID %>% 
  filter(EEZID %in% "925") %>% 
  rename(INDEX = CellID.)

cellID_us <- eez_cellID %>% 
  filter(EEZID %in% "848") %>% 
  rename(INDEX = CellID.)

cellID_me <- eez_cellID %>% 
  filter(EEZID %in% "945") %>% 
  rename(INDEX = CellID.)

# GFDL_LSF_85-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`)
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

mydata_85_lsf <- mydata %>% 
  dplyr::select(-i) %>% 
  rename(INDEX = cellID) %>% 
  left_join(cellID, by = "INDEX") %>% 
  dplyr::select(Lat, `2000`, `2087`) %>% 
  group_by(Lat) %>% 
  summarize_each(funs(sum))

mydata2000 <- mydata_2 %>%
  dplyr::select(Lat, `2000`) %>% 
  group_by(Lat) %>% 
  summarise_each(funs(sum))

mydata2050 <- mydata_2 %>%
  dplyr::select(Lat, `2087`) %>% 
  group_by(Lat) %>% 
  summarise_each(funs(sum))

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/lat_lsf_85_lm.png", width = 700, height = 463)

mydata_2 %>% 
  mutate(percent_change = (((`2087`-`2000`)/`2000`)*100)) %>% 
  ggplot() +
  geom_point(aes(x=Lat, y=percent_change)) +
  geom_smooth(aes(x=Lat, y=percent_change), method = "lm") +
  theme_bw() +
  labs(title="Changes in catch potential between 2087 and 2010 for LSF (RCP 8.5)", x = "Latitude", y="Change in catch potential (%)") +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))

dev.off()

    # alaska
mydata_85_lsf_al <- mydata %>% 
  rename(INDEX = cellID) %>% 
  right_join(cellID_al, by = "INDEX") %>% 
  dplyr::select(`2000`, `2087`) %>% 
  colSums(na.rm = TRUE)

al_85_lsf <- ((mydata_85_lsf_al[2]-mydata_85_lsf_al[1])/mydata_85_lsf_al[1])*100
  
  # canada
  mydata_85_lsf_ca <- mydata %>% 
    rename(INDEX = cellID) %>% 
    right_join(cellID_ca, by = "INDEX") %>% 
    dplyr::select(`2000`, `2087`) %>% 
    colSums(na.rm = TRUE)
  
  ca_85_lsf <-  ((mydata_85_lsf_ca[2]-mydata_85_lsf_ca[1])/mydata_85_lsf_ca[1])*100
  
  # usa
  mydata_85_lsf_us <- mydata %>% 
    rename(INDEX = cellID) %>% 
    right_join(cellID_us, by = "INDEX") %>% 
    dplyr::select(`2000`, `2087`) %>% 
    colSums(na.rm = TRUE)
  
  us_85_lsf <- ((mydata_85_lsf_us[2]-mydata_85_lsf_us[1])/mydata_85_lsf_us[1])*100
  
  # mexico
  mydata_85_lsf_me <- mydata %>% 
    rename(INDEX = cellID) %>% 
    right_join(cellID_me, by = "INDEX") %>% 
    dplyr::select(`2000`, `2087`) %>% 
    colSums(na.rm = TRUE)
  
  me_85_lsf <- ((mydata_85_lsf_me[2]-mydata_85_lsf_me[1])/mydata_85_lsf_me[1])*100

# GFDL_LSF_26 -------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`)
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

mydata_26 <- mydata %>% 
  dplyr::select(-i) %>% 
  rename(INDEX = cellID) %>% 
  left_join(cellID, by = "INDEX") %>% 
  dplyr::select(Lat, `2000`, `2087`) %>% 
  group_by(Lat) %>% 
  summarize_each(funs(sum))

mydata2000_26 <- mydata_26 %>%
  dplyr::select(Lat, `2000`) %>% 
  group_by(Lat) %>% 
  summarise_each(funs(sum))

mydata2050_26 <- mydata_26 %>%
  dplyr::select(Lat, `2087`) %>% 
  group_by(Lat) %>% 
  summarise_each(funs(sum))

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/lat_lsf_26.png", width = 700, height = 463)

mydata_26 %>% 
  mutate(percent_change = (((`2087`-`2000`)/`2000`)*100)) %>% 
  ggplot() +
  geom_point(aes(x=Lat, y=percent_change)) +
  geom_smooth(aes(x=Lat, y=percent_change)) +
  theme_bw() +
  labs(title="Changes in catch potential between 2087 and 2010 for LSF (RCP 2.6)", x = "Latitude", y="Change in catch potential (%)") +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))

dev.off()

# alaska
mydata_26_lsf_al <- mydata %>% 
  rename(INDEX = cellID) %>% 
  right_join(cellID_al, by = "INDEX") %>% 
  dplyr::select(`2000`, `2087`) %>% 
  colSums(na.rm = TRUE)

al_26_lsf <- ((mydata_26_lsf_al[2]-mydata_26_lsf_al[1])/mydata_26_lsf_al[1])*100

# canada
mydata_26_lsf_ca <- mydata %>% 
  rename(INDEX = cellID) %>% 
  right_join(cellID_ca, by = "INDEX") %>% 
  dplyr::select(`2000`, `2087`) %>% 
  colSums(na.rm = TRUE)

ca_26_lsf <-  ((mydata_26_lsf_ca[2]-mydata_26_lsf_ca[1])/mydata_26_lsf_ca[1])*100

# usa
mydata_26_lsf_us <- mydata %>% 
  rename(INDEX = cellID) %>% 
  right_join(cellID_us, by = "INDEX") %>% 
  dplyr::select(`2000`, `2087`) %>% 
  colSums(na.rm = TRUE)

us_26_lsf <- ((mydata_26_lsf_us[2]-mydata_26_lsf_us[1])/mydata_26_lsf_us[1])*100

# mexico
mydata_26_lsf_me <- mydata %>% 
  rename(INDEX = cellID) %>% 
  right_join(cellID_me, by = "INDEX") %>% 
  dplyr::select(`2000`, `2087`) %>% 
  colSums(na.rm = TRUE)

me_26_lsf <- ((mydata_26_lsf_me[2]-mydata_26_lsf_me[1])/mydata_26_lsf_me[1])*100

# SSF 58.0% 8.5; 51.5% 2.6 
# GFDL_SSF_85 -------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`)
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

mydata_85_ssf <- mydata %>% 
  dplyr::select(-i) %>% 
  rename(INDEX = cellID) %>% 
  left_join(cellID, by = "INDEX") %>% 
  dplyr::select(Lat, `2000`, `2087`) %>% 
  group_by(Lat) %>% 
  summarize_each(funs(sum))

mydata2000_85_ssf <- mydata_85_ssf %>%
  dplyr::select(Lat, `2000`) %>% 
  group_by(Lat) %>% 
  summarise_each(funs(sum))

mydata2050_85_ssf <- mydata_85_ssf %>%
  dplyr::select(Lat, `2087`) %>% 
  group_by(Lat) %>% 
  summarise_each(funs(sum))

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/lat_ssf_85.png", width = 700, height = 463)

mydata_85_ssf %>% 
  mutate(percent_change = (((`2087`-`2000`)/`2000`)*100)) %>% 
  ggplot() +
  geom_point(aes(x=Lat, y=percent_change)) +
  geom_smooth(aes(x=Lat, y=percent_change), method = "lm") +
  theme_bw() +
  labs(title="Changes in catch potential between 2087 and 2010 for SSF (RCP 8.5)", x = "Latitude", y="Change in catch potential (%)") +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))

dev.off()

# alaska
mydata_85_ssf_al <- mydata %>% 
  rename(INDEX = cellID) %>% 
  right_join(cellID_al, by = "INDEX") %>% 
  dplyr::select(`2000`, `2087`) %>% 
  colSums(na.rm = TRUE)

al_85_ssf <- ((mydata_85_ssf_al[2]-mydata_85_ssf_al[1])/mydata_85_ssf_al[1])*100

# canada
mydata_85_ssf_ca <- mydata %>% 
  rename(INDEX = cellID) %>% 
  right_join(cellID_ca, by = "INDEX") %>% 
  dplyr::select(`2000`, `2087`) %>% 
  colSums(na.rm = TRUE)

ca_85_ssf <-  ((mydata_85_ssf_ca[2]-mydata_85_ssf_ca[1])/mydata_85_ssf_ca[1])*100

# usa
mydata_85_ssf_us <- mydata %>% 
  rename(INDEX = cellID) %>% 
  right_join(cellID_us, by = "INDEX") %>% 
  dplyr::select(`2000`, `2087`) %>% 
  colSums(na.rm = TRUE)

us_85_ssf <- ((mydata_85_ssf_us[2]-mydata_85_ssf_us[1])/mydata_85_ssf_us[1])*100

# mexico
mydata_85_ssf_me <- mydata %>% 
  rename(INDEX = cellID) %>% 
  right_join(cellID_me, by = "INDEX") %>% 
  dplyr::select(`2000`, `2087`) %>% 
  colSums(na.rm = TRUE)

me_85_ssf <- ((mydata_85_ssf_me[2]-mydata_85_ssf_me[1])/mydata_85_ssf_me[1])*100

# GFDL_SSF_26 -------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`)
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

mydata_26_ssf <- mydata %>% 
  dplyr::select(-i) %>% 
  rename(INDEX = cellID) %>% 
  left_join(cellID, by = "INDEX") %>% 
  dplyr::select(Lat, `2000`, `2087`) %>% 
  group_by(Lat) %>% 
  summarize_each(funs(sum))

mydata2000_26_ssf <- mydata_26_ssf %>%
  dplyr::select(Lat, `2000`) %>% 
  group_by(Lat) %>% 
  summarise_each(funs(sum))

mydata2050_26_ssf <- mydata_26_ssf %>%
  dplyr::select(Lat, `2087`) %>% 
  group_by(Lat) %>% 
  summarise_each(funs(sum))

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/lat_ssf_26.png", width = 700, height = 463)

mydata_26_ssf %>% 
  mutate(percent_change = (((`2087`-`2000`)/`2000`)*100)) %>% 
  ggplot() +
  geom_point(aes(x=Lat, y=percent_change)) +
  geom_smooth(aes(x=Lat, y=percent_change), method = "loess", span = 1) +
  theme_bw() +
  labs(title="Changes in catch potential between 2087 and 2010 for SSF (RCP 2.6)", x = "Latitude", y="Change in catch potential (%)") +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))

dev.off()

      # alaska
      mydata_26_ssf_al <- mydata %>% 
        rename(INDEX = cellID) %>% 
        right_join(cellID_al, by = "INDEX") %>% 
        dplyr::select(`2000`, `2087`) %>% 
        colSums(na.rm = TRUE)
      
      al_26_ssf <- ((mydata_26_ssf_al[2]-mydata_26_ssf_al[1])/mydata_26_ssf_al[1])*100
      
      # canada
      mydata_26_ssf_ca <- mydata %>% 
        rename(INDEX = cellID) %>% 
        right_join(cellID_ca, by = "INDEX") %>% 
        dplyr::select(`2000`, `2087`) %>% 
        colSums(na.rm = TRUE)
      
      ca_26_ssf <-  ((mydata_26_ssf_ca[2]-mydata_26_ssf_ca[1])/mydata_26_ssf_ca[1])*100
      
      # usa
      mydata_26_ssf_us <- mydata %>% 
        rename(INDEX = cellID) %>% 
        right_join(cellID_us, by = "INDEX") %>% 
        dplyr::select(`2000`, `2087`) %>% 
        colSums(na.rm = TRUE)
      
      us_26_ssf <- ((mydata_26_ssf_us[2]-mydata_26_ssf_us[1])/mydata_26_ssf_us[1])*100
      
      # mexico
      mydata_26_ssf_me <- mydata %>% 
        rename(INDEX = cellID) %>% 
        right_join(cellID_me, by = "INDEX") %>% 
        dplyr::select(`2000`, `2087`) %>% 
        colSums(na.rm = TRUE)
      
      me_26_ssf <- ((mydata_26_ssf_me[2]-mydata_26_ssf_me[1])/mydata_26_ssf_me[1])*100

### Lat - number of spp vs lat #####

# GFDL_LSF_85-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`)
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

mydata_2 <- mydata %>% 
  rename(INDEX = cellID) %>% 
  left_join(cellID, by = "INDEX") %>% 
  mutate(percent_change = (((`2087`-`2000`)/`2000`)*100)) %>% 
  dplyr::select(Lat, percent_change, i) %>% 
  filter(percent_change < 0) %>% 
  group_by(Lat, i) %>% 
  summarize_each(funs(sum)) %>% 
  group_by(Lat) %>% 
  count() %>% 
  ggplot() +
  geom_point(aes(x=Lat, y=n)) +
  geom_smooth(aes(x=Lat, y=n), method = "loess", span = 1) +
  theme_bw() +
  labs(title="Changes in catch potential between 2087 and 2010 for SSF (RCP 2.6)", x = "Latitude", y="Change in catch potential (%)") +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))


png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/lat_lsf_85_lm.png", width = 700, height = 463)


# GFDL_LSF_26 -------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`)
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

mydata %>% 
  rename(INDEX = cellID) %>% 
  left_join(cellID, by = "INDEX") %>% 
  mutate(percent_change = (((`2087`-`2000`)/`2000`)*100)) %>% 
  dplyr::select(Lat, percent_change, i) %>% 
  filter(percent_change < 0) %>% 
  group_by(Lat, i) %>% 
  summarize_each(funs(sum)) %>% 
  group_by(Lat) %>% 
  count() %>% 
  ggplot() +
  geom_point(aes(x=Lat, y=n)) +
  geom_smooth(aes(x=Lat, y=n), method = "lm", span = 1) +
  theme_bw() +
  labs(title="Changes in catch potential between 2087 and 2010 for SSF (RCP 2.6)", x = "Latitude", y="Number of species") +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/lat_lsf_26.png", width = 700, height = 463)

mydata_26 %>% 
  mutate(percent_change = (((`2087`-`2000`)/`2000`)*100)) %>% 
  ggplot() +
  geom_point(aes(x=Lat, y=percent_change)) +
  geom_smooth(aes(x=Lat, y=percent_change)) +
  theme_bw() +
  labs(title="Changes in catch potential between 2087 and 2010 for LSF (RCP 2.6)", x = "Latitude", y="Change in catch potential (%)") +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))

dev.off()


# SSF 58.0% 8.5; 51.5% 2.6 
# GFDL_SSF_85 -------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`)
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

mydata_85_ssf <- mydata %>% 
  dplyr::select(-i) %>% 
  rename(INDEX = cellID) %>% 
  left_join(cellID, by = "INDEX") %>% 
  dplyr::select(Lat, `2000`, `2087`) %>% 
  group_by(Lat) %>% 
  summarize_each(funs(sum))

mydata2000_85_ssf <- mydata_85_ssf %>%
  dplyr::select(Lat, `2000`) %>% 
  group_by(Lat) %>% 
  summarise_each(funs(sum))

mydata2050_85_ssf <- mydata_85_ssf %>%
  dplyr::select(Lat, `2087`) %>% 
  group_by(Lat) %>% 
  summarise_each(funs(sum))

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/lat_ssf_85.png", width = 700, height = 463)

mydata_85_ssf %>% 
  mutate(percent_change = (((`2087`-`2000`)/`2000`)*100)) %>% 
  ggplot() +
  geom_point(aes(x=Lat, y=percent_change)) +
  geom_smooth(aes(x=Lat, y=percent_change), method = "lm") +
  theme_bw() +
  labs(title="Changes in catch potential between 2087 and 2010 for SSF (RCP 8.5)", x = "Latitude", y="Change in catch potential (%)") +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))

dev.off()

# GFDL_SSF_26 -------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`)
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

mydata_26_ssf <- mydata %>% 
  dplyr::select(-i) %>% 
  rename(INDEX = cellID) %>% 
  left_join(cellID, by = "INDEX") %>% 
  dplyr::select(Lat, `2000`, `2087`) %>% 
  group_by(Lat) %>% 
  summarize_each(funs(sum))

mydata2000_26_ssf <- mydata_26_ssf %>%
  dplyr::select(Lat, `2000`) %>% 
  group_by(Lat) %>% 
  summarise_each(funs(sum))

mydata2050_26_ssf <- mydata_26_ssf %>%
  dplyr::select(Lat, `2087`) %>% 
  group_by(Lat) %>% 
  summarise_each(funs(sum))

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/lat_ssf_26.png", width = 700, height = 463)

mydata_26_ssf %>% 
  mutate(percent_change = (((`2087`-`2000`)/`2000`)*100)) %>% 
  ggplot() +
  geom_point(aes(x=Lat, y=percent_change)) +
  geom_smooth(aes(x=Lat, y=percent_change), method = "loess", span = 1) +
  theme_bw() +
  labs(title="Changes in catch potential between 2087 and 2010 for SSF (RCP 2.6)", x = "Latitude", y="Change in catch potential (%)") +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))

dev.off()


# Changes in catch potentials by eez ####

me_26_ssf$sector <- "ssf" 
me_26_ssf$eez <- "mexico"
me_26_ssf$RCP <- "2.6" 

al_26_ssf$sector <- "ssf"
al_26_ssf$eez <- "alaska"
al_26_ssf$RCP <- "2.6" 

ca_26_ssf$sector <- "ssf"
ca_26_ssf$eez <- "canada"
ca_26_ssf$RCP <- "2.6" 

us_26_ssf$sector <- "ssf"
us_26_ssf$eez <- "us"
us_26_ssf$RCP <- "2.6" 

me_85_ssf$sector <- "ssf" 
me_85_ssf$eez <- "mexico"
me_85_ssf$RCP <- "8.5" 

al_85_ssf$sector <- "ssf"
al_85_ssf$eez <- "alaska"
al_85_ssf$RCP <- "8.5" 

ca_85_ssf$sector <- "ssf"
ca_85_ssf$eez <- "canada"
ca_85_ssf$RCP <- "8.5" 

us_85_ssf$sector <- "ssf"
us_85_ssf$eez <- "us"
us_85_ssf$RCP <- "8.5" 

me_85_lsf$sector <- "lsf" 
me_85_lsf$eez <- "mexico"
me_85_lsf$RCP <- "8.5" 

al_85_lsf$sector <- "lsf"
al_85_lsf$eez <- "alaska"
al_85_lsf$RCP <- "8.5" 

ca_85_lsf$sector <- "lsf"
ca_85_lsf$eez <- "canada"
ca_85_lsf$RCP <- "8.5" 

us_85_lsf$sector <- "lsf"
us_85_lsf$eez <- "us"
us_85_lsf$RCP <- "8.5" 

me_26_lsf$sector <- "lsf" 
me_26_lsf$eez <- "mexico"
me_26_lsf$RCP <- "2.6" 

al_26_lsf$sector <- "lsf"
al_26_lsf$eez <- "alaska"
al_26_lsf$RCP <- "2.6" 

ca_26_lsf$sector <- "lsf"
ca_26_lsf$eez <- "canada"
ca_26_lsf$RCP <- "2.6" 

us_26_lsf$sector <- "lsf"
us_26_lsf$eez <- "us"
us_26_lsf$RCP <- "2.6" 

aggregate <- al_26_ssf %>% 
  bind_rows(me_26_ssf) %>% 
  bind_rows(us_26_ssf) %>% 
  bind_rows(ca_26_ssf) %>% 
  bind_rows(al_26_lsf) %>% 
  bind_rows(me_26_lsf) %>% 
  bind_rows(us_26_lsf) %>% 
  bind_rows(ca_26_lsf) %>% 
  bind_rows(al_85_lsf) %>% 
  bind_rows(me_85_lsf) %>% 
  bind_rows(us_85_lsf) %>% 
  bind_rows(ca_85_lsf) %>% 
  bind_rows(al_85_ssf) %>% 
  bind_rows(me_85_ssf) %>% 
  bind_rows(us_85_ssf) %>% 
  bind_rows(ca_85_ssf)

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/al_catchpot.png", width = 700, height = 700)

aggregate %>% 
  filter(eez %in% "alaska") %>% 
  ggplot(aes(x=sector,y=`2087`,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge") +
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Sector")+ylab("Percentage Change in Catch between 2087 and 2000") +
  labs(title="Changes in Alaska catch potential") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))
 
dev.off()

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/ca_catchpot.png", width = 700, height = 700)

aggregate %>% 
  filter(eez %in% "canada") %>% 
  ggplot(aes(x=sector,y=`2087`,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge") +
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Sector")+ylab("Percentage Change in Catch between 2087 and 2000") +
  labs(title="Changes in Canada's catch potential") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))

dev.off()

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/us_catchpot.png", width = 700, height = 700)

aggregate %>% 
  filter(eez %in% "us") %>% 
  ggplot(aes(x=sector,y=`2087`,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge") +
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Sector")+ylab("Percentage Change in Catch between 2087 and 2000") +
  labs(title="Changes in USA (West Coast)'s catch potential") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))

dev.off()

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/me_catchpot.png", width = 700, height = 700)

aggregate %>% 
  filter(eez %in% "mexico") %>% 
  ggplot(aes(x=sector,y=`2087`,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge") +
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Sector")+ylab("Percentage Change in Catch between 2087 and 2000") +
  labs(title="Changes in Mexico's catch potential") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))

dev.off()

# Changes btw 2087 and 2000 for Top species by eez ####
# LSF ####

# alaska ####

LSF_species_al[1:10,]

# GFDL_LSF_85-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_al, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_85_al_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

LSF_species_al_top <- LSF_species_al[1:10,] %>% 
  rename(i = taxonID)

GFDL_LSF_85_al_taxa$i <- as.numeric(GFDL_LSF_85_al_taxa$i) 
GFDL_LSF_85_al_taxa <- GFDL_LSF_85_al_taxa %>% 
  right_join(LSF_species_al_top, by = "i")

sum(GFDL_LSF_85_al_taxa$change < 0, na.rm =  TRUE) # 102
sum(GFDL_LSF_85_al_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_LSF_85_al_taxa$RCP <- "8.5"
GFDL_LSF_85_al_taxa$sector <- "LSF"
GFDL_LSF_85_al_taxa$climate <- "GFDL"

# GFDL_LSF_26-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_al, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_26_al_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

LSF_species_al_top <- LSF_species_al[1:10,] %>% 
  rename(i = taxonID)

GFDL_LSF_26_al_taxa$i <- as.numeric(GFDL_LSF_26_al_taxa$i) 
GFDL_LSF_26_al_taxa <- GFDL_LSF_26_al_taxa %>% 
  right_join(LSF_species_al_top, by = "i")

sum(GFDL_LSF_26_al_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_LSF_26_al_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_LSF_26_al_taxa$RCP <- "2.6"
GFDL_LSF_26_al_taxa$sector <- "LSF"
GFDL_LSF_26_al_taxa$climate <- "GFDL"

# maintain levels
GFDL_LSF_26_al_taxa$taxon_name <- factor(GFDL_LSF_26_al_taxa$taxon_name, levels = rev(GFDL_LSF_26_al_taxa$taxon_name))
GFDL_LSF_85_al_taxa$taxon_name <- factor(GFDL_LSF_85_al_taxa$taxon_name, levels = rev(GFDL_LSF_85_al_taxa$taxon_name))

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/top10_lsf_al.png", width = 700, height = 463)

GFDL_LSF_26_al_taxa %>% 
  bind_rows(GFDL_LSF_85_al_taxa) %>% 
  ggplot(aes(x=taxon_name,y=change,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Top 10 LSF Species in Alaska by catch amount")+ylab("Percentage Change in Catch between 2087 and 2000") +
  theme_bw() +
  coord_flip()

dev.off()

# canada ####

LSF_species_ca[1:10,]

# GFDL_LSF_85-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_ca, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_85_ca_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

LSF_species_ca_top <- LSF_species_ca[1:10,] %>% 
  rename(i = taxonID)

GFDL_LSF_85_ca_taxa$i <- as.numeric(GFDL_LSF_85_ca_taxa$i) 
GFDL_LSF_85_ca_taxa <- GFDL_LSF_85_ca_taxa %>% 
  right_join(LSF_species_ca_top, by = "i")

sum(GFDL_LSF_85_ca_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_LSF_85_ca_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_LSF_85_ca_taxa$RCP <- "8.5"
GFDL_LSF_85_ca_taxa$sector <- "LSF"
GFDL_LSF_85_ca_taxa$climate <- "GFDL"

# GFDL_LSF_26-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_ca, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_26_ca_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

LSF_species_ca_top <- LSF_species_ca[1:10,] %>% 
  rename(i = taxonID)

GFDL_LSF_26_ca_taxa$i <- as.numeric(GFDL_LSF_26_ca_taxa$i) 
GFDL_LSF_26_ca_taxa <- GFDL_LSF_26_ca_taxa %>% 
  right_join(LSF_species_ca_top, by = "i")

sum(GFDL_LSF_26_ca_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_LSF_26_ca_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_LSF_26_ca_taxa$RCP <- "2.6"
GFDL_LSF_26_ca_taxa$sector <- "LSF"
GFDL_LSF_26_ca_taxa$climate <- "GFDL"

# maintain levels
GFDL_LSF_26_ca_taxa$taxon_name <- factor(GFDL_LSF_26_ca_taxa$taxon_name, levels = rev(GFDL_LSF_26_ca_taxa$taxon_name))
GFDL_LSF_85_ca_taxa$taxon_name <- factor(GFDL_LSF_85_ca_taxa$taxon_name, levels = rev(GFDL_LSF_85_ca_taxa$taxon_name))



png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/top10_lsf_ca.png", width = 700, height = 463)

GFDL_LSF_26_ca_taxa %>% 
  bind_rows(GFDL_LSF_85_ca_taxa) %>% 
  ggplot(aes(x=taxon_name,y=change,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Top 10 LSF Species in Canada by catch amount")+ylab("Percentage Change in Catch between 2087 and 2000") +
  theme_bw() +
  coord_flip()

dev.off()

# usa ####

LSF_species_us[1:10,]

# GFDL_LSF_85-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_us, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_85_us_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

LSF_species_us_top <- LSF_species_us[1:10,] %>% 
  rename(i = taxonID)

GFDL_LSF_85_us_taxa$i <- as.numeric(GFDL_LSF_85_us_taxa$i) 
GFDL_LSF_85_us_taxa <- GFDL_LSF_85_us_taxa %>% 
  right_join(LSF_species_us_top, by = "i")

sum(GFDL_LSF_85_us_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_LSF_85_us_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_LSF_85_us_taxa$RCP <- "8.5"
GFDL_LSF_85_us_taxa$sector <- "LSF"
GFDL_LSF_85_us_taxa$climate <- "GFDL"

# GFDL_LSF_26-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_us, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_26_us_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

LSF_species_us_top <- LSF_species_us[1:10,] %>% 
  rename(i = taxonID)

GFDL_LSF_26_us_taxa$i <- as.numeric(GFDL_LSF_26_us_taxa$i) 
GFDL_LSF_26_us_taxa <- GFDL_LSF_26_us_taxa %>% 
  right_join(LSF_species_us_top, by = "i")

sum(GFDL_LSF_26_us_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_LSF_26_us_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_LSF_26_us_taxa$RCP <- "2.6"
GFDL_LSF_26_us_taxa$sector <- "LSF"
GFDL_LSF_26_us_taxa$climate <- "GFDL"

# maintain levels
GFDL_LSF_26_us_taxa$taxon_name <- factor(GFDL_LSF_26_us_taxa$taxon_name, levels = rev(GFDL_LSF_26_us_taxa$taxon_name))
GFDL_LSF_85_us_taxa$taxon_name <- factor(GFDL_LSF_85_us_taxa$taxon_name, levels = rev(GFDL_LSF_85_us_taxa$taxon_name))



png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/top10_lsf_us.png", width = 700, height = 463)

GFDL_LSF_26_us_taxa %>% 
  bind_rows(GFDL_LSF_85_us_taxa) %>% 
  ggplot(aes(x=taxon_name,y=change,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Top 10 LSF Species in USA West Coast by catch amount")+ylab("Percentage Change in Catch between 2087 and 2000") +
  theme_bw() +
  coord_flip()


dev.off()

# Mexico ####

LSF_species_me[1:10,]

# GFDL_LSF_85-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_me, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_85_me_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

LSF_species_me_top <- LSF_species_me[1:10,] %>% 
  rename(i = taxonID)

GFDL_LSF_85_me_taxa$i <- as.numeric(GFDL_LSF_85_me_taxa$i) 
GFDL_LSF_85_me_taxa <- GFDL_LSF_85_me_taxa %>% 
  right_join(LSF_species_me_top, by = "i")

sum(GFDL_LSF_85_me_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_LSF_85_me_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_LSF_85_me_taxa$RCP <- "8.5"
GFDL_LSF_85_me_taxa$sector <- "LSF"
GFDL_LSF_85_me_taxa$climate <- "GFDL"

# GFDL_LSF_26-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_me, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_26_me_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

LSF_species_me_top <- LSF_species_me[1:10,] %>% 
  rename(i = taxonID)

GFDL_LSF_26_me_taxa$i <- as.numeric(GFDL_LSF_26_me_taxa$i) 
GFDL_LSF_26_me_taxa <- GFDL_LSF_26_me_taxa %>% 
  right_join(LSF_species_me_top, by = "i")

sum(GFDL_LSF_26_me_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_LSF_26_me_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_LSF_26_me_taxa$RCP <- "2.6"
GFDL_LSF_26_me_taxa$sector <- "LSF"
GFDL_LSF_26_me_taxa$climate <- "GFDL"

# maintain levels
GFDL_LSF_26_me_taxa$taxon_name <- factor(GFDL_LSF_26_me_taxa$taxon_name, levels = rev(GFDL_LSF_26_me_taxa$taxon_name))
GFDL_LSF_85_me_taxa$taxon_name <- factor(GFDL_LSF_85_me_taxa$taxon_name, levels = rev(GFDL_LSF_85_me_taxa$taxon_name))



png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/top10_lsf_me.png", width = 700, height = 463)

GFDL_LSF_26_me_taxa %>% 
  bind_rows(GFDL_LSF_85_me_taxa) %>% 
  ggplot(aes(x=taxon_name,y=change,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Top 10 LSF Species in Mexico by catch amount")+ylab("Percentage Change in Catch between 2087 and 2000") +
  theme_bw() +
  coord_flip()


dev.off()

# SSF ####

# alaska ####

SSF_species_al[1:10,]

# GFDL_LSF_85-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_al, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_SSF_85_al_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

SSF_species_al_top <- SSF_species_al[1:10,] %>% 
  rename(i = taxonID)

GFDL_SSF_85_al_taxa$i <- as.numeric(GFDL_SSF_85_al_taxa$i) 
GFDL_SSF_85_al_taxa <- GFDL_SSF_85_al_taxa %>% 
  right_join(SSF_species_al_top, by = "i")

sum(GFDL_SSF_85_al_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_SSF_85_al_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_SSF_85_al_taxa$RCP <- "8.5"
GFDL_SSF_85_al_taxa$sector <- "SSF"
GFDL_SSF_85_al_taxa$climate <- "GFDL"

# GFDL_LSF_26-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_al, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_SSF_26_al_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

SSF_species_al_top <- SSF_species_al[1:10,] %>% 
  rename(i = taxonID)

GFDL_SSF_26_al_taxa$i <- as.numeric(GFDL_SSF_26_al_taxa$i) 
GFDL_SSF_26_al_taxa <- GFDL_SSF_26_al_taxa %>% 
  right_join(SSF_species_al_top, by = "i")

sum(GFDL_SSF_26_al_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_SSF_26_al_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_SSF_26_al_taxa$RCP <- "2.6"
GFDL_SSF_26_al_taxa$sector <- "SSF"
GFDL_SSF_26_al_taxa$climate <- "GFDL"

# maintain levels
GFDL_SSF_26_al_taxa$taxon_name <- factor(GFDL_SSF_26_al_taxa$taxon_name, levels = rev(GFDL_SSF_26_al_taxa$taxon_name))
GFDL_SSF_85_al_taxa$taxon_name <- factor(GFDL_SSF_85_al_taxa$taxon_name, levels = rev(GFDL_SSF_85_al_taxa$taxon_name))

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/top10_ssf_al.png", width = 700, height = 463)

GFDL_SSF_26_al_taxa %>% 
  bind_rows(GFDL_SSF_85_al_taxa) %>% 
  ggplot(aes(x=taxon_name,y=change,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Top 10 SSF Species in Alaska by catch amount")+ylab("Percentage Change in Catch between 2087 and 2000") +
  theme_bw() +
  coord_flip()

dev.off()

# canada ####

SSF_species_ca[1:10,]

# GFDL_LSF_85-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_ca, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_SSF_85_ca_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change)

SSF_species_ca_top <- SSF_species_ca[1:10,] %>% 
  rename(i = taxonID)

GFDL_SSF_85_ca_taxa$i <- as.numeric(GFDL_SSF_85_ca_taxa$i) 
GFDL_SSF_85_ca_taxa <- GFDL_SSF_85_ca_taxa %>% 
  right_join(SSF_species_ca_top, by = "i")

sum(GFDL_SSF_85_ca_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_SSF_85_ca_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_SSF_85_ca_taxa$RCP <- "8.5"
GFDL_SSF_85_ca_taxa$sector <- "SSF"
GFDL_SSF_85_ca_taxa$climate <- "GFDL"

# GFDL_LSF_26-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_ca, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_SSF_26_ca_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

SSF_species_ca_top <- SSF_species_ca[1:10,] %>% 
  rename(i = taxonID)

GFDL_SSF_26_ca_taxa$i <- as.numeric(GFDL_SSF_26_ca_taxa$i) 
GFDL_SSF_26_ca_taxa <- GFDL_SSF_26_ca_taxa %>% 
  right_join(SSF_species_ca_top, by = "i")

sum(GFDL_SSF_26_ca_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_SSF_26_ca_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_SSF_26_ca_taxa$RCP <- "2.6"
GFDL_SSF_26_ca_taxa$sector <- "SSF"
GFDL_SSF_26_ca_taxa$climate <- "GFDL"

# maintain levels
GFDL_SSF_26_ca_taxa$taxon_name <- factor(GFDL_SSF_26_ca_taxa$taxon_name, levels = rev(GFDL_SSF_26_ca_taxa$taxon_name))
GFDL_SSF_85_ca_taxa$taxon_name <- factor(GFDL_SSF_85_ca_taxa$taxon_name, levels = rev(GFDL_SSF_85_ca_taxa$taxon_name))



png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/top10_ssf_ca.png", width = 700, height = 463)

GFDL_SSF_26_ca_taxa %>% 
  bind_rows(GFDL_SSF_85_ca_taxa) %>% 
  ggplot(aes(x=taxon_name,y=change,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Top 10 SSF Species in Canada by catch amount")+ylab("Percentage Change in Catch between 2087 and 2000") +
  theme_bw() +
  coord_flip()

dev.off()

# usa ####

SSF_species_us[1:10,]

# GFDL_LSF_85-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_us, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_SSF_85_us_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

SSF_species_us_top <- SSF_species_us[1:10,] %>% 
  rename(i = taxonID)

GFDL_SSF_85_us_taxa$i <- as.numeric(GFDL_SSF_85_us_taxa$i) 
GFDL_SSF_85_us_taxa <- GFDL_SSF_85_us_taxa %>% 
  right_join(SSF_species_us_top, by = "i")

sum(GFDL_SSF_85_us_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_SSF_85_us_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_SSF_85_us_taxa$RCP <- "8.5"
GFDL_SSF_85_us_taxa$sector <- "SSF"
GFDL_SSF_85_us_taxa$climate <- "GFDL"

# GFDL_LSF_26-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_us, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_SSF_26_us_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

SSF_species_us_top <- SSF_species_us[1:10,] %>% 
  rename(i = taxonID)

GFDL_SSF_26_us_taxa$i <- as.numeric(GFDL_SSF_26_us_taxa$i) 
GFDL_SSF_26_us_taxa <- GFDL_SSF_26_us_taxa %>% 
  right_join(SSF_species_us_top, by = "i")

sum(GFDL_SSF_26_us_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_SSF_26_us_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_SSF_26_us_taxa$RCP <- "2.6"
GFDL_SSF_26_us_taxa$sector <- "SSF"
GFDL_SSF_26_us_taxa$climate <- "GFDL"

# maintain levels
GFDL_SSF_26_us_taxa$taxon_name <- factor(GFDL_SSF_26_us_taxa$taxon_name, levels = rev(GFDL_SSF_26_us_taxa$taxon_name))
GFDL_SSF_85_us_taxa$taxon_name <- factor(GFDL_SSF_85_us_taxa$taxon_name, levels = rev(GFDL_SSF_85_us_taxa$taxon_name))



png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/top10_ssf_us.png", width = 700, height = 463)

GFDL_SSF_26_us_taxa %>% 
  bind_rows(GFDL_SSF_85_us_taxa) %>% 
  ggplot(aes(x=taxon_name,y=change,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Top 10 SSF Species in USA West Coast by catch amount")+ylab("Percentage Change in Catch between 2087 and 2000") +
  theme_bw() +
  coord_flip()


dev.off()

# Mexico ####

SSF_species_me[1:10,]

# GFDL_LSF_85-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_me, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_SSF_85_me_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

SSF_species_me_top <- SSF_species_me[1:10,] %>% 
  rename(i = taxonID)

GFDL_SSF_85_me_taxa$i <- as.numeric(GFDL_SSF_85_me_taxa$i) 
GFDL_SSF_85_me_taxa <- GFDL_SSF_85_me_taxa %>% 
  right_join(SSF_species_me_top, by = "i")

sum(GFDL_SSF_85_me_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_SSF_85_me_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_SSF_85_me_taxa$RCP <- "8.5"
GFDL_SSF_85_me_taxa$sector <- "SSF"
GFDL_SSF_85_me_taxa$climate <- "GFDL"

# GFDL_LSF_26-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  p <- avg_catch %>% 
    dplyr::select(cellID, `2000`, `2087`) %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_me, by = "INDEX") %>% 
    colSums()
  
  p$i <- substr(filepath[i], 95,100)
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_SSF_26_me_taxa <- mydata %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change, i) %>% 
  arrange(-change) 

SSF_species_me_top <- SSF_species_me[1:10,] %>% 
  rename(i = taxonID)

GFDL_SSF_26_me_taxa$i <- as.numeric(GFDL_SSF_26_me_taxa$i) 
GFDL_SSF_26_me_taxa <- GFDL_SSF_26_me_taxa %>% 
  right_join(SSF_species_me_top, by = "i")

sum(GFDL_SSF_26_me_taxa$change < 0, na.rm = TRUE) # 102
sum(GFDL_SSF_26_me_taxa$change > 0, na.rm = TRUE) # 173

# alaska
GFDL_SSF_26_me_taxa$RCP <- "2.6"
GFDL_SSF_26_me_taxa$sector <- "SSF"
GFDL_SSF_26_me_taxa$climate <- "GFDL"

# maintain levels
GFDL_SSF_26_me_taxa$taxon_name <- factor(GFDL_SSF_26_me_taxa$taxon_name.y, levels = rev(GFDL_SSF_26_me_taxa$taxon_name.y))
GFDL_SSF_85_me_taxa$taxon_name <- factor(GFDL_SSF_85_me_taxa$taxon_name.y, levels = rev(GFDL_SSF_85_me_taxa$taxon_name.y))



png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/top10_ssf_me.png", width = 700, height = 463)

GFDL_SSF_26_me_taxa %>% 
  bind_rows(GFDL_SSF_85_me_taxa) %>% 
  ggplot(aes(x=taxon_name,y=change,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Top 10 SSF Species in Mexico by catch amount")+ylab("Percentage Change in Catch between 2087 and 2000") +
  theme_bw() +
  coord_flip()


dev.off()
