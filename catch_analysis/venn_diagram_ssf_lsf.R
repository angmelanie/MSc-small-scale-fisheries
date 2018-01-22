# Venn diagram

# how many species did you model?

path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/", filename, sep = "")


library(dplyr)
library(tidyverse)
# Find out what proportion each taxa contributes
  # commonspecies
  # SSF_list
  # LSF_list
  # lsfnotssf
  # ssfnotlsf

left_join(SSF_list, LSF_list, by = "taxonID") %>% 
  View()

ssfnotlsf$ID <- "ssfnotlsf"
lsfnotssf$ID <- "lsfnotssf"

# AGGREGATE PNA ####

left_join(LSF_list, lsfnotssf) %>% 
  left_join(mel_assigned) %>% 
  arrange(-sum) %>% 
  View()

mel_assigned_up <- read.csv(file = "C:/Users/angmel/Documents/MSc-small-scale-fisheries/reference_tables/mel_assigned_taxa_updated.csv")


# you need to find out over future accumulated catch

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
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    dplyr::select(-cellID) %>% 
    sum()
  
  taxaIDstring <- substr(filename[i], 1,6)
  
  datalist[[taxaIDstring]] <- p # attaches each csv file as a new list
  
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

LSF_totalcatch <- t(mydata)
LSF_totalcatch <- data.frame(LSF_totalcatch)
LSF_totalcatch$taxonID <- rownames(LSF_totalcatch)
LSF_totalcatch$taxonID <- as.numeric(LSF_totalcatch$taxonID)

# Join with LSF list
LSF_species <- left_join(LSF_list, lsfnotssf) %>%
  left_join(mel_assigned) %>% 
  filter(source %in% c("DROBO", "cygwin")) %>% 
  left_join(LSF_totalcatch) %>% 
  arrange(-LSF_totalcatch)

LSF_species[1:149,] %>% 
  semi_join(SSF_species[1:137,])
  

lsfnotssf_accumulated <- LSF_species %>% 
  filter(ID %in% "lsfnotssf") %>% 
  filter(!LSF_totalcatch %in% NA)

sum(LSF_species$LSF_totalcatch, na.rm = TRUE)
# 129291403
# 139669411
sum(lsfnotssf_accumulated$LSF_totalcatch)
# 977682.8
# 9620206

# less than 1% of LSF is exclusively LSF species, rest >99% is mutual species

SSF_species <- left_join(SSF_list, ssfnotlsf) %>%
  left_join(mel_assigned) %>% 
  filter(source %in% c("DROBO", "cygwin"))
LSF_species2 <- LSF_species[1:149,]

LSF_species2 %>% 
  mutate(per = LSF_totalcatch/sum(LSF_totalcatch)*100) %>% 
  View()

LSF_species3 <- LSF_species2[LSF_species2$LSF_totalcatch > quantile(LSF_species2$LSF_totalcatch,prob=1-50/100),] %>% 
  dplyr::select(taxon_name, LSF_totalcatch, ID) %>% 
  arrange(-LSF_totalcatch) 

LSF_species3$ID[is.na(LSF_species3$ID)] <- "Common"

# SAVE LSF
png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/50_lsf_pna.png", width = 1500, height = 400)

LSF_species3 %>% 
    ggplot() +
  theme_bw() +
  geom_histogram(aes(x=reorder(taxon_name, -LSF_totalcatch), y=LSF_totalcatch, fill = ID), stat = "identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Species composition of the top 50% of large-scale fisheries' catches in Pacific North America", y = "Catch amount", x = "Taxon name") +
  scale_fill_manual(values = c("Grey", "Red"), labels = c("Common", "Unique to LSF")) +
  scale_y_continuous(labels = scales::comma)

dev.off()
LSF_species2[LSF_species2$LSF_totalcatch > quantile(LSF_species2$LSF_totalcatch,prob=1-50/100),]

  # selecting for top 10
top10LSF <- LSF_species2[LSF_species2$LSF_totalcatch > quantile(LSF_species2$LSF_totalcatch,prob=1-50/100),]
top10LSF_taxa <- top10LSF[1:10,c(1,6)]
  


# GFDL_SSF_85-------------------------
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
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    dplyr::select(-cellID) %>% 
    sum()
  
  taxaIDstring <- substr(filename[i], 1,6)
  
  datalist[[taxaIDstring]] <- p # attaches each csv file as a new list
  
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

SSF_totalcatch <- t(mydata)
SSF_totalcatch <- data.frame(SSF_totalcatch)
SSF_totalcatch$taxonID <- rownames(SSF_totalcatch)
SSF_totalcatch$taxonID <- as.numeric(SSF_totalcatch$taxonID)

# ssfnotlsf$ID <- "ssfnotlsf"

# Join with LSF list
SSF_species <- left_join(SSF_list, ssfnotlsf) %>%
  left_join(mel_assigned) %>% 
  filter(source %in% c("DROBO", "cygwin")) %>% 
  left_join(SSF_totalcatch) %>% 
  arrange(-SSF_totalcatch)

ssfnotlsf_accumulated <- SSF_species %>% 
  filter(ID %in% "ssfnotlsf") %>% 
  filter(!SSF_totalcatch %in% NA)

sum(SSF_species$SSF_totalcatch, na.rm = TRUE)
# 28369215
sum(ssfnotlsf_accumulated$SSF_totalcatch)
# 383029.2

# 1.350158% of SSF is exclusively SSF species, rest ~99% is mutual species

SSF_species <- left_join(SSF_list, ssfnotlsf) %>%
  left_join(mel_assigned) %>% 
  filter(source %in% c("DROBO", "cygwin"))

SSF_species2 <- SSF_species[1:137,]

SSF_species3 <- SSF_species2[SSF_species2$SSF_totalcatch > quantile(SSF_species2$SSF_totalcatch,prob=1-50/100),] %>% 
  dplyr::select(taxon_name, SSF_totalcatch, ID) %>% 
  arrange(-SSF_totalcatch) 

SSF_species3[is.na(SSF_species3)] <- 0

top10SSF_taxa <- SSF_species3[1:10,c(1,5)]

SSF_species3$ID[is.na(SSF_species3$ID)] <- "Common"

# SAVE LSF
png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/50_ssf_pna.png", width = 1500, height = 400)

SSF_species3 %>% 
  ggplot() +
  theme_bw() +
  geom_histogram(aes(x=reorder(taxon_name, -SSF_totalcatch), y=SSF_totalcatch, fill = ID), stat = "identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Species composition of the top 50% of small-scale fisheries' catches in Pacific North America", y = "Catch amount", x = "Taxon name") +
  scale_fill_manual(values = c("Grey", "Blue"), labels = c("Common", "Unique to SSF")) +
  scale_y_continuous(labels = scales::comma)

dev.off()


LSF_species2[LSF_species2$LSF_totalcatch > quantile(LSF_species2$LSF_totalcatch,prob=1-50/100),]

left_join(LSF_species3, SSF_species3, by = taxon_name)

###############################MEXICO###############################

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
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_me, by = "INDEX") 
  
  if (sum(p[2,], na.rm = TRUE) > 0 ) {
  q <- p %>%
    dplyr::select(-INDEX) %>%
    dplyr::select(-EEZID) %>% 
    sum()
  
  taxaIDstring <- substr(filename[i], 1,6)
  
  datalist[[taxaIDstring]] <- q # attaches each csv file as a new list
  
  }

  else
    errspp[i] <- q
  }

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

LSF_totalcatch_me <- t(mydata)
LSF_totalcatch_me <- data.frame(LSF_totalcatch_me)
LSF_totalcatch_me$taxonID <- rownames(LSF_totalcatch_me)
LSF_totalcatch_me$taxonID <- as.numeric(LSF_totalcatch_me$taxonID)

# Join with LSF list
LSF_species_me <- left_join(LSF_list, lsfnotssf) %>%
  left_join(mel_assigned) %>% 
  filter(source %in% c("DROBO", "cygwin")) %>% 
  right_join(LSF_totalcatch_me) %>% 
  arrange(-LSF_totalcatch_me)

lsfnotssf_accumulated_me <- LSF_species_me %>% 
  filter(ID %in% "lsfnotssf") %>% 
  filter(!LSF_totalcatch_me %in% NA)

sum(LSF_species_me$LSF_totalcatch_me, na.rm = TRUE)
# 13839535
# 14172476
sum(lsfnotssf_accumulated_me$LSF_totalcatch_me)
# 905577.8
# 1238519
# 6.543412% of LSF is exclusively LSF species, rest >94% is mutual species

LSF_species_me$ID[is.na(LSF_species_me$ID)] <- "Common"

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/10_lsf_me.png", width = 800, height = 450)

LSF_species_me[1:10,] %>% 
  ggplot() +
  theme_bw() +
  geom_histogram(aes(x=reorder(taxon_name, -LSF_totalcatch_me), y=LSF_totalcatch_me, fill = ID), stat = "identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Top 10 species from large-scale fisheries in Mexico (2000-2087)", y = "Catch amount", x = "Taxon name", fill = "") +
  scale_fill_manual(values = c("Grey", "Red"), labels = c("Common", "Unique to LSF")) +
  scale_y_continuous(labels = scales::comma)
dev.off()

# GFDL_SSF_85-------------------------
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
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_me, by = "INDEX") 
  
  if (sum(p[2,], na.rm = TRUE) > 0 ) {
    q <- p %>%
      dplyr::select(-INDEX) %>%
      dplyr::select(-EEZID) %>% 
      sum()
    
    taxaIDstring <- substr(filename[i], 1,6)
    
    datalist[[taxaIDstring]] <- q # attaches each csv file as a new list
    
  }
  
  else
    errspp[i] <- q
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

SSF_totalcatch_me <- t(mydata)
SSF_totalcatch_me <- data.frame(SSF_totalcatch_me)
SSF_totalcatch_me$taxonID <- rownames(SSF_totalcatch_me)
SSF_totalcatch_me$taxonID <- as.numeric(SSF_totalcatch_me$taxonID)

# ssfnotlsf$ID <- "ssfnotlsf"

# Join with LSF list
SSF_totalcatch_me <- SSF_totalcatch_me %>% 
  left_join(mel_assigned_up)
  
SSF_species_me <- left_join(SSF_list, ssfnotlsf) %>%
  left_join(mel_assigned_up) %>% 
  filter(source %in% c("DROBO", "cygwin")) %>% 
  right_join(., SSF_totalcatch_me, by = "taxonID") %>% 
  arrange(-SSF_totalcatch_me)

ssfnotlsf_accumulated_me <- SSF_species_me %>% 
  filter(ID %in% "ssfnotlsf") %>% 
  filter(!SSF_totalcatch_me %in% NA)

sum(SSF_species_me$SSF_totalcatch_me, na.rm = TRUE)
# 3020607
# 8528529
sum(ssfnotlsf_accumulated_me$SSF_totalcatch_me)
# 16913.65
# 9813.925

# 0.5599421% of SSF is exclusively SSF species, rest ~99% is mutual species

SSF_species_me$ID[is.na(SSF_species_me$ID)] <- "Common"


SSF_species_me %>% 
  left_join(mel_assigned_up)

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/10_ssf_me.png", width = 800, height = 450)

SSF_species_me[1:10,] %>% 
  ggplot() +
  theme_bw() +
  geom_histogram(aes(x=reorder(taxon_name.y, -SSF_totalcatch_me), y=SSF_totalcatch_me, fill = ID), stat = "identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Top 10 speciesfrom small-scale fisheries in Mexico (2000-2087)", y = "Catch amount", x = "Taxon name", fill = "") +
  scale_fill_manual( values = c("Grey", "Red"), labels = c("Common", "Unique to SSF")) +
  scale_y_continuous(labels = scales::comma)
dev.off()

###############################UNITED STATES WEST###############################

cellID_USA <- cellID %>% 
  filter(EEZID %in% 848)

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
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_us, by = "INDEX") 
  
  if (sum(p[2,], na.rm = TRUE) > 0 ) {
    q <- p %>%
      dplyr::select(-INDEX) %>%
      dplyr::select(-EEZID) %>% 
      sum()
    
    taxaIDstring <- substr(filename[i], 1,6)
    
    datalist[[taxaIDstring]] <- q # attaches each csv file as a new list
    
  }
  
  else
    errspp[i] <- q
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

LSF_totalcatch_us <- t(mydata)
LSF_totalcatch_us <- data.frame(LSF_totalcatch_us)
LSF_totalcatch_us$taxonID <- rownames(LSF_totalcatch_us)
LSF_totalcatch_us$taxonID <- as.numeric(LSF_totalcatch_us$taxonID)

# Join with LSF list
LSF_species_us <- left_join(LSF_list, lsfnotssf) %>%
  left_join(mel_assigned) %>% 
  filter(source %in% c("DROBO", "cygwin")) %>% 
  left_join(LSF_totalcatch_us) %>% 
  arrange(-LSF_totalcatch_us)

lsfnotssf_accumulated_us <- LSF_species_us %>% 
  filter(ID %in% "lsfnotssf") %>% 
  filter(!LSF_totalcatch_us %in% NA)

sum(LSF_species_us$LSF_totalcatch_us, na.rm = TRUE)
# 17224484
sum(lsfnotssf_accumulated_us$LSF_totalcatch_us)
# 24799.17

# 1.4% of LSF is exclusively LSF species, rest >94% is mutual species
LSF_species_us$ID[is.na(LSF_species_us$ID)] <- "Common"

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/10_lsf_us.png", width = 800, height = 450)

LSF_species_us[1:10,] %>% 
  ggplot() +
  theme_bw() +
  geom_histogram(aes(x=reorder(taxon_name, -LSF_totalcatch_us), y=LSF_totalcatch_us, fill = ID), stat = "identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Top 10 species from large-scale fisheries in USA West Coast (2000-2087)", y = "Catch amount", x = "Taxon name", fill = "") +
  scale_fill_manual(values = c("Grey", "Red"), labels = c("Common", "Unique to LSF")) +
  scale_y_continuous(labels = scales::comma)
dev.off()

# GFDL_SSF_85-------------------------
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
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_us, by = "INDEX") 
  
  if (sum(p[2,], na.rm = TRUE) > 0 ) {
    q <- p %>%
      dplyr::select(-INDEX) %>%
      dplyr::select(-EEZID) %>% 
      sum()
    
    taxaIDstring <- substr(filename[i], 1,6)
    
    datalist[[taxaIDstring]] <- q # attaches each csv file as a new list
    
  }
  
  else
    errspp[i] <- q
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

SSF_totalcatch_us <- t(mydata)
SSF_totalcatch_us <- data.frame(SSF_totalcatch_us)
SSF_totalcatch_us$taxonID <- rownames(SSF_totalcatch_us)
SSF_totalcatch_us$taxonID <- as.numeric(SSF_totalcatch_us$taxonID)

# ssfnotlsf$ID <- "ssfnotlsf"

# Join with LSF list
SSF_species_us<- left_join(SSF_list, ssfnotlsf) %>%
  left_join(mel_assigned) %>% 
  filter(source %in% c("DROBO", "cygwin")) %>% 
  left_join(SSF_totalcatch_us) %>% 
  arrange(-SSF_totalcatch_us)

ssfnotlsf_accumulated_us <- SSF_species_us %>% 
  filter(ID %in% "ssfnotlsf") %>% 
  filter(!SSF_totalcatch_us %in% NA)

sum(SSF_species_us$SSF_totalcatch_us, na.rm = TRUE)
# 6478701
sum(ssfnotlsf_accumulated_us$SSF_totalcatch_us)
# 127927.7

# 1.97% of SSF is exclusively SSF species, rest ~99% is mutual species
SSF_species_us$ID[is.na(SSF_species_us$ID)] <- "Common"

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/10_ssf_us.png", width = 800, height = 450)

SSF_species_us[1:10,] %>% 
  ggplot() +
  theme_bw() +
  geom_histogram(aes(x=reorder(taxon_name, -SSF_totalcatch_us), y=SSF_totalcatch_us, fill = ID), stat = "identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Top 10 species from small-scale fisheries in USA West Coast (2000-2087)", y = "Catch amount", x = "Taxon name", fill = "") +
  scale_fill_manual(values = c("Grey", "Red"), labels = c("Common", "Unique to SSF")) +
  scale_y_continuous(labels = scales::comma)
dev.off()


###############################CANADA###############################

cellID_Canada <- cellID %>% 
  filter(EEZID %in% 925)

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
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_ca, by = "INDEX") 
  
  if (sum(p[2,], na.rm = TRUE) > 0 ) {
    q <- p %>%
      dplyr::select(-INDEX) %>%
      dplyr::select(-EEZID) %>% 
      sum()
    
    taxaIDstring <- substr(filename[i], 1,6)
    
    datalist[[taxaIDstring]] <- q # attaches each csv file as a new list
    
  }
  
  else
    errspp[i] <- q
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

LSF_totalcatch_ca <- t(mydata)
LSF_totalcatch_ca <- data.frame(LSF_totalcatch_ca)
LSF_totalcatch_ca$taxonID <- rownames(LSF_totalcatch_ca)
LSF_totalcatch_ca$taxonID <- as.numeric(LSF_totalcatch_ca$taxonID)

# Join with LSF list
LSF_species_ca <- left_join(LSF_list, lsfnotssf) %>%
  left_join(mel_assigned) %>% 
  filter(source %in% c("DROBO", "cygwin")) %>% 
  right_join(LSF_totalcatch_ca) %>% 
  arrange(-LSF_totalcatch_ca)

lsfnotssf_accumulated_ca <- LSF_species_ca %>% 
  filter(ID %in% "lsfnotssf") %>% 
  filter(!LSF_totalcatch_ca %in% NA)

sum(LSF_species_ca$LSF_totalcatch_ca, na.rm = TRUE)
# 15905288
sum(lsfnotssf_accumulated_ca$LSF_totalcatch_ca)
# 34192.22

# 0.2149739% of LSF is exclusively LSF species, rest >94% is mutual species
LSF_species_ca$ID[is.na(LSF_species_ca$ID)] <- "Common"

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/10_lsf_ca.png", width = 800, height = 450)

LSF_species_ca[1:10,] %>% 
  ggplot() +
  theme_bw() +
  geom_histogram(aes(x=reorder(taxon_name, -LSF_totalcatch_ca), y=LSF_totalcatch_ca, fill = ID), stat = "identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Top 10 speciesfrom large-scale fisheries in Canada Pacific (2000-2087)", y = "Catch amount", x = "Taxon name", fill = "") +
  scale_fill_manual(values = c("Grey", "Red"), labels = c("Common", "Unique to LSF")) +
  scale_y_continuous(labels = scales::comma)
dev.off()

# GFDL_SSF_85-------------------------
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
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_ca, by = "INDEX") 
  
  if (sum(p[2,], na.rm = TRUE) > 0 ) {
    q <- p %>%
      dplyr::select(-INDEX) %>%
      dplyr::select(-EEZID) %>% 
      sum()
    
    taxaIDstring <- substr(filename[i], 1,6)
    
    datalist[[taxaIDstring]] <- q # attaches each csv file as a new list
    
  }
  
  else
    errspp[i] <- q
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

SSF_totalcatch_ca <- t(mydata)
SSF_totalcatch_ca <- data.frame(SSF_totalcatch_ca)
SSF_totalcatch_ca$taxonID <- rownames(SSF_totalcatch_ca)
SSF_totalcatch_ca$taxonID <- as.numeric(SSF_totalcatch_ca$taxonID)

# ssfnotlsf$ID <- "ssfnotlsf"
SSF_totalcatch_ca <- right_join(SSF_totalcatch_ca, mel_assigned_up)

# Join with LSF list
SSF_species_ca<- left_join(SSF_list, ssfnotlsf) %>%
  left_join(mel_assigned) %>% 
  filter(source %in% c("DROBO", "cygwin")) %>% 
  right_join(SSF_totalcatch_ca) %>% 
  arrange(-SSF_totalcatch_ca)

ssfnotlsf_accumulated_ca <- SSF_species_ca %>% 
  filter(ID %in% "ssfnotlsf") %>% 
  filter(!SSF_totalcatch_ca %in% NA)

sum(SSF_species_ca$SSF_totalcatch_ca, na.rm = TRUE)
# 4719748
sum(ssfnotlsf_accumulated_ca$SSF_totalcatch_ca)
# 114474.1

# 2.425428% of SSF is exclusively SSF species, rest ~99% is mutual species
SSF_species_ca$ID[is.na(SSF_species_ca$ID)] <- "Common"

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/10_ssf_ca.png", width = 800, height = 450)

SSF_species_ca[c(1:9,11),] %>% 
  ggplot() +
  theme_bw() +
  geom_histogram(aes(x=reorder(taxon_name, -SSF_totalcatch_ca), y=SSF_totalcatch_ca, fill = ID), stat = "identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Top 10 species from small-scale fisheries in Canada Pacific (2000-2087)", y = "Catch amount", x = "Taxon name", fill = "") +
  scale_fill_manual(values = c("Grey", "Red"), labels = c("Common", "Unique to SSF")) +
  scale_y_continuous(labels = scales::comma)
dev.off()

###############################ALASKA###############################

cellID_Alaska <- cellID %>% 
  filter(EEZID %in% 959)

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
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_al, by = "INDEX") 
  
  if (sum(p[2,], na.rm = TRUE) > 0 ) {
    q <- p %>%
      dplyr::select(-INDEX) %>%
      dplyr::select(-EEZID) %>% 
      sum()
    
    taxaIDstring <- substr(filename[i], 1,6)
    
    datalist[[taxaIDstring]] <- q # attaches each csv file as a new list
    
  }
  
  else
    errspp[i] <- q
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

LSF_totalcatch_al <- t(mydata)
LSF_totalcatch_al <- data.frame(LSF_totalcatch_al)
LSF_totalcatch_al$taxonID <- rownames(LSF_totalcatch_al)
LSF_totalcatch_al$taxonID <- as.numeric(LSF_totalcatch_al$taxonID)

# Join with LSF list
LSF_species_al <- left_join(LSF_list, lsfnotssf) %>%
  left_join(mel_assigned) %>% 
  filter(source %in% c("DROBO", "cygwin")) %>% 
  right_join(., LSF_totalcatch_al) %>% 
  arrange(-LSF_totalcatch_al)

lsfnotssf_accumulated_al <- LSF_species_al %>% 
  filter(ID %in% "lsfnotssf") %>% 
  filter(!LSF_totalcatch_al %in% NA)

sum(LSF_species_al$LSF_totalcatch_al, na.rm = TRUE)
# 87440869
sum(lsfnotssf_accumulated_al$LSF_totalcatch_al)
# 24310.55

# 0.02780227% of LSF is exclusively LSF species, rest >94% is mutual species
LSF_species_al$ID[is.na(LSF_species_al$ID)] <- "Common"

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/10_lsf_al.png", width = 800, height = 450)

LSF_species_al[1:10,] %>% 
  ggplot() +
  theme_bw() +
  geom_histogram(aes(x=reorder(taxon_name, -LSF_totalcatch_al), y=LSF_totalcatch_al, fill = ID), stat = "identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Top 10 species from large-scale fisheries in Alaska (2000-2087)", y = "Catch amount", x = "Taxon name", fill = "") +
  scale_fill_manual(values = c("Grey", "Red"), labels = c("Common", "Unique to LSF")) +
  scale_y_continuous(labels = scales::comma)
dev.off()


# GFDL_SSF_85-------------------------
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
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_al, by = "INDEX") 
  
  if (sum(p[2,], na.rm = TRUE) > 0 ) {
    q <- p %>%
      dplyr::select(-INDEX) %>%
      dplyr::select(-EEZID) %>% 
      sum()
    
    taxaIDstring <- substr(filename[i], 1,6)
    
    datalist[[taxaIDstring]] <- q # attaches each csv file as a new list
    
  }
  
  else
    errspp[i] <- q
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

SSF_totalcatch_al <- t(mydata)
SSF_totalcatch_al <- data.frame(SSF_totalcatch_al)
SSF_totalcatch_al$taxonID <- rownames(SSF_totalcatch_al)
SSF_totalcatch_al$taxonID <- as.numeric(SSF_totalcatch_al$taxonID)

# ssfnotlsf$ID <- "ssfnotlsf"

# Join with LSF list
SSF_species_al<- left_join(SSF_list, ssfnotlsf) %>%
  left_join(mel_assigned) %>% 
  filter(source %in% c("DROBO", "cygwin")) %>% 
  right_join(SSF_totalcatch_al) %>% 
  arrange(-SSF_totalcatch_al)

ssfnotlsf_accumulated_al <- SSF_species_al %>% 
  filter(ID %in% "ssfnotlsf") %>% 
  filter(!SSF_totalcatch_al %in% NA) %>% 
  arrange(-SSF_totalcatch_al)

sum(SSF_species_al$SSF_totalcatch_al, na.rm = TRUE)
# 88742762
# 16548024
sum(ssfnotlsf_accumulated_al$SSF_totalcatch_al)
# 1326203
# 140671.6

# 1.494435 of SSF is exclusively SSF species, rest ~99% is mutual species

SSF_species_al$ID[is.na(SSF_species_al$ID)] <- "Common"

png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/10_ssf_al.png", width = 800, height = 450)

SSF_species_al[1:10,] %>% 
  ggplot() +
  theme_bw() +
  geom_histogram(aes(x=reorder(taxon_name, -SSF_totalcatch_al), y=SSF_totalcatch_al, fill = ID), stat = "identity") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  labs(title = "Top 10 species from small-scale fisheries in Alaska (2000-2087)", y = "Catch amount", x = "Taxon name", fill = "") +
  scale_fill_manual(values = c("Grey", "Red"), labels = c("Common", "Unique to SSF")) +
  scale_y_continuous(labels = scales::comma)
dev.off()
