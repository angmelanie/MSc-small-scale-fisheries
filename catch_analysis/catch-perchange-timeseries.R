# Remove spatial component, aggregate
# Time series - how does LSF and SSF catch PERCENTAGE change from 2000-2089?

#######################LARGE-SCALE FISHERIES###########################

#############GFDL#############

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
    gather(year, catch, -c(cellID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) %>% 
    summarize(year_total = sum(catch))
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_85_per_ts <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  mutate(change = ((year_total-377958.8)/377958.8)*100) 

GFDL_LSF_85_per_ts$RCP <- "8.5"
GFDL_LSF_85_per_ts$climate <- "GFDL"
GFDL_LSF_85_per_ts$sector <- "LSF"

GFDL_LSF_ts_plot <- GFDL_LSF_85_per_ts %>% 
  ggplot(aes(x=year,y=change)) +
  geom_point(aes(x=year,y=change, color = RCP)) +
  theme_bw() +
  labs(title = "Large-scale fisheries catches in PNA") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = c("2000", "2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1", direction = -1, name = "RCP")


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
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    gather(year, catch, -c(cellID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) %>% 
    summarize(year_total = sum(catch))
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_26_per_ts <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  mutate(change = ((year_total-1605615)/1605615)*100) 

GFDL_LSF_26_per_ts$RCP <- "2.6"
GFDL_LSF_26_per_ts$climate <- "GFDL"
GFDL_LSF_26_per_ts$sector <- "LSF"




#####Plotting data########
GFDL_LSF_per_ts <- GFDL_LSF_26_per_ts %>% 
  bind_rows(GFDL_LSF_85_per_ts)

GFDL_LSF_ts_plot <- GFDL_LSF_per_ts %>% 
  ggplot(aes(x=year,y=change)) +
  geom_point(aes(x=year,y=change, color = RCP)) +
  theme_bw() +
  labs(title = "Large-scale fisheries catches in PNA") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = c("2000", "2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1", direction = -1, name = "RCP")



#############IPSL#############
# IPSL_LSF_85-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/IPSL/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/IPSL/85/avg/", filename, sep = "")

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
    gather(year, catch, -c(cellID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) %>% 
    summarize(year_total = sum(catch))
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

IPSL_LSF_85_per_ts$RCP <- "8.5"
IPSL_LSF_85_per_ts$climate <- "IPSL"
IPSL_LSF_85_per_ts$sector <- "LSF"

IPSL_LSF_85_per_ts <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  mutate(change = ((year_total-1404140)/1404140)*100) 

# IPSL_LSF_26 -------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/IPSL/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/IPSL/26/avg/", filename, sep = "")

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
    gather(year, catch, -c(cellID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) %>% 
    summarize(year_total = sum(catch))
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

IPSL_LSF_26_per_ts$RCP <- "2.6"
IPSL_LSF_26_per_ts$climate <- "IPSL"
IPSL_LSF_26_per_ts$sector <- "LSF"

IPSL_LSF_26_per_ts <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  mutate(change = ((year_total-1411143)/1411143)*100) 

########PLOT DATA#########

LSF_per_ts <- GFDL_LSF_26_per_ts %>% 
  bind_rows(GFDL_LSF_85_per_ts) %>% 
  bind_rows(IPSL_LSF_26_per_ts) %>% 
  bind_rows(IPSL_LSF_85_per_ts)

LSF_ts_plot <- LSF_per_ts %>% 
  ggplot(aes(x=year,y=change)) +
  geom_point(aes(x=year,y=change, color = RCP, shape = climate)) +
  theme_bw() +
  labs(title = "Large-scale fisheries catches in PNA") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = c("2000", "2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1", direction = -1, name = "RCP")


###########################SMALL SCALE FISHERIES############################

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
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    gather(year, catch, -c(cellID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) %>% 
    summarize(year_total = sum(catch))
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

GFDL_SSF_85_per_ts$RCP <- "8.5"
GFDL_SSF_85_per_ts$climate <- "GFDL"
GFDL_SSF_85_per_ts$sector <- "SSF"

GFDL_SSF_85_per_ts <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  mutate(change = ((year_total-398669.2)/398669.2)*100) 


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
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    gather(year, catch, -c(cellID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) %>% 
    summarize(year_total = sum(catch))
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

GFDL_SSF_26_per_ts$RCP <- "2.6"
GFDL_SSF_26_per_ts$climate <- "GFDL"
GFDL_SSF_26_per_ts$sector <- "SSF"

GFDL_SSF_26_per_ts <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  mutate(change = ((year_total-401949.2)/401949.2)*100) 


# IPSL_SSF_85 -------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/IPSL/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/IPSL/85/avg/", filename, sep = "")

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
    gather(year, catch, -c(cellID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) %>% 
    summarize(year_total = sum(catch))
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

IPSL_SSF_85_per_ts$RCP <- "8.5"
IPSL_SSF_85_per_ts$climate <- "IPSL"
IPSL_SSF_85_per_ts$sector <- "SSF"

IPSL_SSF_85_per_ts <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  mutate(change = ((year_total-379822.8)/379822.8)*100) 


# IPSL_SSF_26 -------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/IPSL/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/SSF/IPSL/26/avg/", filename, sep = "")

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
    gather(year, catch, -c(cellID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) %>% 
    summarize(year_total = sum(catch))
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

IPSL_SSF_26_per_ts$RCP <- "2.6"
IPSL_SSF_26_per_ts$climate <- "IPSL"
IPSL_SSF_26_per_ts$sector <- "SSF"

IPSL_SSF_26_per_ts <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  mutate(change = ((year_total-377958.8)/377958.8)*100) 

######PLOT DATA#########
SSF_PNA_per_ts <- GFDL_SSF_85_per_ts %>%
  bind_rows(GFDL_SSF_26_per_ts) %>% 
  bind_rows(IPSL_SSF_85_per_ts) %>% 
  bind_rows(IPSL_SSF_26_per_ts)


SSF_ts_per_plot <- SSF_PNA_per_ts %>% 
  ggplot(aes(x=year,y=change)) +
  geom_point(aes(x=year,y=change, color = RCP, shape = climate)) +
  theme_bw() +
  labs(title = "Small-scale fisheries catches in PNA") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = c("2000", "2010", "2020", "2030", "2040", "2050", "2060", "2070", "2080")) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1", direction = -1, name = "RCP")

