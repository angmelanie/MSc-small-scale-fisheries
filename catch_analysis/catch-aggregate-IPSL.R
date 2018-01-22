# remove spatial, aggregate catch by eez by RCP by sector


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

# percentage change

IPSL_LSF_85 <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  spread(year, year_total) %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change)

write.csv(IPSL_LSF_85, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/IPSL_LSF_85")
IPSL_LSF_85 <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/IPSL_LSF_85")


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

# percentage change

IPSL_LSF_26 <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  spread(year, year_total) %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change)

write.csv(IPSL_LSF_26, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/IPSL_LSF_26")
IPSL_LSF_26 <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/IPSL_LSF_26")


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

# percentage change

IPSL_SSF_85 <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  spread(year, year_total) %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change)

write.csv(IPSL_SSF_85, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/IPSL_SSF_85")
IPSL_SSF_85 <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/IPSL_SSF_85")


# GFDL_SSF_26 -------------------------
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

# percentage change

IPSL_SSF_26 <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  spread(year, year_total) %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change)

write.csv(IPSL_SSF_26, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/IPSL_SSF_26")
IPSL_SSF_26 <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/IPSL_SSF_26")


###### test area
IPSL_SSF_26 %>% 
  ggplot(aes(x=year,y=year_total)) +
  geom_point(aes(x=year,y=year_total))
