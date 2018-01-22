# Remove spatial component, aggregate
# Time series

# Question 1: 

# What are the projected impacts on catches of small-scale fisheries within the region/EEZs?
# AKA how does catch amount change in the future between Alaska, Canada, USA and Mexico?

# Let's work within each EEZ - find total catch by year within each EEZ
# change path for RCP scenarios and EEZ

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

GFDL_LSF_85_ts <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total))

GFDL_LSF_85_ts$RCP <- "8.5"
GFDL_LSF_85_ts$climate <- "GFDL"
GFDL_LSF_85_ts$sector <- "LSF"


write.csv(GFDL_LSF_85, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/GFDL_LSF_85")


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

GFDL_LSF_26 <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  spread(year, year_total) %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change)

write.csv(GFDL_LSF_26, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/GFDL_LSF_26")


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

# percentage change

GFDL_SSF_85 <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  spread(year, year_total) %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change)

write.csv(GFDL_SSF_85, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/GFDL_SSF_85")


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

# percentage change

GFDL_SSF_26 <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  spread(year, year_total) %>% 
  mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  dplyr::select(`2000`, `2087`, change)

write.csv(GFDL_SSF_26, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/GFDL_SSF_26")


###### test area
GFDL_SSF_26 %>% 
  ggplot(aes(x=year,y=year_total)) +
  geom_point(aes(x=year,y=year_total))
