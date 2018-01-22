# catch by eez GFDL

# GFDL_LSF_85-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/85/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

cellID_mexico <- cellID %>% 
  filter(EEZID %in% 945)

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_mexico, by = "INDEX") %>% 
    gather(year, catch, -c(INDEX), -c(EEZID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) 
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_85_me <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(catch)) 
GFDL_LSF_85_me$RCP <- "8.5"
GFDL_LSF_85_me$climate <- "GFDL"
GFDL_LSF_85_me$sector <- "LSF"

# GFDL_LSF_26-------------------------
# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/LSF/GFDL/year2100/26/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

cellID_mexico <- cellID %>% 
  filter(EEZID %in% 945)

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){
  
  # upload average years
  avg_catch <- read.csv(filepath[i])
  
  # rename columns, remove X
  colnames(avg_catch) <- c("cellID", 2000:2087)
  
  # this still maintains cellID info
  p <- avg_catch %>% 
    rename(INDEX = cellID) %>% 
    inner_join(cellID_mexico, by = "INDEX") %>% 
    gather(year, catch, -c(INDEX), -c(EEZID), na.rm=TRUE) %>% 
    dplyr::select(.,year, catch) %>% 
    group_by(year) 
  
  datalist[[i]] <- p # attaches each csv file as a new list
}

# bind all the list together
mydata <- dplyr::bind_rows(datalist)

# percentage change

GFDL_LSF_26_me <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(catch)) 
GFDL_LSF_26_me$RCP <- "2.6"
GFDL_LSF_26_me$climate <- "GFDL"
GFDL_LSF_26_me$sector <- "LSF"

  # spread(year, year_total) %>% 
  # mutate(change = ((`2087`-`2000`)/`2000`)*100) %>% 
  # dplyr::select(`2000`, `2087`, change)

GFDL_LSF_me <- GFDL_LSF_26_me %>% 
  bind_rows(GFDL_LSF_85_me)

GFDL_LSF_me %>% 
  ggplot(aes(x=year,y=year_total)) +
  geom_point(aes(x=year,y=year_total, color = RCP))

write.csv(GFDL_LSF_85, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/GFDL_LSF_85")
