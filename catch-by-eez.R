# Question 1: 

# What are the projected impacts on catches of small-scale fisheries within the region/EEZs?
# AKA how does catch amount change in the future between Alaska, Canada, USA and Mexico?

# Let's work within each EEZ - find total catch by year within each EEZ

# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Canada/GFDL85/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/Canada/GFDL85/avg/", filename, sep = "")

# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){

# upload average years
avg_catch <- read.csv(filepath[i])

# rename columns, remove X
colnames(avg_catch) <- c("cellID", 2000:2051)

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

# sum by each year
Canada <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total))


