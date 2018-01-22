# Question 1: 

# What are the projected impacts on catches of small-scale fisheries within the region/EEZs?
# AKA how does catch amount change in the future between Alaska, Canada, USA and Mexico?

# Let's work within each EEZ - find total catch by year within each EEZ
# change path for RCP scenarios and EEZ

# specify path, generate list of files to loop through
path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Mexico/GFDL85/SSF/avg/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/Mexico/GFDL85/SSF/avg/", filename, sep = "")

# AGGREGATE TOTAL - FOR LOOP
# create empty list for aggregation
datalist <- list()

# loop through each .csv file in folder and sum by year
for (i in 1:length(filepath)){

# upload average years
avg_catch <- read.csv(filepath[i])

# rename columns, remove X
colnames(avg_catch) <- c("cellID", 2000:2050)

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

# ALASKA -----------------------------------------
#Alaska26LSF
Alaska26LSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(Alaska26LSF = year_total)
write.csv(Alaska26LSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Alaska26LSF")

#Alaska26SSF
Alaska26SSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(Alaska26SSF = year_total)
write.csv(Alaska26SSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Alaska26SSF")

#Alaska85LF
Alaska85LSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(Alaska85LSF = year_total)
write.csv(Alaska85LSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Alaska85LSF")

#Alaska85SSF
Alaska85SSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(Alaska85SSF = year_total)
write.csv(Alaska85SSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Alaska85SSF")

# Canada ------------------

Canada85LSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(Canada85LSF = year_total)
write.csv(Canada85LSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Canada85LSF")

Canada85SSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(Canada85SSF = year_total)
write.csv(Canada85SSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Canada85SSF")

#Canada85LF
Canada26LSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(Canada26LSF = year_total)
write.csv(Canada26LSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Canada26LSF")

#Canada85SSF
Canada26SSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(Canada26SSF = year_total)
write.csv(Canada26SSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Canada26SSF")

################## USA ####################
# Aggregate all species within EEZ by year
# Repeat this for each RCP scenario for each EEZ!
# USA West 8.5
USA85LSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(USA85LSF = year_total)
write.csv(USA85LSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/USA85LSF")

USA26LSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(USA26LSF = year_total)
write.csv(USA26LSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/USA26LSF")

USA85SSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(USA85SSF = year_total)
write.csv(USA85SSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/USA85SSF")

USA26SSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(USA26SSF = year_total)
write.csv(USA26SSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/USA26SSF")

########################Mexico###############
Mexico85LSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(Mexico85LSF = year_total)
write.csv(Mexico85LSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Mexico85LSF")

Mexico26LSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(Mexico26LSF = year_total)
write.csv(Mexico26LSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Mexico26LSF")

Mexico85SSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(Mexico85SSF = year_total)
write.csv(Mexico85SSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Mexico85SSF")

Mexico26SSF <- mydata %>% 
  group_by(year) %>% 
  summarize(year_total = sum(year_total)) %>% 
  rename(Mexico26SSF = year_total)
write.csv(Mexico26SSF, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Mexico26SSF")


# join all data into one massive PNA dataframe
PNAcatch <- Canada85 %>% 
  full_join(Canada26, by = "year") %>% 
  full_join(Alaska85, by = "year") %>% 
  full_join(Alaska26, by = "year") %>% 
  full_join(USA85, by = "year") %>% 
  full_join(USA26, by = "year") %>% 
  full_join(Mexico26, by = "year") %>% 
  full_join(Mexico85, by = "year") 

PNAcatch_long <- PNAcatch %>% 
  gather(scenario, catch, -c(year)) 

# plot it! altogether
plotme <- PNAcatch_long %>% 
  ggplot(aes(x=year, y=catch)) +
  geom_point(aes(colour=scenario)) +
  theme_bw() +
  labs(title = "BY EEZ - Projection of a sample of small-scale fisheries catches in PNA") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = scales::comma) + #get rid of sci notation with scales::comma
  scale_x_discrete(breaks = c("2000", "2010", "2020", "2030", "2040", "2050"))
  
ggsave("catch-by-eez.png", path = "C:/Users/angmel/Documents/firstchapter/SAU_DBEM_RESULTS/AGGREGATE/")


# lets do between eez for RCP85 scenario
EEZ_RCP85 <- Canada85 %>% 
  full_join(Alaska85, by = "year") %>% 
  full_join(USA85, by = "year") %>% 
  full_join(Mexico85, by = "year") %>% 
  gather(scenario, catch, -c(year)) %>% 
  ggplot(aes(x=year, y=catch)) +
  geom_point(aes(colour = scenario))


