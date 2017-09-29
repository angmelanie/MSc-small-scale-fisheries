# Let's examine error log

# What's in here?
# Taxa where DBEM says there is no record of their occurrences in the proposed EEZ
# BUT SAU catch says its being caught!
# This could be because of the way I split the MMF etc. "G" species?

# set wd
setwd("~/firstchapter/R codes/MSc-small-scale-fisheries/DBEM x SAU/errorlog")

# ALASKA read file and combine error logs
# ALASKA: 6 error out of 52 species
ALASKA26D <- read.csv("ALASKA26DROBO") %>% 
  na.omit()
ALASKA85D <- read.csv("ALASKA85DROBO") %>% 
  na.omit()
ALASKA_error <- full_join(ALASKA26D, ALASKA85D)
ALASKA_error$eez <- "Alaska"


# CANADA HAS NO ERROR

# USA WEST
# 9 out of 73
USA26D <- read.csv("USA26DROBO") %>% 
  na.omit()
USA85D <- read.csv("USA85DROBO") %>% 
  na.omit()
USA26C <- read.csv("USA26CYGWIN") %>% 
  na.omit()
USA85C <- read.csv("USA85CYGWIN") %>% 
  na.omit()
USA_error <- full_join(USA26D, USA85D) %>% 
  full_join(USA26C) %>% 
  full_join(USA85C)
USA_error$eez <- "USA"


# MEXICO
# 15 out of 56
MEXICO26D <- read.csv("MEXICO26DROBO") %>% 
  na.omit()
MEXICO85D <- read.csv("MEXICO85DROBO") %>% 
  na.omit()
MEXICO26C <- read.csv("MEXICO26CYGWIN") %>% 
  na.omit()
MEXICO85C <- read.csv("MEXICO85CYGWIN") %>% 
  na.omit()
Mexico_error <- full_join(MEXICO26D, MEXICO85D) %>% 
  full_join(MEXICO26C) %>% 
  full_join(MEXICO85C)
Mexico_error$eez <- "Mexico"


# lets see all the errors together
error_report <- bind_rows(ALASKA_error, USA_error) %>% 
  bind_rows(Mexico_error)
error_report$X <- NULL
write.csv(error_report, "error_report.csv")
