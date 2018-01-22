path <- "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/"
filename <- dir(path)
filepath <- paste("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/", filename, sep = "")

# read Alaska files
Alaska26LSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Alaska26LSF") %>% 
  select(-X) %>% 
  mutate(sector = "LSF") %>% 
  mutate(RCP = "2.6") %>% 
  rename(scenario = Alaska26LSF)
Alaska85LSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Alaska85LSF") %>% 
  select(-X) %>% 
  mutate(sector = "LSF") %>% 
  mutate(RCP = "8.5") %>% 
  rename(scenario = Alaska85LSF)
Alaska85SSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Alaska85SSF") %>% 
  select(-X) %>% 
  mutate(sector = "SSF") %>% 
  mutate(RCP = "8.5") %>% 
  rename(scenario = Alaska85SSF)
Alaska26SSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Alaska26SSF") %>% 
  select(-X) %>% 
  mutate(sector = "SSF") %>% 
  mutate(RCP = "2.6") %>% 
  rename(scenario = Alaska26SSF)

# read Canada files
Canada26LSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Canada26LSF") %>% 
  select(-X)%>% 
  mutate(sector = "LSF") %>% 
  mutate(RCP = "2.6") %>% 
  rename(scenario = Canada26LSF)
Canada85LSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Canada85LSF") %>% 
  select(-X)%>% 
  mutate(sector = "LSF") %>% 
  mutate(RCP = "8.5") %>% 
  rename(scenario = Canada85LSF)
Canada85SSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Canada85SSF") %>% 
  select(-X) %>% 
  mutate(sector = "SSF") %>% 
  mutate(RCP = "8.5") %>% 
  rename(scenario = Canada85SSF)
Canada26SSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Canada26SSF") %>% 
  select(-X) %>% 
  mutate(sector = "SSF") %>% 
  mutate(RCP = "2.6")%>% 
  rename(scenario = Canada26SSF)

# read US files
USA26LSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/USA26LSF") %>% 
  select(-X)%>% 
  mutate(sector = "LSF") %>% 
  mutate(RCP = "2.6") %>% 
  rename(scenario = USA26LSF)
USA85LSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/USA85LSF") %>% 
  select(-X) %>% 
  mutate(sector = "LSF") %>% 
  mutate(RCP = "8.5") %>% 
  rename(scenario = USA85LSF)
USA85SSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/USA85SSF") %>% 
  select(-X) %>% 
  mutate(sector = "SSF") %>% 
  mutate(RCP = "8.5") %>% 
  rename(scenario = USA85SSF)
USA26SSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/USA26SSF") %>% 
  select(-X) %>% 
  mutate(sector = "SSF") %>% 
  mutate(RCP = "2.6") %>% 
  rename(scenario = USA26SSF)

# read Mexico files
Mexico26LSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Mexico26LSF") %>% 
  select(-X)%>% 
  mutate(sector = "LSF") %>% 
  mutate(RCP = "2.6") %>% 
  rename(scenario = Mexico26LSF)
Mexico85LSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Mexico85LSF") %>% 
  select(-X)%>% 
  mutate(sector = "LSF") %>% 
  mutate(RCP = "8.5") %>% 
  rename(scenario = Mexico85LSF)
Mexico85SSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Mexico85SSF") %>% 
  select(-X)%>% 
  mutate(sector = "SSF") %>% 
  mutate(RCP = "8.5") %>% 
  rename(scenario = Mexico85SSF)
Mexico26SSF <- read.csv("C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/Mexico26SSF") %>% 
  select(-X) %>% 
  mutate(sector = "SSF") %>% 
  mutate(RCP = "2.6") %>% 
  rename(scenario = Mexico26SSF)

# Canada EEZ -  both RCP, both sectors
Canada_lineplot <- Canada85SSF %>% 
  bind_rows(Canada85LSF) %>% 
  bind_rows(Canada26LSF) %>%
  bind_rows(Canada26SSF) %>%
  # gather(scenario, catch, -c(year)) %>% 
  ggplot(aes(x=year, y=scenario)) +
  geom_line(aes(colour = RCP, linetype = sector), size = 1) +
  scale_color_manual(values = c("#4667EF", "#F1315F")) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Future projections of catch within Canada (Pacific)'s EEZ ", y = "Catch (t)", x = "Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0))

dev.copy(png, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/canadacatch.png")
dev.off()

# Alaska EEZ -  both RCP, both sectors
Alaska_lineplot <- Alaska85SSF %>% 
  bind_rows(Alaska85LSF) %>% 
  bind_rows(Alaska26LSF) %>%
  bind_rows(Alaska26SSF) %>%
  # gather(scenario, catch, -c(year)) %>% 
  ggplot(aes(x=year, y=scenario)) +
  geom_line(aes(colour = RCP, linetype = sector), size = 1) +
  scale_color_manual(values = c("#4667EF", "#F1315F")) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Future projections of catch within Alaska's EEZ ", y = "Catch (t)", x = "Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0))

dev.copy(png, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/alaskacatch.png")
dev.off()

# USA EEZ -  both RCP, both sectors
USA_lineplot <- USA85SSF %>% 
  bind_rows(USA85LSF) %>% 
  bind_rows(USA26LSF) %>%
  bind_rows(USA26SSF) %>%
  # gather(scenario, catch, -c(year)) %>% 
  ggplot(aes(x=year, y=scenario)) +
  geom_line(aes(colour = RCP, linetype = sector), size = 1) +
  scale_color_manual(values = c("#4667EF", "#F1315F")) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Future projections of catch within USA (West Coast)'s EEZ ", y = "Catch (t)", x = "Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0))

dev.copy(png, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/usacatch.png")
dev.off()

# Mexico EEZ -  both RCP, both sectors
USA_lineplot <- Mexico85SSF %>% 
  bind_rows(Mexico85LSF) %>% 
  bind_rows(Mexico26LSF) %>%
  bind_rows(Mexico26SSF) %>%
  # gather(scenario, catch, -c(year)) %>% 
  ggplot(aes(x=year, y=scenario)) +
  geom_line(aes(colour = RCP, linetype = sector), size = 1) +
  scale_color_manual(values = c("#4667EF", "#F1315F")) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Future projections of catch within Mexico(Pacific)'s EEZ ", y = "Catch (t)", x = "Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0))

dev.copy(png, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/mexicocatch.png")
dev.off()

#############################
# build one large data frame
PNAcatch_all <- Canada85SSF %>% 
  bind_rows(Canada26SSF) %>% 
  bind_rows(Canada85LSF) %>% 
  bind_rows(Canada26LSF) %>% 
  bind_rows(Alaska85SSF) %>% 
  bind_rows(Alaska26SSF) %>% 
  bind_rows(Alaska85LSF) %>% 
  bind_rows(Alaska26LSF) %>% 
  bind_rows(USA85LSF) %>% 
  bind_rows(USA26LSF) %>% 
  bind_rows(USA26SSF) %>% 
  bind_rows(USA85SSF) %>% 
  bind_rows(Mexico26LSF) %>% 
  bind_rows(Mexico85LSF) %>% 
  bind_rows(Mexico26SSF) %>% 
  bind_rows(Mexico85SSF)

# catch all
PNA_lineplot <- PNAcatch_all %>% 
  # gather(scenario, catch, -c(year)) %>% 
  group_by(sector, RCP, year) %>% 
  summarize(catch_sum = sum(scenario)) %>% 
  ggplot(aes(x=year, y=catch_sum)) +
  geom_point(aes(colour = RCP, shape = sector), size = 1) +
  scale_color_manual(values = c("#4667EF", "#F1315F")) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Future projections of catch in Pacific North America", y = "Catch (t)", x = "Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0))

# catch ssf
PNA_SSF_plot <- PNAcatch_all %>% 
  # gather(scenario, catch, -c(year)) %>% 
  filter(sector == "SSF") %>% 
  group_by(sector, RCP, year) %>% 
  summarize(catch_sum = sum(scenario)) %>% 
  ggplot(aes(x=year, y=catch_sum)) +
  geom_point(aes(colour = RCP), size = 1) +
  scale_color_manual(values = c("#4667EF", "#F1315F")) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Future projections of small-scale fisheries catch in Pacific North America", y = "Catch (t)", x = "Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0))
dev.copy(png, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/pna_ssf_catch.png")
dev.off()

# catch lsf
PNA_LSF_plot <- PNAcatch_all %>% 
  # gather(scenario, catch, -c(year)) %>% 
  filter(sector == "LSF") %>% 
  group_by(sector, RCP, year) %>% 
  summarize(catch_sum = sum(scenario)) %>% 
  ggplot(aes(x=year, y=catch_sum)) +
  geom_point(aes(colour = RCP), size = 1) +
  scale_color_manual(values = c("#4667EF", "#F1315F")) +
  theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Future projections of large-scale fisheries catch in Pacific North America", y = "Catch (t)", x = "Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(expand = c(0, 0))
dev.copy(png, "C:/Users/angmel/Documents/MSc-small-scale-fisheries/SAU_DBEM_RESULTS/AGGREGATE/pna_lsf_catch.png")
dev.off()

# gather into long format
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

# CanadaEEZ <- Canada85SSF %>% 
#     bind_rows(Canada85LSF) %>% 
#     bind_rows(Canada26LSF) %>%
#     bind_rows(Canada26SSF) %>%
#     # gather(scenario, catch, -c(year)) %>% 
#     ggplot(aes(x=year, y=scenario)) +
#     geom_line(aes(colour = RCP, shape = sector), size = 1.5) +
#     scale_color_manual(values = c("#4667EF", "#F1315F")) +
#     theme_classic() +
#     scale_y_continuous(labels = scales::comma) +
#     labs(title = "Future projections of catch within Canada's EEZ ", y = "Catch (t)", x = "Year") +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_continuous(expand = c(0, 0))


EEZ_Alaska <- Alaska85SSF %>% 
  full_join(Alaska85LSF, by = "year") %>% 
  full_join(Alaska26LSF, by = "year") %>%
  full_join(Alaska26SSF, by = "year") %>%
  gather(scenario, catch, -c(year)) %>% 
  ggplot(aes(x=year, y=catch)) +
  geom_point(aes(colour = scenario)) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) + #get rid of sci notation with scales::comma
  scale_x_discrete(breaks = c("2000", "2010", "2020", "2030", "2040", "2050"))

EEZ_USA <- USA85SSF %>% 
  full_join(USA85LSF, by = "year") %>% 
  full_join(USA26LSF, by = "year") %>%
  full_join(USA26SSF, by = "year") %>%
  gather(scenario, catch, -c(year)) %>% 
  ggplot(aes(x=year, y=catch)) +
  geom_point(aes(colour = scenario)) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) + #get rid of sci notation with scales::comma
  scale_x_discrete(breaks = c("2000", "2010", "2020", "2030", "2040", "2050"))

EEZ_Mexico <- Mexico85SSF %>% 
  full_join(Mexico85LSF, by = "year") %>% 
  full_join(Mexico26LSF, by = "year") %>%
  full_join(Mexico26SSF, by = "year") %>%
  gather(scenario, catch, -c(year)) %>% 
  ggplot(aes(x=year, y=catch)) +
  geom_point(aes(colour = scenario)) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) + #get rid of sci notation with scales::comma
  scale_x_discrete(breaks = c("2000", "2010", "2020", "2030", "2040", "2050"))



