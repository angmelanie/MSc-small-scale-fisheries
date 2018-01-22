# Remove spatial component, aggregate

IPSL_SSF_26$ID <- "SSF_26"
IPSL_SSF_85$ID <- "SSF_85"
IPSL_LSF_26$ID <- "LSF_26"
IPSL_LSF_85$ID <- "LSF_85"

IPSL_SSF_26$RCP <- "26"
IPSL_SSF_85$RCP <- "85"
IPSL_LSF_26$RCP <- "26"
IPSL_LSF_85$RCP <- "85"

IPSL_SSF_26$sector <- "SSF"
IPSL_SSF_85$sector <- "SSF"
IPSL_LSF_26$sector <- "LSF"
IPSL_LSF_85$sector <- "LSF"

IPSL_SSF_26$climate <- "IPSL"
IPSL_SSF_85$climate <- "IPSL"
IPSL_LSF_26$climate <- "IPSL"
IPSL_LSF_85$climate <- "IPSL"

IPSL_LSF_26 %>% 
  bind_rows(IPSL_LSF_85) %>% 
  bind_rows(IPSL_SSF_26) %>% 
  bind_rows(IPSL_SSF_85)


GFDL_SSF_26$ID <- "SSF_26"
GFDL_SSF_85$ID <- "SSF_85"
GFDL_LSF_26$ID <- "LSF_26"
GFDL_LSF_85$ID <- "LSF_85"

GFDL_SSF_26$RCP <- "26"
GFDL_SSF_85$RCP <- "85"
GFDL_LSF_26$RCP <- "26"
GFDL_LSF_85$RCP <- "85"

GFDL_SSF_26$sector <- "SSF"
GFDL_SSF_85$sector <- "SSF"
GFDL_LSF_26$sector <- "LSF"
GFDL_LSF_85$sector <- "LSF"

GFDL_SSF_26$climate <- "GFDL"
GFDL_SSF_85$climate <- "GFDL"
GFDL_LSF_26$climate <- "GFDL"
GFDL_LSF_85$climate <- "GFDL"


GFDL_LSF_26 %>%
  bind_rows(GFDL_LSF_85) %>%
  bind_rows(GFDL_SSF_26) %>%
  bind_rows(GFDL_SSF_85) %>%
  bind_rows(IPSL_LSF_26) %>%
  bind_rows(IPSL_LSF_85) %>%
  bind_rows(IPSL_SSF_26) %>%
  bind_rows(IPSL_SSF_85) %>%
  arrange(ID) %>%
  ggplot(aes(x=ID,y=change,fill=factor(RCP), alpha = climate))+
  geom_bar(stat="identity",position="dodge")


png("C:/Users/angmel/Documents/MSc-small-scale-fisheries/REPORT/pna_catchpot.png", width = 700, height = 700)

GFDL_LSF_26 %>% 
  bind_rows(GFDL_LSF_85) %>% 
  bind_rows(GFDL_SSF_26) %>% 
  bind_rows(GFDL_SSF_85) %>% 
  bind_rows(IPSL_LSF_26) %>% 
  bind_rows(IPSL_LSF_85) %>% 
  bind_rows(IPSL_SSF_26) %>% 
  bind_rows(IPSL_SSF_85) %>%
  arrange(ID) %>% 
  dplyr::select(ID, RCP, sector, climate, change) %>%
  spread(climate, change) %>% 
  group_by(ID) %>% 
  mutate(average = mean(c(GFDL, IPSL))) %>% 
  ggplot(aes(x=sector,y=average,fill=factor(RCP)))+
  geom_bar(stat="identity",position="dodge") + 
  scale_fill_brewer(palette = "Set1", direction = -1, name = "RCP") +
  xlab("Sector")+ylab("Percentage Change in Catch between 2087 and 2000") +
  labs(title="Changes in PNA's catch potential") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5,vjust=-.4, face="bold"))

dev.off()

# to add in error bars representing IPSL and GFDL
# geom_errorbar(aes(ymin = IPSL, ymax = GFDL)) +
