# Collecting Linf via web scraping

# Purpose: rfishbase does not provide all the parameters needed to model species distributions
# Web scraping is used as a tool to extract info for multiple species
# While this method does provide more info that was not present in rfishbase,
# Due to the slight differences in formatting and structure of each species fishbase page,
# This proved to be largely unsuccessful to collect metadata on a large scale
# For example, some species may not have a growth page, or the "life history" page may be 4th
# or 5th placement in the main species page

# load packages
library(rvest)
library(dplyr)
library(scales)
library(stringr)
library(tidyverse)

# data prep
# replace space with -
meta_fish_hyphenated <- gsub(" ", "-", meta_fish_updated$sciname)

# test run with subset
# meta_fish_hyphenated_test <- gsub(" ", "-", meta_fish_updated$sciname[1:4])

################################################################################
# METHOD 1: Scrape from fishbase > growth page

compile_Linf <- list()
errspp <- numeric()

for (i in 1:length(meta_fish_hyphenated)){
  
  species_url <- paste("http://www.fishbase.ca/summary/", meta_fish_hyphenated[i], ".html", sep = "")
  species_mainpage <- read_html(species_url)

  url_1 <- species_mainpage %>% 
    html_nodes(".ss-moreinfo~ .ss-moreinfo+ .ss-moreinfo .slabel1:nth-child(3) a") %>% 
    html_attr("href")
  
  if (length(url_1) > 0){
  
  url_2 <- paste("http://www.fishbase.org", url_1, sep = "")
  
  species_growth <- read_html(url_2)
  
  Linf <- species_growth %>% 
    html_nodes(".lalign:nth-child(2)") %>% 
    html_text()
  Linf_edit <- gsub('^.*inf\\s*|\\s*cm.*$', '', Linf)
  Linf_df <- data.frame(Linf_edit, meta_fish_hyphenated[i])
  
  compile_Linf[[i]] <- Linf_df
  
  }
  
  else {
    errspp[i] <- meta_fish_hyphenated[i]
  }
}

compile_Linf_list <- bind_rows(compile_Linf)
compile_Linf_dataframe <- as.data.frame(compile_Linf_list)
write_csv(compile_Linf_dataframe, "Linf_webscrap.csv")

############################################################################
# Scrape from fishbase > life history


compile_Linf <- list()
errspp <- numeric()

for (i in 1:length(meta_fish_hyphenated_test)){
  
  species_url <- paste("http://www.fishbase.ca/summary/", "Lutjanus-jordani", ".html", sep = "")
  species_mainpage <- read_html(species_url)
  
  url_1 <- species_mainpage %>% 
    html_nodes(".smallSpace:nth-child(49) .slabel1:nth-child(4) a") %>% 
    html_attr("href")
  
  if (length(url_1) > 0){
    
    url_2 <- paste("http://www.fishbase.org", url_1, sep = "")
    
    species_growth <- read_html(url_2)
    
    Linf <- species_growth %>% 
      html_nodes(".lalign:nth-child(2)") %>% 
      html_text()
    Linf_edit <- gsub('^.*inf\\s*|\\s*cm.*$', '', Linf)
    Linf_df <- data.frame(Linf_edit, meta_fish_hyphenated[i])
    
    compile_Linf[[i]] <- Linf_df
    
  }
  
  else {
    errspp[i] <- meta_fish_hyphenated[i]
  }
}

compile_Linf_list <- bind_rows(compile_Linf)
compile_Linf_dataframe <- as.data.frame(compile_Linf_list)
write_csv(compile_Linf_dataframe, "Linf_webscrap.csv")


