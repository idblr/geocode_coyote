##############################################################################
## California Town Sizes
## -----------
## Scrape wikipedia table
## In order to obtain town size information for classifying observation location confidence
##
## Created by: Ian Buller (@idblr)
## Created on: July 18, 2018
##
## Notes:
# A) Uses rvest package
##############################################################################

# ------- ####
# PACKAGE #
# ------- ####
library(rvest)

# ---------------- ####
# Wikipedia Scrape #
# ---------------- ####

url <- "https://en.wikipedia.org/wiki/List_of_cities_and_towns_in_California"

cal_towns <- url %>% # import url
  read_html() %>% # read url
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>% # xpath location of node for table
  html_table(fill = T) # populate a table 
cal_towns <- cal_towns[[1]] # extract table information

# ---------------- ####
# Data Exportation #
# ---------------- ####
write.csv(cal_towns, file = "california_towns.csv")





