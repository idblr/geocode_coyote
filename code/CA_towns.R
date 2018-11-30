##############################################################################
## Town size data extracted from Wikipedia
## ------------
## Using California as an example
##
## Created by: Ian Buller (@idblr)
## Created on: September 17, 2018
##
## Modified by:
## Modified on:
##
## Notes:
#   A) Code for information used for geocode confidence classification (see geocode_confidence.Rnw)
#   B) Cities, towns, incorporated areas, and unincorporated areas
#   C) Uses the rvest package
##############################################################################

# -------- ####
# PACKAGES #
# -------- ####
library(rvest)

# ---- ####
# DATA #
# ---- ####
url <- "https://en.wikipedia.org/wiki/List_of_cities_and_towns_in_California"

cal_towns <- url %>% # input url
  read_html() %>% # read the url
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>% # node for table
  html_table(fill = T) # fill table in R (as a list)

cal_towns <- cal_towns[[1]] # choose correct table from list

# ------ ####
# EXPORT #
# ------ ####
write.csv(cal_towns, file = "california_towns.csv") # save data as .csv
