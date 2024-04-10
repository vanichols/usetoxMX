# Purpose: See how many of the CIMMYT ingredients are in the usetox database
# Created: April 10 2024
# Purpose: read in Simon's giant database and write it as an rds file
# NOTE: the original file is not saved on github bc it is too big
# Modified:


# load libraries ----------------------------------------------------------

library(readxl)
library(tidyverse)
library(janitor)


# clean the coding environment --------------------------------------------

rm(list = ls())



# hmmm --------------------------------------------------------------------

d <- read_excel("data/raw/4.-Agricultural supplies_february2024.xlsx", sheet = "Agricultural supplies", skip = 1)
write_rds(d, "data/raw/cimmyt-data.rds")

