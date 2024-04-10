# Purpose: See how many of the CIMMYT ingredients are in the usetox database
# Created: 4/17/2023
# Modified:


# load libraries ----------------------------------------------------------

library(readxl)
library(tidyverse)
library(janitor)


# clean the coding environment --------------------------------------------

rm(list = ls())

# set working directory to wherever this code file is saved -------------------------------------
# (don't worry about understanding this chunk of code)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# cimmyt database -------------------------------------------------

d_cim <- 
  read_excel("../data/simone/List of active ingredients.xlsx") %>%  
  #--this cleans up the names of the columns and makes them easier to reference
  clean_names()

#--the pluses are annoying
#--I will separate those entries into two separate entries

d_cim2 <- 
  d_cim %>% 
  #-- + is a special symbol so I'm replacing it with a more normal one
  mutate(correcte_ia = str_replace_all(correcte_ia, "\\+", ",")) %>% 
#--now separate entries with a ,
  separate(correcte_ia, into = c("chem1", "chem2"), sep = ",")

#--now make it one long list and make everything lower case
d_cim3 <- 
  d_cim2 %>% 
  select(chem1, chem2) %>% 
  pivot_longer(1:2) %>% 
  select(value) %>% 
  filter(!is.na(value)) %>% 
  mutate(value = str_remove_all(value, " "),
         value = str_remove_all(value, "-"),
         value = str_to_lower(value)) %>% 
  distinct() %>% 
  rename("cimmyt" = value)

d_cim3


# we need to assign a CAS number to each chemical, or remove it -----------


d_cim3 %>% 
  write_csv("../data/simone/cimmyt_needs-cas-numbers.csv")
