# Purpose: See how many of the CIMMYT ingredients are banned in the us
# Created: 7/11/23
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
  mutate(cimmyt = "x") %>% 
  rename("name" = value) %>% 
  arrange(name) %>% 
  distinct()

d_cim3

# banned substances -------------------------------------------------------

d_us1 <- 
  read_excel("../data/Donley2019-banned-pesticides.xlsx", sheet = "S9", skip = 2) %>% 
  select(1:2) %>% 
  rename(value = 1) %>% 
  mutate(value = str_remove_all(value, " "),
         value = str_remove_all(value, "-"),
         value = str_to_lower(value)) %>% 
  rename(name = 1)  %>% 
  mutate(status = ifelse(Status == 3, "approved", "other")) %>% 
  select(-Status)


# try binding -------------------------------------------------------------

d_com <- 
  d_cim3 %>% 
  left_join(d_us1) 

d_com %>% 
  write_csv("../data-proc/banned.csv")

d_com %>%
  group_by(status) %>% 
  summarise(n = n())
