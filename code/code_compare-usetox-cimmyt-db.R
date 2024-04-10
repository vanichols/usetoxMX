# Purpose: See how many of the CIMMYT ingredients are in the usetox database
# Created: 4/17/2023
# Modified:


# load libraries ----------------------------------------------------------

library(readxl)
library(tidyverse)
library(janitor)


# set working directory to wherever this code file is saved -------------------------------------
# (don't worry about understanding this chunk of code)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# usetox organic database -------------------------------------------------

#--the .. tell it to go up a folder

d_ut <- read_excel("../data/usetox/USEtox_substance_data_organics.xlsx",
                   sheet = "Database", 
                   skip = 1) %>%  
  #--this cleans up the names of the columns and makes them easier to reference
  clean_names()

d_ut

#--the data is very messy, here we clean it up and just keep the CAS RN and Name columns
d_ut2 <- 
  d_ut %>% 
  #--remove the first three rows
  slice(-(1:3)) %>% 
  #--select the columns we want
  select(name)

#--the names of the chemicals are a mix of captial/lower case etc
#--here we squish them into all lower case without punctuation or spaces

d_ut3 <- 
  d_ut2 %>% 
  mutate(name = str_replace_all(name, "[^[:alnum:]]", ""),
         name = str_remove_all(name, " "),
         name = str_to_lower(name)) %>% 
  arrange(name)

d_ut4 <- 
  d_ut3 %>% 
  mutate(usetox = "x")

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


# compare the two ---------------------------------------------------------

d_comp<- 
  d_cim3 %>% 
  left_join(d_ut4) %>% 
  arrange(name)

#--write it to an excel file so we can look at it
d_comp

#--43 of the chemicals have matches, but a lot of the non-matches are spelling 
d_comp %>% 
  filter(!is.na(usetox))


