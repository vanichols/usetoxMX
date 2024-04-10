# Purpose: Explore and describe database
# Created: 4/101/2024
# Modified:


# load libraries ----------------------------------------------------------

library(readxl)
library(tidyverse)
library(janitor)


# clean the coding environment --------------------------------------------

rm(list = ls())


# data --------------------------------------------------------------------

d <- read_rds("data/raw/cimmyt-data.rds")

d1 <- 
  d %>% 
  clean_names()

names(d1)


# 2. slim down variables --------------------------------------------------

#--don't need all of these, keep only the ones we really want

randomkeep <- round(runif(10, 0, 100000))

d1.tmp <- 
  d1 %>% 
  mutate(r_num = 1:n()) %>% 
  filter(r_num %in% randomkeep)

names(d1.tmp)

#--the plot ID might give you a crop in another database
d2 <- 
  d1 %>% 
  select(1:5, 7:9, 10, 11, 12, 14, 
         15, 40, 41, 42, 43, 48:50) %>% 
  mutate_if(is.character, str_to_lower) %>% 
  mutate_if(is.character, as.factor) %>% 
  #--I don't see a unique identifier for the row, add one
  mutate(obs_id = paste0("obs", "_", 1:n()))

summary(d2)


  

# 3. eliminate fertilizers ------------------------------------------------
d2.tmp <- 
  d2 %>% 
  group_by(categoria_del_producto) %>% 
  summarise(n = n()) %>%
  arrange(n)

d2.tmp %>% 
  ggplot(aes(reorder(categoria_del_producto, n), n)) + 
  geom_col() + 
  coord_flip()


d3 <- 
  d2 %>% 
  filter(categoria_del_producto %in% c("herbicida", "insecticida", "fungicida", "bactericida"))


# 4. look at units --------------------------------------------------------

d4.tmp <- 
  d3 %>% 
  group_by(unidad_ha) %>% 
  summarise(n = n()) %>%
  arrange(n)

d4.tmp %>% 
  ggplot(aes(reorder(unidad_ha, n), n)) + 
  geom_col() + 
  geom_text(aes(x = unidad_ha, y = n + 5, label = n)) +
  coord_flip()

#--understand the ingredients that are removed at some point
#--i.e. are some ais only applied in paquetes?
#--the ingredients need cleaned up first though

d4 <- 
  d3 %>% 
  filter(unidad_ha %in% c("kg", "l"))


# 5. separate active ingredients --------------------------------------------

d5.tmp <- 
  d4 %>%   #-- + is a special symbol so I'm replacing it with a more normal one
  mutate(ingrediente_activo = str_replace_all(ingrediente_activo, "\\+", "XXXXX")) %>% 
  #--now separate entries with a XXXXX
  separate(ingrediente_activo, into = c("ai1", "ai2", "ai3","ai4"), sep = "XXXXX") %>% 
#--just use the unique identifier for each row to work with this
  select(obs_id, ai1, ai2, ai3, ai4) %>% 
  pivot_longer(2:5) %>%  
  filter(!is.na(value)) %>% 
  select(obs_id, active_ing = value)


d5 <- 
  d4 %>% 
  left_join(d5.tmp) %>% 
  select(obs_id, everything())



# 6. eliminate biological controls ----------------------------------------

# Trichogramma is a genus of minute polyphagous wasps that are endoparasitoids of insect eggs.
d6.tmp <- 
  d5 %>% 
  filter(nombre_del_producto_aplicado == "control biologico") 

d6 <- 
  d5 %>% 
  filter(!grepl("control bio", nombre_del_producto_aplicado)) %>% 
  #--get rid of the bullshit I don't want to look at
  select(obs_id, categoria_del_producto:active_ing, -tipo_de_producto)


# 7. make sure quantitites are reasonable? ------------------------------------------------------------------

#--maybe a discussion to have

#--this seems like a typo, other ammounts for this ingredient are not this high
d6 %>% 
  filter(cantidad_de_producto_aplicado > 1000)

d6 %>% 
  filter(nombre_del_producto_aplicado == "imidacloprid") %>% 
  ggplot(aes(unidad_ha, cantidad_de_producto_aplicado)) + 
  geom_jitter() + 
  facet_grid(.~nombre_del_producto_aplicado)

#--I think we should eliminate the kg units, and anything over 10 l per ha
d6 %>% 
  filter(nombre_del_producto_aplicado == "imidacloprid") %>% 
  arrange(-cantidad_de_producto_aplicado)

d7.tmp1 <- 
  d6 %>% 
  #--4 instances of kg
  filter(!(nombre_del_producto_aplicado == "imidacloprid" & unidad_ha == "kg")) %>% 
  filter(!(nombre_del_producto_aplicado == "imidacloprid" & cantidad_de_producto_aplicado > 10)) 


#####STOPPED####

# NAs for active ingredients ----------------------------------------------

# just weird ones i've seen -----------------------------------------------

# get rid of obs_308066, "mezcla física 88.5-69-26" with no active ingredient


# number of times ai is applied -------------------------------------------


# amount of ai applied ----------------------------------------------------


  