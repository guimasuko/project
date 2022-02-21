rm(list = ls())
library("terra")
library("raster")
library("dplyr")
library("spData")
library("spDataLarge")
library("geobr")
library("sf")
library("ggplot2")


if(!require("pacman"))install.packages("pacman")
pacman::p_load(terra, spData)

url = 'C:/Users/Usuario/git/project/data/brasil_coverage_2020.tif'
my_rast = rast(url)
# plot(my_rast)

rj = read_municipality(code_muni='RJ', year =2020)
rj_vec = vect(rj) 
# plot(rj)

srtm_cropped = crop(my_rast, rj_vec)
#plot(srtm_cropped)

srtm_masked = mask(srtm_cropped, rj_vec)
# plot(srtm_masked)

srtm_extracted = extract(srtm_masked, rj_vec)

pixels_mun = srtm_extracted %>%
  group_by(ID) %>% 
  summarise(n_pixels = n())

forest_mun = srtm_extracted %>%
  group_by(ID) %>% 
  filter(brasil_coverage_2020 %in% c(3,4,5,49)) %>% 
  summarise(n_forest = n())


rj =  rj %>%
  mutate(ID = 1:92) %>% 
  left_join(pixels_mun, by = "ID")

rj =  rj %>%
  mutate(ID = 1:92) %>% 
  left_join(forest_mun, by = "ID")

rj = rj %>%
  mutate(`Razão Florestal` = (n_forest/n_pixels))

texto_azul <- element_text(color = "#201c5c", family = "mono")

ggplot(rj) +
  geom_sf(aes(fill = `Razão Florestal`)) +
  theme(text = texto_azul,
  panel.background = element_rect(fill = "white"),
  axis.title=element_blank(),
  axis.text=element_blank(),
  axis.ticks=element_blank(), 
  title = element_text(color = '#201c5c', face = 'bold')) +
  labs(title = "Cobertura Vegetal",
         subtitle = "por município do estado do Rio de Janeiro",
         x = NULL, y = NULL)+
  scale_fill_viridis_c(option = 'D')
         
# Data frame final com os municípios e suas razões florestais
rj_ratio = rj %>%
  select(name_muni, name_state, `Razão Florestal`)

write.csv(rj_ratio, file="data/rj_ratio.csv")
