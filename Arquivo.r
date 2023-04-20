library(dplyr)
dados <- read.csv("Pokemon_full.csv")
df_grass <- filter(dados, type == "grass")
df_grass


dados %>% filter(type == "grass")

df_fogo_e_agua <- dados %>% filter(type == "fire" | type == "water")

dados %>% filter(grepl("fly", name))

dados %>% filter(grepl("bee", name) | grepl("saur", name))
dados %>% head()
