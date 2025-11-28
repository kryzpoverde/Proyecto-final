# Proyecto final – Visualización de datos
# 01_cleaning.R
# Limpieza inicial de datos 
# Autora: Golondrina Segura

# Librerías

library(tidyverse)
library(janitor)

# Leer datos 
spells_raw <- read_csv("data/raw/spells.csv")
monsters_raw <- read_csv("data/raw/monsters.csv")

# Ver columnas para saber cúales seleccionar para el análisis

names(spells_raw)
names(monsters_raw)

# Limpieza datos "spells"

spells_clean <- spells_raw %>%
  clean_names() %>%
  select(name, level, bard, cleric, druid, paladin, ranger, sorcerer, warlock, 
         wizard, range, material_component, duration)

# Limpieza de datos "monsters"

monsters_clean <- monsters_raw %>%
  clean_names() %>%
  select(name, category, cr, size, type, ac, hp_number, resistances, vulnerabilities, immunities)

# Guardar datos limpios

write_csv(spells_clean, "data/procesados/spells_clean.csv")
write_csv(monsters_clean, "data/procesados/monsters_clean.csv")


