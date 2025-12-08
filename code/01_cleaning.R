# Proyecto final – Visualización de datos
# 01_cleaning.R
# Limpieza inicial de datos 
# Autora: Golondrina Segura

# Librerías

library(tidyverse)
library(janitor)

# Leer datos 
spells_raw <- read_csv("data/raw/spells_raw.csv")
monsters_raw <- read_csv("data/raw/monsters_raw.csv")

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
  select(name, category, cr, size, type, ac, hp_number,
         resistances, vulnerabilities, immunities) %>%
  
  # Hay que normalizar el texto de "charmed" en inmunidades ya que tienen texto extra que los diferencia pero quieren decir lo mismo
  mutate(
    immunities = str_trim(immunities),
    immunities = case_when(
      str_detect(immunities, regex("Charmed", ignore_case = TRUE)) ~ "Charmed", 
      TRUE ~ immunities
    )
  ) %>%
  
  # Creamos columna nueva que separa los monstruos en categorias de amenaza según su CR
  mutate(
    cr_group = case_when(
      cr <= 2  ~ "Noob",
      cr <= 6  ~ "Intermedio",
      cr <= 10 ~ "Avanzado",
      cr <= 16 ~ "Épico",
      TRUE     ~ "Legendario"
    )
  )



# Guardar datos limpios

write_csv(spells_clean, "data/procesados/spells_clean.csv")
write_csv(monsters_clean, "data/procesados/monsters_clean.csv")


