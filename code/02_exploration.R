# Proyecto final – Visualización de datos
# 02_exploration.R
# Análisis exploratorio de los datos
# Autora: Golondrina Segura

# Cargar librerías

library(tidyverse)

# Leer datos procesados

spells <- read_csv("data/procesados/spells_clean.csv")
monsters <- read_csv("data/procesados/monsters_clean.csv")

# Observar los datos

glimpse(spells)
glimpse(monsters) #existen datos NA en filas de resistencias, vulnerabilidades e inmunidades, pero estos no son 
                  #datos faltantes por lo que no se eliminan, se interpretan como que el monstruo no posee nada de este carácter.

# Vamos a empezar revisando cuántos hechizos hay por nivel

spells_por_nivel <- spells %>%
  count(level, name = "cantidad_hechizos")
spells_por_nivel

# Graficar resultado

ggplot(spells_por_nivel, aes(x = level, y = cantidad_hechizos)) +
  geom_col(fill = "#9333FF", alpha = 0.6) +
  scale_x_continuous(breaks = 0:9) +  
  scale_y_continuous(breaks = seq(0, 60, by = 5)) +  
  labs(
    title = "Cantidad de hechizos por nivel",
    x = "Nivel del hechizo",
    y = "Número de hechizos"
  ) +
  theme_classic()

#Ahora revisamos cantidad de montruos por CR (challenge rating)
#Para esto, vamos a agregar una columna que agrupa
#los monstruos según su CR (noob, intermedio, avanzado, épico y legendario)

monsters <- monsters %>%
  mutate(cr_group = case_when(
    cr <= 2 ~ "Noob",
    cr <= 6 ~ "Intermedio",
    cr <= 10 ~ "Avanzado",
    cr <= 16 ~ "Épico",
    TRUE ~ "Legendario"
  ))

#Ahora graficamos el resultado

ggplot(monsters, aes(x = factor(cr_group,
                                levels = c("Noob", "Intermedio", "Avanzado", "Épico", "Legendario")))) +
  geom_bar(fill = "#FF8F78", alpha = 0.8) +
  scale_y_continuous(breaks = seq(0, max(table(monsters$cr_group)), by = 50)) +
  labs(
    title = "Distribución de monstruos por nivel de amenaza",
    x = "Nivel de amenaza",
    y = "Cantidad de monstruos"
  ) +
  theme_classic()




