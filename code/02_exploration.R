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

#Ahora vamos a analizar hechizos por su disponibilidad y cantidad para distintas clases

#Primero seleccionamos las columnas que nos interesan y las pasamos a formato largo para gráficos posteriores

spell_long <- spells %>%
  select(level, bard, cleric, druid, paladin, ranger, sorcerer, warlock, wizard) %>%
  pivot_longer(
    cols = -level,
    names_to = "clase",
    values_to = "puede_usar"
  ) %>%
  filter(puede_usar == TRUE)

#Ahora vamos a ver el total de hechizos disponibles por clase

spell_long %>%
  count(clase, name = "n_hechizos") %>%
  ggplot(aes(x = reorder(clase, n_hechizos), y = n_hechizos)) +
  geom_col(fill = "#9333FF") +
  coord_flip() +
  labs(
    title = "Total de hechizos disponibles por clase",
    x = "Clase",
    y = "Número de hechizos"
  )

#Ahora ver cuántos hechizos (por nivel) tiene disponible cada clase con un heatmap

heatmap_data <- spell_long %>%
  count(clase, level, name = "n_hechizos")

ggplot(heatmap_data, aes(x = level, y = clase, fill = n_hechizos)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "darkred") +
  scale_x_continuous(breaks = 0:9) +
  labs(
    title = "Distribución del poder mágico entre clases de D&D",
    x = "Nivel del hechizo",
    y = "Clase",
    fill = "Hechizos"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Ahora pasamos a la base de montruos, primero revisamos cantidad de montruos por CR (challenge rating)

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

#Vamos a ver número de monstruos por vulnerabilidad

#Seleccionamos la columna de vulnerabilidad

vuln <- monsters %>%
  select(vulnerabilities)

#Como hay monstruos con más de una vulnerabilidad, separamos estas para que se cuenten independientemente

vuln <- vuln %>%
  separate_rows(vulnerabilities, sep = ", ")

#Limpiamos NA de monstruos sin vulnerabilidades

vuln <- vuln %>%
  filter(!is.na(vulnerabilities),
         vulnerabilities != "")

#Ahora si vemos cuantos monstruos hay por vulnerabilidad

vuln_resumen <- vuln %>%
  count(vulnerabilities, name = "n_monstruos")
vuln_resumen

#Graficamos

ggplot(vuln_resumen, aes(x = n_monstruos,
                         y = reorder(vulnerabilities, n_monstruos))) +
  geom_col(fill = "pink") +
  labs(
    title = "Cantidad de monstruos vulnerables por tipo de daño",
    x = "Número de monstruos",
    y = "Tipo de daño"
  ) +
  scale_y_discrete(labels = function(x) stringr::str_trunc(x, 16, ellipsis = "")) +
  theme_minimal()

#Repetimos el proceso para inmunidades

immune <- monsters %>%
  select(immunities) %>%
  separate_rows(immunities, sep = ", |; ") %>%
  mutate(
    immunities = str_trim(immunities),
    immunities = case_when(
      str_detect(immunities, regex("Charmed", ignore_case = TRUE)) ~ "Charmed", #aquí agregué un paso extra para normalizar la categoria "charmed" ya que venían dos casos escrutis distintos pero querían decir lo mismo.
      TRUE ~ immunities
    )
  ) %>%
  filter(immunities != "", !is.na(immunities))
immune_resumen <- immune %>%
  count(immunities, name = "n_monstruos")
immune_resumen


#Graficamos

ggplot(immune_resumen,
       aes(x = n_monstruos,
           y = reorder(immunities, n_monstruos))) +
  geom_col(fill = "orange") +
  labs(
    title = "Cantidad de monstruos inmunes a condiciones",
    x = "Número de monstruos",
    y = "Tipo de condición"
  ) +
  scale_y_discrete(labels = function(x) stringr::str_trunc(x, 20, ellipsis = "")) +
  theme_classic()

#Comparación de ambos dataset










