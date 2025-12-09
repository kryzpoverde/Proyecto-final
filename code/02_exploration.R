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
  count(level, name = "n_spells")
spells_por_nivel

# Graficar resultado

ggplot(spells_por_nivel, aes(x = level, y = n_spells)) +
  geom_col() +
  labs(
    title = "Hechizos por nivel",
    x = "Nivel",
    y = "Cantidad"
  )


#Ahora vamos a analizar hechizos por su disponibilidad y cantidad para distintas clases

#Primero seleccionamos las columnas que nos interesan y las pasamos a formato largo para análisis posteriores

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
  ggplot(aes(x = clase, y = n_hechizos)) +
  geom_col() +
  labs(
    title = "Hechizos por clase",
    x = "Clase",
    y = "Cantidad"
  )


#Ahora ver cuántos hechizos (por nivel) tiene disponible cada clase con un heatmap

heatmap_data <- spell_long %>%
  count(clase, level, name = "n_hechizos")

ggplot(heatmap_data, aes(x = level, y = clase, fill = n_hechizos)) +
  geom_tile() +
  labs(
    title = "Hechizos por nivel y clase",
    x = "Nivel",
    y = "Clase"
  )


#Ahora pasamos a la base de montruos, primero revisamos cantidad de montruos por CR (challenge rating)

ggplot(monsters, aes(x = cr_group)) +
  geom_bar() +
  labs(
    title = "Monstruos por categoría de CR",
    x = "Categoría de CR",
    y = "Cantidad"
  )

#Vamos a ver número de monstruos por vulnerabilidad

vuln_resumen <- monsters %>%
  select(vulnerabilities) %>% #seleccionamos la columna de vulnerabilidad
  separate_rows(vulnerabilities, sep = ", ") %>% #como hay monstruos que tienen más de una vulnerabilidad, separamos para que se cuenten independientemente
  filter(!is.na(vulnerabilities), vulnerabilities != "") %>% #filtramos NA de monstruos sin vulnerabilidades
  count(vulnerabilities, name = "n_monstruos") #contar cuantos monstruos hay por vulnerabilidad

#Graficamos

ggplot(vuln_resumen, aes(x = vulnerabilities, y = n_monstruos)) +
  geom_col() +
  labs(
    title = "Monstruos por vulnerabilidad",
    x = "Tipo de vulnerabilidad",
    y = "Cantidad"
  )

#Repetimos el proceso para inmunidades

immune_resumen <- monsters %>%
  select(immunities) %>%
  separate_rows(immunities, sep = ", |; ") %>% #aquí además de separar por "," también separé por ";" porque la columna de inmunidades venía con estas dos separaciones
  filter(immunities != "", !is.na(immunities)) %>%
  count(immunities, name = "n_monstruos")


#Graficamos

ggplot(immune_resumen, aes(x = immunities, y = n_monstruos)) +
  geom_col() +
  labs(
    title = "Monstruos por condición de inmunidad",
    x = "Tipo de condición",
    y = "Cantidad"
  )


#Por último, vamos a comparar la distribución de hechizos y monstruos a través de los niveles
#Para esto elegí un gráfico de lineas comparativas

#Escalar los CR de los monstruos y dejarlos de 0 a 9 igual que los hechizos

monsters_scaled <- monsters %>%
  mutate(cr_bin = floor((cr / max(cr, na.rm = TRUE)) * 9))

#Proporcionar hechizos y monstruos

spells_por_nivel <- spells_por_nivel %>%
  mutate(prop_spells = n_spells / sum(n_spells))
monsters_por_nivel <- monsters_scaled %>%
  count(cr_bin, name = "n_monsters") %>%
  mutate(prop_monsters = n_monsters / sum(n_monsters))

#Unimos ambos datasets en uno para graficar

comparativo <- bind_rows(
  spells_por_nivel %>% transmute(nivel = level, proporcion = prop_spells, tipo = "Hechizos"),
  monsters_por_nivel %>% transmute(nivel = cr_bin, proporcion = prop_monsters, tipo = "Monstruos")
)

#Graficamos

ggplot(comparativo, aes(x = nivel, y = proporcion, color = tipo)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Comparación de distribución relativa entre hechizos y monstruos",
    x = "Nivel / CR equivalente",
    y = "Proporción",
    color = "Categoría"
  )













