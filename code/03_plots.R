# Proyecto final – Visualización de datos
# 03_plots.R
# Gráficos finales
# Autora: Golondrina Segura

# Cargar librerías

library(tidyverse)

# Leer datos procesados

spells <- read_csv("data/procesados/spells_clean.csv")
monsters <- read_csv("data/procesados/monsters_clean.csv")

# Vamos a empezar revisando cuántos hechizos hay por nivel

spells_por_nivel <- spells %>%
  count(level, name = "n_spells")

p_hechizos_nivel <- ggplot(spells_por_nivel, aes(x = level, y = n_spells)) +
  geom_col(fill = "#9333FF", alpha = 0.6) +
  scale_x_continuous(breaks = 0:9) +
  scale_y_continuous(breaks = seq(0, 60, by = 5)) +
  labs(
    title = "Cantidad de hechizos por nivel",
    x = "Nivel del hechizo",
    y = "Número de hechizos"
  ) +
  theme_classic()
p_hechizos_nivel

#Ahora vamos a analizar hechizos por su disponibilidad y cantidad para distintas clases

spell_long <- spells %>%
  select(level, bard, cleric, druid, paladin, ranger, sorcerer, warlock, wizard) %>%
  pivot_longer(
    cols      = -level,
    names_to  = "clase",
    values_to = "puede_usar"
  ) %>%
  filter(puede_usar == TRUE)

p_hechizos_clase <- spell_long %>%
  count(clase, name = "n_hechizos") %>%
  ggplot(aes(x = reorder(clase, n_hechizos), y = n_hechizos)) +
  geom_col(fill = "#9333FF") +
  coord_flip() +
  labs(
    title = "Total de hechizos disponibles por clase",
    x = "Clase",
    y = "Número de hechizos"
  ) +
  theme_classic()
p_hechizos_clase

#Ahora ver cuántos hechizos (por nivel) tiene disponible cada clase con un heatmap

heatmap_data <- spell_long %>%
  count(clase, level, name = "n_hechizos")

p_heatmap_clases <- ggplot(heatmap_data, aes(x = level, y = clase, fill = n_hechizos)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "darkred") +
  scale_x_continuous(breaks = 0:9) +
  labs(
    title = "Distribución del poder mágico entre clases de D&D",
    x    = "Nivel del hechizo",
    y    = "Clase",
    fill = "Hechizos"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_heatmap_clases

#Ahora pasamos a la base de montruos, primero revisamos cantidad de montruos por nivel de amenaza

p_monstruos_cr <- ggplot(
  monsters,
  aes(x = factor(cr_group,
                 levels = c("Noob", "Intermedio", "Avanzado", "Épico", "Legendario")))
) +
  geom_bar(fill = "#FF8F78", alpha = 0.8) +
  scale_y_continuous(
    breaks = seq(0, max(table(monsters$cr_group)), by = 50)
  ) +
  labs(
    title = "Distribución de monstruos por nivel de amenaza",
    x = "Nivel de amenaza",
    y = "Cantidad de monstruos"
  ) +
  theme_classic()
p_monstruos_cr

#Vamos a ver número de monstruos por vulnerabilidad

vuln_resumen <- monsters %>%
  select(vulnerabilities) %>% #seleccionamos la columna de vulnerabilidad
  separate_rows(vulnerabilities, sep = ", ") %>% #como hay monstruos que tienen más de una vulnerabilidad, separamos para que se cuenten independientemente
  filter(!is.na(vulnerabilities), vulnerabilities != "") %>% #filtramos NA de monstruos sin vulnerabilidades
  count(vulnerabilities, name = "n_monstruos") #contar cuantos monstruos hay por vulnerabilidad

p_vulnerabilidades <- ggplot(vuln_resumen,
                             aes(x = n_monstruos,
                                 y = reorder(vulnerabilities, n_monstruos))) +
  geom_col(fill = "pink") +
  labs(
    title = "Cantidad de monstruos vulnerables por tipo de daño",
    x = "Número de monstruos",
    y = "Tipo de daño"
  ) +
  scale_y_discrete(labels = function(x) stringr::str_trunc(x, 16, ellipsis = "")) +
  theme_minimal()
p_vulnerabilidades

#Repetimos el proceso para inmunidades

immune_resumen <- monsters %>%
  select(immunities) %>%
  separate_rows(immunities, sep = ", |; ") %>% #aquí además de separar por "," también separé por ";" porque la columna de inmunidades venía con estas dos separaciones
  filter(immunities != "", !is.na(immunities)) %>%
  count(immunities, name = "n_monstruos")

p_inmunidades <- ggplot(immune_resumen,
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
p_inmunidades

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

p_comparativo <- ggplot(comparativo, aes(x = nivel, y = proporcion, color = tipo)) +
  geom_line(size = 1.4) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 0:9) +
  scale_color_manual(values = c("Hechizos" = "#9333FF", "Monstruos" = "#FF8F78")) +
  labs(
    title = "Comparación de distribución relativa entre hechizos y monstruos",
    x = "Nivel / CR equivalente",
    y = "Proporción",
    color = "Categoría"
  ) +
  theme_classic()

#guardar archivos

ggsave("figures/hechizos_por_nivel.png", p_hechizos_nivel, width = 8, height = 5)
ggsave("figures/hechizos_por_clase.png", p_hechizos_clase, width = 8, height = 5)
ggsave("figures/hechizos_heatmap.png", p_heatmap_clases, width = 8, height = 5)
ggsave("figures/monstruos_por_cr.png", p_monstruos_cr, width = 8, height = 5)
ggsave("figures/monstruos_vulnerabilidad.png", p_vulnerabilidades, width = 8, height = 5)
ggsave("figures/monstruos_inmunidad.png", p_inmunidades, width = 8, height = 5)
ggsave("figures/comparativo_hechizos_monstruos.png", p_comparativo, width = 8, height = 5)