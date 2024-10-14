# Librerías y bases -----------------
rm(list = ls())

pacman::p_load(tidyverse, Hmisc)

load("bases/latinobarometro_select.RData")

theme_set(theme_light())


# Arreglos de variables -----------------
latinobarometro <- latinobarometro %>% 
  mutate(arreglo_impuestos = ifelse(arreglo_impuestos == 2, 0, 1)) %>% 
  filter(pais_f != "Esp")

# Descriptivos -------------------

## Tendencias variables de evasión en el tiempo ----------------

latinobarometro %>%
  group_by(anio) %>%
  summarise(
    promedio = weighted.mean(evasion, wt = wt, na.rm = T)) %>%   
  ggplot(aes(x = anio, y = promedio)) +
  geom_point(size = 1.5) +
  labs(title = "Promedio de la escala de evasión de impuestos en América Latina. 1998-2023",
       caption = "Fuente: elaboración propia en base a Latinobarómetro") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(1998, 2023, 1)) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1))

ggsave("graficos/promedio_evasion_impuestos.png", width = 8, height = 6)

latinobarometro %>% 
  group_by(anio) %>%
  summarise(
    promedio = weighted.mean(arreglo_impuestos, wt = wt, na.rm = T)) %>% 
  filter(!is.na(promedio)) %>% 
  ggplot(aes(x = as.factor(anio), y = promedio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Porcentaje de personas que pagaron menos impuestos de lo debido \nen América Latina. 1998-2020",
       caption = "Fuente: elaboración propia en base a Latinobarómetro") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, .4, 0.05), limits = c(0, .4))


ggsave("graficos/porcentaje_evasion_impuestos.png", width = 8, height = 6)


# Tendencias variables evasión por país -------------------

promedio_general <- latinobarometro %>% 
  filter(anio == 2023) %>%
  summarise(
    promedio = weighted.mean(evasion, wt = wt, na.rm = T))

latinobarometro %>%
  group_by(pais_f, anio) %>%
  summarise(
    promedio = weighted.mean(evasion, wt = wt, na.rm = T),
    sd = sqrt(wtd.var(evasion, weights = wt, na.rm = T)),  # Desviación estándar ponderada
    n = sum(wt),  # Tamaño ponderado
    error = qt(0.975, df=n-1) * sd / sqrt(n),  # Intervalo de confianza al 95%
    ic_inf = promedio - error,  
    ic_sup = promedio + error) %>%  
  filter(anio == 2023) %>% 
  ggplot(aes(x = fct_reorder(pais_f, promedio, .desc = T), y = promedio)) +
  geom_hline(yintercept = promedio_general$promedio, linetype = "dashed", color = "red") +
  geom_pointrange(aes(ymin = ic_inf, ymax = ic_sup)) +
  labs(title = "Promedio de la escala de evasión de impuestos en América Latina. 2023",
       caption = "Fuente: elaboración propia en base a Latinobarómetro") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
  coord_flip()

ggsave("graficos/promedio_evasion_impuestos_pais.png", width = 8, height = 6)


latinobarometro %>% 
  filter(anio == 2020) %>%
  group_by(pais_f) %>%
  summarise(
    promedio = weighted.mean(arreglo_impuestos, wt = wt, na.rm = T)) %>% 
  filter(!is.na(promedio)) %>% 
  ggplot(aes(x = fct_reorder(pais_f, promedio, .desc = T), y = promedio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Porcentaje de personas que pagaron menos impuestos de lo debido \nen América Latina. 2020",
       caption = "Fuente: elaboración propia en base a Latinobarómetro") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, .4, 0.05), limits = c(0, .4)) +
  coord_flip()

ggsave("graficos/porcentaje_evasion_impuestos_pais.png", width = 8, height = 6)
