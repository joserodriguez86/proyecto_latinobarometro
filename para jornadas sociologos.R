rm(list = ls())

pacman::p_load(tidyverse, Hmisc, gtsummary, GDAtools, jtools, huxtable, survey, ggoxford, ggtext, lme4, performance, sjPlot, ggsci, flextable)

theme_set(theme_light())

load("bases/latinobarometro_select.RData")
load("bases/base_datos_agregados.RData")

#id único
latinobarometro <- latinobarometro %>%
  group_by(pais, anio) %>%
  mutate(id_fila = paste0(pais, anio, row_number())) %>%
  ungroup()


#Banderas
latinobarometro <- latinobarometro  %>%  
  mutate(iso3 = countrycode::countrycode(pais,
                                         origin = "iso3n",
                                         destination = "iso3c"))

#Unión bases individual y agregada
latinobarometro <- latinobarometro %>% 
  left_join(datos_paises, by = c("anio", "iso3")) 

latinobarometro <- latinobarometro %>% 
  mutate(anio_pais = interaction(iso3, anio))

latinobarometro_sel <- latinobarometro %>% 
  filter(anio %in% c(2011, 2013, 2015, 2017, 2018, 2020, 2023, 2024), !iso3 %in% c("ESP", "VEN", "NIC"))

rm(latinobarometro, datos_paises)





latinobarometro_sel %>% 
  filter(!is.na(clase_subjetiva4), pais_f == "Arg") %>% 
  group_by(anio, clase_subjetiva4) %>% 
  tally(wt) %>% 
  mutate(porcentaje = n / sum(n)) %>% 
  ggplot(aes(x = as.character(anio), y = porcentaje, fill = clase_subjetiva4)) +
  geom_col(position = "fill") +
  geom_text(aes(label = if_else(porcentaje > 0.05, 
                                scales::percent(porcentaje, accuracy = 1), 
                                "")), 
            position = position_fill(vjust = 0.5), size = 3) +
  labs(
    title = "Distribución de la población según clase subjetiva",
    subtitle = "Argentina. 2011-2024",
    fill = "Clase subjetiva",
    caption = "Fuente: elaboración propia en base a Latinobarómetro"
  ) +
  scale_fill_locuszoom() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9), 
    legend.position = "bottom",
    strip.text = element_text(color = "black")
  )

ggsave("graficos/clase_sub_arg.png", width = 8, height = 5, dpi = 300)
