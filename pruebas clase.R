# Carga bases---------------

rm(list = ls())

pacman::p_load(tidyverse, Hmisc, gtsummary, GDAtools, jtools, huxtable, survey, ggoxford, ggtext, lme4, performance, sjPlot)

load("bases/latinobarometro_select.RData")
load("bases/base_datos_agregados.RData")

#Banderas
latinobarometro <- latinobarometro %>% 
  mutate(iso3 = countrycode::countrycode(pais,
                                         origin = "iso3n",
                                         destination = "iso3c"))

#Unión bases individual y agregada
latinobarometro <- latinobarometro %>% 
  left_join(datos_paises, by = c("anio", "iso3")) 

latinobarometro <- latinobarometro %>% 
  mutate(anio_pais = interaction(iso3, anio))

latinobarometro2018 <- latinobarometro %>% 
  filter(anio >= 2023)


# Arreglo variables------------
latinobarometro2018 <- latinobarometro2018 %>% 
  mutate(
    clase_media = factor(case_when(clase_subjetiva_f %in% c("Alta", "Media alta",
                                                            "Media") ~ "Media",
                                   TRUE ~ "Media baja / Baja"),
                         levels = c("Media baja / Baja", "Media")))








# Multinivel-----------------

mod1 <- glmer(clase_media ~ 1 + (1 | anio_pais), data = latinobarometro2018, family = binomial(link = "logit"), weights = wt)

icc(mod1)




# Gráficos---------------------
latinobarometro2018 %>% 
  group_by(iso3, anio, clase_media) %>%
  tally(wt = wt) %>%
  mutate(porcentaje = n / sum(n)) %>% 
  filter(clase_media == "Media") %>% 
  ggplot(aes(x = as.character(anio), y = porcentaje)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 1)), 
            position = position_stack(vjust = 0.5), size = 2) +
  labs(title = "Porcentaje de personas que se consideran clase media \nen América Latina. 2018-2023",
       caption = "Fuente: elaboración propia en base a Latinobarómetro") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~iso3, ncol = 4)
  

