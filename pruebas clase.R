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

latinobarometro_sel <- latinobarometro %>% 
  filter(anio %in% c(2011, 2013, 2017, 2018, 2020, 2023), !iso3 %in% c("ESP", "VEN", "NIC"))


# Arreglo variables------------
latinobarometro_sel <- latinobarometro_sel %>% 
  group_by(anio_pais) %>% 
  mutate(edad_cw = scale(edad, center = T, scale = T)) %>% 
  ungroup() %>% 
  mutate(
    clase_media = factor(case_when(clase_subjetiva_f %in% c("Alta", "Media alta",
                                                            "Media") ~ 1,
                                   TRUE ~ 0)),
    pib_ppa_cgm = scale(pib_ppa,center = T, scale = T),
    gini_cgm = scale(gini, center = T, scale = T),
    informalidad_cgm = scale(informalidad, center = T, scale = T),
    ocupaciones_medias_cgm = scale(ocupaciones_medias, center = T, scale = T),
    empleo_cgm = scale(emp_publico, center = T, scale = T),
    palma_cgm = scale(palma, center = T, scale = T))

# Multinivel-----------------

mod1 <- glmer(clase_media ~ 1 + (1 | anio_pais), data = latinobarometro_sel, family = binomial(link = "logit"))

icc(mod1)


mod2 <- glmer(clase_media ~ edad_cw + sexo2 + (1 | anio_pais), data = latinobarometro_sel, family = binomial(link = "logit"))

mod3 <- glmer(clase_media ~ edad_cw + sexo2 + pib_ppa_cgm + ocupaciones_medias_cgm + informalidad_cgm + empleo_cgm + palma_cgm + (1 | anio_pais), data = latinobarometro_sel, family = binomial(link = "logit"))

compare_performance(mod1, mod2, mod3)


tab_model(mod1, mod2, mod3, p.style = "stars",
          show.ci = F,
          show.icc = FALSE,
          show.aic = T,
          p.threshold = c(0.1, 0.05, 0.01))


library(sjPlot)

# Suponiendo que tu modelo completo se llama mod3:
plot_model(mod3, 
           type = "est", 
           transform = "exp",  # para mostrar Odds Ratios
           show.values = TRUE, 
           value.offset = 0.3,
           title = "Efectos fijos sobre la probabilidad de autopercibirse clase media",
           axis.labels = c("Índice de Gini", "Empleo público", "Informalidad", 
                           "Ocupaciones medias", "PIB per cápita PPA",
                           "Sexo: Mujer", "Edad (centrada por grupo)"),
           vline.color = "grey") +
  theme_minimal()

plot_model(mod3, 
           type = "re", 
           sort.est = TRUE,
           title = "Efectos aleatorios por año-país",
           dot.size = 2) +
  theme_minimal()


re <- ranef(mod3)$anio_pais %>% 
  as.data.frame() %>%
  rename(effect = `(Intercept)`) %>%
  mutate(anio_pais = rownames(ranef(mod3)$anio_pais)) %>%
  arrange(desc(effect))  # Ordenar de mayor a menor

# Graficar
ggplot(re, aes(x = reorder(anio_pais, effect), y = effect)) +
  geom_point() +
  coord_flip() +
  labs(x = "Año-País", y = "Efecto aleatorio (intercepto)", 
       title = "Efectos aleatorios por año-país") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5)) 


ggsave("graficos/efectos_aleatorios.png", width = 8, height = 6, bg = "white")

# Pruebas---------------------
latinobarometro_sel %>% 
  group_by(iso3, anio, clase_media) %>%
  tally(wt = wt) %>%
  mutate(porcentaje = n / sum(n)) %>% 
  filter(clase_media == 1) %>% 
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


tabla <- latinobarometro2018 %>% 
  group_by(anio, ideologia) %>% 
  tally() %>% 
  pivot_wider(names_from = ideologia, values_from = n)
  
  

