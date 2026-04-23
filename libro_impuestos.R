# Librerías y datos ----------------

pacman::p_load(tidyverse, Hmisc, gtsummary, GDAtools, jtools, huxtable, survey, ggoxford, ggtext, factoextra, paletteer)

load("bases/latinobarometro_select.RData")

theme_set(
  theme_bw(base_size = 9) +
    theme(
      panel.grid.major = element_line(linewidth = 0.2, color = "grey90"),
      panel.grid.minor = element_blank()
    )
)


# Construcción variables ----------------------
latinobarometro <- latinobarometro %>% 
  mutate(arreglo_impuestos = ifelse(arreglo_impuestos == 2, 0, 1),
         evasion_dic = ifelse(evasion == 1, 1, 0),
         evasion_dic2 = ifelse(evasion == 1, 0, 1),
         evasion_dic3 = ifelse(evasion >= 7, 1, 0),
         argentina_lat = case_when(pais == 32 ~ "Argentina",
                                   TRUE ~ "Resto América Latina"),
         pro_mercado_dic = factor(case_when(pro_mercado_f == "Muy de acuerdo" ~ "De acuerdo",
                                            pro_mercado_f == "De acuerdo" ~ "De acuerdo",
                                            pro_mercado_f == "En desacuerdo" ~ "En desacuerdo",
                                            pro_mercado_f == "Muy en desacuerdo" ~ "En desacuerdo",
                                            TRUE ~ NA_character_)),
         clase_subjetiva3 = factor(case_when(clase_subjetiva4 %in% c("Media alta", "Media") ~ "Media / Alta",
                                             TRUE ~ clase_subjetiva4),
                                   levels = c("Media / Alta", "Media baja", "Baja"))) %>% 
  filter(pais_f != "Esp")


#ACM------------------------
latinobarometro <- latinobarometro %>%  
  mutate(trabajo_f = case_when(
    estado_ocupacional_f %in% c("Temporalmente no trabaja", "Retirado/pensionado") ~ trabajo_anterior_f, 
    estado_ocupacional_f %in% c("No trabaja", "Estudiante") ~ "No trabaja",
    TRUE ~ trabajo_actual_f))


#2020
var_obj_2020 <- latinobarometro %>%
  filter(anio == 2020) %>%
  select(trabajo_f, nivel_ed, bienes_comp, bienes_lava, bienes_telef, bienes_celular, bienes_auto, bienes_cloaca, bienes_aguapot,
         bienes_aguacal, bienes_casa, wt)

mca2020 <- speMCA(var_obj_2020[,1:11], ncp = 2, row.w = var_obj_2020$wt)

mca2020 <- flip.mca(mca2020, dim = 1)

modif.rate(mca2020)$modif

contribuciones <- tabcontrib(mca2020, dim = 1, best = F)

openxlsx::write.xlsx(contribuciones, "graficos/evasion/contribuciones_mca_2020.xlsx")

ggcloud_variables(mca2020, vlab = T, shapes = T, shapesize = 1.5, force = 2,
                  textsize = 2.3, ) + 
  labs(title = "MCA 2020")

#Construcción de índice (estandarizado)
estandarizar_ponderado <- function(x, w) {
  media_ponderada <- sum(x * w) / sum(w)
  var_ponderada <- sum(w * (x - media_ponderada)^2) / sum(w)
  desv_ponderada <- sqrt(var_ponderada)
  return((x - media_ponderada) / desv_ponderada)
}

coords_2020 <- data.frame(anio = 2020, dim1 = mca2020$ind$coord[,1])


# Filtrar y unir con coordenadas
latinobarometro2020 <- latinobarometro %>%
  filter(anio == 2020) %>%
  bind_cols(dim1 = coords_2020$dim1) %>%
  mutate(dim1_std = estandarizar_ponderado(dim1, wt),
         dim1_01= scales::rescale(dim1, to = c(0, 1)))


#Resultados------------------

##Evasión y tiempo----------------------
prom_evasion <- latinobarometro %>% 
  summarise(evasion = weighted.mean(arreglo_impuestos, wt = wt, na.rm = T))          

latinobarometro %>% 
  group_by(anio, pais_f) %>% 
  summarise(evasion = weighted.mean(arreglo_impuestos, wt = wt, na.rm = T)) %>%
  na.omit() %>% 
  ggplot(aes(x = anio, y = evasion, group = 1)) +
  # geom_line(na.rm = T) +
  geom_hline(yintercept = prom_evasion$evasion, linetype = "dashed", color = "red", size = .3) +
  geom_point(size = .7) +
  geom_text(
    data = function(d) d %>% filter(anio == 2020),
    aes(label = scales::percent(evasion, accuracy = 1)),
    size = 2, vjust = -0.6, hjust = 1
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6.5),
    strip.text = element_text(size = 6.5, margin = margin(t = 2, b = 2))
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""), breaks = seq(0, .5, 0.15), limits = c(0, .5)) +
  scale_x_continuous(breaks = seq(1998, 2020, 2), limits = c(1998, 2020)) +
  facet_wrap(~pais_f, ncol = 3)


ggsave("graficos/libro/porcentaje_evasion_impuestos.png", width = 4.9, height = 3.5, dpi = 300)

##Justificación evasión-----------------
latinobarometro %>%
  filter(!is.na(evasion_dic)) %>% 
  group_by(anio, argentina_lat) %>%
  summarise(promedio = weighted.mean(evasion_dic, wt = wt, na.rm = T)) %>%   
  ggplot(aes(x = anio, y = promedio, fill = argentina_lat, colour = argentina_lat)) +
  geom_point() +
  geom_line() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  scale_y_continuous(limits = c(0, .8), breaks = seq(0, .8, .1), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(1998, 2003, 2005, 2008, 2009, 2010, 2011, 2013, 2015, 2016, 2023))

ggsave("graficos/libro/tendencias_evasion_just.png", width = 4.9, height = 3.5, dpi = 300)


##Estratificación y evasión------------------
modelo <- latinobarometro2020 %>% 
  filter(pais_f != "Chi") %>% 
  group_by(pais_f) %>% 
  summarise(
    promedio = weighted.mean(dim1_std, wt = wt, na.rm = TRUE),
    evasion = weighted.mean(arreglo_impuestos, wt = wt, na.rm = TRUE)
  ) |> 
  lm(evasion ~ promedio, data = _)

r2 <- summary(modelo)$r.squared


latinobarometro2020 %>%
  group_by(pais_f) %>%
  filter(pais_f != "Chi") %>% 
  summarise(
    promedio = weighted.mean(dim1_std, wt = wt, na.rm = T),
    evasion = weighted.mean(arreglo_impuestos, wt = wt, na.rm = T)) %>% 
  ggplot(aes(x = promedio, y = evasion)) +
  geom_point(size = 1.5, alpha = .5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = .5, color = "red") +
  ggrepel::geom_text_repel(aes(label = pais_f), size = 2, show.legend = FALSE) +
  annotate("text",
           x = 0.32, y = 0.39,  # Ajustá posición
           label = paste0("R² = ", round(r2, 3)),
           size = 3, hjust = 0) +
  labs(
    # title = "Gráfico 4. Incumplimiento impositivo y estratificación social objetiva",
    #    subtitle = "Países de América Latina. 2020. Error estándar en gris.",
    #    caption = "Fuente: elaboración propia en base a Latinobarómetro.  
    #    Chile fue descartado en este gráfico ya que puede considerarse un caso *outlier*.",
    y = "Arreglo para pagar menos impuestos",
    x = "Estratificación social objetiva media de países") +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.caption = ggtext::element_markdown(size = 8)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(.15, .4, 0.05), limits = c(.15, .4)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1), breaks = seq(-1, 1, 0.2), limits = c(-1, 1))

ggsave("graficos/libro/estratificacion_evasion.png", width = 4.9, height = 3.5, dpi = 300)


## Clase subjetiva y evasión --------------

latinobarometro2020 %>%
  filter(!is.na(clase_subjetiva4)) %>% 
  group_by(pais_f, clase_subjetiva4) %>%
  summarise(
    promedio = weighted.mean(arreglo_impuestos, wt = wt, na.rm = T)) %>% 
  ggplot(aes(x = clase_subjetiva4, y = promedio)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = .8) +
  geom_hline(yintercept = prom_evasion$evasion, linetype = "dashed", color = "red") +
  # labs(title = "Gráfico 5. Porcentaje de personas que conoce a alguien que pagó menos \nimpuestos de lo que debía, según clase social subjetiva",
  #      subtitle = "Países de América Latina. 2020 (Porcentaje general en línea punteada roja).",
  #      caption = "Fuente: elaboración propia en base a Latinobarómetro") +
  theme(
    title = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    strip.text = element_text(size = 9, margin = margin(t = 2, b = 2))
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""), breaks = seq(0, .65, 0.15), limits = c(0, .65)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  facet_wrap(~pais_f, ncol = 2)


ggsave("graficos/libro/porcentaje_evasion_impuestos_clase_subjetiva.png", width = 4.9, height = 7, dpi = 300)


## Regresión ---------
latinobarometro2020$just_dist_ingresos_dic <- relevel(latinobarometro2020$just_dist_ingresos_dic, ref = "Muy justa-justa-injusta")
latinobarometro2020$clase_subjetiva4 <- relevel(latinobarometro2020$clase_subjetiva4, ref = "Baja")
latinobarometro2020$clase_subjetiva3 <- relevel(latinobarometro2020$clase_subjetiva3, ref = "Baja")
latinobarometro2020$pro_mercado_dic <- relevel(latinobarometro2020$pro_mercado_dic, ref = "De acuerdo")
latinobarometro2020$pais_f <- relevel(latinobarometro2020$pais_f, ref = "Mex")
latinobarometro2020$sexo2 <- relevel(latinobarometro2020$sexo2, ref = "Mujer")
latinobarometro2020$ayuda_pobres_dinero_f <- relevel(latinobarometro2020$ayuda_pobres_dinero_f, ref = "No")
latinobarometro2020$ayuda_gobierno_tramo_f <- relevel(latinobarometro2020$ayuda_gobierno_tramo_f, ref = "A ninguno")
latinobarometro2020$arreglo_impuestos <- as.factor(latinobarometro2020$arreglo_impuestos)
latinobarometro2020$arreglo_impuestos <- relevel(latinobarometro2020$arreglo_impuestos, ref = "0")
latinobarometro2020$corrupcion_f <- relevel(latinobarometro2020$corrupcion_f, ref = "Nada")

mod1 <- glm(arreglo_impuestos ~ edad + sexo2 + dim1_std + clase_subjetiva4 + just_dist_ingresos_dic + pro_mercado_dic + ayuda_pobres_dinero_f + ayuda_gobierno_tramo_f + pago_impuestos_dic + aceptabilidad_desigualdad + corrupcion_f + ideologia + pais_f, data = latinobarometro2020, weights = wt, family = binomial(link = "logit"), na.action = na.omit)

mod2 <- glm(arreglo_impuestos ~ edad + sexo2 + dim1_std + clase_subjetiva4 + just_dist_ingresos_dic + pro_mercado_dic + ayuda_pobres_dinero_f + ayuda_gobierno_tramo_f + pago_impuestos_dic + aceptabilidad_desigualdad + corrupcion_f + ideologia, data = subset(latinobarometro2020, pais_f == "Arg"), weights = wt, family = binomial(link = "logit"), na.action = na.omit)

reg_coef <- plot_summs(mod1, ci_level = 0.91,
                       point.size = 1.5,
                       line.size = c(0.5, 1),
                       exp = T,
                       coefs = c("Edad" = "edad",
                               "Varón" = "sexo2Varón",
                               "Estratificación objetiva" = "dim1_std",
                               "Clase sub. media alta (ref: baja)" = "clase_subjetiva4Media alta",
                               "Clase sub. media (ref: baja)" = "clase_subjetiva4Media",
                               "Clase sub. media baja (ref: baja)" = "clase_subjetiva4Media baja",
                               "Distribución muy injusta" = "just_dist_ingresos_dicMuy injusta",
                               "En desacuerdo mercado" = "pro_mercado_dicEn desacuerdo",
                               "Ayudar con dinero a pobres" = "ayuda_pobres_dinero_fSi",
                               "Ayuda gob. tramo bajo (ref: A ninguno)" = "ayuda_gobierno_tramo_fBajo",
                               "Ayuda gob. tramo medio (ref: A ninguno)" = "ayuda_gobierno_tramo_fMedio",
                               "Ayuda gob. tramo alto (ref: A ninguno)" = "ayuda_gobierno_tramo_fAlto",
                               "Ayuda gob. a todos (ref: A ninguno)" = "ayuda_gobierno_tramo_fA todos por igual",
                               "Todos deben pagar impuestos por igual (ref: solo ricos)" = "pago_impuestos_dicTodos pagan por igual",
                               "Aceptabilidad de la desigualdad" = "aceptabilidad_desigualdad",
                               "Algún progreso contra corrupción" = "corrupcion_fMucho-Algo-Poco",
                               "Ideologia (izq-der)" = "ideologia"))


reg_coef +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.position= "bottom",
        legend.title = element_blank()) 

ggsave("graficos/libro/regresion.png", width = 6, height = 4, dpi = 300)

tabla <- export_summs(mod1, mod2, model.names = c("Efecto fijo países", "Sólo Argentina"), 
                     exp = T, stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
                     error_pos = "same",
                     coefs = c("Edad" = "edad",
                               "Varón" = "sexo2Varón",
                               "Estratificación objetiva" = "dim1_std",
                               "Clase sub. media alta" = "clase_subjetiva4Media alta",
                               "Clase sub. media" = "clase_subjetiva4Media",
                               "Clase sub. media baja" = "clase_subjetiva4Media baja",
                               "Distribución muy injusta" = "just_dist_ingresos_dicMuy injusta",
                               "En desacuerdo mercado" = "pro_mercado_dicEn desacuerdo",
                               "Ayudar con dinero a pobres" = "ayuda_pobres_dinero_fSi",
                               "Ayuda gob. tramo bajo (ref: A ninguno)" = "ayuda_gobierno_tramo_fBajo",
                               "Ayuda gob. tramo medio" = "ayuda_gobierno_tramo_fMedio",
                               "Ayuda gob. tramo alto" = "ayuda_gobierno_tramo_fAlto",
                               "Ayuda gob. a todos" = "ayuda_gobierno_tramo_fA todos por igual",
                               "Todos deben pagar impuestos por igual (ref: solo ricos)" = "pago_impuestos_dicTodos pagan por igual",
                               "Aceptabilidad de la desigualdad" = "aceptabilidad_desigualdad",
                               "Algún progreso contra corrupción" = "corrupcion_fMucho-Algo-Poco",
                               "Ideologia (izq-der)" = "ideologia"))

tabla %>%  
  set_caption("Modelos de regresión logística. Odds ratios. Evasión impositiva. 2020")

tabla <- tabla %>% 
  as_flextable() %>%  
  flextable::save_as_docx(path = "graficos/libro/regresion.docx")
