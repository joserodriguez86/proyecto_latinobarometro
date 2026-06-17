latinobarometro_sel %>% 
  filter(!is.na(clase_subjetiva4)) %>% 
  group_by(anio, pais_f, clase_subjetiva4) %>% 
  tally(wt) %>% 
  mutate(porcentaje = n / sum(n)) %>% 
  ggplot(aes(x = as.character(anio), y = porcentaje, fill = clase_subjetiva4)) +
  geom_col(position = "fill") +
  # labs(
  #   title = "Gráfico 3. Distribución de la población según clase subjetiva",
  #   subtitle = "Países seleccionados de América Latina. 2011-2024",
  #   fill = "Clase subjetiva",
  #   caption = "Fuente: elaboración propia en base a Latinobarómetro"
  # ) +
  scale_fill_locuszoom() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8), 
    legend.position = "bottom",
    strip.text = element_text(color = "black")
  ) +
  facet_wrap(~pais_f, ncol = 4)
