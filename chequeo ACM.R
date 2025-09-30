library(dplyr)
library(ggplot2)
library(scales)

# 1. Resumen estadístico por año
resumen_indice <- latinobarometro_sel %>%
  group_by(anio) %>%
  summarise(
    min_std = min(dim1_std, na.rm = TRUE),
    max_std = max(dim1_std, na.rm = TRUE),
    mean_std = weighted.mean(dim1_std, wt, na.rm = TRUE),
    sd_std = sqrt(sum(wt*(dim1_std - weighted.mean(dim1_std, wt, na.rm=TRUE))^2)/sum(wt)),
    min_01 = min(dim1_01, na.rm = TRUE),
    max_01 = max(dim1_01, na.rm = TRUE),
    mean_01 = weighted.mean(dim1_01, wt, na.rm = TRUE)
  )

print(resumen_indice)

# 2. Revisión de NAs
na_check <- latinobarometro_sel %>%
  summarise(
    na_std = sum(is.na(dim1_std)),
    na_01 = sum(is.na(dim1_01))
  )

print(na_check)

# 3. Correlación entre versiones del índice
cor_std_01 <- cor(latinobarometro_sel$dim1_std,
                  latinobarometro_sel$dim1_01,
                  use = "complete.obs")
cat("Correlación entre dim1_std y dim1_01:", cor_std_01, "\n")

# 4. Correlación con variables originales (ejemplo: bienes_auto)
cor_bienes <- cor(latinobarometro_sel$dim1_std,
                  as.numeric(latinobarometro_sel$bienes_auto),
                  use = "complete.obs")
cat("Correlación entre dim1_std y bienes_auto:", cor_bienes, "\n")

# 5. Visualización de la distribución del índice por año
ggplot(latinobarometro_sel, aes(x = dim1_std)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~anio, scales = "free") +
  labs(title = "Distribución del índice MCA estandarizado por año",
       x = "dim1_std",
       y = "Cantidad de casos") +
  theme_minimal()



ggplot(latinobarometro_sel, aes(x = dim1_std, fill = factor(anio))) +
  geom_density(alpha = 0.4) +
  geom_rug(aes(color = factor(anio)), sides = "b", alpha = 0.5) +
  labs(title = "Distribución del índice de estatus objetivo por año",
       x = "Índice estandarizado (dim1_std)",
       y = "Densidad",
       fill = "Año",
       color = "Año") +
  theme_minimal()

