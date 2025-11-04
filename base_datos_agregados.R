#Librerías----------------
library(tidyverse)
library(CepalStatR)
library(WDI)
library(Rilostat)
library(archive)

indicadores <- call.indicators(language.en = F)

data("WDI_data", package = "WDI")
countries <- WDI_data[["country"]]

#Consultas---------

#Ingresos tributarios (CEPAL)
ing_tributarios <- call.data(id.indicator = 821, language.en = F)

ingresos_tributarios <- ing_tributarios %>% 
  filter(iso3 %in% c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU",
                     "SLV", "GTM", "HND", "MEX", "NIC", "PAN", "PRY", "PER",
                     "URY", "VEN"),
         `Cobertura institucional__IngTri` == "Gobierno general",
         Clasificacion_impuestos %in% c("Total ingresos tributarios", "Ingresos tributarios directos",
                                        "Impuestos sobre los ingresos utilidades y ganancias de capital",
                                        "Ingresos tributarios indirectos",
                                        "Impuestos generales sobre bienes y servicios",
                                        "Total ingresos tributarios (incluyendo contribuciones sociales)"),
         
         Años >= 2010)

ingresos_tributarios <- ingresos_tributarios %>%
  pivot_wider(id_cols = c(iso3, Años),  
              names_from = Clasificacion_impuestos, 
              values_from = value) %>%
  rename(pais = iso3, 
         anio = Años, 
         impuestos_tot = 'Total ingresos tributarios',
         impuestos_dir = 'Ingresos tributarios directos',
         impuestos_renta = 'Impuestos sobre los ingresos utilidades y ganancias de capital',
         impuestos_ind = 'Ingresos tributarios indirectos',
         impuestos_iva = 'Impuestos generales sobre bienes y servicios',
         impuestos_tot_cs = 'Total ingresos tributarios (incluyendo contribuciones sociales)')


#Ingresos tributarios banco mundial
options(scipen = 999)

ingresos_tributarios_wb <- read.csv("bases/rev_tax_data.csv")

ingresos_tributarios_wb <- ingresos_tributarios_wb %>%
  filter(iso3_code %in% c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU",
                     "SLV", "GTM", "HND", "MEX", "NIC", "PAN", "PRY", "PER",
                     "URY", "VEN"),
         Year2 >= 1998)

ingresos_tributarios_wb_tot <- ingresos_tributarios_wb %>%
  filter(indicator.name == "Tax Revenue") %>%
  select(iso3_code, Year2, value, Capacity, Gap) %>%
  rename(pais = iso3_code,
         anio = Year2,
         impuestos = value,
         capacidad = Capacity,
         gap = Gap)

# save(ingresos_tributarios_wb_tot, file = "bases/ingresos_tributarios_wb_tot.RData")

#Gini (CEPAL)

gini <- call.data(id.indicator = 3289, language.en = F)

gini_hog <- gini %>% 
  filter(iso3 %in% c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU",
                     "SLV", "GTM", "HND", "MEX", "NIC", "PAN", "PRY", "PER",
                     "URY", "VEN"),
         (`Área geográfica` == "Nacional" & `País` != "Argentina") | 
           (`Área geográfica` == "Urbana" & `País` == "Argentina"),
         Años >= 2010)

gini_hog <- gini_hog %>%
  select(iso3, Años, value) %>% 
  rename(pais = iso3, 
         anio = Años, 
         gini = value)


#Deciles de ingresos (CEPAL)

deciles <- call.data(id.indicator = 3288, language.en = F)
deciles_hog <- deciles %>% 
  filter(iso3 %in% c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU",
                     "SLV", "GTM", "HND", "MEX", "NIC", "PAN", "PRY", "PER",
                     "URY", "VEN"),
         (`Área geográfica` == "Nacional" & `País` != "Argentina") | 
           (`Área geográfica` == "Urbana" & `País` == "Argentina"),
         Años >= 2010)

deciles_hog <- deciles_hog %>%
  select(iso3, Años, value) %>% 
  rename(pais = iso3, 
         anio = Años, 
         palma = value)



#PIB per cápita (WDI)

# View(WDIsearch('gdp'))

datos_wdi <- WDI(country = c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU",
                "SLV", "GTM", "HND", "MEX", "NIC", "PAN", "PRY", "PER",
                "URY", "VEN"), 
    indicator = c("pib_ppa" = "NY.GDP.PCAP.PP.KD"), 
    start = 2010, 
    end = 2024, extra = FALSE, cache = NULL, latest = NULL, language = "es")

pib <- datos_wdi %>%
  select(iso3c, year, pib_ppa) %>% 
  rename(pais = iso3c, 
         anio = year) 


#Empleo público (OIT)
toc <- get_ilostat_toc(lang = 'es')

dat <- get_ilostat(id = 'EMP_TEMP_SEX_AGE_INS_NB_A', 
                   filters = list(
                     ref_area = c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU",
                                  "SLV", "GTM", "HND", "MEX", "NIC", "PAN", "PRY", "PER",
                                  "URY", "VEN"),
                     sex = 'T', 
                     classif1 = 'AGE_AGGREGATE_TOTAL'))

empleo_publico <- dat %>% 
  filter(classif2 != "INS_SECTOR_PRI", time >= 2010) %>% 
  group_by(ref_area, time) %>%
  summarise(emp_publico = (obs_value[classif2 == "INS_SECTOR_PUB"] / obs_value[classif2 == "INS_SECTOR_TOTAL"])*100)


empleo_publico <- empleo_publico %>%
  rename(pais = ref_area, 
         anio = time) %>% 
  mutate(anio = as.numeric(anio))
  


#Informalidad laboral (OIT)

dat <- get_ilostat(id = 'EMP_NIFL_SEX_RT_A', 
                   filters = list(
                     ref_area = c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU",
                                  "SLV", "GTM", "HND", "MEX", "NIC", "PAN", "PRY", "PER",
                                  "URY", "VEN"),
                     sex = 'T'))

trabajo_informal <- dat %>% 
  filter(time >= 2010) %>% 
  select(ref_area, time, obs_value)

trabajo_informal <- trabajo_informal %>%
  rename(pais = ref_area, 
         anio = time,
         informalidad = obs_value) %>% 
  mutate(anio = as.numeric(anio))


#Ocupaciones medias (OIT-CEPAL)

dat <- get_ilostat(id = 'EMP_TEMP_SEX_OC2_NB_A', 
                   filters = list(
                     ref_area = c("ARG", "BOL", "BRA", "CHL", "COL", "DOM", "ECU",
                                  "SLV", "GTM", "HND", "MEX", "NIC",
                                  "URY", "VEN"),
                     sex = 'T'))

sectores_medios <- dat %>%
  filter(time >= 2010) %>%
  filter(!str_detect(classif1, "TOTAL")) %>% 
  group_by(ref_area, time) %>%
  mutate(
    total_ocup = sum(obs_value, na.rm = T),
    ocup_pct = (obs_value / total_ocup)*100,
    ocupaciones_medias = case_when((ref_area == "BRA" & time == 2011) |
                                     (ref_area == "DOM" & time %in% c(2010:2014)) |
                                     (ref_area == "ECU" & time %in% c(2010:2012)) |
                                     (ref_area == "HND" & time %in% c(2010:2014)) |
                                     (ref_area == "NIC" & time %in% c(2010,2012, 
                                                                  2014)) |
                                     (ref_area == "SLV" & time %in% c(2010:2012)) |
                                     (ref_area == "URY" & time == 2010) ~ 
                                     sum(ocup_pct[classif1 %in% c("OC2_ISCO88_21", 
                                                                  "OC2_ISCO88_22", 
                                                                  "OC2_ISCO88_23", 
                                                                  "OC2_ISCO88_24", 
                                                                  "OC2_ISCO88_31", 
                                                                  "OC2_ISCO88_32",
                                                                  "OC2_ISCO88_33",
                                                                  "OC2_ISCO88_34",
                                                                  "OC2_ISCO88_41",
                                                                  "OC2_ISCO88_42")], na.rm = T),
                                   TRUE ~ sum(ocup_pct[classif1 %in% c("OC2_ISCO08_21", 
                                                                       "OC2_ISCO08_22", 
                                                                       "OC2_ISCO08_23", 
                                                                       "OC2_ISCO08_24", 
                                                                       "OC2_ISCO08_25", 
                                                                       "OC2_ISCO08_26", 
                                                                       "OC2_ISCO08_31", 
                                                                       "OC2_ISCO08_32",
                                                                       "OC2_ISCO08_33",
                                                                       "OC2_ISCO08_34",
                                                                       "OC2_ISCO08_35",
                                                                       "OC2_ISCO08_41",
                                                                       "OC2_ISCO08_42",
                                                                       "OC2_ISCO08_43",
                                                                       "OC2_ISCO08_44")], na.rm = T)))


sectores_medios <- sectores_medios %>%
  rename(pais = ref_area, 
         anio = time) %>% 
  mutate(anio = as.numeric(anio)) %>%
  summarise(ocupaciones_medias = mean(ocupaciones_medias, na.rm = T)) %>% 
  ungroup()

#datos de cepal para paraguay, costa rica, panama y peru
ocupacion_cepal <- call.data(id.indicator = 755, language.en = F)
ocupacion_hog <- ocupacion_cepal %>% 
  filter(iso3 %in% c("PRY", "CRI", "PAN", "PER"),
         Sexo == "Ambos sexos",
         `Grupos ocupacionales CIUO 88` %in% c("2.Profesionales, científicos e intelectuales", "3.Técnicos y profesionales de nivel medio", "4.Empleados de oficina"),
         Años >= 2010)

ocupacion_hog <- ocupacion_hog %>%
  select(iso3, Años, value) %>% 
  rename(pais = iso3, 
         anio = Años) 

ocupacion_hog <- ocupacion_hog %>%
  group_by(pais, anio) %>%
  summarise(ocupaciones_medias = sum(value, na.rm = T)) %>% 
  ungroup()

sectores_medios <- sectores_medios %>% 
  add_row(ocupacion_hog)

#Ocupaciones profesionales (OIT-CEPAL)
dat <- get_ilostat(id = 'EMP_TEMP_SEX_OC2_NB_A', 
                   filters = list(
                     ref_area = c("ARG", "BOL", "BRA", "CHL", "COL", "DOM", "ECU",
                                  "SLV", "GTM", "HND", "MEX", "NIC",
                                  "URY", "VEN"),
                     sex = 'T'))

sectores_profesionales <- dat %>%
  filter(time >= 2010) %>%
  filter(!str_detect(classif1, "TOTAL")) %>% 
  group_by(ref_area, time) %>%
  mutate(
    total_ocup = sum(obs_value, na.rm = T),
    ocup_pct = (obs_value / total_ocup)*100,
    ocupaciones_profesionales = case_when((ref_area == "BRA" & time == 2011) |
                                     (ref_area == "DOM" & time %in% c(2010:2014)) |
                                     (ref_area == "ECU" & time %in% c(2010:2012)) |
                                     (ref_area == "HND" & time %in% c(2010:2014)) |
                                     (ref_area == "NIC" & time %in% c(2010,2012, 
                                                                      2014)) |
                                     (ref_area == "SLV" & time %in% c(2010:2012)) |
                                     (ref_area == "URY" & time == 2010) ~ 
                                     sum(ocup_pct[classif1 %in% c("OC2_ISCO88_21", 
                                                                  "OC2_ISCO88_22", 
                                                                  "OC2_ISCO88_23", 
                                                                  "OC2_ISCO88_24")], na.rm = T),
                                   TRUE ~ sum(ocup_pct[classif1 %in% c("OC2_ISCO08_21", 
                                                                       "OC2_ISCO08_22", 
                                                                       "OC2_ISCO08_23", 
                                                                       "OC2_ISCO08_24", 
                                                                       "OC2_ISCO08_25", 
                                                                       "OC2_ISCO08_26")], na.rm = T)))


sectores_profesionales <- sectores_profesionales %>%
  rename(pais = ref_area, 
         anio = time) %>% 
  mutate(anio = as.numeric(anio)) %>%
  summarise(ocupaciones_profesionales = mean(ocupaciones_profesionales, na.rm = T)) %>% 
  ungroup()

#datos de cepal para paraguay, costa rica, panama y peru
ocupacion_cepal <- call.data(id.indicator = 755, language.en = F)
ocupacion_hog <- ocupacion_cepal %>% 
  filter(iso3 %in% c("PRY", "CRI", "PAN", "PER"),
         Sexo == "Ambos sexos",
         `Grupos ocupacionales CIUO 88` %in% c("2.Profesionales, científicos e intelectuales"),
         Años >= 2010)

ocupacion_hog <- ocupacion_hog %>%
  select(iso3, Años, value) %>% 
  rename(pais = iso3, 
         anio = Años) 

ocupacion_hog <- ocupacion_hog %>%
  group_by(pais, anio) %>%
  summarise(ocupaciones_profesionales = sum(value, na.rm = T)) %>% 
  ungroup()

sectores_profesionales <- sectores_profesionales %>% 
  add_row(ocupacion_hog)


#Ocupaciones cuentapropia (CEPAL)

cuentapropia <- call.data(id.indicator = 136, language.en = F)

cuentapropia <- cuentapropia %>% 
  filter(iso3 %in% c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU",
                     "SLV", "GTM", "HND", "MEX", "NIC", "PAN", "PRY", "PER",
                     "URY", "VEN"),
         (`Área geográfica` == "Nacional" & `País` != "Argentina") | 
           (`Área geográfica` == "Urbana" & `País` == "Argentina"),
         Años >= 2010,
         Sexo == "Ambos sexos",
         `Categoría ocupacional` == "Cuenta propia") %>% 
  select(iso3, Años, value) %>% 
  rename(pais = iso3, 
         anio = Años, 
         cuentapropia = value)



#Unión bases agregadas--------------

base_agregadas <- pib %>% 
  left_join(ingresos_tributarios, by = c("pais", "anio")) %>% 
  left_join(ingresos_tributarios_wb_tot, by = c("pais", "anio")) %>%
  left_join(gini_hog, by = c("pais", "anio")) %>% 
  left_join(deciles_hog, by = c("pais", "anio")) %>% 
  left_join(empleo_publico, by = c("pais", "anio")) %>% 
  left_join(trabajo_informal, by = c("pais", "anio")) %>% 
  left_join(sectores_medios, by = c("pais", "anio")) %>% 
  left_join(sectores_profesionales, by = c("pais", "anio")) %>% 
  left_join(cuentapropia, by = c("pais", "anio"))


prueba <- base_agregadas %>% 
  filter(anio == 2010 | anio == 2020)


#Arreglo datos faltantes-------------
datos_paises <- base_agregadas %>%
  group_by(pais) %>%       
  arrange(pais, anio) %>%        
  mutate(across(
    starts_with("impuestos"),
    ~ ifelse(anio == 2023 & is.na(.x), lag(.x), .x))) %>%  
  fill(gini, .direction = "downup") %>%
  fill(palma, .direction = "downup") %>%
  fill(emp_publico, .direction = "downup") %>%
  fill(informalidad, .direction = "downup") %>%
  fill(ocupaciones_medias, .direction = "downup") %>%
  fill(ocupaciones_profesionales, .direction = "downup") %>%
  fill(cuentapropia, .direction = "downup") %>%
  ungroup() %>% 
  rename(iso3 = pais)




# Guardar la base de datos--------------
save(datos_paises, file = "bases/base_datos_agregados.RData")
