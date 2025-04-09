#Librerías----------------
library(tidyverse)
library(CepalStatR)
library(WDI)
library(Rilostat)

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
         Clasificacion_impuestos == "Total ingresos tributarios (incluyendo contribuciones sociales)",
         Años >= 2010)

ingresos_tributarios <- ingresos_tributarios %>% 
  select(iso3, Años, value) %>% 
  rename(pais = iso3, 
         anio = Años, 
         impuestos = value)

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

View(WDIsearch('gdp'))

datos_wdi <- WDI(country = c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU",
                "SLV", "GTM", "HND", "MEX", "NIC", "PAN", "PRY", "PER",
                "URY", "VEN"), 
    indicator = c("pib_ppa" = "NY.GDP.PCAP.PP.KD"), 
    start = 2010, 
    end = 2023, extra = FALSE, cache = NULL, latest = NULL, language = "es")

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


#Ocupaciones medias (OIT)

dat <- get_ilostat(id = 'EMP_TEMP_SEX_OC2_NB_A', 
                   filters = list(
                     ref_area = c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU",
                                  "SLV", "GTM", "HND", "MEX", "NIC", "PAN", "PRY", "PER",
                                  "URY", "VEN"),
                     sex = 'T'))

sectores_medios <- dat %>%
  filter(time >= 2010) %>%
  filter(!str_detect(classif1, "TOTAL")) %>% 
  group_by(ref_area, time) %>%
  mutate(
    total_ocup = sum(obs_value, na.rm = T),
    ocup_pct = (obs_value / total_ocup)*100,
    ocupaciones_medias = sum(ocup_pct[classif1 %in% c("OC2_ISCO08_21", 
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
                                                      "OC2_ISCO08_44")], na.rm = T))


sectores_medios <- sectores_medios %>%
  rename(pais = ref_area, 
         anio = time) %>% 
  mutate(anio = as.numeric(anio)) %>%
  summarise(ocupaciones_medias = mean(ocupaciones_medias, na.rm = T))


#Unión bases agregadas--------------

base_agregadas <- pib %>% 
  left_join(ingresos_tributarios, by = c("pais", "anio")) %>% 
  left_join(gini_hog, by = c("pais", "anio")) %>% 
  left_join(deciles_hog, by = c("pais", "anio")) %>% 
  left_join(empleo_publico, by = c("pais", "anio")) %>% 
  left_join(trabajo_informal, by = c("pais", "anio")) %>% 
  left_join(sectores_medios, by = c("pais", "anio"))


prueba <- base_agregadas %>% 
  filter(anio == 2010 | anio == 2020)
