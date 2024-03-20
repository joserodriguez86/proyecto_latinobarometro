# Carga de bases ----------------------------
rm(list = ls())

pacman::p_load(tidyverse, googledrive, haven)

# Autenticar la sesión de Google Drive
drive_deauth()

# Definir el ID de la carpeta
folder_id <- "1YpKKrlu7qTQ4hP-E2Q6hSqvxGNPHmrCi"

# Obtener los archivos dentro de la carpeta
files <- drive_ls(as_id(folder_id))

# Descargar cada archivo
lapply(files$id, function(file_id) {
  drive_download(as_id(file_id), overwrite = TRUE)
})

latinobarometro1998 <- read_dta("latinobarometro1998.dta")
latinobarometro2000 <- read_dta("latinobarometro2000.dta")
latinobarometro2001 <- read_dta("latinobarometro2001.dta")
latinobarometro2002 <- read_dta("latinobarometro2002.dta")
latinobarometro2003 <- read_dta("latinobarometro2003.dta")
latinobarometro2004 <- read_dta("latinobarometro2004.dta")
latinobarometro2005 <- read_dta("latinobarometro2005.dta")
latinobarometro2006 <- read_dta("latinobarometro2006.dta")
latinobarometro2007 <- read_dta("latinobarometro2007.dta")
latinobarometro2008 <- read_dta("latinobarometro2008.dta")
latinobarometro2009 <- read_dta("latinobarometro2009.dta")
latinobarometro2010 <- read_dta("latinobarometro2010.dta")
latinobarometro2011 <- read_dta("latinobarometro2011.dta")
latinobarometro2013 <- read_dta("latinobarometro2013.dta")
latinobarometro2015 <- read_dta("latinobarometro2015.dta")
latinobarometro2016 <- read_dta("latinobarometro2016.dta")
latinobarometro2017 <- read_dta("latinobarometro2017.dta")
latinobarometro2018 <- read_dta("latinobarometro2018.dta")
latinobarometro2020 <- read_dta("latinobarometro2020.dta")
latinobarometro2023 <- read_dta("latinobarometro2023.dta")

latinobarometro1998 <- zap_labels(latinobarometro1998)
latinobarometro2000 <- zap_labels(latinobarometro2000)
latinobarometro2001 <- zap_labels(latinobarometro2001)
latinobarometro2002 <- zap_labels(latinobarometro2002)
latinobarometro2003 <- zap_labels(latinobarometro2003)
latinobarometro2004 <- zap_labels(latinobarometro2004)
latinobarometro2005 <- zap_labels(latinobarometro2005)
latinobarometro2006 <- zap_labels(latinobarometro2006)
latinobarometro2007 <- zap_labels(latinobarometro2007)
latinobarometro2008 <- zap_labels(latinobarometro2008)
latinobarometro2009 <- zap_labels(latinobarometro2009)
latinobarometro2010 <- zap_labels(latinobarometro2010)
latinobarometro2011 <- zap_labels(latinobarometro2011)
latinobarometro2013 <- zap_labels(latinobarometro2013)
latinobarometro2015 <- zap_labels(latinobarometro2015)
latinobarometro2016 <- zap_labels(latinobarometro2016)
latinobarometro2017 <- zap_labels(latinobarometro2017)
latinobarometro2018 <- zap_labels(latinobarometro2018)
latinobarometro2020 <- zap_labels(latinobarometro2020)
latinobarometro2023 <- zap_labels(latinobarometro2023)






unlink(c("latinobarometro1998.dta", "latinobarometro2000.dta", "latinobarometro2001.dta", "latinobarometro2002.dta", "latinobarometro2003.dta", "latinobarometro2004.dta", "latinobarometro2005.dta", "latinobarometro2006.dta", "latinobarometro2007.dta", "latinobarometro2008.dta", "latinobarometro2009.dta", "latinobarometro2010.dta", "latinobarometro2011.dta", "latinobarometro2013.dta", "latinobarometro2015.dta", "latinobarometro2016.dta", "latinobarometro2017.dta", "latinobarometro2018.dta", "Latinobarometro2020.dta", "Latinobarometro2023.dta"))


#Pegado de bases ----------------
latinobarometro <- latinobarometro1998 %>% 
  bind_rows(latinobarometro2000, latinobarometro2001, latinobarometro2002, latinobarometro2003, latinobarometro2004, latinobarometro2005, latinobarometro2006, latinobarometro2007, latinobarometro2008, latinobarometro2009, latinobarometro2010, latinobarometro2011, latinobarometro2013, latinobarometro2015, latinobarometro2016, latinobarometro2017, latinobarometro2018, latinobarometro2020, latinobarometro2023)

rm(latinobarometro1998, latinobarometro2000, latinobarometro2001, latinobarometro2002, latinobarometro2003, latinobarometro2004, latinobarometro2005, latinobarometro2006, latinobarometro2007, latinobarometro2008, latinobarometro2009, latinobarometro2010, latinobarometro2011, latinobarometro2013, latinobarometro2015, latinobarometro2016, latinobarometro2017, latinobarometro2018, latinobarometro2020, latinobarometro2023)


#Construcción de variables---------
latinobarometro <- latinobarometro %>% 
  mutate(anio = case_when(numinves == 16 ~ 2011,
                          numinves == 17 ~ 2013,
                          numinves == 18 ~ 2015,
                          numinves == 23 ~ 2023,
                          NUMINVES == 2018 ~ 2018,
                          TRUE ~ numinves),
         pais = ifelse(is.na(idenpa), IDENPA, idenpa),
         pais_f = factor(latinobarometro$idenpa, labels = 
                           c("Arg", "Bol", "Bra", "Chi", "Col", "Cri", "Dom", "Ecu", 
                             "Slv", "Gua", "Hon", "Mex", "Nic", "Pan", "Par", "Per",
                             "Esp", "Uru", "Ven")),
         entrevista = ifelse(anio == 2018, NUMENTRE, numentre),
         wt = case_when(anio == 1998 ~ pondera,
                        anio == 2018 ~ WT,
                        TRUE ~ wt))



latinobarometro <- latinobarometro %>% 
  mutate(democracia = case_when(anio == 1998 ~ sp28,
                                anio == 2000 ~ P29ST,
                                anio == 2001 ~ p46st,
                                anio == 2002 ~ p32st,
                                anio == 2003 ~ p14st,
                                anio == 2004 ~ p13st,
                                anio == 2005 ~ p16st,
                                anio == 2006 ~ p17st,
                                anio == 2007 ~ p9st,
                                anio == 2008 ~ p13st,
                                anio == 2009 ~ p10st,
                                anio == 2010 ~ P10ST,
                                anio == 2011 ~ P13ST,
                                anio == 2013 ~ P12STGBS,
                                anio == 2015 ~ P11STGBS,
                                anio == 2016 ~ P8STGBS,
                                anio == 2017 ~ P8STGBS,
                                anio == 2018 ~ P12STGBS,
                                anio == 2020 ~ p10stgbs,
                                anio == 2023 ~ P10STGBS),
         satisf_democracia = case_when(anio == 1998 ~ sp29,
                                       anio == 2000 ~ P30ST,
                                       anio == 2001 ~ p45st,
                                       anio == 2002 ~ p33st,
                                       anio == 2003 ~ p15st,
                                       anio == 2004 ~ p14st,
                                       anio == 2005 ~ p18st,
                                       anio == 2006 ~ p21st,
                                       anio == 2007 ~ p12st,
                                       anio == 2008 ~ p22st_a,
                                       anio == 2009 ~ p12st_a,
                                       anio == 2010 ~ P11ST_A,
                                       anio == 2011 ~ P14ST_A,
                                       anio == 2013 ~ P13TGB_A,
                                       anio == 2015 ~ P12TG_A,
                                       anio == 2016 ~ P9STGBSA,
                                       anio == 2017 ~ P9STGBSC_A,
                                       anio == 2018 ~ P13STGBS.A,
                                       anio == 2020 ~ P11STGBS_A,
                                       anio == 2023 ~ P11STGBS_A),
         garantia_politica = case_when(anio == 2007 ~ p18na,
                                       anio == 2008 ~ p26st_a,
                                       anio == 2009 ~ p15st_a,
                                       anio == 2011 ~ P41ST_A,
                                       anio == 2015 ~ P50ST_A,
                                       anio == 2020 ~ p47st_a,
                                       anio == 2023 ~ P41ST_A,
                                       TRUE ~ NA_real_),
         garantia_profesion = case_when(anio == 2007 ~ p18nb,
                                     anio == 2008 ~ p26st_b,
                                     anio == 2009 ~ p15st_b,
                                     anio == 2011 ~ P41ST_B,
                                     anio == 2015 ~ P50ST_B,
                                     anio == 2020 ~ p47st_b,
                                     anio == 2023 ~ P41ST_B,
                                     TRUE ~ NA_real_),
         garantia_medioambiente = case_when(anio == 2007 ~ p18nc,
                                              anio == 2008 ~ p26st_c,
                                              anio == 2009 ~ p15st_c,
                                              anio == 2011 ~ P41ST_C,
                                              anio == 2015 ~ P50ST_C,
                                              anio == 2020 ~ p47st_c,
                                              anio == 2023 ~ P41ST_C,
                                              TRUE ~ NA_real_),
         garantia_propprivada = case_when(anio == 2007 ~ p18nd,
                                          anio == 2008 ~ p26st_d,
                                          anio == 2009 ~ p15st_d,
                                          anio == 2011 ~ P41ST_D,
                                          anio == 2015 ~ P50ST_D,
                                          anio == 2020 ~ p47st_d,
                                          anio == 2023 ~ P41ST_D,
                                          TRUE ~ NA_real_),
         garantia_distriqueza = case_when(anio == 2007 ~ p18ne,
                                          anio == 2008 ~ p26st_e,
                                          anio == 2009 ~ p15st_e,
                                          anio == 2011 ~ P41ST_E,
                                          anio == 2015 ~ P50ST_E,
                                          anio == 2020 ~ p47st_e,
                                          anio == 2023 ~ P41ST_E,
                                          TRUE ~ NA_real_),
         garantia_genero = case_when(anio == 2007 ~ p18nf,
                                     anio == 2008 ~ p26st_f,
                                     anio == 2009 ~ p15st_f,
                                     anio == 2011 ~ P41ST_F,
                                     anio == 2015 ~ P50ST_F,
                                     anio == 2020 ~ p47st_f,
                                     anio == 2023 ~ P41ST_F,
                                     TRUE ~ NA_real_),
         garantia_oportunidades = case_when(anio == 2007 ~ p18ng,
                                            anio == 2008 ~ p26st_g,
                                            anio == 2009 ~ p15st_g,
                                            anio == 2011 ~ P41ST_G,
                                            anio == 2015 ~ P50ST_G,
                                            anio == 2020 ~ p47st_g,
                                            anio == 2023 ~ P41ST_G,
                                            TRUE ~ NA_real_),
         garantia_expresion = case_when(anio == 2007 ~ p18nh,
                                        anio == 2008 ~ p26st_h,
                                        anio == 2009 ~ p15st_h,
                                        anio == 2011 ~ P41ST_H,
                                        anio == 2015 ~ P50ST_H,
                                        anio == 2020 ~ p47st_h,
                                        anio == 2023 ~ P41ST_H,
                                        TRUE ~ NA_real_),
         garantia_religion = case_when(anio == 2007 ~ p18ni,
                                       anio == 2008 ~ p26st_i,
                                       anio == 2009 ~ p15st_i,
                                       anio == 2011 ~ P41ST_I,
                                       anio == 2015 ~ P50ST_I,
                                       anio == 2020 ~ p47st_i,
                                       anio == 2023 ~ P41ST_I,
                                       TRUE ~ NA_real_),
         garantia_crimen = case_when(anio == 2007 ~ p18nj,
                                     anio == 2008 ~ p26st_j,
                                     anio == 2009 ~ p15st_j,
                                     anio == 2011 ~ P41ST_J,
                                     anio == 2015 ~ P50ST_J,
                                     anio == 2020 ~ p47st_j,
                                     anio == 2023 ~ P41ST_J,
                                     TRUE ~ NA_real_),
         garantia_seguridadsocial = case_when(anio == 2007 ~ p18nk,
                                              anio == 2008 ~ p26st_k,
                                              anio == 2009 ~ p15st_k,
                                              anio == 2011 ~ P41ST_K,
                                              anio == 2015 ~ P50ST_K,
                                              anio == 2020 ~ p47st_k,
                                              anio == 2023 ~ P41ST_K,
                                              TRUE ~ NA_real_),
         garantia_solidariad = case_when(anio == 2007 ~ p18nl,
                                         anio == 2008 ~ p26st_l,
                                         anio == 2009 ~ p15st_l,
                                         anio == 2011 ~ P41ST_L,
                                         anio == 2015 ~ P50ST_L,
                                         anio == 2020 ~ p47st_l,
                                         anio == 2023 ~ P41ST_L,
                                         TRUE ~ NA_real_),
         garantia_trabajo = case_when(anio == 2007 ~ p18nm,
                                      anio == 2008 ~ p26st_m,
                                      anio == 2009 ~ p15st_m,
                                      anio == 2011 ~ P41ST_M,
                                      anio == 2015 ~ P50ST_M,
                                      anio == 2020 ~ p47st_m,
                                      anio == 2023 ~ P41ST_M,
                                      TRUE ~ NA_real_),
         ideologia = case_when(anio == 1998 ~ sp52,
                               anio == 2000 ~ P52ST,
                               anio == 2001 ~ p54st,
                               anio == 2002 ~ p64st,
                               anio == 2003 ~ p60st,
                               anio == 2004 ~ p87st,
                               anio == 2005 ~ p34st,
                               anio == 2006 ~ p47st,
                               anio == 2007 ~ p67st,
                               anio == 2008 ~ p56st,
                               anio == 2009 ~ p69st,
                               anio == 2010 ~ P60ST,
                               anio == 2011 ~ P76ST,
                               anio == 2013 ~ P41ST,
                               anio == 2015 ~ P27ST,
                               anio == 2016 ~ P17ST,
                               anio == 2017 ~ P19STC,
                               anio == 2018 ~ P22ST,
                               anio == 2020 ~ p18st,
                               anio == 2023 ~ P16ST))
