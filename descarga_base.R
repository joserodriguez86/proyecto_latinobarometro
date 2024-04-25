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


# Guardar la base de datos
save(latinobarometro, file = "latinobarometro.RData")

# Subir la base de datos a Google Drive
drive_auth()
carpeta <- drive_find("Proyecto Latinobarometro", type = "folder")
drive_upload("latinobarometro.RData", path = carpeta, type = NULL)
