# Carga de bases ----------------------------
rm(list = ls())

pacman::p_load(tidyverse, googledrive)

drive_auth()

library(googledrive)

# Autenticar la sesión de Google Drive
drive_auth()

# Definir el ID de la carpeta
folder_id <- "1YpKKrlu7qTQ4hP-E2Q6hSqvxGNPHmrCi"

# Obtener los archivos dentro de la carpeta
files <- drive_ls(as_id(folder_id))

# Descargar cada archivo
lapply(files$id, function(file_id) {
  drive_download(as_id(file_id), overwrite = TRUE)
})

latinobarometro1998 <- foreign::read.dta("latinobarometro1998.dta", convert.factors = F)
latinobarometro2000 <- foreign::read.dta("latinobarometro2000.dta", convert.factors = F)
latinobarometro2001 <- foreign::read.dta("latinobarometro2001.dta", convert.factors = F)
latinobarometro2002 <- foreign::read.dta("latinobarometro2002.dta", convert.factors = F)
latinobarometro2003 <- foreign::read.dta("latinobarometro2003.dta", convert.factors = F)
latinobarometro2004 <- foreign::read.dta("latinobarometro2004.dta", convert.factors = F)
latinobarometro2005 <- foreign::read.dta("latinobarometro2005.dta", convert.factors = F)
latinobarometro2006 <- foreign::read.dta("latinobarometro2006.dta", convert.factors = F)
latinobarometro2007 <- foreign::read.dta("latinobarometro2007.dta", convert.factors = F)
latinobarometro2008 <- foreign::read.dta("latinobarometro2008.dta", convert.factors = F)
latinobarometro2009 <- foreign::read.dta("latinobarometro2009.dta", convert.factors = F)
latinobarometro2010 <- foreign::read.dta("latinobarometro2010.dta", convert.factors = F)
latinobarometro2011 <- foreign::read.dta("latinobarometro2011.dta", convert.factors = F)
load("latinobarometro2013.rdata")
load("latinobarometro2015.rdata")
load("latinobarometro2016.rdata")
load("latinobarometro2017.rdata")
latinobarometro2018 <- readRDS("Latinobarometro2018.rds")
load("Latinobarometro2020.rdata")
latinobarometro2023 <- readRDS("Latinobarometro2023.rds")


unlink(c("latinobarometro1998.dta", "latinobarometro2000.dta", "latinobarometro2001.dta", "latinobarometro2002.dta", "latinobarometro2003.dta", "latinobarometro2004.dta", "latinobarometro2005.dta", "latinobarometro2006.dta", "latinobarometro2007.dta", "latinobarometro2008.dta", "latinobarometro2009.dta", "latinobarometro2010.dta", "latinobarometro2011.dta", "latinobarometro2013.rdata", "latinobarometro2015.rdata", "latinobarometro2016.rdata", "latinobarometro2017.rdata", "latinobarometro2018.rds", "latinobarometro2020.rds", "latinobarometro2023.rds"))
