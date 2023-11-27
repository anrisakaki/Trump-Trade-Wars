library(tidyverse)
library(haven)
library(sf)
library(fixest)
library(modelsummary)
library(bookdown)
library(ggeffects)
library(xtable)
library(concordance)
library(did)
library(staggered)

rm(list=ls()) 

here <- here::here

setwd("C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/Trump China Trade War/")

here() # check that gives correct path

# Loading LFS files 
LFS <- list.files(pattern = "LFS(.*)dta$")
LFS <- lapply(LFS, read_dta) 

LFS_2015 <- LFS[[1]]
LFS_2016 <- LFS[[2]]
LFS_2017 <- LFS[[3]]
LFS_2018 <- LFS[[4]]
LFS_2019 <- LFS[[5]]
LFS_2020 <- LFS[[6]]

# Loading Enterprise Census files 

DN <- list.files(pattern = "dn(.*)dta$")
DN <- lapply(DN, read_dta)

DN_2014 <- DN[[2]]
DN_2015 <- DN[[3]]
DN_2016 <- DN[[4]]
DN_2016_fdi <- DN[[5]]
DN_2017 <- DN[[6]]
DN_2018 <- DN[[7]]
DN_2019 <- DN[[8]]
DN_2019a <- DN[[9]]
DN_2019_fdi <- DN[[10]]

# Loading export data files 

HH <- list.files(pattern = "hh(.*)dta$")
HH <- lapply(HH, read_dta)

HH_2016 <- HH[[1]]
HH_2017 <- HH[[2]]
HH_2018 <- HH[[3]]
HH_2019 <- HH[[4]]

GC <- list.files(pattern = "gc(.*)dta$")
GC <- lapply(GC, read_dta)

GC_2016 <- GC[[1]]
GC_2017 <- GC[[2]]
GC_2018 <- GC[[3]]
GC_2019 <- GC[[4]]

TL <- list.files(pattern = "tl(.*)dta$")
TL <- lapply(TL, read_dta)

TL_2017 <- TL[[1]]
TL_2018 <- TL[[2]]

# Product data 

SP <- list.files(pattern = "sp(.*)dta$")
SP <- lapply(SP, read_dta)

SP_2014 <- SP[[4]]
SP_2015 <- SP[[5]]
SP_2016 <- SP[[6]]
SP_2017 <- SP[[7]]
SP_2018 <- SP[[8]]
SP_2019 <- SP[[9]]

hs10_trump <- read_dta(file = "did10dig_year.dta")

# US tariffs 
us_tariffs <- read_dta(file = "us_import_tariffs.dta")

us_chn_tariff <- read.csv("us_chn_tariff.csv")

trump_isic <- read_dta(file = "TRUMP_ISIC.dta")

# ISCO (occupation) SKILL LEVEL FILE 
isco <- read_dta(file = "ISCO.dta")

# Concordance 
i4_i3 <- read_dta(file = "Concordance_I4_to_I3.dta")
hs_i3 <- read_dta(file = "Concordance_HS_to_I3.dta")

vsic0793 <- read.csv(file = "VSIC 2007 - VSIC 1993 conversion.csv")
vsic1807 <- read_csv(file = "VSIC 2018 - VSIC 2007 conversion.csv")

vsic93 <- read_csv(file = "VSIC 1993 (English).csv")

masp_hs8 <- read_csv(file = "masp_hs_concord.csv")
