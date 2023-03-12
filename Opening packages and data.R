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

rm(list=ls()) 

setwd("C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/Trump China Trade War/")

# Loading LFS files 
LFS <- list.files(pattern = "LFS(.*)dta$")
LFS <- lapply(LFS, read_dta) 

LFS_2015 <- LFS[[1]]
LFS_2016 <- LFS[[2]]
LFS_2017 <- LFS[[3]]
LFS_2018 <- LFS[[4]]
LFS_2019 <- LFS[[5]]
LFS_2020 <- LFS[[6]]

# US tariffs 
us_tariffs <- read_dta(file = "us_import_tariffs.dta")

us_chn_tariff <- read.csv("us_chn_tariff.csv")

# ISCO (occupation) SKILL LEVEL FILE 
isco <- read_dta(file = "ISCO.dta")

# Concordance 
i4_i3 <- read_dta(file = "Concordance_I4_to_I3.dta")
hs_i3 <- read_dta(file = "Concordance_HS_to_I3.dta")
