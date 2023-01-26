library(tidyverse)
library(haven)
library(gtsummary)
library(gt)
library(sf)
library(fixest)
library(modelsummary)
library(bookdown)
library(kableExtra)
library(ggeffects)
library(xtable)
library(concordance)

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

# ISCO (occupation) SKILL LEVEL FILE 
isco <- read_dta(file = "ISCO.dta")

# ISIC (sector) FILE WITH TRUMP TARIFFS 
isic <- read_dta(file = "ISIC.dta")
isic_trump <- read_dta(file = "TRUMP_ISIC.dta")

# HS10 data with trade and tariff data 

hs10_trump <- read_dta(file = "did10dig_year.dta")

