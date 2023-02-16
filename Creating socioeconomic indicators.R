LFS_1520 <- c("LFS_2015", "LFS_2016", "LFS_2017", "LFS_2018", "LFS_2019", "LFS_2020")

for (i in LFS_1520){
  if(i %in% c("LFS_2015", "LFS_2017", "LFS_2018")){
    assign(i, get(i) %>%
             mutate(Female = as.numeric(C3 == 2))
    )
  }
  
  if(i %in% c("LFS_2019", "LFS_2020")){
    assign(i, get(i) %>%
             mutate(Female = as.numeric(C03 == 2))
    )
  }
  
  if(i %in% c("LFS_2016")){
    assign(i, get(i) %>%
             mutate(Female = as.numeric(c3 == 2))
    )
  }
}

LFS_2015 <- LFS_2015 %>% 
  mutate(work = as.numeric(C14 == 1),
         urban = as.numeric(TTNT == 1),
         formal = as.numeric(C26 == 1 & work == 1),
         casual_contract = as.numeric(C29 < 4),
         FDI = as.numeric(C24 == 11 & work == 1),
         year = 2015) %>% 
  rename(ISIC = C23,
         ISCO = C22,
         district = HUYEN,
         province = TINH,
         weight = Weight_final_2019,
         educ = C12,
         wage = C40A, 
         month = THANGDT,
         age = C5,
         hours = C42)

LFS_2016 <- LFS_2016 %>% 
  mutate(work = as.numeric(c14 == 1),
         urban = as.numeric(TTNT < 1),
         formal = as.numeric(c26 == 1 & work == 1),
         casual_contract = as.numeric(c29 < 4),
         FDI = as.numeric(c24 == 11 & work == 1),
         year = 2016) %>% 
  rename(ISIC = c23,
         ISCO = c22,
         district = Mahuyen,
         province = TINH,
         weight = Weight_final_2019,
         educ = c12,
         wage = c40, 
         age = c5,
         month = THANGDT,
         wage = c40,
         hours = c42)

LFS_2017 <- LFS_2017 %>% 
  mutate(work = as.numeric(C16 == 1),
         urban = as.numeric(TTNT == 1),
         formal = as.numeric(C28 == 1 & work == 1),
         casual_contract = as.numeric(C31 < 4 & work == 1),
         FDI = as.numeric(C26 == 11 & work == 1),
         year = 2017) %>% 
  rename(ISIC = C25,
         ISCO = C24,
         district = HUYEN,
         province = TINH,
         weight = weight_final_2019,
         educ = C14,
         wage = C39, 
         month = THANGDT,
         age = C5,
         hours = C41A) 

LFS_2018 <- LFS_2018 %>% 
  mutate(work = as.numeric(C21 == 1),
         urban = as.numeric(TTNT == 1),
         formal = as.numeric(C33 == 1 & work == 1),
         casual_contract = as.numeric(C36 < 4),
         FDI = as.numeric(C31 == 11 & work == 1),
         year = 2018) %>% 
  rename(ISIC = C30C,
         ISCO = C29C,
         district = HUYEN,
         province = TINH,
         weight = Weight_final_2019,
         educ = C17,
         wage = C44, 
         month = THANGDT,
         age = C5,
         hours = C46A) 

LFS_2019 <- LFS_2019 %>% 
  mutate(work = as.numeric(C19 == 1),
         urban = as.numeric(TTNT == 1),
         formal = as.numeric(C46 == 1 & work == 1),
         casual_contract = as.numeric(C50A < 4 & work == 1),
         FDI = as.numeric(C45 == 12 & work == 1),
         year = 2019) %>% 
  rename(ISIC = C44C,
         ISCO = C43C,
         district = MAHUYEN,
         province = MATINH,
         weight = Weight_final_2019,
         educ = C17B,
         wage = C70, 
         month = THANGDT,
         age = C05,
         hours = C66) 

LFS_2020 <- LFS_2020 %>% 
  mutate(work = as.numeric(C20 == 1),
         urban = as.numeric(TTNT == 1),
         formal = as.numeric(C47 == 1 & work == 1),
         casual_contract = as.numeric(C50 < 4 & work == 1),
         FDI = as.numeric(C46 == 12 & work == 1),
         year = 2020) %>% 
  rename(ISIC = C45B,
         ISCO = C43B,
         district = MAHUYEN,
         province = MATINH,
         weight = cal_weight_2020,
         educ = C16,
         wage = C72, 
         month = ThangDT,
         age = C05,
         hours = C66) 

for (i in LFS_1520){
  assign(i, get(i) %>% 
           select(province, district, urban, age, educ, work, ISIC, wage, hours, formal, casual_contract, FDI, Female, year, month, weight) %>% 
           mutate(year_month = year*100 + month))
}
