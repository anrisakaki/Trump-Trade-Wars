######################################
# SUMMARISING SECTOR-LEVEL VARIABLES #
######################################
# 2017
female_sector_17 <- LFS_2017 %>% 
  filter(work == 1) %>% 
  select(ISIC, Female, FDI, weight, month) %>% 
  group_by(ISIC, month) %>% 
  count(Female, wt = weight) %>% 
  filter(Female == 1) %>% 
  select(-c(Female)) %>% 
  rename(n_female = n)

lfp_sector_17 <- LFS_2017 %>% 
  filter(work == 1) %>% 
  select(ISIC, month, work, weight) %>% 
  group_by(ISIC, month) %>% 
  count(work, wt = weight) %>% 
  select(-c(work)) %>% 
  rename(n_workers = n)

formal_sector_17 <- LFS_2017 %>% 
  filter(work == 1) %>% 
  select(ISIC, month, formal, weight) %>% 
  group_by(ISIC, month) %>% 
  count(formal, wt = weight) %>% 
  filter(formal == 1) %>% 
  select(-c(formal)) %>% 
  rename(n_formal = n)

sector_17 <- list(female_sector_17, lfp_sector_17, formal_sector_17) %>% 
  reduce(full_join, by = c("ISIC", "month")) %>% 
  mutate(female_ratio = n_female/n_workers,
         formal_ratio = n_formal/n_workers,
         year = 2017,
         year_month = year*100 + month)

# 2018 
female_sector_18 <- LFS_2018 %>% 
  filter(work == 1) %>% 
  select(ISIC, Female, FDI, weight, month) %>% 
  group_by(ISIC, month) %>% 
  count(Female, wt = weight) %>% 
  filter(Female == 1) %>% 
  select(-c(Female)) %>% 
  rename(n_female = n)

lfp_sector_18 <- LFS_2018 %>% 
  filter(work == 1) %>% 
  select(ISIC, month, work, weight) %>% 
  group_by(ISIC, month) %>% 
  count(work, wt = weight) %>% 
  select(-c(work)) %>% 
  rename(n_workers = n)

formal_sector_18 <- LFS_2018 %>% 
  filter(work == 1) %>% 
  select(ISIC, month, formal, weight) %>% 
  group_by(ISIC, month) %>% 
  count(formal, wt = weight) %>% 
  filter(formal == 1) %>% 
  select(-c(formal)) %>% 
  rename(n_formal = n)

sector_18 <- list(female_sector_18, lfp_sector_18, formal_sector_18) %>% 
  reduce(full_join, by = c("ISIC", "month")) %>% 
  mutate(female_ratio = n_female/n_workers,
         formal_ratio = n_formal/n_workers,
         year = 2018,
         year_month = year*100 + month)

# 2019 
female_sector_19 <- LFS_2019 %>% 
  filter(work == 1) %>% 
  select(ISIC, Female, FDI, weight, month) %>% 
  group_by(ISIC, month) %>% 
  count(Female, wt = weight) %>% 
  filter(Female == 1) %>% 
  select(-c(Female)) %>% 
  rename(n_female = n)

lfp_sector_19 <- LFS_2019 %>% 
  filter(work == 1) %>% 
  select(ISIC, month, work, weight) %>% 
  group_by(ISIC, month) %>% 
  count(work, wt = weight) %>% 
  select(-c(work)) %>% 
  rename(n_workers = n)

formal_sector_19 <- LFS_2019 %>% 
  filter(work == 1) %>% 
  select(ISIC, month, formal, weight) %>% 
  group_by(ISIC, month) %>% 
  count(formal, wt = weight) %>% 
  filter(formal == 1) %>% 
  select(-c(formal)) %>% 
  rename(n_formal = n)

sector_19 <- list(female_sector_19, lfp_sector_19, formal_sector_19) %>% 
  reduce(full_join, by = c("ISIC", "month")) %>% 
  mutate(female_ratio = n_female/n_workers,
         formal_ratio = n_formal/n_workers,
         year = 2019,
         year_month = year*100 + month)
