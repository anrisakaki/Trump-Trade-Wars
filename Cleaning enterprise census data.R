###################################
# CLEANING ENTERPRISE CENSUS DATA # 
###################################

concord16 <- DN_2016 %>%
  select(nganh_kd) %>%
  rename(nkd16 = nganh_kd) %>%
  distinct() %>% 
  mutate(vsic07 = as.numeric(nkd16)) %>% 
  filter(!(nkd16 %in% c(1461, 2101, 8101, 8990, 1450)))

concorda <- left_join(vsic0793, concord16, by = "vsic07")

concordb <- merge(vsic1893, vsic1807, by = "vsic2018")
concordc <- vsic0793 %>% 
  mutate(vsic07 = ifelse(vsic07 < 10101, str_sub(as.character(vsic07), start = 1, end = 3),
                         str_sub(as.character(vsic07), start = 1, end = 4)))

concord1893 <- merge(concordb, concordc, by = "vsic07") %>% 
  select(vsic2018, vsic93) %>% distinct()

dn14 <- DN_2014 %>% 
  rename(vsic07 = nganh_kd,
         n_workers = tsld,
         n_fworkers = tsldnu,
         n_workers_eoy = ld11,
         n_fworkers_eoy = ld12,
         n_informal = ld31,
         n_finformal = ld32,
         export_value = tgxk_tt, 
         wage = tn1,
         net_turnover = kqkd5,
         net_turnover1 = kqkdc,
         pretax_profit = kqkd22) %>% 
  mutate(year = 2014) %>% 
  select("year", "vsic07", "tinh", "huyen", "xa", "ma_thue", "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit")

dn14 <- merge(dn14, concorda, by = "vsic07") %>% 
  select(-"nkd16")

dn15 <- DN_2015 %>% 
  rename(vsic07 = nganh_kd,
         n_workers = tsld,
         n_fworkers = tsldnu,
         n_workers_eoy = ld11,
         n_fworkers_eoy = ld12,
         n_informal = ld31,
         n_finformal = ld32,
         wage = tn1,
         export_value = tgxk_tt, 
         net_turnover = kqkd5,
         net_turnover1 = kqkdc,         
         pretax_profit = kqkd20) %>% 
  mutate(year = 2015) %>% 
  select("year", "vsic07", "tinh", "huyen", "xa", "ma_thue", "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit")

dn15 <- merge(dn15, concorda, by = "vsic07") %>% 
  select(-"vsic07")

dn_16fdi <- DN_2016_fdi %>% 
  mutate(FDI_oc = nvpd1,
         FDI_share = vpdn11/ vpd11) %>% 
  select(ma_thue, ma_thue2, FDI_oc, FDI_share)

DN_2016 <- left_join(DN_2016, dn_16fdi, by = c("ma_thue", "ma_thue2"))

dn16 <- DN_2016 %>% 
  rename(nkd16 = nganh_kd,
         n_workers = tsld,
         n_fworkers = tsldnu,
         n_workers_eoy = ld11,
         n_fworkers_eoy = ld21,
         n_informal = ld41,
         wage = tn1,
         net_turnover = kqkd5,
         net_turnover1 = kqkdc,         
         pretax_profit = kqkd20) %>% 
  mutate(year = 2016) %>% 
  select("year", "nkd16", "tinh", "huyen", "xa", "ma_thue", "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit", "FDI_share", "FDI_oc")

dn16 <- merge(dn16, concorda, by = "nkd16") %>% 
  select(-"vsic07")

dn17 <- DN_2017 %>% 
  rename(nkd16 = nganh_kd,
         n_workers = tsld,
         n_fworkers = tsldnu,
         n_workers_eoy = ld11,
         n_fworkers_eoy = ld21,
         n_informal = ld41,
         wage = tn1,
         net_turnover = kqkd3,
         net_turnover1 = kqkdc,
         FDI_share = vpd63/vpd13,
         FDI_oc = nvpd1,
         pretax_profit = kqkd7) %>% 
  mutate(year = 2017) %>% 
  select("year", "nkd16", "tinh", "huyen", "xa", "ma_thue", "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit", "FDI_share", "FDI_oc")

dn17 <- merge(dn17, concorda, by = "nkd16") %>% 
  select(-"vsic07")

dn18 <- DN_2018 %>% 
  rename(nkd16 = nganh_kd,
         n_workers = tsld,
         n_fworkers = tsldnu,
         n_workers_eoy = ld11,
         n_fworkers_eoy = ld21,
         n_informal = ld41,
         wage = tn1,
         net_turnover = kqkd3,
         net_turnover1 = kqkdc,
         FDI_share = vpd63/vpd13,
         FDI_oc = nvpd1,         
         pretax_profit = kqkd7) %>% 
  mutate(year = 2018) %>% 
  select("year", "nkd16", "tinh", "huyen", "xa", "ma_thue", "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit", "FDI_share", "FDI_oc")

dn18 <- merge(dn18, concorda, by = "nkd16") %>% 
  select(-"vsic07")

dn19_fdi <- DN_2019_fdi %>% 
  rename(FDI_oc = manuoc) %>% 
  select(masothue, FDI_oc)

DN_2019 <- bind_rows(DN_2019, DN_2019a)

DN_2019 <- left_join(DN_2019, dn19_fdi, by = "masothue", multiple = "all")

dn19 <- DN_2019 %>% 
  rename(tinh = matinh_d,
         huyen = mahuyen,
         xa = maxa_die,
         ma_thue = masothue,
         lhdn = loaihinh,
         n_workers = solaodon,
         n_fworkers = v23_a,
         n_workers_eoy = v24_a,
         n_fworkers_eoy = v25_a,
         n_informal = v27_a,
         wage = v30_a,
         net_turnover = doanhthu,
         pretax_profit = loinhuan,
         FDI_share = v63_a/vondieul) %>% 
  mutate(year = 2019,
         vsic2018 = as.numeric(manganhc)) %>% 
  select("year", "vsic2018", "tinh", "huyen", "xa", "ma_thue", "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit", "FDI_share", "FDI_oc")

dn19 <- merge(dn19, concord1893, by = "vsic2018") %>%
  select(-"vsic2018")

dn1419a <- c("dn14", "dn15", "dn16", "dn17", "dn18", "dn19")

for(i in dn1419a){
  
  assign(i, get(i) %>%
           mutate(vsic93 = sprintf("%04d", vsic93),
                  isic3 = case_when(
                    vsic93 == "0112" ~ "0111",
                    vsic93 == "0113" ~ "0111",
                    vsic93 == "0114" ~ NA_character_,
                    vsic93 == "0115" ~ "0113",
                    vsic93 == "0116" ~ "0112",
                    vsic93 == "0117" ~ "0111",
                    vsic93 == "0123" ~ "0122",
                    vsic93 == "9011" ~ "9211",
                    vsic93 == "9014" ~ "9214",
                    TRUE ~ vsic93
                  )))
  
}

dn1419 <- bind_rows(dn14, dn15, dn16, dn17, dn18, dn19) %>% 
  group_by(tinh, huyen, xa, ma_thue) %>% 
  mutate(id = cur_group_id()) %>% 
  mutate(female_share = n_fworkers/n_workers,
         female_share_eoy = n_fworkers_eoy/n_workers_eoy,
         FDI = ifelse(lhdn > 10, 1, 0)) %>% 
  select(-c("nkd16", "vsic07", "vsic93")) %>% 
  distinct() %>% 
  select(id, year, isic3, everything())

write_dta(dn1419, "dn1419.dta")
save(dn1419, file = "dn1419.rda")