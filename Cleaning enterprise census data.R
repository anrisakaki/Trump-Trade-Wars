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
         pretax_profit = kqkd22,
         export = co_xk,
         exp_value = tgxk_tt) %>% 
  mutate(year = 2014) %>% 
  select("year", "vsic07", "tinh", "huyen", "xa", "ma_thue", "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit", export, exp_value)

dn14 <- merge(dn14, concorda, by = "vsic07") %>% 
  select(-"nkd16") %>% 
  distinct()

dn15 <- DN_2015 %>% 
  rename(vsic07 = nganh_kd,
         n_workers = tsld,
         n_fworkers = tsldnu,
         n_workers_eoy = ld11,
         n_fworkers_eoy = ld12,
         n_informal = ld31,
         n_finformal = ld32,
         wage = tn1,
         exp_value = tgxk_tt, 
         net_turnover = kqkd5,
         net_turnover1 = kqkdc,         
         pretax_profit = kqkd20) %>% 
  mutate(year = 2015,
         export = ifelse(exp_value > 0, 1, 0)) %>% 
  select("year", "vsic07", "tinh", "huyen", "xa", "ma_thue", "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit", export, exp_value)

dn15 <- merge(dn15, concorda, by = "vsic07") %>% 
  select(-"vsic07") %>% 
  distinct()

dn_16fdi <- DN_2016_fdi %>% 
  mutate(FDI_oc = nvpd1,
         FDI_share = vpdn11/ vpd11) %>% 
  select(ma_thue, ma_thue2, FDI_oc, FDI_share)

DN_2016 <- left_join(DN_2016, dn_16fdi, by = c("ma_thue", "ma_thue2")) %>% distinct()

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
  select("year", "nkd16", "tinh", "huyen", "xa", "ma_thue", ma_thue2, "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
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
  select("year", "nkd16", "tinh", "huyen", "xa", "ma_thue", ma_thue2, "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
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
  select("year", "nkd16", "tinh", "huyen", "xa", "ma_thue", ma_thue2, "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
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

dn19 <- merge(dn19, vsic1893, by = "vsic2018") %>%
  select(-"vsic2018")

####################################################
# CLEANING EXPORT AND INTERMEDIATE PROCESSING DATA #
####################################################

# Export 

exp16 <- GC_2016 %>%
  rename(exp_value = trigia_e42,
         huyen = huyencn) %>%
  mutate(export = ifelse(exp_value > 0, 1, 0)) %>% 
  select("tinh", "huyen", "ma_thue", ma_thue2, "export", "exp_value")

dn16 <- left_join(dn16, exp16, by = c("tinh", "huyen", "ma_thue", "ma_thue2"))

exp17 <- GC_2017 %>% 
  rename(exp_value = trigia_e42,
         huyen = huyencn) %>% 
  mutate(export = ifelse(exp_value > 0, 1, 0)) %>% 
  select(tinh, huyen, ma_thue, ma_thue2, export, exp_value)  

dn17 <- left_join(dn17, exp17, by = c("tinh", "huyen", "ma_thue", "ma_thue2"))

exp18 <- GC_2018 %>% 
  rename(exp_value = trigia_e42) %>% 
  mutate(export = ifelse(exp_value > 0, 1, 0)) %>% 
  select(tinh, ma_thue, ma_thue2, export, exp_value)   

dn18 <- left_join(dn18, exp18, by = c("tinh", "ma_thue", "ma_thue2"))

exp19 <- GC_2019 %>% 
  rename(ma_thue = masothue,
         exp_value = trigiank) %>% 
  mutate(export = ifelse(exp_value > 0, 1, 0)) %>% 
  select(ma_thue, export, exp_value)

dn19 <- left_join(dn19, exp19, by = "ma_thue")

# Intermediary processing 

ip16 <- HH_2016 %>% 
  filter(manuoc != "VN") %>% 
  rename(country = manuoc,
         postprocess_value = cot2,
         process_fee = cot6) %>%
  mutate(china = ifelse(country == "CN", 1, 0),
         year = 2016) %>% 
  select(year, tinh, ma_thue, ma_thue2, country, china, postprocess_value, process_fee)

ind16 <- dn16 %>% 
  select(ma_thue, ma_thue2, isic3)

ip16 <- merge(ip16, ind16, by = c("ma_thue", "ma_thue2"))
  
ip17 <- HH_2017 %>% 
  filter(manuoc != "VN") %>% 
  rename(country = manuoc,
         postprocess_value = hanghoa_nn,
         process_fee = chiphi_nn) %>% 
  mutate(china = ifelse(country == "CN", 1, 0),
         year = 2017) %>% 
  select(year, ma_thue, ma_thue2, country, china, postprocess_value, process_fee)

ind17 <- dn17 %>% 
  select(ma_thue, ma_thue2, isic3)

ip17 <- merge(ip17, ind17, by = c("ma_thue", "ma_thue2"))

ip18 <- HH_2018 %>% 
  filter(manuoc != "VN") %>% 
  rename(country = manuoc,
         postprocess_value = hanghoa_nn,
         process_fee = chiphi_hh) %>% 
  mutate(china = ifelse(country == "CN", 1, 0),
         year = 2018) %>% 
  select(year, ma_thue, ma_thue2, country, china, postprocess_value, process_fee)  

ind18 <- dn18 %>% 
  select(ma_thue, ma_thue2, isic3)

ip18 <- merge(ip18, ind18, by = c("ma_thue", "ma_thue2"))

ip19 <- HH_2019 %>% 
  filter(manuoc != "VN") %>% 
  rename(country = manuoc,
         postprocess_value = tongtrig,
         process_fee = phigiaco,
         ma_thue = masothue,
         ma_thue2 = v3_a) %>%
  mutate(china = ifelse(country == "CN", 1, 0),
         year = 2019) %>% 
  select(year, ma_thue, ma_thue2, country, china, postprocess_value, process_fee)

ind19 <- dn19 %>% 
  select(ma_thue, isic3)

ip19 <- merge(ip19, ind19, by = "ma_thue")

###################################
# COMPILING INTO SINGLE DATAFRAME #
###################################

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
         FDI = ifelse(lhdn > 10, 1, 0),
         export = ifelse(exp_value > 0, 1, 0),
         export = ifelse(is.na(export), 0 , export),
         exp_value = ifelse(exp_value == 0, NA, exp_value)) %>% 
  select(-c("nkd16", "vsic07", "vsic93")) %>% 
  distinct() %>% 
  select(id, year, isic3, everything()) %>% 
  select(-"ma_thue2")

write_dta(dn1419, "dn1419.dta")
save(dn1419, file = "dn1419.rda")

ip1619 <- bind_rows(ip16, ip17, ip18, ip19) %>% 
  select(-"tinh")

save(ip1619, file = "ip1619.rda")
write_dta(ip1619, "ip1619.dta")
