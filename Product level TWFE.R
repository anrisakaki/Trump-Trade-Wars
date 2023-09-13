# TWFE USING ENTERPRISE CENSUS # 

load("dn1419.rda")

dn1419 <- dn1419 %>% 
  # replacing never treated units per fixest package 
  mutate(first_treated = ifelse(is.na(first_treated), 10000, first_treated))

# event studies 

iplot(list(
  feols(log(wage) ~ i(ttt, treated17, -1)| id + year,
            dn1419),
  feols(log(wage) ~ sunab(first_treated, year)| id + year,
        dn1419)), main = "Effect on labour cost")

iplot(list(
  feols(n_workers ~ i(ttt, treated, -1)| id + year,
        dn1419),
  feols(n_workers ~ sunab(first_treated, year)| id + year,
        dn1419)), main = "Effect on firm size")

iplot(list(
  feols(fworkers ~ i(ttt, treated17, -1)| id + year,
        dn1419),
  feols(fworkers ~ sunab(first_treated, year)| id + year,
        dn1419)), main = "Effect on share of female workers")


# switching to making new products and entering / exiting 

