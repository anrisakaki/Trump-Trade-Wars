# Descriptive statistics # 

yearly_avg <- dn1419 %>% 
  group_by(year, treated) %>% 
  summarise(avg_wage = mean(wage, na.rm = T),
            avg_nworkers= mean(n_workers, na.rm = T),
            avg_fshare = mean(female_share, na.rm = T)) %>% 
  filter(!is.na(treated))

ggplot(yearly_avg, aes(x = year, y = (avg_wage), colour = as.factor(treated))) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks=seq(2014,2019,1)) +
  labs(x = "Year",
       y = "Mean wage ('000 VND)") +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

ggplot(yearly_avg, aes(x = year, y = (avg_volume), colour = as.factor(willbe_treated))) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks=seq(2014,2018,1)) +
  labs(x = "Year",
       y = "Total volume") +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

