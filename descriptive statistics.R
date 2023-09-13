# Descriptive statistics # 

yearly_avg <- dn1419 %>% 
  group_by(year, treated) %>% 
  summarise(avg_wage = mean(wage, na.rm = T),
            avg_nworkers= mean(n_workers_eoy, na.rm = T),
            avg_fshare = mean(fworkers_eoy, na.rm = T)) %>% 
  filter(!is.na(treated))

ggplot(yearly_avg, aes(x = year, y = (avg_wage), colour = as.factor(treated))) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks=seq(2014,2019,1)) +
  labs(x = "Year",
       y = "Avg. labour cost (million VND)") +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank())
ggsave("labour_cost1419.jpeg", width = 7, height = 7)

ggplot(yearly_avg, aes(x = year, y = (avg_nworkers), colour = as.factor(treated))) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks=seq(2014,2019,1)) +
  labs(x = "Year",
       y = "Avg. firm size (# workers)") +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank())
ggsave("firm_size1419.jpeg", width = 7, height = 7)

ggplot(yearly_avg, aes(x = year, y = (avg_fshare*100), colour = as.factor(treated))) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks=seq(2014,2019,1)) +
  labs(x = "Year",
       y = "Avg. share of female workers (%)") +
  theme_minimal() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.title=element_blank())
ggsave("fworkers_1419.jpeg", width = 7, height = 7)
