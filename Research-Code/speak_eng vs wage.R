# Non-native Time Series Graph (Speaks Eng vs Wage)

nn_data1 <- data_cleaned %>%
  filter(native == 0) %>% 
  group_by(YEAR, speak_eng_home) %>% 
  summarize(mean_salary = wtd.quantile(log(INCWAGE),
                                       weights = HHWT,
                                       probs = .5)) %>% 
  mutate(quantile = .5)

nn_data2 <- data_cleaned %>%
  filter(native == 0) %>% 
  group_by(YEAR, speak_eng_home) %>% 
  summarize(mean_salary = wtd.quantile(log(INCWAGE),
                                       weights = HHWT,
                                       probs = .25)) %>% 
  mutate(quantile = .25)

nn_data3 <- data_cleaned %>%
  filter(native == 0) %>% 
  group_by(YEAR, speak_eng_home) %>% 
  summarize(mean_salary = wtd.quantile(log(INCWAGE),
                                       weights = HHWT,
                                       probs = .75)) %>% 
  mutate(quantile = .75)

nn_data4 <- data_cleaned %>%
  filter(native == 0) %>% 
  group_by(YEAR, speak_eng_home) %>% 
  summarize(mean_salary = wtd.quantile(log(INCWAGE),
                                       weights = HHWT,
                                       probs = .9)) %>% 
  mutate(quantile = .9)

nn_data5 <- data_cleaned %>%
  filter(native == 0) %>% 
  group_by(YEAR, speak_eng_home) %>% 
  summarize(mean_salary = wtd.quantile(log(INCWAGE),
                                       weights = HHWT,
                                       probs = .1)) %>% 
  mutate(quantile = .1)

nn_data <- nn_data1 %>% 
  bind_rows(nn_data2, nn_data3, nn_data4, nn_data5)

nn_data$speak_eng_home <- factor(nn_data$speak_eng_home ,
                               levels = c("TRUE", "FALSE"))


gg4 <- nn_data %>%
  ggplot(aes(x = YEAR, y = mean_salary, linetype = speak_eng_home,
             color = factor(quantile),
             group=interaction(speak_eng_home, quantile))) +
  geom_line() + geom_point() +
  labs(title = "Non-Native Mean Log Wages Over the Years by Language Spoken at Home",
       x = "Year",
       y = "Mean Log Wages ($)") +
  theme_bw() + 
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(color = "Quartile")


gg3 | gg4

