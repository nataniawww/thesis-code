# Time Series Graph (Native vs Wage)

ncleaned_data1 <- data_cleaned %>%
  group_by(YEAR, native) %>%
  summarize(mean_salary = wtd.quantile(log(INCWAGE),
                                   weights = HHWT,
                                   probs = .5)) %>% 
  mutate(quantile = .5)

ncleaned_data2 <- data_cleaned %>%
  group_by(YEAR, native) %>%
  summarize(mean_salary = wtd.quantile(log(INCWAGE),
                                       weights = HHWT,
                                       probs = .25)) %>% 
  mutate(quantile = .25)

ncleaned_data3 <- data_cleaned %>%
  group_by(YEAR, native) %>%
  summarize(mean_salary = wtd.quantile(log(INCWAGE),
                                       weights = HHWT,
                                       probs = .75)) %>% 
  mutate(quantile = .75)

ncleaned_data4 <- data_cleaned %>%
  group_by(YEAR, native) %>%
  summarize(mean_salary = wtd.quantile(log(INCWAGE),
                                       weights = HHWT,
                                       probs = .9)) %>% 
  mutate(quantile = .9)

ncleaned_data5 <- data_cleaned %>%
  group_by(YEAR, native) %>%
  summarize(mean_salary = wtd.quantile(log(INCWAGE),
                                       weights = HHWT,
                                       probs = .1)) %>% 
  mutate(quantile = .1)

ncleaned_data <- ncleaned_data1 %>% 
  bind_rows(ncleaned_data2, ncleaned_data3, ncleaned_data4, ncleaned_data5)

ncleaned_data$native <- factor(ncleaned_data$native,
                               levels = c("TRUE", "FALSE"))

gg3 <- ncleaned_data %>%
  ggplot(aes(x = YEAR, y = mean_salary, linetype = native,
             color = factor(quantile),
             group=interaction(native, quantile))) +
  geom_line() + geom_point() +
  labs(title = "Mean Log Wages Over the Years by Nativity",
       x = "Year",
       y = "Mean Log Wages ($)") +
  theme_bw() +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(color = "Quartile") #+
  #scale_color_manual(labels = c("10th", "25th", "50th", "75th", "90th"),
                #     name = "Quantile")


gg3

