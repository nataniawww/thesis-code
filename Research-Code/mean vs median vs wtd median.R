# Showing why weighted median should be considered among all three cases

# TC
#install.packages("Hmisc")
library(Hmisc)
summarize <- dplyr::summarize

gg0 <- data %>%
  group_by(YEAR, native) %>%
  summarize(mean_salary = mean(INCWAGE)) %>%
  ggplot(aes(x = YEAR, y = mean_salary, color = native)) +
  geom_line() +
  labs(title = "Mean Salary Over the Years by Nativity",
       x = "Year",
       y = "Mean Salary") +
  theme_bw()


gg1tc <- data %>%
  group_by(YEAR, native) %>%
  summarize(median_salary = median(INCWAGE)) %>%
  ggplot(aes(x = YEAR, y = median_salary, color = native)) +
  geom_line() +
  labs(title = "Median Salary Over the Years by Nativity",
       x = "Year",
       y = "Median Salary") +
  theme_bw()


gg1tcwtd <- data %>%
  group_by(YEAR, native) %>%
  summarize(median_salary = wtd.quantile(INCWAGE,
                                       weights = HHWT,
                                       probs = 0.5)) %>%
  ggplot(aes(x = YEAR, y = median_salary, color = native)) +
  geom_line() +
  labs(title = "Wtd Median Salary Over the Years by Nativity",
       x = "Year",
       y = "Median Salary") +
  theme_bw()

(gg1tc | gg1tcwtd) / gg0

# hmisc - wtd.quantile(var, weights = HHWT, probs = .5)
# hmisc - wtd.mean(var, weights = HHWT)