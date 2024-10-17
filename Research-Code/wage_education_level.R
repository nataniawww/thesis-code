# Sample (n=1000); 
# Conditions: 1. below median income
# 2. stay in USA for more than 0 years
# 3. not native
# 4. have not attended college at all 
sample1 <- sample_frac(data_cleaned[data_cleaned$YRSUSA1 > 0 &
                                      data_cleaned$native!=1 & 
                                      data_cleaned$post_grad!=1,], 
                       size = 0.1) #10%

sample1 %>%
  ggplot(aes(x=YRSUSA1, y = log(INCWAGE))) + 
  geom_pointdensity() + 
  scale_color_viridis_c()

smoothScatter(sample1$YRSUSA1, 
              log(sample1$INCWAGE))

sample1 %>%
  ggplot(aes(x=YRSUSA1, y = log(INCWAGE))) +
  geom_point(alpha = 0.1) +
  geom_rug(alpha = 0.01)

sample1 %>%
  ggplot(aes(x=YRSUSA1, y = log(INCWAGE))) +
  geom_bin2d()

h1 <- hcl.colors(10, palette = "Dynamic")

sample1 %>%
  ggplot(aes(x = YRSUSA1, y = log(INCWAGE), color = factor(RACE))) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "lm", 
              se = FALSE) +  # Remove aes(color = factor(RACE)) here
  scale_color_manual(name = "Race", 
                     labels = c("White",
                                "Black/African American", 
                                "American Indian or Alaska Native",
                                "Chinese",
                                "Japanese",
                                "Other Asian or Pacific Islander",
                                "Other race, nec",
                                "Two major races",
                                "Three or more major races"),
                     values = h1) +  
  labs(title = "Years in US vs Log Income Wages",
       subtitle = "10% sample of non-natives who have not attended post-grad, by race",
       y = "Log Wages",
       x = "Years in the United States") +
  theme_minimal()



# 10% sample of non-natives who have not attended college, by race and language spoken at home
sample_frac(data_cleaned[data_cleaned$YRSUSA1 > 0 &
                           data_cleaned$native!=1 & 
                           data_cleaned$college_grad!=1 & 
                           data_cleaned$some_college!=1 &
                           data_cleaned$post_grad!=1,], 0.1) %>%
  ggplot(aes(x = YRSUSA1, y = log(INCWAGE), color = factor(RACE))) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "lm", 
              se = FALSE) +  # Remove aes(color = factor(RACE)) here
  scale_color_manual(name = "Race", 
                     labels = c("White",
                                "Black/African American", 
                                "American Indian or Alaska Native",
                                "Chinese",
                                "Japanese",
                                "Other Asian or Pacific Islander",
                                "Other race",
                                "Two major races",
                                "Three or more major races"),
                     values = h1) +  
  labs(title = "Years in United States vs Log Income Wages",
       subtitle = "10% sample of non-natives who have not attended college, by race and language spoken at home",
       y = "Log Wages",
       x = "Years") +
  theme_minimal() +
  facet_wrap(~speak_eng_home)

sample_frac(data_cleaned[data_cleaned$YRSUSA1 > 0 &
                           data_cleaned$native!=1 & 
                           data_cleaned$college_grad!=1 & 
                           data_cleaned$some_college!=1 &
                           data_cleaned$post_grad!=1,], 0.05) %>%
  ggplot(aes(x = YRSUSA1, y = log(INCWAGE))) +
  geom_jitter(alpha = 0.1) +
  theme_minimal() +
  facet_wrap(~RACE) +
  geom_smooth(method = "lm",
              se = FALSE) 






sample_frac(data_cleaned[data_cleaned$YRSUSA1 > 0 &
                           data_cleaned$native!=1 & 
                           data_cleaned$college_grad!=1 & 
                           data_cleaned$some_college!=1 &
                           data_cleaned$post_grad!=1,], 0.1) %>%
  ggplot(aes(x = YRSUSA1, y = log(INCWAGE), color = speak_eng_home)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "lm") +
  stat_summary_bin(fun='mean', bins=10,
                   color='black', size=1.5, geom='point') +
  theme_minimal() +
  facet_wrap(~agg_race) +  
  labs(title = "Years in United States vs Log Income Wages",
       subtitle = "10% sample of non-natives who have not attended any college, by race and language spoken at home",
       y = "Log Wages",
       x = "Years") +
  scale_color_manual(breaks=c("TRUE", "FALSE"),
                     values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D"))


# sample_frac(data_cleaned[data_cleaned$YRSUSA1 > 0 &
#                            data_cleaned$native!=1 & 
#                            data_cleaned$college_grad==TRUE,], 0.1) %>%
#   ggplot(aes(x = YRSUSA1, y = log(INCWAGE), color = speak_eng_home)) +
#   geom_jitter(alpha = 0.1) +
#   geom_smooth(method = "lm") +
#   stat_summary_bin(fun='mean', bins=10,
#                    color='black', size=1.5, geom='point') +
#   theme_minimal() +
#   facet_wrap(~agg_race) +
#   labs(title = "Years in United States vs Log Income Wages",
#        subtitle = "10% sample of non-natives who have graduated from college, by race and language spoken at home",
#        y = "Log Wages",
#        x = "Years") +
#   scale_color_manual(breaks=c("TRUE", "FALSE"),
#                      values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D"))


data_cleaned[data_cleaned$YRSUSA1 > 0 &
               data_cleaned$YRSUSA1 < 50,]%>%
  group_by(YRSUSA1, speak_eng_home) %>%
  summarize(mean_logwage = mean(log(INCWAGE))) %>%
  #print(n=92) %>%
  ggplot(aes(x=YRSUSA1, y=mean_logwage, color = speak_eng_home)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se =  FALSE) +
  scale_color_manual(breaks=c("TRUE", "FALSE"),
                     values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) + 
  theme_minimal() +
  labs(title = "Years in USA vs Mean Log Wage",
       subtitle = "Grouped by language spoken at home",
       y = "Mean Log Wage ($)",
       x = "Years in USA")


# Plot 1
p1 <- data_cleaned[data_cleaned$native!=1 &
                                 data_cleaned$college_grad!=1 & 
                                 data_cleaned$some_college!=1 &
                                 data_cleaned$post_grad!=1 &
                                 data_cleaned$YRSUSA1 > 0 &
                                 data_cleaned$YRSUSA1 < 50,]%>%
  group_by(YRSUSA1, agg_race, speak_eng_home) %>%
  summarize(mean_logwage = mean(log(INCWAGE))) %>%
  #print(n=92) %>%
  ggplot(aes(x=YRSUSA1, y=mean_logwage, color = speak_eng_home)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se =  FALSE) +
  scale_color_manual(breaks=c("TRUE", "FALSE"),
                     values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) + 
  theme_minimal() +
  facet_wrap(~agg_race) +
  labs(title = "Years in USA vs Mean Log Wage",
       subtitle = "Non-natives who have not attended college, by race and language spoken at home",
       y = "Mean Log Wage ($)",
       x = "Years in USA")

data_cleaned[data_cleaned$native!=1 &
                           data_cleaned$agg_race %in% c("White", "Asian or Pacific Islander") & 
                           data_cleaned$speak_eng_home == TRUE &
                           data_cleaned$college_grad!=1 & 
                           data_cleaned$some_college!=1 &
                           data_cleaned$post_grad!=1 &
                           data_cleaned$YRSUSA1 > 0 &
                           data_cleaned$YRSUSA1 < 50,] %>%
  group_by(YEAR, YRSUSA1, agg_race) %>%
  summarize(mean_logwage = mean(log(INCWAGE))) %>%
  #print(n=92) %>%
  ggplot(aes(x=YRSUSA1, y=mean_logwage)) +
  geom_jitter(alpha = 0.7, color = "#00BFC4") +
  geom_smooth(method = "lm", se =  FALSE, color = "#00BFC4") +
  theme_minimal() +
  facet_wrap(~YEAR) +
  labs(title = "Years in USA vs Mean Log Wage",
       subtitle = "White and Asian Non-natives who speaks English at home and have not attended college",
       y = "Mean Log Wage ($)",
       x = "Years in USA")

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

data_cleaned[data_cleaned$native!=1 &
               data_cleaned$agg_race %in% c("White","Asian or Pacific Islander") &
               data_cleaned$college_grad!=1 & 
               data_cleaned$some_college!=1 &
               data_cleaned$post_grad!=1 &
               data_cleaned$speak_eng_home == TRUE &
               data_cleaned$YRSUSA1 > 12 &
               data_cleaned$YRSUSA1 < 19,] %>%
  select(YEAR, YRIMMIG, YRSUSA1, INCWAGE) %>%
  filter(INCWAGE < 39525) %>%
  # group_by(YEAR, YRIMMIG, YRSUSA1) %>%
  # summarize(mean_logwage = mean(log(INCWAGE))) %>%
  #print(n=92) %>%
  ggplot(aes(x=YRSUSA1, y=log(INCWAGE))) +
  geom_jitter(alpha = 0.7) +
  # geom_smooth(method = "lm", se =  FALSE) +
  # scale_color_manual(breaks=c("TRUE", "FALSE"),
  #                    values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) +
  theme_minimal() +
  # facet_wrap(~YRIMMIG) +
  labs(title = "Years in USA vs Mean Log Wage",
       subtitle = "White Non-natives who have not attended college, by Years of Immigration and language spoken at home",
       y = "Mean Log Wage ($)",
       x = "Years in USA")

drop1 <- data_cleaned[data_cleaned$native!=1 &
               data_cleaned$agg_race %in% c("White","Asian or Pacific Islander") &
               data_cleaned$college_grad!=1 & 
               data_cleaned$some_college!=1 &
               data_cleaned$post_grad!=1 &
               data_cleaned$speak_eng_home == TRUE &
               data_cleaned$YRSUSA1 > 12 & data_cleaned$YRSUSA1 < 22,] %>%
  select(YEAR, YRIMMIG, YRSUSA1, INCWAGE) %>%
  mutate(logwage = log(INCWAGE))
pairs(drop1[,-4])
ou <- which(drop1$logwage < 3)
drop1[-ou,]
pairs(drop1[-ou,][,-4])

drop_dat <- data_cleaned[data_cleaned$native!=1 &
                           data_cleaned$college_grad!=1 & 
                           data_cleaned$some_college!=1 &
                           data_cleaned$post_grad!=1 &
                           data_cleaned$YRSUSA1 > 0 &
                           data_cleaned$YRSUSA1 < 50,] %>%
  mutate(logwage = log(INCWAGE))

drop_dat %>%
  select(YEAR, YRIMMIG, YRSUSA1, INCWAGE) %>%
  filter(INCWAGE < 39525 & YRSUSA1 < 28 & YRSUSA1 > 7) %>%
  group_by(YEAR, YRIMMIG, YRSUSA1) %>%
  summarize(mean_logwage = mean(log(INCWAGE))) %>%
  #print(n=92) %>%
  ggplot(aes(x=YRSUSA1, y=mean_logwage)) +
  geom_jitter(alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 30, 1), 
                     labels = seq(0, 30, 1))

summary1 <- drop_dat %>%
  select(YEAR, YRIMMIG, YRSUSA1, INCWAGE) %>%
  filter(INCWAGE < 39525) %>%
  group_by(YEAR, YRIMMIG, YRSUSA1) %>%
  summarize(mean_logwage = mean(log(INCWAGE)))

# Year of Census and Years of Immigration of points that dropped
summary1[which(summary1$mean_logwage < 9 &
                 summary1$YRSUSA1 > 12 & summary1$YRSUSA1 < 18),] %>%
  ggplot() +geom_bar(aes(x=YRIMMIG))
  pull(YRIMMIG) %>%
  unique(YRIMMIG)





drop_dat[-ou3,] %>%
  select(YEAR, YRIMMIG, YRSUSA1, INCWAGE) %>%
  filter(INCWAGE < 39525) %>%
  group_by(YEAR, YRIMMIG, YRSUSA1) %>%
  # summarize(mean_logwage = mean(log(INCWAGE))) %>%
  #print(n=92) %>%
  ggplot(aes(x=YRSUSA1, y=log(INCWAGE))) +
  geom_jitter(alpha = 0.7) + facet_wrap(~YEAR)

ou3 <- which(drop_dat$logwage < 3.75 &
               drop_dat$YRSUSA1 < 18 &
               drop_dat$YRSUSA1 > 11 &
               drop_dat$YEAR %in% c(2000, 2006, 2021))

drop_dat[ou3,] %>%
  select(YEAR, YRIMMIG, YRSUSA1, logwage, INCWAGE) %>%
  filter(YRSUSA1 < 18 & YRSUSA1 > 11 & 
           YEAR %in% c(2000, 2006, 2021)) %>%
  ggplot() + geom_bar(aes(x=as.factor(YRSUSA1)))


  # # geom_smooth(method = "lm", se =  FALSE) +
  # # scale_color_manual(breaks=c("TRUE", "FALSE"),
  # #                    values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) +
  # theme_minimal() +
  # # facet_wrap(~YRIMMIG) +
  # labs(title = "Years in USA vs Mean Log Wage",
  #      subtitle = "White Non-natives who have not attended college, by Years of Immigration and language spoken at home",
  #      y = "Mean Log Wage ($)",
  #      x = "Years in USA")






# Plot 2 (No drop in trend)
p2 <- data_cleaned[data_cleaned$native!=1 &
               data_cleaned$college_grad==TRUE &
               data_cleaned$YRSUSA1 > 0 &
               data_cleaned$YRSUSA1 < 50,] %>%
  group_by(YRSUSA1, agg_race, speak_eng_home) %>%
  summarize(mean_logwage = mean(log(INCWAGE))) %>%
  #print(n=92) %>%
  ggplot(aes(x=YRSUSA1, y=mean_logwage, color = speak_eng_home)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se =  FALSE) +
  scale_color_manual(breaks=c("TRUE", "FALSE"),
                     values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) + 
  theme_minimal() +
  facet_wrap(~agg_race) +
  labs(title = "Years in United States vs Mean Log Wage",
       subtitle = "Non-natives who have graduated from college, by race and language spoken at home",
       y = "Mean Log Wage ($)",
       x = "Years of Immigration")

p1 | p2
