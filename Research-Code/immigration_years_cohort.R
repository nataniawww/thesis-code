data_cleaned %>%
  ggplot(aes(x=log(INCWAGE))) +
  geom_histogram() +
  theme_bw()

# Plot
data_cleaned[data_cleaned$native!=1 &
               data_cleaned$YRSUSA1 > 0 &
               data_cleaned$YRSUSA1 < 50,] %>%
  group_by(YRSUSA1, speak_eng_home) %>%
  summarize(mean_logwage = mean(log(INCWAGE))) %>%
  #print(n=92) %>%
  ggplot(aes(x=YRSUSA1, y=mean_logwage, color = speak_eng_home)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", 
              se = FALSE) +
  scale_color_manual(breaks=c("TRUE", "FALSE"),
                     values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) + 
  theme_bw() +
  labs(title = "Years of Immigration vs Mean Log Wage (Grouped by Language Spoken at Home)",
       y = "Mean Log Wage ($)",
      x = "Years of Immigration")
           
data_cleaned[data_cleaned$native!=1,] %>%
  group_by(YRNATUR) %>%
  summarize(mean_logwage = mean(log(INCWAGE))) %>%
  print(n=88) %>%
  ggplot(aes(x=factor(YRNATUR), y=mean_logwage)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  theme_bw()


# Years in USA against Log Wage, alpha = 0.1
data_cleaned[data_cleaned$native!=1,] %>%
  ggplot(aes(x=YRSUSA1, y = log(INCWAGE))) +
  geom_jitter(alpha = 0.1) + 
  theme_bw()

# Heatmap (hexagonal binning)
data_cleaned[data_cleaned$native!=1,] %>%
  ggplot(aes(x=YRSUSA1, y = log(INCWAGE))) + 
  stat_binhex()

data_cleaned[data_cleaned$native!=1,] %>%
  ggplot(aes(x=YRSUSA1, y = log(INCWAGE))) + 
  geom_hex() +
  scale_fill_viridis_c() +
  geom_jitter(shape = '.', col = 'white')

smoothScatter(data_cleaned[data_cleaned$native!=1,]$YRSUSA1, 
              log(data_cleaned[data_cleaned$native!=1,]$INCWAGE))

data_cleaned[data_cleaned$native!=1,] %>%
  ggplot(aes(x=YRSUSA1, y = log(INCWAGE))) + 
  geom_pointdensity() + 
  scale_color_viridis_c()

# Scatter
data_cleaned[data_cleaned$native!=1,] %>%
  ggplot(aes(x=YRSUSA1, y = log(INCWAGE))) +
  geom_point(alpha = 0.1) +
  geom_rug(alpha = 0.01)

# Traditional Heatmap (Rectangular binning)
data_cleaned[data_cleaned$native!=1,] %>%
  ggplot(aes(x=YRSUSA1, y = log(INCWAGE))) +
  geom_bin2d()