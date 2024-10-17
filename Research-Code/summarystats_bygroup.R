# Summary Stats
summary <- data_cleaned %>%
  group_by(native, speak_eng_home) %>%
  summarize("Min" = min(INCWAGE),
            "1st Qu." = quantile(INCWAGE, 0.25),
            "Median" = median(INCWAGE), 
            "3rd Qu." = quantile(INCWAGE, 0.75),
            "Max" = max(INCWAGE))
summary

ft1 <- flextable(summary)
ft1 <- add_header_row(ft1,
                     colwidths = c(2, 1, 3, 1),
                     values = c("Characteristics", " ", "Income Wages ($)", " ")
)
ft1 <- theme_vanilla(ft1)
ft1 <- add_footer_lines(ft1, 
                       "American Community Survey (ACS) respondent's total pre-tax wage and salary income measurements (not adjusted for inflation), 5-year estimates 2000 to 2021.")
ft1 <- color(ft1, part = "footer", color = "#666666")
ft1 <- set_caption(ft1, caption = "Table 1: Summary Statistics of Income Wages")
ft1 %>% 
  save_as_docx(path = "summarystats.docx")

# Density estimations
den_wage <- density(data$INCWAGE)
deny <- density(y)

# Plot
ggplot(data, aes(x = log(INCWAGE))) +
  geom_density(color = "blue", fill = "blue", alpha = 0.328) +
  labs(title = "Density Plot of Income Wages",
       x = "Log wages ($)",
       y = "Density") +
  theme_bw() 


d1 <- data_cleaned %>%
  ggplot(aes(x = log(INCWAGE), color = speak_eng_home)) +
  geom_density(alpha = 0.328) +
  labs(title = "Density Plot of Income Wages (speak english)",
       x = "Log wages ($)",
       y = "Density") +
  theme_bw() +
  scale_linetype_manual(values = c("solid", "dashed")) +
  xlim(5,15)


d2 <- data_cleaned %>%
  ggplot(aes(x = log(INCWAGE), color = native)) +
  geom_density(alpha = 0.328) +
  labs(title = "Density Plot of Income Wages (native)",
       x = "Log wages ($)",
       y = "Density") +
  theme_bw() +
  scale_linetype_manual(values = c("solid", "dashed")) +
  xlim(5,15)

d1 / d2


data_cleaned%>%
  group_by(native, speak_eng_home)%>%
  reframe(med_wage = median(INCWAGE))

data_cleaned%>%
  group_by(native, speak_eng_home)%>%
  reframe(quant_wage = quantile(INCWAGE, c(0.25, 0.5, 0.75)), prob = c(0.25, 0.5, 0.75))


probs <- c(0, 0.25, 0.5, 0.75, 1)

# Plot 1
dens1 <- density(log(data_cleaned$INCWAGE[which(data_cleaned$native==TRUE & data_cleaned$speak_eng_home==TRUE)]))
df1 <- data.frame(x=dens1$x, y=dens1$y)
quantiles1 <- quantile(log(data_cleaned$INCWAGE[which(data_cleaned$native==TRUE & data_cleaned$speak_eng_home==TRUE)]), prob=probs)
df1$quant <- factor(findInterval(df1$x,quantiles1))

gg_den1 <- ggplot(df1, aes(x,y)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  scale_x_continuous(breaks=quantiles1) + 
  scale_fill_brewer(guide="none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Native & Speaks English at Home",
       x ="Log Wages ($)", 
       y = "Density")

# Plot 2
dens2 <- density(log(data_cleaned$INCWAGE[which(data_cleaned$native==TRUE & data_cleaned$speak_eng_home==FALSE)]))
df2 <- data.frame(x=dens2$x, y=dens2$y)
quantiles2 <- quantile(log(data_cleaned$INCWAGE[which(data_cleaned$native==TRUE & data_cleaned$speak_eng_home==FALSE)]), prob=probs)
df2$quant <- factor(findInterval(df2$x,quantiles2))

gg_den2 <- ggplot(df2, aes(x,y)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  scale_x_continuous(breaks=quantiles2) + 
  scale_fill_brewer(guide="none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Native & Do Not Speak English at Home",
       x ="Log Wages ($)", 
       y = "Density")

# Plot 3
dens3 <- density(log(data_cleaned$INCWAGE[which(data_cleaned$native==FALSE & data_cleaned$speak_eng_home==TRUE)]))
df3 <- data.frame(x=dens3$x, y=dens3$y)
quantiles3 <- quantile(log(data_cleaned$INCWAGE[which(data_cleaned$native==FALSE & data_cleaned$speak_eng_home==TRUE)]), prob=probs)
df3$quant <- factor(findInterval(df3$x,quantiles3))

gg_den3 <- ggplot(df3, aes(x,y)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  scale_x_continuous(breaks=quantiles3) + 
  scale_fill_brewer(guide="none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Not Native & Speaks English at Home",
       x ="Log Wages ($)", 
       y = "Density")

# Plot 4
dens4 <- density(log(data_cleaned$INCWAGE[which(data_cleaned$native==FALSE & data_cleaned$speak_eng_home==FALSE)]))
df4 <- data.frame(x=dens4$x, y=dens4$y)
quantiles4 <- quantile(log(data_cleaned$INCWAGE[which(data_cleaned$native==FALSE & data_cleaned$speak_eng_home==FALSE)]), prob=probs)
df4$quant <- factor(findInterval(df4$x,quantiles4))

gg_den4 <- ggplot(df4, aes(x,y)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  scale_x_continuous(breaks=quantiles4) + 
  scale_fill_brewer(guide="none") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Not Native & Do not Speak English at Home",
       x ="Log Wages ($)", 
       y = "Density")

(gg_den1 | gg_den3) / (gg_den2 | gg_den4) + 
  plot_annotation(title = "Kernel Distribution of Income by Wages", 
                  theme = theme(plot.title = element_text(hjust = 0.5)))



## Another way

# Group 1: native==FALSE & speak_eng_home==FALSE
# Group 2: native==FALSE & speak_eng_home==TRUE
# Group 3: native==TRUE & speak_eng_home==FALSE
# Group 4: native==TRUE & speak_eng_home==TRUE

map_group <- function(x,y) {
  case_when(
    x == 0 & y == 0 ~ "Group 1",  # Group 1
    x == 0 & y == 1 ~ "Group 2",  # Group 2
    x == 1 & y == 0 ~ "Group 3",  # Group 3
    x == 1 & y == 1 ~ "Group 4",  # Group 4
  )
}

data_cleaned <- data_cleaned %>%
  mutate(groupings = map_group(native, speak_eng_home))

data_cleaned[,data_cleaned$groupings]=lapply(data_cleaned[,data_cleaned$groupings], as.factor)


# ALternative PLot
label_wrap <- function(width) {
  force(width)
  function(x) {
    unlist(lapply(strwrap(x, width = width, simplify = FALSE), paste0, collapse = "\n"))
  }
}

# Define label renaming

# Define label renaming
label_rename <- c("4" = "Native & Speaks English at Home",
                  "3" = "Native & Does not Speak English at Home",
                  "2" = "Non-native & Speaks English at Home",
                  "1" = "Non-native & Does not Speak English at Home")
data_cleaned <- data_cleaned %>%
  mutate(group = case_when(
    group  == "4" ~ "Native & Speaks English at Home",
    group  == "3" ~ "Native & Does not Speak English at Home",
    group  == "2" ~ "Non-native & Speaks English at Home",
    group  == "1" ~ "Non-native & Does not Speak English at Home"
  ))


ggplot(data_cleaned, aes(x = log(INCWAGE), 
                         y = group, 
                         group = group,
                         fill = factor(stat(quantile)))) +
  stat_density_ridges(scale =0.8,
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = 4) +
  scale_fill_brewer(labels = c("0.25", "0.5", "0.75", "1")) +
  scale_y_discrete(labels = label_wrap(10)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  theme_minimal() + 
  labs(title = "Kernel Distribution of Income by Wages",
       x = "Log Wages ($)", 
       y = "Group",
       fill = "Quartile")
