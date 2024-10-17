nrow(data[data$INCWAGE==0,])/nrow(data)

data_cleaned%>%
  ggplot(aes(x=log(INCWAGE))) +
  geom_histogram() +
  theme_bw()

# binned scatterplot (includes 0)
binsreg(data_cleaned$INCWAGE, as.numeric(data_cleaned$YEAR), line = c(5,5))

# binned scatterplot (excludes 0)
binsreg(data_nonzero$INCWAGE, as.numeric(data_nonzero$YEAR), line = c(3,3))

# BASE MODEL - OLS model 1 (lm0): regression results (non-zero data)
lm0 <- lm(log(INCWAGE)  ~ native + speak_eng_home, data=data_cleaned)
summary(lm0)
texreg(lm0) # print lm0 as latex

# flextable (lm0)
#ft3 <- as_flextable(lm0)
#ft3 <- theme_vanilla(ft3)
#ft3 <- add_footer_lines(ft3, 
#                        "Source: American Community Survey (ACS), 5-year estimates 2000 to 2021.")
#ft3 <- color(ft3, part = "footer", color = "#666666")
#ft3 <- set_caption(ft3, caption = "Table 2: OLS Regression Model 1 Results")
#ft3 %>% 
#  save_as_docx(path = "lm0.docx")

#ft3

# OLS model 2: regression results (non-zero data)
summary(lm(log(INCWAGE) ~ native + speak_eng_home 
           + AGE + SEX + OCC + some_college + college_grad + post_grad + YRIMMIG + as.factor(STATEFIP) + as.factor(YEAR), 
           data=data_nonzero))

# OLS model 2 (lm_overall): regression results (cleaned data)
lm_overall <- lm(log(INCWAGE) ~ native + speak_eng_home 
                 + AGE + SEX + OCC + some_college + college_grad + post_grad + as.factor(YRSUSA1) + as.factor(YEAR) + as.factor(STATEFIP) , 
           data=data_cleaned)
texreg(lm_overall) # print lm_overall as latex

# OLS model 3 (lm_overall): regression results (cleaned data)
lm_overall1 <- lm(log(INCWAGE) ~ native + speak_eng_home + native*RACE 
                 + AGE + SEX + OCC + some_college + college_grad + post_grad + as.factor(YRSUSA1) + as.factor(YEAR) + as.factor(STATEFIP) , 
                 data=data_cleaned)
texreg(lm_overall1) # print lm_overall as latex


lm_overall2 <- lm(log(INCWAGE) ~ native + speak_eng_home 
                  + native*factor(agg_race) + speak_eng_home*factor(agg_race)
                  + AGE + SEX + OCC + some_college + college_grad + post_grad + factor(YRSUSA1) + factor(YEAR) + factor(STATEFIP) , 
                  data=data_cleaned)

texreg(lm_overall2) # print lm_overall as latex

# flextable (lm_overall)
#ft2 <- as_flextable(lm_overall)
#ft2 <- theme_vanilla(ft2)
#ft2 <- add_footer_lines(ft2, 
#                        "Source: American Community Survey (ACS), 5-year estimates 2000 to 2021.")
#ft2 <- color(ft2, part = "footer", color = "#666666")
#ft2 <- set_caption(ft2, caption = "Table 3: OLS Regression Model 2 Results")
#ft2 %>% 
#  save_as_docx(path = "lm2.docx")

#ft2


