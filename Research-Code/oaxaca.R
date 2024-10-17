library(oaxaca)

# Function - reverse speak_eng_home
reverse_lang <- function(x) {
  case_when(
    x == TRUE ~ FALSE,  
    x == FALSE ~ TRUE
  )
}

# Data and Samples

# Immigrants
immigrants <- data_cleaned[data_cleaned$native!=1,] %>%
  mutate(dont_speak_eng_home = reverse_lang (speak_eng_home))

immigrants %>% 
  ggplot(aes(x=speak_eng_home)) +
  geom_bar() # Barchart of language spoken distribution 
nrow(immigrants)

immigrants %>% 
  ggplot(aes(x=eng_proficiency)) +
  geom_bar()


# Sample 1
sample1 <- sample_frac(data_cleaned, 
                       size = 0.1) %>%
  mutate(dont_speak_eng_home = reverse_lang(speak_eng_home))

sample1 %>% 
  ggplot(aes(x=eng_proficiency)) +
  geom_bar()


sample1 %>% 
  ggplot(aes(x=speak_eng_home)) +
  geom_bar()

nrow(sample1)
sum(sample1$speak_eng_home==1)
sample1$dont_speak_eng_home <- as.logical(sample1$dont_speak_eng_home)

# Sample 2
sample2 <- sample_frac(data_cleaned[data_cleaned$YRSUSA1 > 0 &
                                      data_cleaned$YRSUSA1 < 50 &
                                      data_cleaned$native!=1,],
                       size = 0.1) %>%
  mutate(dont_speak_eng_home = reverse_lang(speak_eng_home))
nrow(sample2)
sum(sample2$speak_eng_home==1)
sample2$dont_speak_eng_home <- as.logical(sample2$dont_speak_eng_home)

# Sample 3
noncollege_immigrants <- immigrants[immigrants$college_grad!=1 & 
                                      immigrants$some_college!=1 &
                                      immigrants$post_grad!=1,] %>%
  mutate(dont_speak_eng_home = reverse_lang(speak_eng_home))
nrow(noncollege_immigrants)
sum(noncollege_immigrants$speak_eng_home==1)
noncollege_immigrants$dont_speak_eng_home <- as.logical(noncollege_immigrants$dont_speak_eng_home)



# Result 0: Using Sample 1
results0 <- oaxaca(log(INCWAGE) ~ AGE + SEX + RACE + YRSUSA1 + eng_proficiency | dont_speak_eng_home, 
         data=sample1)

results0$n # Group A: speak_eng_home==TRUE
results0$y

results0$twofold$overall

# Plot 0: 10% Sample of Cleaned Data
plot0 <- plot(results0, decomposition = "twofold", group.weight = -1,
     title = "Blinder-Oaxaca Decomposition Plot (10% Sample of Cleaned Data)",
     variable.labels = c("AGE" = "Age", 
                         "SEX" = "Sex",
                         "RACE" = "Race",
                         "YRSUSA1" = "Years Immigrated",
                         "eng_proficiency" = "English Proficiency")) 
plot0 

# Result 1: Using all immigrants
results1 <- oaxaca(log(INCWAGE) ~ AGE + SEX + some_college + college_grad + post_grad + YRSUSA1 | dont_speak_eng_home, 
                   data=immigrants)

results1$n # Group A: speak_eng_home==TRUE
results1$y

results1$twofold$overall

# Results 1.1: comparison results - speak_eng_home
results1.1 <- oaxaca(log(INCWAGE) ~ AGE + SEX + some_college + college_grad + post_grad + YRSUSA1 | speak_eng_home, 
                   data=immigrants)

results1.1$n # Group A: speak_eng_home==TRUE
results1.1$y
results1.1$twofold$overall
plot1.1 <- plot(results1.1, decomposition = "twofold", group.weight = -1,
     title = "All Non-natives",
     variable.labels = c("AGE" = "Age", 
                         "SEX" = "Sex",
                         "some_collegeTRUE" = "Some college",
                         "college_gradTRUE" = "College graduate",
                         "post_gradTRUE" = "Post Graduate",
                         "RACE" = "Race",
                         "YRSUSA1" = "Years Immigrated",
                         "eng_proficiency" = "English Proficiency"))

# Plot 1: All Immigrants
plot1 <- plot(results1, decomposition = "twofold", group.weight = -1,
     title = "All Non-natives",
     variable.labels = c("AGE" = "Age", 
                         "SEX" = "Sex",
                         "some_collegeTRUE" = "Some college",
                         "college_gradTRUE" = "College graduate",
                         "post_gradTRUE" = "Post Graduate",
                         "RACE" = "Race",
                         "YRSUSA1" = "Years Immigrated",
                         "eng_proficiency" = "English Proficiency"))

plot1

plot1.1 | plot1
                         

# Result 2: Using Sample 2
results2 <- oaxaca(log(INCWAGE) ~ AGE + SEX + some_college + college_grad + post_grad + YRSUSA1 | dont_speak_eng_home, 
                   data=sample2)

results2$n # Group A: speak_eng_home==TRUE
results2$y

results2$twofold$overall

# Plot 2: 10% Sample Non-natives
plot2 <- plot(results2, decomposition = "twofold", group.weight = -1,
              title = "10% Sample of Non-natives Immigrated for 0-50 Years",
              variable.labels = c("AGE" = "Age", 
                                  "SEX" = "Sex",
                                  "some_collegeTRUE" = "Some college",
                                  "college_gradTRUE" = "College graduate",
                                  "post_gradTRUE" = "Post Graduate",
                                  "YRSUSA1" = "Years Immigrated",
                                  "eng_proficiency" = "English Proficiency"))                                            

plot2

# Result 3: Using Non-college Immigrants
results3 <- oaxaca(log(INCWAGE) ~ AGE + SEX + YRSUSA1 | dont_speak_eng_home, 
                   data=noncollege_immigrants)

results3$n # Group A: speak_eng_home==TRUE
results3$y

results3$twofold$overall

# Plot 3: Non-college Immigrants
plot3 <- plot(results3, decomposition = "twofold", group.weight = -1,
              title = "Non-natives who have not attended any college",
              variable.labels = c("AGE" = "Age", 
                                  "SEX" = "Sex",
                                  "YRSUSA1" = "Years Immigrated",
                                  "eng_proficiency" = "English Proficiency"))                                            

plot3

plot(results3, decomposition = "twofold", group.weight = -1,
     unexplained.split = TRUE, 
     components = c("unexplained A", "unexplained B"), 
     component.labels = c("unexplained A" = "In Favor of those who speak English at home", 
                          "unexplained B" = "Against those who do not speak English at home"),
     variables = c("AGE", "YRSUSA1"), 
     variable.labels = c("AGE" = "Age", 
                         "YRSUSA1" = "Years Immigrated"))

plot1 | plot2

library(ggpubr)

blinderplot1<- ggarrange(plot1, plot3, 
                 ncol=2, nrow=1, 
                 common.legend = TRUE,
                 legend="bottom")
annotate_figure(blinderplot1, 
                top = text_grob("Blinder-Oaxaca Decomposition Plot", 
                                face = "bold", size = 14))
