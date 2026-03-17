library(readxl) 
library(readr)
library(ISLR) 
library(dplyr) 
library(caret) 
library(tidyr) 
library(dslabs) 
library(gridExtra) 
library(kernlab) 
library(probably) 
library(vtable) 
library(car) 
library(jtools) 
library(lmtest) 
library(AICcmodavg) 
library(ARTool)

knitr::opts_chunk$set(echo = TRUE) 


library(readr)
edu <- read_csv("C:/Users/david/OneDrive/Documents/STA classes/STA 5900/Group Project 3/Education-1.csv")
View(edu)

library(dplyr)

# Step 1: Exclude rows where Followed Recommend is 9999
edu_group <- edu %>%
  filter(`Followed Recommend` != 9999) %>%
  mutate(group = as.factor(`Followed Recommend`))

View(edu_group)


  # Q#01; Variables involved: Portfolio Total Score, Group; Method: One Way Anova
  
  # Define Portfolio Total Score vector (PTS)
  edu_group$`Followed Recommend` <- as.factor(edu_group$`Followed Recommend`)

  # Summary statistics (overall)
  summary(edu_group$`Portfolio Total Score`)
  sd(edu_group$`Portfolio Total Score`)
  
  # Boxplot with mean and SD displayed in the legend
  boxplot(edu_group$`Portfolio Total Score`,
          ylab = "Portfolio Total Score",
          main = "PTS of Students")
  legend("topright",
         legend = c(paste("Mean:", round(mean(edu_group$`Portfolio Total Score`), 2)),
                    paste("SD:", round(sd(edu_group$`Portfolio Total Score`), 2))))
  
  # Group-level summary stats
  edu_group %>%
    group_by(group) %>%
    summarise(
      Min = min(`Portfolio Total Score`, na.rm = TRUE),
      Q1 = quantile(`Portfolio Total Score`, 0.25, na.rm = TRUE),
      Median = median(`Portfolio Total Score`, na.rm = TRUE),
      Q3 = quantile(`Portfolio Total Score`, 0.75, na.rm = TRUE),
      Max = max(`Portfolio Total Score`, na.rm = TRUE),
      Mean_PTS = mean(`Portfolio Total Score`, na.rm = TRUE),
      SD_PTS = sd(`Portfolio Total Score`, na.rm = TRUE),
      n = n()
    )
  
  # Enhanced boxplot with group means overlaid as red dots and lines
  ggplot(edu_group, aes(x = group, y = `Portfolio Total Score`, fill = group)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", color = "black", size = 3) +  # Red dot for group mean
    stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linetype = "dashed") +  # Line across means
    labs(title = "Portfolio Total Score by Recommendation Group",
         x = "Followed Recommendation",
         y = "Portfolio Total Score") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Shapiro-Wilk test for normality per group
  ggplot(edu_group, aes(sample = `Portfolio Total Score`)) +
    stat_qq_band(aes(fill = `Followed Recommend`), alpha = 0.2) +  # Confidence band
    stat_qq_point() +  # Q-Q points
    stat_qq_line() +   # Q-Q line
    facet_wrap(~ `Followed Recommend`) +
    labs(title = "Q-Q Plots with Confidence Bands by Group",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
  
  edu_group %>%
    group_by(group) %>%
    summarise(p_value = shapiro.test(`Portfolio Total Score`)$p.value)
  
  # One-Way ANOVA
  anova1 <- aov(`Portfolio Total Score` ~ group, data = edu_group)
  summary(anova1)
  
  # Post-hoc (if ANOVA is significant)
  TukeyHSD(anova1)

  # Q#02; Variables Involved: Comp AVG Grade, Group; Method: One-Way Anova

  # Define Comp AVG Grade (CG)
  edu_group$`Comp AVG Grade`
  
  # Summary statistics (overall)
  summary(edu_group$`Comp AVG Grade`)
  sd(edu_group$`Comp AVG Grade`)
  
  # Boxplot with mean and SD displayed in the legend
  boxplot(edu_group$`Comp AVG Grade`,
          ylab = "GPA Grading Scale",
          main = "Students Comp AVG Grades")
  legend("topright",
         legend = c(paste("Mean:", round(mean(edu_group$`Comp AVG Grade`), 2)),
                    paste("SD:", round(sd(edu_group$`Comp AVG Grade`), 2))))
  
  # Group-level summary stats
  edu_group %>%
    group_by(group) %>%
    summarise(
      Min = min(`Comp AVG Grade`, na.rm = TRUE),
      Q1 = quantile(`Comp AVG Grade`, 0.25, na.rm = TRUE),
      Median = median(`Comp AVG Grade`, na.rm = TRUE),
      Q3 = quantile(`Comp AVG Grade`, 0.75, na.rm = TRUE),
      Max = max(`Comp AVG Grade`, na.rm = TRUE),
      Mean = mean(`Comp AVG Grade`, na.rm = TRUE),
      SD = sd(`Comp AVG Grade`, na.rm = TRUE),
      n = n()
    )
  
  # Enhanced boxplot with group means overlaid as red dots and lines
  ggplot(edu_group, aes(x = group, y = `Comp AVG Grade`, fill = group)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", color = "black", size = 3) +  # Red dot for group mean
    stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linetype = "dashed") +  # Line across means
    labs(title = "Comp AVG Grade by Recommendation Group",
         x = "Followed Recommendation",
         y = "GPA Grading Scale") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # QQ plot
  ggplot(edu_group, aes(sample = `Comp AVG Grade`)) +
    stat_qq_band(aes(fill = `Followed Recommend`), alpha = 0.2) +  # Confidence band
    stat_qq_point() +  # Q-Q points
    stat_qq_line() +   # Q-Q line
    facet_wrap(~ `Followed Recommend`) +
    labs(title = "Q-Q Plots with Confidence Bands by Group",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
  
  # Shapiro-Wilk test for normality per group
  edu_group %>%
    group_by(group) %>%
    summarise(p_value = shapiro.test(`Comp AVG Grade`)$p.value)
  
  # nonparametric kruskal wallis test
  kruskal.test(`Comp AVG Grade` ~ group, data = edu_group)
  
  
  # One-Way ANOVA
  # anova2 <- aov(`Comp AVG Grade` ~ group, data = edu_group)
  # summary(anova2)
  
  # Post-hoc (if ANOVA is significant)
  # TukeyHSD(anova2)

  # Q#03; Variables Involved: Portfolio Total Score, College; Method: One-Way Anova
    
  # Define Comp AVG Grade (CG) and College
  edu_group$`Portfolio Total Score`
  edu_group$College <- as.factor(edu_group$College)
  
  # Summary statistics (overall)
  summary(edu_group$`Portfolio Total Score`)
  sd(edu_group$`Portfolio Total Score`)
  
  # Boxplot with mean and SD displayed in the legend
  boxplot(edu_group$`Portfolio Total Score`,
          ylab = "Portfolio Total Score",
          main = "PTS of Students")
  legend("topright",
         legend = c(paste("Mean:", round(mean(edu_group$`Portfolio Total Score`), 2)),
                    paste("SD:", round(sd(edu_group$`Portfolio Total Score`), 2))))
  
  # boxplot with group means overlaid as dots and lines
  ggplot(edu_group, aes(x = College, y = `Portfolio Total Score`, fill = College)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
    stat_summary(fun = mean, aes(group = 1), geom = "line", color = "black", linetype = "dashed") +
    labs(title = "Portfolio Total Score by College",
         x = "College",
         y = "Portfolio Total Score") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Group-level summary stats
  edu_group %>%
    group_by(College) %>%
    summarise(
      Min = min(`Portfolio Total Score`, na.rm = TRUE),
      Q1 = quantile(`Portfolio Total Score`, 0.25, na.rm = TRUE),
      Median = median(`Portfolio Total Score`, na.rm = TRUE),
      Q3 = quantile(`Portfolio Total Score`, 0.75, na.rm = TRUE),
      Max = max(`Portfolio Total Score`, na.rm = TRUE),
      Mean_PTS = mean(`Portfolio Total Score`, na.rm = TRUE),
      SD_PTS = sd(`Portfolio Total Score`, na.rm = TRUE),
      n = n()
    )
  
  library(qqplotr)
  
  # QQ plot
  ggplot(edu_group, aes(sample = `Portfolio Total Score`)) +
    stat_qq_band(aes(fill = `College`), alpha = 0.2) +  # Confidence band
    stat_qq_point() +  # Q-Q points
    stat_qq_line() +   # Q-Q line
    facet_wrap(~ `College`) +
    labs(title = "Q-Q Plots with Confidence Bands by College",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
  
  # Shapiro-Wilk test for normality per College
  edu_group %>%
    group_by(College) %>%
    summarise(p_value = shapiro.test(`Portfolio Total Score`)$p.value)
  
  # One-Way ANOVA
  anova3 <- aov(`Portfolio Total Score` ~ College, data = edu_group)
  summary(anova3)
  
  # Post-hoc (if ANOVA is significant)
  TukeyHSD(anova3)

  
  # Q#04; Variables Involved: Comp AVG Grade, Gender; Method: independent 2-sample t-test
  
  # Define Comp AVG Grade (CG)
  edu_group$`Comp AVG Grade`
  edu_group$Sex <- as.factor(edu_group$Sex, levels = c(1, 2), labels = c("Female", "Male"))
  
  # Summary statistics (overall)
  summary(edu_group$`Comp AVG Grade`)
  sd(edu_group$`Comp AVG Grade`)
  
  # Boxplot with mean and SD displayed in the legend
  boxplot(edu_group$`Comp AVG Grade`,
          ylab = "Comp AVG Grade",
          main = "Comp AVG Grade of Students")
  legend("topright",
         legend = c(paste("Mean:", round(mean(edu_group$`Comp AVG Grade`), 2)),
                    paste("SD:", round(sd(edu_group$`Comp AVG Grade`), 2))))
  
  # Enhanced boxplot with Gender means overlaid as red dots and lines
  ggplot(edu_group, aes(x = Sex, y = `Comp AVG Grade`, fill = Sex)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", color = "black", size = 3) +  # Red dot for group mean
    stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linetype = "dashed") +  # Line across means
    labs(title = "Comp AVG Grade by Gender",
         x = "Gender",
         y = "Comp AVG Grade") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Group-level summary stats
  edu_group %>%
    group_by(Sex) %>%
    summarise(
      Min = min(`Comp AVG Grade`, na.rm = TRUE),
      Q1 = quantile(`Comp AVG Grade`, 0.25, na.rm = TRUE),
      Median = median(`Comp AVG Grade`, na.rm = TRUE),
      Q3 = quantile(`Comp AVG Grade`, 0.75, na.rm = TRUE),
      Max = max(`Comp AVG Grade`, na.rm = TRUE),
      Mean = mean(`Comp AVG Grade`, na.rm = TRUE),
      SD = sd(`Comp AVG Grade`, na.rm = TRUE),
      n = n()
    )
  
  # qq plot
  ggplot(edu_group, aes(sample = `Comp AVG Grade`)) +
    stat_qq_band(aes(fill = `Sex`), alpha = 0.2) +  # Confidence band
    stat_qq_point() +  # Q-Q points
    stat_qq_line() +   # Q-Q line
    facet_wrap(~ `Sex`) +
    labs(title = "Q-Q Plots with Confidence Bands by Gender",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
  
  # Shapiro-Wilk test for normality per group
  edu_group %>%
    group_by(Sex) %>%
    summarise(p_value = shapiro.test(`Comp AVG Grade`)$p.value)
  
  # fails normality 
  wilcox.test(`Comp AVG Grade` ~ Sex, data = edu_group)
  
  # Q05: Variables Involved: Gender, Sequence; Method: Chi-Squared Test of Independence
  
  # Recode variables if needed
  edu_group$Sex <- as.factor(edu_group$Sex, levels = c(1, 2), labels = c("Female", "Male"))
  edu_group$Sequence <- factor(edu_group$Sequence)
  
  # create table
  gender_seq <- table(edu_group$Sex, edu_group$Sequence)
  gender_seq
  
  # test
  chisq.test(gender_seq)


  # Q06: Variables Involved: URM, Group; Method: Chi-Squared Test of Independence
  
  # variables 
  edu_group$URM <- factor(edu_group$URM, labels = c("Not URM", "URM"))  # Assuming 0 = Not URM, 1 = URM
  edu_group$group <- factor(edu_group$`Followed Recommend`)
  
  # Create contingency table
  urm_group <- table(edu_group$URM, edu_group$group)
  urm_group
  
  # Perform Chi-Squared Test of Independence
  chisq.test(urm_group)
  

  # Q07: Variables Involved: Portfolio Rubric Category Score, Gender, Rubric Category
  # Method: Two-way Anova
    
  library(tidyr)
  library(dplyr)
  
  test <- edu_group %>%
    pivot_longer(cols = c('Process Revision', 'Critical Reading', 'Rhetorical Analysis',
                          'Research', 'Style', 'Grammar'),
                 names_to = "Category",
                 values_to = "Score") %>%
    mutate(Gender = ifelse(Sex == 1, "Female", "Male"))
  
  mod1.2way <- aov(Score ~ Category*Sex, data=test)
  mod1.2way
  
  library(ggplot2)
  
  # Boxplot of Score by Rubric Category
  ggplot(test, aes(x = Category, y = Score, fill = Category)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", color = "black", size = 2) +  # Red dot for group mean
    stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linetype = "dashed") +  # Line across means
    labs(title = "Rubric Category vs. Portfolio Rubric Score",
         x = "Rubric Category",
         y = "Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Summary statistics whole gender
  test %>%
    group_by(Category) %>%
    summarise(
      n = n(),
      Min = min(Score, na.rm = TRUE),
      Q1 = quantile(Score, 0.25, na.rm = TRUE),
      Median = median(Score, na.rm = TRUE),
      Q3 = quantile(Score, 0.75, na.rm = TRUE),
      Max = max(Score, na.rm = TRUE),
      Mean = mean(Score, na.rm = TRUE),
      SD = sd(Score, na.rm = TRUE)
    ) %>%
    arrange(Category)
  
  library(dplyr)
  
  test$Gender <- factor(test$Gender, levels = c("Female", "Male"))
  test$Category <- factor(test$Category)
  
  # boxplot by gender
  ggplot(test, aes(x = Gender, y = Score, fill = Gender)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", color = "black", size = 2) +  # Red dot for group mean
    stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linetype = "dashed") +  # Line across means
    facet_wrap(~ Category, scales = "free_y") +
    labs(
      title = "Rubric Category Scores by Gender",
      x = "Gender",
      y = "Score"
    ) +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold"))
  
  # Summary statistics by gender
  test %>%
    group_by(Category, Gender) %>%
    summarise(
      n = n(),
      Min = min(Score, na.rm = TRUE),
      Q1 = quantile(Score, 0.25, na.rm = TRUE),
      Median = median(Score, na.rm = TRUE),
      Q3 = quantile(Score, 0.75, na.rm = TRUE),
      Max = max(Score, na.rm = TRUE),
      Mean = mean(Score, na.rm = TRUE),
      SD = sd(Score, na.rm = TRUE)
    ) %>%
    arrange(Category, Gender)
  
  # two-way interaction
  ggplot(test, aes(x = Category, y = Score, fill = as.factor(Gender))) +
    geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.6) +  # Boxplots with fill and transparency
    stat_summary(aes(group = Gender, color = as.factor(Gender)), fun = mean, geom = "line", 
                 position = position_dodge(width = 0.75), size = 1.2) +  # Mean interaction lines
    stat_summary(aes(group = Gender, color = as.factor(Gender)), fun = mean, geom = "point", 
                 position = position_dodge(width = 0.75), size = 3) +  # Mean points
    labs(title = "Interaction of Category and Gender", x = "Category", y = "Score", fill = "Gender", color = "Category") +
    theme_minimal()
  
  # Function to compute mean and sd
  compute_stats <- function(df, group_vars) {
    df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        Mean = mean(Score),
        SD = sd(Score),
        .groups = 'drop'
      )
  }
  
  # Compute statistics for two-way interactions
  stats_SG <- compute_stats(test, c("Category", "Gender"))
  stats_SG
  
  # residuals
  resid1 = mod1.2way$residuals
  
  qqnorm(resid1) # Q-Q plot
  qqline(resid1, col = "red") # Adds the reference line
  
  # shapiro test
  shapiro.test(resid1)
  
  # nonparametric
  # Now run ART ANOVA
  mod_art <- art(Score ~ Category * Gender, data = test)
  
  # Type III ANOVA table
  anova(mod_art)
  
  # post hoc-test
  art.con(mod_art, "Gender", adjust = "bonferroni")
  art.con(mod_art, "Category", adjust = "bonferroni")

  test$Gender <- factor(test$Gender, levels = c("Female", "Male"))
  
  # Q08: Variables: Portfolio Rubric Score, Portfolio Rubric Category;
  # Method: One-way ANOVA
  
  # Boxplot of Score by Rubric Category
  ggplot(test, aes(x = Category, y = Score, fill = Category)) +
    geom_boxplot() +
    stat_summary(fun = mean, geom = "point", color = "black", size = 2) +  # Red dot for group mean
    stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linetype = "dashed") +  # Line across means
    labs(title = "Rubric Category vs. Portfolio Rubric Score",
         x = "Rubric Category",
         y = "Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Summary statistics whole gender
  test %>%
    group_by(Category) %>%
    summarise(
      n = n(),
      Min = min(Score, na.rm = TRUE),
      Q1 = quantile(Score, 0.25, na.rm = TRUE),
      Median = median(Score, na.rm = TRUE),
      Q3 = quantile(Score, 0.75, na.rm = TRUE),
      Max = max(Score, na.rm = TRUE),
      Mean = mean(Score, na.rm = TRUE),
      SD = sd(Score, na.rm = TRUE)
    ) %>%
    arrange(Category)
  
  
  # qq plots of all categories
  ggplot(test, aes(sample = Score)) +
    stat_qq_band(aes(fill = Category), alpha = 0.2) + # Confidence band
    stat_qq_point() + # Points
    stat_qq_line() + # Q-Q line
    facet_wrap(~ Category) +
    labs(title = "Q-Q Plots with Confidence Bands by Category",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()  
  
  # shapiro test
  test %>%
    group_by(Category) %>%
    summarise(p_value = shapiro.test(Score)$p.value)
  
  test$Category <- factor(test$Category)
  
  # nonparametric kruskal wallis test
  mod_art8 <- art(Score ~ Category, data = test)
  kruskal.test(Score ~ Category, data = test)
  
  # Type III ANOVA table
  anova(mod_art8)
  
  # post hoc-test
  art.con(mod_art8, "Category", adjust = "bonferroni")
  

  # Q09: Variables: Porfolio Rubric Category Score, Group, Rubric Category 
  # Method: Two-Way Anova 
  
  test$`Followed Recommend` <- factor(test$`Followed Recommend`)
  
  # Boxplot of Score by Rubric Category by Group (`Followed Recommend`)
  ggplot(test, aes(x = Category, y = Score, fill = `Followed Recommend`)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    stat_summary(
      fun = mean, geom = "point", aes(group = `Followed Recommend`), shape = 21, color = "black",
      fill = "white", size = 2, position = position_dodge(width = 0.75)) +
    stat_summary( fun = mean, geom = "line", aes(group = `Followed Recommend`, color = `Followed Recommend`),
                  position = position_dodge(width = 0.75) ) +
    labs(
      title = "Rubric Category vs. Portfolio Rubric Score by Group",
      x = "Rubric Category",
      y = "Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Summary statistics whole sequence
  test %>%
    group_by(Category, `Followed Recommend`) %>%
    summarise(
      Min = min(Score, na.rm = TRUE),
      Q1 = quantile(Score, 0.25, na.rm = TRUE),
      Median = median(Score, na.rm = TRUE),
      Q3 = quantile(Score, 0.75, na.rm = TRUE),
      Max = max(Score, na.rm = TRUE),
      Mean = mean(Score, na.rm = TRUE),
      SD = sd(Score, na.rm = TRUE)
    ) %>%
    arrange(Category)
  
  
  mod9.2way <- aov(Score ~ Category*`Followed Recommend`,test)
  resid9 <- mod9.2way$residuals
  
  # qq plots of all categories by Group
  ggplot(test, aes(sample = Score)) +
    stat_qq_band(aes(fill = `Followed Recommend`), alpha = 0.2) + # Confidence band
    stat_qq_point() + # Points
    stat_qq_line() + # Q-Q line
    facet_wrap(~ `Followed Recommend`) +
    labs(title = "Q-Q Plots with Confidence Bands by Group",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal() 
  
  #shaprio test 
  test %>%
    group_by(`Followed Recommend`) %>%
    summarise(p_value = shapiro.test(resid9)$p.value)
  
  # Rename variable to avoid issues
  test$Followed_Recommend <- test$`Followed Recommend`
  
  # nonparametric
  # Now run ART ANOVA
  mod_art9 <- art(Score ~ Category*Followed_Recommend,test)
  
  # Type III ANOVA table
  anova(mod_art9)
  
  # post hoc-test
  art.con(mod_art9, "Category", adjust = "bonferroni")
  art.con(mod_art9, "Followed_Recommend", adjust = "bonferroni")
  
  
  # Q10: Variables Involved: Eng105 & 106 Grade, Course,
  # Method: Dependent 2 Sample T-Test
  
  test2 = edu_group
  
  # Add Student ID
  test2 <- test2 %>%
    mutate(Student_ID = row_number())
  
  # Pivot longer
  test2 <- test2 %>%
    pivot_longer(cols = c('ENG 105 Grade', 'ENG 106 Grade'),
                 names_to = "Course",
                 values_to = "Grade") %>%
    filter(!is.na(Grade))
  
  # Boxplot with mean and SD displayed in the legend (Both ENG 105 and 106)
  boxplot(test2$Grade,
          ylab = "Grade",
          main = "Grade Distributions for 2-quarter Sequences")
  legend("topright",
         legend = c(paste("Mean:", round(mean(test2$Grade), 2)),
                    paste("SD:", round(sd(test2$Grade), 2))))
  
  # grade distribution for all 2 quarter sequence 
  
  # Summary statistics
  test2 %>%
    summarise(
      Min = min(Grade, na.rm = TRUE),
      Q1 = quantile(Grade, 0.25, na.rm = TRUE),
      Median = median(Grade, na.rm = TRUE),
      Q3 = quantile(Grade, 0.75, na.rm = TRUE),
      Max = max(Grade, na.rm = TRUE),
      Mean = mean(Grade, na.rm = TRUE),
      SD = sd(Grade, na.rm = TRUE)
    )
  
  # Boxplot of Course Grade by Course (ENG 105 or 106)
  ggplot(test2, aes(x = Course, y = Grade, fill = Course)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    stat_summary(
      fun = mean, geom = "point", aes(group = Course), shape = 21, color = "black",
      fill = "white", size = 2, position = position_dodge(width = 0.75)) +
    stat_summary(
      fun = mean, geom = "line", aes(group = 1), 
      color = "black", size = 1, position = position_dodge(width = 0.75)
    ) +
    labs(
      title = "Course Grade on ENG 105 and 106",
      x = "Course",
      y = "Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  # Summary statistics
  test2 %>%
    group_by(Course) %>%
    summarise(
      Min = min(Grade, na.rm = TRUE),
      Q1 = quantile(Grade, 0.25, na.rm = TRUE),
      Median = median(Grade, na.rm = TRUE),
      Q3 = quantile(Grade, 0.75, na.rm = TRUE),
      Max = max(Grade, na.rm = TRUE),
      Mean = mean(Grade, na.rm = TRUE),
      SD = sd(Grade, na.rm = TRUE)
    )
  
  # create line for residuals
  line10 <- aov(Grade ~ Course, data=test2)
  resid10 <- line10$residuals
  
  qqnorm(resid10) # Q-Q plot
  qqline(resid10, col = "red") # Adds the reference line
  
  # ggplot of QQ plot
  library(ggpubr)
  
  ggqqplot(resid10, conf.int = TRUE) + ggtitle("Q-Q Plot of Residuals with Confidence Band")
  
  # shapiro test
  shapiro.test(resid10) # not normal
  
  # conduct nonparametric wilcoxn test
  # Pivot wider for paired t-test
  test2_wide <- test2 %>%
    pivot_wider(names_from = Course, values_from = Grade)
  
  # Normality of difference
  test2_wide <- test2_wide %>%
    mutate(Diff = `ENG 106 Grade` - `ENG 105 Grade`)
  
  shapiro.test(test2_wide$Diff)
  
  # nonparametric;  wilcoxon test
  
  # Create a new dataset by pivoting the data to wide format
  test2_wide <- test2 %>%
    pivot_wider(names_from = Course, values_from = Grade)
  
  # Compute the difference between ENG 106 and ENG 105 grades
  test2_wide <- test2_wide %>%
    mutate(Diff = `ENG 106 Grade` - `ENG 105 Grade`)
  
  # Check normality of the differences (Shapiro-Wilk test)
  shapiro.test(test2_wide$Diff)  # Perform Shapiro-Wilk test on the differences
  
  # Now perform the Wilcoxon signed-rank test
  wilcox.test(test2_wide$`ENG 105 Grade`, test2_wide$`ENG 106 Grade`, paired = TRUE)
  
  # Paired t-test
  # t.test(test2_wide$`ENG 105 Grade`, test2_wide$`ENG 106 Grade`, paired = TRUE)
  
  
  # Q11: Variables: ENG 100, 101, and 102 Grade
  # Method: Repeated Measures One Way ANOVA (nonparametric alternative: Friedman Test)
  
  test3 = edu_group
  
  # Add Student ID
  test3 <- test3 %>%
    mutate(Student_ID = row_number())
  
  # Pivot longer: Course and Grade
  test3 <- test3 %>%
    pivot_longer(cols = c('ENG 100 Grade', 'ENG 101 Grade', 'ENG 102 Grade'),
                 names_to = "Course",
                 values_to = "Grade") %>%
    filter(!is.na(Grade))  # remove NA grades
  
  # Boxplot with mean and SD displayed in the legend (Both ENG 105 and 106)
  boxplot(test3$Grade,
          ylab = "Grade",
          main = "Grade Distributions for 3-quarter Sequences")
  legend("topright",
         legend = c(paste("Mean:", round(mean(test3$Grade), 2)),
                    paste("SD:", round(sd(test3$Grade), 2))))
  
  # Summary statistics
  test3 %>%
    summarise(
      Min = min(Grade, na.rm = TRUE),
      Q1 = quantile(Grade, 0.25, na.rm = TRUE),
      Median = median(Grade, na.rm = TRUE),
      Q3 = quantile(Grade, 0.75, na.rm = TRUE),
      Max = max(Grade, na.rm = TRUE),
      Mean = mean(Grade, na.rm = TRUE),
      SD = sd(Grade, na.rm = TRUE)
    )
  
  # Boxplot by individual courses
  ggplot(test3, aes(x = Course, y = Grade, fill = Course)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    stat_summary(
      fun = mean, geom = "point", aes(group = Course), shape = 21, color = "black",
      fill = "white", size = 2, position = position_dodge(width = 0.75)
    ) +
    stat_summary(
      fun = mean, geom = "line", aes(group = 1),
      color = "black", size = 1, position = position_dodge(width = 0.75)
    ) +
    labs(
      title = "Grade Distributions by Course (ENG 100, ENG 101, ENG 102)",
      x = "Course",
      y = "Grade"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Summary statistics
  test3 %>%
    group_by(Course) %>%
    summarise(
      Min = min(Grade, na.rm = TRUE),
      Q1 = quantile(Grade, 0.25, na.rm = TRUE),
      Median = median(Grade, na.rm = TRUE),
      Q3 = quantile(Grade, 0.75, na.rm = TRUE),
      Max = max(Grade, na.rm = TRUE),
      Mean = mean(Grade, na.rm = TRUE),
      SD = sd(Grade, na.rm = TRUE)
    )
  
  # from the equation 
  model_lm <- lm(Grade ~ Course, data = test3)
  
  # Extract residuals
  resid11 <- residuals(model_lm)
  
  # Shapiro-Wilk test for normality of residuals
  shapiro.test(resid11)
  
  # Q-Q plot of residuals
  qqnorm(resid11)
  qqline(resid11, col = "red")
  
  # ggplot of QQ plot
  library(ggpubr)
  
  ggqqplot(resid11, conf.int = TRUE) + ggtitle("Q-Q Plot of Residuals with Confidence Band")
  
  
  # Nonparametric alternative: Friedman Test
  
  # Pivot wider for Friedman test (each row = one student, columns = ENG 100, 101, 102 grades)
  test3_wide <- test3 %>%
    pivot_wider(names_from = Course, values_from = Grade)
  
  # Conduct Friedman Test
  friedman.test(as.matrix(test3_wide[, c("ENG 100 Grade", "ENG 101 Grade", "ENG 102 Grade")]))
  
  # Q12: Variables: Eng 105 and 106 Grade, Course, Group;
  # Method: Repeated Measure Two-Way Anova
  
  library(nlme)
  
  # rename followed recommed
  test2 <- test2 %>%
    rename(Followed_Recommend = `Followed Recommend`)
  
  # Compute mean grades by Followed_Recommend
  mean_grades <- test2 %>%
    group_by(Followed_Recommend) %>%
    summarize(Grade = mean(Grade, na.rm = TRUE)) %>%
    mutate(Followed_Recommend = as.factor(Followed_Recommend))
  
  # Plot
  ggplot(test2, aes(x = Followed_Recommend, y = Grade, fill = Followed_Recommend)) +
    geom_boxplot(width = 0.6) +
    stat_summary(
      fun = mean, geom = "point", shape = 21, color = "black", fill = "white", size = 2
    ) +
    # Line connecting means
    geom_line(data = mean_grades, aes(x = as.numeric(Followed_Recommend), y = Grade),
              color = "black", size = 1, group = 1) +
    labs(
      title = "2-Quarter Sequences by Group",
      x = "Followed Recommendation",
      y = "Grade",
      fill = "Followed Recommendation"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  # Summary statistics
  test2 %>%
    group_by(Followed_Recommend) %>%
    summarise(
      Min = min(Grade, na.rm = TRUE),
      Q1 = quantile(Grade, 0.25, na.rm = TRUE),
      Median = median(Grade, na.rm = TRUE),
      Q3 = quantile(Grade, 0.75, na.rm = TRUE),
      Max = max(Grade, na.rm = TRUE),
      Mean = mean(Grade, na.rm = TRUE),
      SD = sd(Grade, na.rm = TRUE)
    )
  
  # Course by Grade
  ggplot(test2, aes(x = Course, y = Grade, fill = Followed_Recommend)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    stat_summary(
      fun = mean, geom = "point", aes(group = Followed_Recommend), 
      shape = 21, color = "black", fill = "white", size = 2,
      position = position_dodge(width = 0.75)
    ) +
    stat_summary(
      fun = mean, geom = "line", aes(group = Followed_Recommend),
      color = "black", size = 1,
      position = position_dodge(width = 0.75)
    ) +
    labs(
      title = "2-Quarter Sequences by Course & Group",
      x = "Course",
      y = "Grade",
      fill = "Followed Recommendation"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  # Summary statistics
  test2 %>%
    group_by(Course, Followed_Recommend) %>%
    summarise(
      Min = min(Grade, na.rm = TRUE),
      Q1 = quantile(Grade, 0.25, na.rm = TRUE),
      Median = median(Grade, na.rm = TRUE),
      Q3 = quantile(Grade, 0.75, na.rm = TRUE),
      Max = max(Grade, na.rm = TRUE),
      Mean = mean(Grade, na.rm = TRUE),
      SD = sd(Grade, na.rm = TRUE)
    )
  
  # Rename the column to remove the space
  colnames(test2)[colnames(test2) == "Followed Recommend"] <- "Followed_Recommend"
  
  # set as factor
  test2$Followed_Recommend <- as.factor(test2$Followed_Recommend)
  
  # linear model
  m.mod3 <- lme(Grade ~ Course * Followed_Recommend,  # interaction between Course and Group
                random = ~1 | Student_ID,
                data = test2)
  
  # Residuals and Shapiro-Wilk Test
  resid12 <- m.mod3$residuals
  shapiro.test(resid12)
  
  qqnorm(resid12)
  qqline(resid12, col = "red")
  
  library(ggpubr)
  library(ggplot2)
  
  # place as numerical; fix error
  resid12 <- as.numeric(resid12)
  
  ggqqplot(resid12, conf.int = TRUE) + ggtitle("Q-Q Plot of Residuals with Confidence Band")
  
  # not normal; ART ANOVA
  # fit the ART model
  
  test2$Course <- as.factor(test2$Course)
  Course <- test2$Course 
  
  art_model12 <- art(Grade ~ Course * Followed_Recommend + (1|Student_ID), data = test2)
  
  # run ANOVA on the aligned ranks
  anova(art_model12)
  
  art.con(art_model12, "Course", adjust = "bonferroni")
  art.con(art_model12, "Followed_Recommend", adjust = "bonferroni")

  ####
  # m.mod3 <- lme(Grade ~ Course * 'Followed Recommend', # Fixed effects: main + interaction
  #              random = ~1 | Student_ID, # Random intercept for repeated measures
  #              data = test2)
  
  # resid5 <- rm.mod3$residuals
  # shapiro.test(resid5)
  
  
  # Q13 Variables: Eng100, 101, 102 Grade, Course, Group;
  # Method: Repeated Measure Two Way Anova
  
  # Rename the column to remove the space
  colnames(test3)[colnames(test3) == "Followed Recommend"] <- "Followed_Recommend"
  
  # Compute mean grades by Followed_Recommend
  mean_grades <- test3 %>%
    group_by(Followed_Recommend) %>%
    summarize(Grade = mean(Grade, na.rm = TRUE))
  
  # Plot
  ggplot(test3, aes(x = Followed_Recommend, y = Grade, fill = Followed_Recommend)) +
    geom_boxplot(width = 0.6) +
    stat_summary(
      fun = mean, geom = "point", shape = 21, color = "black", fill = "white", size = 2
    ) +
    geom_line(data = mean_grades,
              aes(x = Followed_Recommend, y = Grade, group = 1),
              color = "black", size = 1) +
    labs(
      title = "3-Quarter Sequences by Group",
      x = "Followed Recommendation",
      y = "Grade",
      fill = "Followed Recommendation"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  
  # Summary statistics
  test3 %>%
    group_by(Followed_Recommend) %>%
    summarise(
      Min = min(Grade, na.rm = TRUE),
      Q1 = quantile(Grade, 0.25, na.rm = TRUE),
      Median = median(Grade, na.rm = TRUE),
      Q3 = quantile(Grade, 0.75, na.rm = TRUE),
      Max = max(Grade, na.rm = TRUE),
      Mean = mean(Grade, na.rm = TRUE),
      SD = sd(Grade, na.rm = TRUE)
    )
  
  # Course by Grade
  ggplot(test3, aes(x = Course, y = Grade, fill = Followed_Recommend)) +
    geom_boxplot(position = position_dodge(width = 0.75)) +
    stat_summary(
      fun = mean, geom = "point", aes(group = Followed_Recommend), 
      shape = 21, color = "black", fill = "white", size = 2,
      position = position_dodge(width = 0.75)
    ) +
    stat_summary(
      fun = mean, geom = "line", aes(group = Followed_Recommend),
      color = "black", size = 1,
      position = position_dodge(width = 0.75)
    ) +
    labs(
      title = "3-Quarter Sequences by Course & Group",
      x = "Course",
      y = "Grade",
      fill = "Followed Recommendation"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  # Summary statistics
  test3 %>%
    group_by(Course, Followed_Recommend) %>%
    summarise(
      Min = min(Grade, na.rm = TRUE),
      Q1 = quantile(Grade, 0.25, na.rm = TRUE),
      Median = median(Grade, na.rm = TRUE),
      Q3 = quantile(Grade, 0.75, na.rm = TRUE),
      Max = max(Grade, na.rm = TRUE),
      Mean = mean(Grade, na.rm = TRUE),
      SD = sd(Grade, na.rm = TRUE)
    )
  
  # set as factor
  test3$Followed_Recommend <- as.factor(test3$Followed_Recommend)
  test3$Course <- as.factor(test3$Course)
  
  rm.mod4 <- lme(Grade ~ Course * Followed_Recommend, # Fixed effects: main + interaction
                 random = ~1 | Student_ID, # Random intercept for repeated measures
                data = test3)
  
  resid13 <- rm.mod4$residuals
  
  qqnorm(resid13)
  qqline(resid13, col = "red")
  
  library(ggpubr)
  library(ggplot2)
  
  # place as numerical; fix error
  resid13 <- as.numeric(resid13)
  
  ggqqplot(resid13, conf.int = TRUE) + ggtitle("Q-Q Plot of Residuals with Confidence Band")
  
  shapiro.test(resid13) 
  
  # non parametric non normal
  rm.mod4 <- art(Grade ~ Followed_Recommend*Course + (1|Student_ID), test3)
  
  anova(rm.mod4)
  
  # Q14) Portfolio Total Score, Comp AVG Grade, Group
  # Method: Multiple Linear Regression with Interactions (parametric) 
  
  # Fit a multiple linear regression model with interaction
  model14 <- lm(`Portfolio Total Score` ~ `Comp AVG Grade` * `Followed Recommend`, data = edu_group)
  
  # Summary of the model
  summary(model14)
  
  # residual test
  resid14 <- model14$residuals
  
  qqnorm(resid14)
  qqline(resid14, col = "red")
  
  library(ggpubr)
  library(ggplot2)
  
  # place as numerical; fix error
  resid13 <- as.numeric(resid14)
  
  ggqqplot(resid14, conf.int = TRUE) + ggtitle("Q-Q Plot of Residuals with Confidence Band")
  
  # shapiro test
  shapiro.test(model14$residuals)
  
  # Best Subsets Regression
  # library(leaps)
  
  # regsubsets(`Portfolio Total Score` ~ `Comp AVG Grade` * `Followed Recommend`, data = edu_group)
  
  # anova of model
  anova(model14)
  
  # Q15) Variables: Portfolio Total Score, Comp AVG Grade, Group 
  # Method: Correlation coefficients (parametric) 
  
  # Compute correlations by Group
  edu_group %>%
    group_by(`Followed Recommend`) %>%
    summarize(
      correlation = cor(`Portfolio Total Score`, `Comp AVG Grade`, method = "pearson"),
      p_value = cor.test(`Portfolio Total Score`, `Comp AVG Grade`, method = "pearson")$p.value
    )
  
  
