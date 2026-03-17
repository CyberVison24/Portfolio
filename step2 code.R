library(readxl) 
library(readr)
library(ISLR) 
library(dplyr) 
library(ggplot2)
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

------------------------
  
library(readr)
emily_OED <- read_csv("STA classes/STA 5900/Group Project 2/Emily_OED.csv")
View(emily_OED)

# remove column "No."
emily_OED <- emily_OED[, !names(emily_OED) %in% "No."]
View(emily_OED)

# rename columns
emily_OED <- emily_OED %>% rename(Time = A)
emily_OED <- emily_OED %>% rename(PH = B)
emily_OED <- emily_OED %>% rename(Ratio = C)

View(emily_OED)

# Reshape the data: pivot K/S columns into a single column
library(tidyverse)

emily_OED <- emily_OED %>%
  pivot_longer(cols = starts_with("K/S"), 
               names_to = "K_S_Type", 
               values_to = "K_S_Value")

# View the transformed data
view(emily_OED)

# rename columns
emily_OED <- emily_OED %>% rename("KS" = K_S_Value )
emily_OED <- emily_OED %>% rename("K/S Type" = K_S_Type )

# View the transformed data
view(emily_OED)

# part 1 data

  # (1) convert names to factors 
  Time <- as.factor(emily_OED$Time)
  PH <- as.factor(emily_OED$PH)
  Ratio <- as.factor(emily_OED$Ratio)
  KS <- emily_OED$KS
  
  Time
  PH
  Ratio
  KS

  # 5 number summary, mean and standard deviation
  summary(KS)
  
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 4.095   6.029   7.533   7.602   9.167  12.040 
  
  IQR(KS)   # 3.14
  mean(KS); sd(KS) # 7.60, 2.02
  
  #plot for all ABS vals
  boxplot(KS, ylab = "Color Strength (K/S)", main = "Color Strength of Dye Samples")
  legend("topright",c("Mean: 7.60", "SD: 2.02")) 

  # (2) each factor Time, pH, Ratio
  
  #Time in minutes (30, 60, 90)
  
    # Time (minutes) at 30; level 1
      time30_df = emily_OED %>% filter(Time == 1) 
  
      # mean and sd for Time = 30
      m_time30 <- mean(time30_df$KS, na.rm = TRUE) 
      sd_time30 <- sd(time30_df$KS, na.rm = TRUE) 
  
      m_time30; sd_time30   # 5.44, 0.89
  
  
    # Time (minutes) at 60; level 2
      time60_df = emily_OED %>% filter(Time == 2) 
  
      # mean and sd for Time = 40
      m_time60 <- mean(time60_df$KS, na.rm = TRUE) 
      sd_time60 <- sd(time60_df$KS, na.rm = TRUE) 
  
      m_time60; sd_time60 # 8.09, 1.80
  
    # Time (minutes) at 90; level 3
      time90_df = emily_OED %>% filter(Time == 3) 
  
      # mean and sd for Time = 60
      m_time90 <- mean(time90_df$KS, na.rm = TRUE) 
      sd_time90 <- sd(time90_df$KS, na.rm = TRUE) 
  
      m_time90; sd_time90 # 9.27, 0.73
  
  # combine for Time
  KS.Time.30 <- time30_df$KS
  KS.Time.60 <- time60_df$KS
  KS.Time.90 <- time90_df$KS
  
  # Boxplot for Time
  data1 <- data.frame("30" = KS.Time.30, "60" = KS.Time.60, "90" = KS.Time.90)
  names(data1) <- c("30", "60", "90")  # Set proper column names
  
  # 5 number summary
  summary(data1) 
  
  # Compute means
  means1 <- c(m_time30, m_time60, m_time90)
  
  # Boxplot for Time
  boxplot(data1, main = "Color Strength by Time", xlab = "Time (minutes)",
          ylab = "Color Strength (K/S)", col= c("red", "green", "blue"), names = c("30", "60", "90"))
  
  # Add points for the means
  points(1:3, means1, col="grey", pch=16, cex=1.5)  # pch=16 for filled circles
  
  # Add a line connecting the means
  lines(1:3, means1, col="grey", lwd=2, lty=1)  # lty=2 makes it dashed
  
  legend("topright", legend = c("5.44", "8.10", "9.27"), 
         fill = c("red", "green", "blue"), xpd = TRUE,
         inset = c(0, 0), title = "Mean:")
  
  # pH at 4, 7 and 10 
  
    # pH at 4; level 1
      ph4_df = emily_OED %>% filter(PH == 1) 
    
      # mean and sd for ph = 4
      m_ph4 <- mean(ph4_df$KS, na.rm = TRUE) 
      sd_ph4 <- sd(ph4_df$KS, na.rm = TRUE) 
    
      m_ph4; sd_ph4  # 7.24, 2.34 

    # pH at 7; level 2
      ph7_df = emily_OED %>% filter(PH == 2) 
    
      # mean and sd for ph = 7
      m_ph7 <- mean(ph7_df$KS, na.rm = TRUE) 
      sd_ph7 <- sd(ph7_df$KS, na.rm = TRUE) 
    
      m_ph7; sd_ph7 # 8.56, 1.92
  
    # pH at 10
      ph10_df = emily_OED %>% filter(PH == 3) 
      
      # mean and sd for ph = 10
      m_ph10 <- mean(ph10_df$KS, na.rm = TRUE) 
      sd_ph10 <- sd(ph10_df$KS, na.rm = TRUE) 
      
      m_ph10; sd_ph10 # 7.00, 1.48
  
  # combine for pH
  KS.pH.4 <- ph4_df$KS
  KS.pH.7 <- ph7_df$KS
  KS.pH.10 <- ph10_df$KS
  
  # merged boxplot
  data2<- data.frame("4" = KS.pH.4, "7" = KS.pH.7, "10" = KS.pH.10)
  names(data2) <- c("4", "7", "10")  # Set proper column names
  
  # 5 number summary
  summary(data2) 
  
  # Compute means
  means2 <- c(m_ph4, m_ph7, m_ph10)
  
  # Boxplot for pH
  boxplot(data2, main = "K/S by pH", xlab = "pH",
          ylab = "Color Strength (K/S)", col= c("red", "green", "blue"), names = c("4", "7", "10"))
  
  # Add points for the means
  points(1:3, means2, col="grey", pch=16, cex=1.5)  # pch=16 for filled circles
  
  # Add a line connecting the means
  lines(1:3, means2, col="grey", lwd=2, lty=1)  # lty=2 makes it dashed
  
  legend("topright", legend = c("7.25", "8.56", "7.00"), 
         fill = c("red", "green", "blue"), xpd = TRUE,
         inset = c(0, 0), title = "Mean:")
  
  # Ratio at 70/60, 100/30, 130/0 
  
    # ratio at 70/60; level 1
      ratio1_df = emily_OED %>% filter(Ratio == 1) 
      
      # mean and sd for ratio at 70/60
      m_ratio1 <- mean(ratio1_df$KS, na.rm = TRUE) 
      sd_ratio1 <- sd(ratio1_df$KS, na.rm = TRUE) 
      
      m_ratio1; sd_ratio1  # 6.86, 2.12
    
    # ratio at 100/30; level 2
      ratio2_df = emily_OED %>% filter(Ratio == 2) 
      
      # mean and sd for ratio at 100/30
      m_ratio2 <- mean(ratio2_df$KS, na.rm = TRUE) 
      sd_ratio2 <- sd(ratio2_df$KS, na.rm = TRUE) 
      
      m_ratio2; sd_ratio2  # 7.45, 1.24
      
    # ratio at 130/0; level 3
      ratio3_df = emily_OED %>% filter(Ratio == 3) 
      
      # mean and sd for ratio at 130/0
      m_ratio3 <- mean(ratio3_df$KS, na.rm = TRUE) 
      sd_ratio3 <- sd(ratio3_df$KS, na.rm = TRUE) 
      
      m_ratio3; sd_ratio3  # 8.50, 2.31
    
  # combine for ratio
  KS.ratio.1 <- ratio1_df$KS
  KS.ratio.2 <- ratio2_df$KS
  KS.ratio.3 <- ratio3_df$KS
    
  # merged boxplot
  data3<- data.frame("70ml/60ml" = KS.ratio.1, "100ml/30ml" = KS.ratio.2,
                     "130ml/0ml" = KS.ratio.3)
  names(data3) <- c("70ml/60ml", "100ml/30ml", "130ml/0ml")  # Set proper column names
  
  # 5 number summary
  summary(data3) 
  
  # Compute means
  means3 <- c(m_ratio1, m_ratio2, m_ratio3)
  
  # Boxplot for Ratio
  boxplot(data3, main = "K/S by Ratio", xlab = "Ratio (Extraction/Water)",
          ylab = "Color Strength (K/S)", col= c("red", "green", "blue"), 
          names = c("70ml/60ml", "100ml/30ml", "130ml/0ml"))
  
  # Add points for the means
  points(1:3, means3, col="grey", pch=16, cex=1.5)  # pch=16 for filled circles
  
  # Add a line connecting the means
  lines(1:3, means3, col="grey", lwd=2, lty=1)  # lty=2 makes it dashed
  
  legend("topright", legend = c("6.86", "7.45", "8.50"), 
         fill = c("red", "green", "blue"), xpd = TRUE,
         inset = c(0, 0), title = "Mean:")

  # combine plots
  par(mfrow = c(1,3), pin = c(2,2))
  
    # Boxplot for Time
      boxplot(data1, main = "K/S by Time", xlab = "Time (minutes)",
              ylab = "Color Strength (K/S)", col= c("red", "green", "blue"), names = c("30", "60", "90"))
      
      # Add points for the means
      points(1:3, means1, col="grey", pch=16, cex=1.5)  # pch=16 for filled circles
      
      # Add a line connecting the means
      lines(1:3, means1, col="grey", lwd=2, lty=1)  # lty=2 makes it dashed
      
      legend("topleft", legend = c("5.44", "8.10", "9.27"), 
             fill = c("red", "green", "blue"), xpd = TRUE,
             inset = c(0, 0), title = "Mean:")
  
    # Boxplot for pH
      boxplot(data2, main = "K/S by pH", xlab = "pH",
              ylab = "Color Strength (K/S)", col= c("red", "green", "blue"), names = c("4", "7", "10"))
      
      # Add points for the means
      points(1:3, means2, col="grey", pch=16, cex=1.5)  # pch=16 for filled circles
      
      # Add a line connecting the means
      lines(1:3, means2, col="grey", lwd=2, lty=1)  # lty=2 makes it dashed
      
      legend("topright", legend = c("7.25", "8.56", "7.00"), 
             fill = c("red", "green", "blue"), xpd = TRUE,
             inset = c(0, 0), title = "Mean:")
  
  
    # Boxplot for Ratio
      boxplot(data3, main = "K/S by Ratio", xlab = "Ratio (Extraction/Water)",
              ylab = "Color Strength (K/S)", col= c("red", "green", "blue"), 
              names = c("70ml/60ml", "100ml/30ml", "130ml/0ml"))
      
      # Add points for the means
      points(1:3, means3, col="grey", pch=16, cex=1.5)  # pch=16 for filled circles
      
      # Add a line connecting the means
      lines(1:3, means3, col="grey", lwd=2, lty=1)  # lty=2 makes it dashed
      
      legend("topleft", legend = c("6.86", "7.45", "8.50"), 
             fill = c("red", "green", "blue"), xpd = TRUE,
             inset = c(0, 0), title = "Mean:")
  
  # turn off n different plots
  dev.off()
  
  # two way interactions 
  
  # Ensure categorical variables are treated as factors
  emily_OED$Time <- as.factor(emily_OED$Time)
  emily_OED$PH <- as.factor(emily_OED$PH)
  emily_OED$Ratio <- as.factor(emily_OED$Ratio)
  
  # Plot: Time and pH interactions
  twoplot1 = ggplot(emily_OED, aes(x = Time, y = ABS, fill = as.factor(PH))) +
    geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.6) +  
    stat_summary(aes(group = PH, color = as.factor(PH)), fun = mean, geom = "line", 
                 position = position_dodge(width = 0.75), size = 1.2) +  
    stat_summary(aes(group = PH, color = as.factor(PH)), fun = mean, geom = "point", 
                 position = position_dodge(width = 0.75), size = 3) +  
    scale_x_discrete(labels = c("1" = "30", "2" = "60", "3" = "90")) +  
    scale_fill_discrete(labels = c("1" = "4", "2" = "7", "3" = "10")) +  
    scale_color_discrete(labels = c("1" = "4", "2" = "7", "3" = "10")) +  
    labs(title = "Time and pH Interaction", x = "Time (minutes)", y = "K/S", fill = "pH", color = "pH") +
    theme_minimal() +
    theme(legend.position = c(0, 1), legend.justification = c(0, 1))
  
  # Plot: Time and Ratio interactions
  twoplot2 = ggplot(emily_OED, aes(x = Time, y = ABS, fill = as.factor(Ratio))) +
    geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.6) +  
    stat_summary(aes(group = Ratio, color = as.factor(Ratio)), fun = mean, geom = "line", 
                 position = position_dodge(width = 0.75), size = 1.2) +  
    stat_summary(aes(group = Ratio, color = as.factor(Ratio)), fun = mean, geom = "point", 
                 position = position_dodge(width = 0.75), size = 3) +  
    scale_x_discrete(labels = c("1" = "30", "2" = "60", "3" = "90")) +  
    scale_fill_discrete(labels = c("1" = "70ml/60ml", "2" = "100ml/30ml", "3" = "130ml/0ml")) +  
    scale_color_discrete(labels = c("1" = "70ml/60ml", "2" = "100ml/30ml", "3" = "130ml/0ml")) +  
    labs(title = "Time and Ratio Interaction", x = "Time (minutes)", y = "K/S", fill = "Ratio", color = "Ratio") +
    theme_minimal() +
    theme(legend.position = c(0, 1), legend.justification = c(0, 1))
  
  # Plot: pH and Ratio interactions
  twoplot3 = ggplot(emily_OED, aes(x = PH, y = ABS, fill = as.factor(Ratio))) +
    geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.6) +  
    stat_summary(aes(group = Ratio, color = as.factor(Ratio)), fun = mean, geom = "line", 
                 position = position_dodge(width = 0.75), size = 1.2) +  
    stat_summary(aes(group = Ratio, color = as.factor(Ratio)), fun = mean, geom = "point", 
                 position = position_dodge(width = 0.75), size = 3) +  
    scale_x_discrete(labels = c("1" = "4", "2" = "7", "3" = "10")) +  
    scale_fill_discrete(labels = c("1" = "70ml/60ml", "2" = "100ml/30ml", "3" = "130ml/0ml")) +  
    scale_color_discrete(labels = c("1" = "70ml/60ml", "2" = "100ml/30ml", "3" = "130ml/0ml")) +  
    labs(title = "pH and Ratio Interaction", x = "pH", y = "K/S", fill = "Ratio", color = "Ratio") +
    theme_minimal() +
    theme(legend.position = c(0, 1), legend.justification = c(0, 1))
  
  # Arrange plots in a 1x3 grid
  grid.arrange(twoplot1, twoplot2, twoplot3, ncol = 3)
  
  # Function to compute mean and sd
  compute_stats <- function(df, group_vars) {
    df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        Mean = mean(ABS),
        SD = sd(ABS),
        .groups = 'drop'
      )
  }
  
  # Compute statistics for two-way interactions
  stats_AB <- compute_stats(emily_OED, c("Time", "PH"))
  stats_AC <- compute_stats(emily_OED, c("Time", "Ratio"))
  stats_BC <- compute_stats(emily_OED, c("PH", "Ratio"))
  
  # Print results
  print("Mean and SD for Two-Way Interactions:")
  print(stats_AB)
  print(stats_AC)
  print(stats_BC)
  
  # three way interaction 
  
  # Rename factor levels for Time, PH, and Ratio
  emily <- emily_OED %>%
    mutate(Time = factor(Time, levels = c("1", "2", "3"), labels = c("30", "60", "90")),
           PH = factor(PH, levels = c("1", "2", "3"), labels = c("4", "7", "10")),
           Ratio = factor(Ratio, levels = c("1", "2", "3"), labels = c("70ml/60ml", "100ml/30ml", "130ml/0ml")))
  
  # Compute means for each Time, PH, and Ratio combination
  means_data <- emily %>%
    group_by(Time, PH, Ratio) %>%
    summarise(mean_ABS = mean(KS, na.rm = TRUE), .groups = 'drop')
  
  # Three-way interaction boxplot AxB at C levels
  ggplot(emily, aes(x = Time, y = KS, fill = PH)) +
    geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.75)) +  # Boxplots
    geom_point(data = means_data, aes(x = Time, y = mean_ABS, color = PH), 
               size = 3, position = position_dodge(width = 0.75)) +  # Mean dots on boxplots
    geom_line(data = means_data, aes(x = Time, y = mean_ABS, color = PH, group = PH), 
              size = 1, position = position_dodge(width = 0.75)) +  # Connecting means
    facet_wrap(~ Ratio) +  # Facet by Ratio levels
    labs(x = "Time (minutes)", y = "K/S", fill = "pH", color = "pH",
         title = "Time & pH at Different Ratios") +
    theme_minimal()
  
  # Three-way interaction boxplot AxC at B levels
  ggplot(emily, aes(x = Time, y = KS, fill = Ratio)) +
    geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.75)) +  # Boxplots
    geom_point(data = means_data, aes(x = Time, y = mean_ABS, color = Ratio), 
               size = 3, position = position_dodge(width = 0.75)) +  # Mean dots on boxplots
    geom_line(data = means_data, aes(x = Time, y = mean_ABS, color = Ratio, group = Ratio), 
              size = 1, position = position_dodge(width = 0.75)) +  # Connecting means
    facet_wrap(~ PH) +  # Facet by PH levels
    labs(x = "Time (minutes)", y = "K/S", fill = "Ratio", color = "Ratio",
         title = "Time & Ratio at Different pH Levels") +
    theme_minimal()
  
  # Three-way interaction boxplot with means and interaction lines
  ggplot(emily, aes(x = PH, y = KS, fill = Ratio)) +
    geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.75)) +  # Boxplots
    geom_point(data = means_data, aes(x = PH, y = mean_ABS, color = Ratio), 
               size = 3, position = position_dodge(width = 0.75)) +  # Mean dots on boxplots
    geom_line(data = means_data, aes(x = PH, y = mean_ABS, color = Ratio, group = Ratio), 
              size = 1, position = position_dodge(width = 0.75)) +  # Connecting means
    facet_wrap(~ Time) +  # Facet by Ratio levels
    labs(x = "pH", y = "K/S", fill = "Ratio", color = "Ratio",
         title = "pH & Ratio at Different Times") +
    theme_minimal()
    
  # Compute statistics for three-way interaction
  stats_ABC <- compute_stats(emily_OED, c("Time", "PH", "Ratio", "KS"))
  
  print(stats_ABC, n = 45)
  
# turn off n different plots
dev.off()

# part 2

# variables 
Time <- emily_OED$Time     # A
PH <- emily_OED$PH         # B
Ratio <- emily_OED$Ratio   # C

# check dot products
library(pracma)

#Full Factorial Design (All data)
# dot(Time,PH)    # AB
# dot(Time,Ratio) # AC
# dot(PH,Ratio)   # BC

# model
model <- lm(KS ~ Time*PH*Ratio)
summary(model)

# anova
full <- aov(model)
summary(full)

# post hoc test
TukeyHSD(full, "Time")
TukeyHSD(full, "PH")
TukeyHSD(full, "Ratio")

# residual analysis

  #Residuals 
  resd = residuals(model)
  resd
  
  plot(fitted(model), resd, xlab = "fitted values", 
       ylab = "residuals", main = "Residuals vs Fitted")
  abline(h=0)
  
  # Residuals boxplot 
  boxplot(resd, main = "Boxplot of Residuals", ylab = "Residuals")
  
  # Residuals histogram
  hist(resd, main = "Residuals of Histogram", ylab = "Residuals")
  
  # normality (shapiro-wilk test) normal vs not normal
  shapiro.test(resd) # normal
  
  # constant variance (Breusch-Pagan test) constant vs non-constant
  bptest(model) #constant
  
  # randomness test (Durbin-Watson test) random vs not random
  dwtest(model) # random 
  
  # non normality 
  qqnorm(resd)
  qqline(resd)

# part 3
  # model is normal stop here
  
  
  
  
  emily <- read_csv("STA classes/STA 5900/Group Project 2/Emily_OED.csv")
  View(emily)
  
  # remove column "No."
  emily <- emily[, !names(emily) %in% "No."]
  View(emily)
  
  # rename columns
  emily <- emily %>% rename(Time = A)
  emily <- emily %>% rename(PH = B)
  emily <- emily %>% rename(Ratio = C)
  
  View(emily)
  
  # change levels
  emily <- emily %>%
    mutate_all(~ case_when(
      . == 1 ~ -1,
      . == 2 ~ 0,
      . == 3 ~ 1,
      TRUE ~ . # Keep other values unchanged
    ))
  
  view(emily)
  
  # Reshape the data: pivot K/S columns into a single column
  library(tidyverse)
  
  emily <- emily %>%
    pivot_longer(cols = starts_with("K/S"), 
                 names_to = "K_S_Type", 
                 values_to = "K_S_Value")
  
  # View the transformed data
  view(emily)
  
  # rename columns
  emily <- emily %>% rename("KS" = K_S_Value )
  emily <- emily %>% rename("K/S Type" = K_S_Type )
  
  # View the transformed data
  view(emily)
  
  # Ratio 1 at (A x B)
  Ratio1_df <- emily %>%
    filter(Ratio == -1) %>%
    mutate(Time = as.factor(Time), 
           PH = as.factor(PH))
  
  Ratio1_df
  
  # Create the plot
  ggplot(Ratio1_df, aes(x = Time, y = KS, group = interaction(Time, PH))) +
    geom_boxplot(aes(fill = PH), alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", aes(group = Time),
                 size = 1.2, fill = "black", shape = 23) +
    stat_summary(fun = mean, geom = "line", aes(group = PH, color = PH),
                 size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette = "Set1") +  # Works properly now
    scale_color_brewer(palette = "Set1") +
    labs(title = "Time and PH at Ratio = 1", x = 'Time',
         y = 'K/S', color = 'PH', fill = "PH") +
    theme_minimal()
  
  # Ratio 2 at (A x B)
  Ratio2_df <- emily %>%
    filter(Ratio == 0) %>%
    mutate(Time = as.factor(Time), 
           PH = as.factor(PH))
  
  Ratio2_df
  
  # Create the plot
  ggplot(Ratio2_df, aes(x = Time, y = KS, group = interaction(Time, PH))) +
    geom_boxplot(aes(fill = PH), alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", aes(group = Time),
                 size = 1.2, fill = "black", shape = 23) +
    stat_summary(fun = mean, geom = "line", aes(group = PH, color = PH),
                 size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette = "Set1") +  # Works properly now
    scale_color_brewer(palette = "Set1") +
    labs(title = "Time and PH at Ratio = 1", x = 'Time',
         y = 'K/S', color = 'PH', fill = "PH") +
    theme_minimal()

  # Ratio 3 at (A x B)
  Ratio3_df <- emily %>%
    filter(Ratio == 1) %>%
    mutate(Time = as.factor(Time), 
           PH = as.factor(PH))
  
  Ratio3_df
  
  # Create the plot
  ggplot(Ratio3_df, aes(x = Time, y = KS, group = interaction(Time, PH))) +
    geom_boxplot(aes(fill = PH), alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", aes(group = Time),
                 size = 1.2, fill = "black", shape = 23) +
    stat_summary(fun = mean, geom = "line", aes(group = PH, color = PH),
                 size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette = "Set1") +  # Works properly now
    scale_color_brewer(palette = "Set1") +
    labs(title = "Time and PH at Ratio = 1", x = 'Time',
         y = 'K/S', color = 'PH', fill = "PH") +
    theme_minimal()
  
  # PH 1 at (A x C)
  PH1_df <- emily %>%
    filter(PH == -1) %>%
    mutate(Time = as.factor(Time), 
           Ratio = as.factor(Ratio))
  
  PH1_df
  
  # Create the plot
  ggplot(PH1_df, aes(x = Time, y = KS, group = interaction(Time, Ratio))) +
    geom_boxplot(aes(fill = Ratio), alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", aes(group = Time),
                 size = 1.2, fill = "black", shape = 23) +
    stat_summary(fun = mean, geom = "line", aes(group = Ratio, color = Ratio),
                 size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette = "Set1") +  # Works properly now
    scale_color_brewer(palette = "Set1") +
    labs(title = "Time and Ratio at PH = 1", x = 'Time',
         y = 'K/S', color = 'Ratio', fill = "Ratio") +
    theme_minimal()
  
  # PH 2 at (A x C)
  PH2_df <- emily %>%
    filter(PH == 0) %>%
    mutate(Time = as.factor(Time), 
           Ratio = as.factor(Ratio))
  
  PH2_df
  
  # Create the plot
  ggplot(PH2_df, aes(x = Time, y = KS, group = interaction(Time, Ratio))) +
    geom_boxplot(aes(fill = Ratio), alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", aes(group = Time),
                 size = 1.2, fill = "black", shape = 23) +
    stat_summary(fun = mean, geom = "line", aes(group = Ratio, color = Ratio),
                 size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette = "Set1") +  # Works properly now
    scale_color_brewer(palette = "Set1") +
    labs(title = "Time and Ratio at PH = 2", x = 'Time',
         y = 'K/S', color = 'Ratio', fill = "Ratio") +
    theme_minimal()
  
  # PH 2 at (A x C)
  PH3_df <- emily %>%
    filter(PH == 1) %>%
    mutate(Time = as.factor(Time), 
           Ratio = as.factor(Ratio))
  
  PH3_df
  
  # Create the plot
  ggplot(PH3_df, aes(x = Time, y = KS, group = interaction(Time, Ratio))) +
    geom_boxplot(aes(fill = Ratio), alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", aes(group = Time),
                 size = 1.2, fill = "black", shape = 23) +
    stat_summary(fun = mean, geom = "line", aes(group = Ratio, color = Ratio),
                 size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette = "Set1") +  # Works properly now
    scale_color_brewer(palette = "Set1") +
    labs(title = "Time and Ratio at PH = 3", x = 'Time',
         y = 'K/S', color = 'Ratio', fill = "Ratio") +
    theme_minimal()
  
  # Time = 1 at (B x C)
  Time1_df <- emily %>%
    filter(Time == -1) %>%
    mutate(PH = as.factor(PH), 
           Ratio = as.factor(Ratio))
  
  Time1_df
  
  # Create the plot
  ggplot(Time1_df, aes(x = PH, y = KS, group = interaction(PH, Ratio))) +
    geom_boxplot(aes(fill = Ratio), alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", aes(group = PH),
                 size = 1.2, fill = "black", shape = 23) +
    stat_summary(fun = mean, geom = "line", aes(group = Ratio, color = Ratio),
                 size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette = "Set1") +  # Works properly now
    scale_color_brewer(palette = "Set1") +
    labs(title = "pH and Ratio at Time = 1", x = 'pH',
         y = 'K/S', color = 'Ratio', fill = "Ratio") +
    theme_minimal()
  
  # Time = 2 at (B x C)
  Time2_df <- emily %>%
    filter(Time == 0) %>%
    mutate(PH = as.factor(PH), 
           Ratio = as.factor(Ratio))
  
  Time2_df
  
  # Create the plot
  ggplot(Time2_df, aes(x = PH, y = KS, group = interaction(PH, Ratio))) +
    geom_boxplot(aes(fill = Ratio), alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", aes(group = PH),
                 size = 1.2, fill = "black", shape = 23) +
    stat_summary(fun = mean, geom = "line", aes(group = Ratio, color = Ratio),
                 size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette = "Set1") +  # Works properly now
    scale_color_brewer(palette = "Set1") +
    labs(title = "pH and Ratio at Time = 2", x = 'pH',
         y = 'K/S', color = 'Ratio', fill = "Ratio") +
    theme_minimal()
  
  # Time = 3 at (B x C)
  Time3_df <- emily %>%
    filter(Time == 1) %>%
    mutate(PH = as.factor(PH), 
           Ratio = as.factor(Ratio))
  
  Time3_df
  
  # Create the plot
  ggplot(Time3_df, aes(x = PH, y = KS, group = interaction(PH, Ratio))) +
    geom_boxplot(aes(fill = Ratio), alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", aes(group = PH),
                 size = 1.2, fill = "black", shape = 23) +
    stat_summary(fun = mean, geom = "line", aes(group = Ratio, color = Ratio),
                 size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette = "Set1") +  # Works properly now
    scale_color_brewer(palette = "Set1") +
    labs(title = "pH and Ratio at Time = 3", x = 'pH',
         y = 'K/S', color = 'Ratio', fill = "Ratio") +
    theme_minimal()
  
  