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
emily <- read_csv("STA classes/STA 5900/Group Project 2/Emily_FF_clean.csv")
View(emily)

#remove entry column
emily <- emily[,!names(emily) %in% "...1"]

# see!!! 
View(emily)

# get the names
names(emily)

# convert names to factors 
A <- as.factor(emily$Temperature)
B <- as.factor(emily$Time)
C <- as.factor(emily$PH)
ABS <- emily$ABS

A
B
C
ABS

# part 1 Summarize and Visualize the Data: 
#   Summarize the dependent variable using descriptive statistics 
#   (five-number summary, mean, standard deviation) and graphical display
#     (boxplot; indicate the mean to each boxplot) (1) for all observations; 
#       (2) by each factor;

# (1) all observations

  # 5 number summary, mean and standard deviation
  summary(ABS)
  
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.2126  0.3533  0.9603  1.4667  2.8519  4.1962
  
  IQR(ABS)   # 2.50
  mean(ABS); sd(ABS) # 1.47, 1.26
  
  #plot for all ABS vals
  boxplot(ABS, ylab = "ABS", main = "All Observations")
  legend("topright",c("Mean: 1.467", "SD: 1.257")) 
  
# (2) each factor (3) = Temperature, Time and pH
  
#Temperature 
  
  # Temperature at 25°C
    temp25_df = emily %>% filter(A == 25) 
    temp25_df
    
    # mean and sd for temperature 25°C
    m_temp25 <- mean(temp25_df$ABS, na.rm = TRUE) 
    sd_temp25 <- sd(temp25_df$ABS, na.rm = TRUE) 
    
    m_temp25; sd_temp25 # 1.44, 1,27
    
    # plot for temperature 25°C
    # plot1 <- boxplot(temp25_df$ABS, main="ABS at Temperature 25°C", ylab="Values", col="red" ) 
    # legend("topright",c("Mean: 1.44", "SD: 1.27")) 
  
  # Temperature at 50°C
    temp50_df = emily %>% filter(A == 50) 
    
    # mean and sd for temperature 50°C
    m_temp50 <- mean(temp50_df$ABS, na.rm = TRUE) 
    sd_temp50 <- sd(temp50_df$ABS, na.rm = TRUE) 
    
    m_temp50; sd_temp50 # 1.50, 1.32
  
    # plot2 <- boxplot(temp50_df$ABS, main="ABS at Temperature 50°C", ylab="Values", col="blue" ) 
    # legend("topright",c("Mean: 1.50", "SD: 1.32")) 
  
  # Temperature at 80°C
    temp80_df = emily %>% filter(A == 80) 
    
    # mean and sd for temperature 80°C
    m_temp80 <- mean(temp80_df$ABS, na.rm = TRUE) 
    sd_temp80 <- sd(temp80_df$ABS, na.rm = TRUE) 
    
    m_temp80; sd_temp80   # 1.46, 1.24
    
    # plot for temperature 80°C
    # plot3 <- boxplot(temp80_df$ABS, main="ABS at Temperature 80°C", ylab="Values", col="green" ) 
    # legend("topright",c("Mean: 1.46", "SD: 1.24")) 
  

  # combine for Temperature
  ABS.Temp.25 <- temp25_df$ABS
  ABS.Temp.50 <- temp50_df$ABS
  ABS.Temp.80 <- temp80_df$ABS
  
  # merged boxplot for temperature;
  data1 <- data.frame(ABS.Temp.25, ABS.Temp.50, ABS.Temp.80)
  names(data1) <- c("25°C", "50°C", "80°C")  # Set proper column names
  
  # 5 number summary
  summary(data1) 
  
  # Compute means
  means1 <- c(m_temp25, m_temp50, m_temp80)
  
  # Boxplot for Temperature
  boxplot(data1, main = "ABS vs Temperature", xlab = "Temperature (Celsius)",
          ylab = "Values", col= c("red","blue","green"), names = c("25°C", "50°C", "80°C"))
  
  # Add points for the means
  points(1:3, means1, col="black", pch=16, cex=1.5)  # pch=16 for filled circles
  
  # Add a line connecting the means
  lines(1:3, means1, col="black", lwd=2, lty=1)  # lty=2 makes it dashed
  
  legend("topright", legend = c("1.438", "1.501", "1.461"), 
         fill = c("red", "blue", "green"), xpd = TRUE,
         inset = c(0, 0), title = "Mean:")
  
# Time (minutes) at 20, 40 and 60
  
  # Time (minutes) at 20
    time20_df = emily %>% filter(Time == 20) 
    
    # mean and sd for Time = 20
    m_time20 <- mean(time20_df$ABS, na.rm = TRUE) 
    sd_time20 <- sd(time20_df$ABS, na.rm = TRUE) 
    
    m_time20; sd_time20   # 1.34, 1.14
    
    #plot4 <- boxplot(time20_df$ABS, main="ABS at 20 Minutes", ylab="Values", col="red" ) 
    #legend("topright",c("Mean: 1.34", "SD: 1.14")) 
  
  
  # Time (minutes) at 40
    time40_df = emily %>% filter(Time == 40) 
    
    # mean and sd for Time = 40
    m_time40 <- mean(time40_df$ABS, na.rm = TRUE) 
    sd_time40 <- sd(time40_df$ABS, na.rm = TRUE) 
    
    m_time40; sd_time40 # 1.48, 1.29
    
    #plot5 <- boxplot(time40_df$ABS, main="ABS at 40 Minutes", ylab="Values", col="blue" ) 
    #legend("topright",c("Mean: 1.48", "SD: 1.29")) 
  
  # Time (minutes) at 60
    time60_df = emily %>% filter(Time == 60) 
    
    # mean and sd for Time = 60
    m_time60 <- mean(time60_df$ABS, na.rm = TRUE) 
    sd_time60 <- sd(time60_df$ABS, na.rm = TRUE) 
    
    m_time60; sd_time60 # 1.58, 1.36
    
    #plot6<- boxplot(time60_df$ABS, main="ABS at 60 Minutes", ylab="Values", col="green" ) 
    #legend("topright",c("Mean: 1.58", "SD: 1.36")) 
  
  # combine for Time
  ABS.Time.20 <- time20_df$ABS
  ABS.Time.40 <- time40_df$ABS
  ABS.Time.60 <- time60_df$ABS
  
  # Boxplot for Time
  data2 <- data.frame("20" = ABS.Time.20, "40" = ABS.Time.40, "60" = ABS.Time.60)
  names(data2) <- c("20", "40", "60")  # Set proper column names
  
  # 5 number summary
  summary(data2) 
  
  # Compute means
  means2 <- c(m_time20, m_time40, m_time60)
  
  # Boxplot for Time
  boxplot(data2, main = "ABS vs Time", xlab = "Time (minutes)",
          ylab = "Values", col= c("red","blue","green"), names = c("20", "40", "60"))
  
  # Add points for the means
  points(1:3, means2, col="black", pch=16, cex=1.5)  # pch=16 for filled circles
  
  # Add a line connecting the means
  lines(1:3, means2, col="black", lwd=2, lty=1)  # lty=2 makes it dashed
  
  legend("topright", legend = c("1.336", "1.480", "1.575"), 
         fill = c("red", "blue", "green"), xpd = TRUE,
         inset = c(0, 0), title = "Mean:")
  
  
# pH at 4, 7 and 10 
  
  # pH at 4
  ph4_df = emily %>% filter(PH == 4) 
  
  # mean and sd for ph = 4
  m_ph4 <- mean(ph4_df$ABS, na.rm = TRUE) 
  sd_ph4 <- sd(ph4_df$ABS, na.rm = TRUE) 
  
  m_ph4; sd_ph4  # 0.298, 0.062 
  
  #plot7 <- boxplot(ph4_df$ABS, main="ABS at PH 4", ylab="Values", col="red" ) 
  #legend("topright",c("Mean: 0.298", "SD: 0.062")) 
  
  # pH at 7
  ph7_df = emily %>% filter(PH == 7) 
  
  # mean and sd for ph = 7
  m_ph7 <- mean(ph7_df$ABS, na.rm = TRUE) 
  sd_ph7 <- sd(ph7_df$ABS, na.rm = TRUE) 
  
  m_ph7; sd_ph7 # 0.979, 0.103
  
  #plot8 <- boxplot(ph7_df$ABS, main="ABS at PH 7", ylab="Values", col="blue" ) 
  #legend("topright",c("Mean: 0.979", "SD: 0.103")) 
  
  # pH at 10
  ph10_df = emily %>% filter(PH == 10) 
  
  # mean and sd for ph = 10
  m_ph10 <- mean(ph10_df$ABS, na.rm = TRUE) 
  sd_ph10 <- sd(ph10_df$ABS, na.rm = TRUE) 
  
  m_ph10; sd_ph10 # 3.133, 0.537 
  
  #plot9<- boxplot(ph10_df$ABS, main="ABS at PH 10", ylab="Values", col="green" ) 
  #legend("topright",c("Mean: 3.133", "SD: 0.537")) 
  
  # combine for pH
  ABS.pH.4 <- ph4_df$ABS
  ABS.pH.7 <- ph7_df$ABS
  ABS.pH.10 <- ph10_df$ABS
  
  # merged boxplot
  data3<- data.frame("4" = ABS.pH.4, "7" = ABS.pH.7, "10" = ABS.pH.10)
  names(data3) <- c("4", "7", "10")  # Set proper column names
  
  # 5 number summary
  summary(data3) 
  
  # Compute means
  means3 <- c(m_ph4, m_ph7, m_ph10)
  
  # Boxplot for Temperature
  boxplot(data3, main = "ABS vs pH", xlab = "pH",
          ylab = "Values", col= c("red","blue","green"), names = c("4", "7", "10"))
  
  # Add points for the means
  points(1:3, means3, col="black", pch=16, cex=1.5)  # pch=16 for filled circles
  
  # Add a line connecting the means
  lines(1:3, means3, col="black", lwd=2, lty=1)  # lty=2 makes it dashed
  
  legend("topright", legend = c("0.292", "0.969", "3.133"), 
         fill = c("red", "blue", "green"), xpd = TRUE,
         inset = c(0, 0), title = "Mean:")

  # two way interactions 
  
    # Convert to factors for plotting
    A2 <- as.factor(emily$Temperature)  #Temperature
    B2 <- as.factor(emily$Time)   # Time
    C2 <- as.factor(emily$PH)     # pH 
    
    # Plot boxplot with Temperature and Time interactions
    
    ggplot(emily, aes(x = A2, y = ABS, fill = as.factor(B2))) +
      geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.6) +  # Boxplots with fill and transparency
      stat_summary(aes(group = B2, color = as.factor(B2)), fun = mean, geom = "line", 
                   position = position_dodge(width = 0.75), size = 1.2) +  # Mean interaction lines
      stat_summary(aes(group = B2, color = as.factor(B2)), fun = mean, geom = "point", 
                   position = position_dodge(width = 0.75), size = 3) +  # Mean points
      labs(title = "Interaction of Temperature and Time", x = "Temperature", y = "ABS", fill = "Time", color = "Time") +
      theme_minimal()
    
    # Plot boxplot with Temperature and pH interactions
    
    ggplot(emily, aes(x = A2, y = ABS, fill = as.factor(C2))) +
      geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.6) +  # Boxplots with fill and transparency
      stat_summary(aes(group = C2, color = as.factor(C2)), fun = mean, geom = "line", 
                   position = position_dodge(width = 0.75), size = 1.2) +  # Mean interaction lines
      stat_summary(aes(group = C2, color = as.factor(C2)), fun = mean, geom = "point", 
                   position = position_dodge(width = 0.75), size = 3) +  # Mean points
      labs(title = "Interaction of Temperature and pH", x = "Time", y = "ABS", fill = "PH", color = "PH") +
      theme_minimal()
    
    # Plot boxplot with interaction lines for Time and pH
    ggplot(emily, aes(x = B2, y = ABS, fill = as.factor(C2))) +
      geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.6) +  # Boxplots with fill and transparency
      stat_summary(aes(group = C2, color = as.factor(C2)), fun = mean, geom = "line", 
                   position = position_dodge(width = 0.75), size = 1.2) +  # Mean interaction lines
      stat_summary(aes(group = C2, color = as.factor(C2)), fun = mean, geom = "point", 
                   position = position_dodge(width = 0.75), size = 3) +  # Mean points
      labs(title = "Interaction of Time and pH", x = "Time", y = "ABS", fill = "PH", color = "PH") +
      theme_minimal()
    
  # three way interaction 
  
    # Interaction for AxB at different pH levels (4, 7, 10)
    
      # pH 4 at (A x B)
      ph4_df <- emily %>%
        filter(PH == 4) %>%
        mutate(Temperature = as.factor(Temperature), 
               Time = as.factor(Time))
      
      # Create the plot
      ggplot(ph4_df, aes(x = Temperature, y = ABS, group = interaction(Temperature, Time))) +
        geom_boxplot(aes(fill = Time), alpha = 0.3) +
        stat_summary(fun = mean, geom = "point", aes(group = Temperature),
                     size = 1.2, fill = "black", shape = 23) +
        stat_summary(fun = mean, geom = "line", aes(group = Time, color = Time),
                     size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
        scale_fill_brewer(palette = "Set1") +  # Works properly now
        scale_color_brewer(palette = "Set1") +
        labs(title = "Temperature and Time at pH = 4", x = 'Temperature',
             y = 'Absorbance (ABS)', color = 'Time', fill = "Time") +
        theme_minimal()
      
      # pH 7 at (A x B)
      ph7_df <- emily %>%
        filter(PH == 7) %>%
        mutate(Temperature = as.factor(Temperature), 
               Time = as.factor(Time))
      
      # Create the plot
      ggplot(ph7_df, aes(x = Temperature, y = ABS, group = interaction(Temperature, Time))) +
        geom_boxplot(aes(fill = Time), alpha = 0.3) +
        stat_summary(fun = mean, geom = "point", aes(group = Temperature),
                     size = 1.2, fill = "black", shape = 23) +
        stat_summary(fun = mean, geom = "line", aes(group = Time, color = Time),
                     size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
        scale_fill_brewer(palette = "Set1") +  # Works properly now
        scale_color_brewer(palette = "Set1") +
        labs(title = "Temperature and Time at pH = 7", x = 'Temperature',
             y = 'Absorbance (ABS)', color = 'Time', fill = "Time") +
        theme_minimal()
      
      # pH 10 at (A x B)
      ph10_df <- emily %>%
        filter(PH == 10) %>%
        mutate(Temperature = as.factor(Temperature), 
               Time = as.factor(Time))
      
      # Create the plot
      ggplot(ph10_df, aes(x = Temperature, y = ABS, group = interaction(Temperature, Time))) +
        geom_boxplot(aes(fill = Time), alpha = 0.3) +
        stat_summary(fun = mean, geom = "point", aes(group = Temperature),
                     size = 1.2, fill = "black", shape = 23) +
        stat_summary(fun = mean, geom = "line", aes(group = Time, color = Time),
                     size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
        scale_fill_brewer(palette = "Set1") +  # Works properly now
        scale_color_brewer(palette = "Set1") +
        labs(title = "Temperature and Time at pH = 10", x = 'Temperature',
             y = 'Absorbance (ABS)', color = 'Time', fill = "Time") +
        theme_minimal()
    
    # Interaction for AxC at different Time levels (20, 40, 60)
    
      # Time = 20 at (A x C)
      time20_df <- emily %>% filter(Time == 20) %>%
        mutate(Temperature = as.factor(Temperature), 
               PH = as.factor(PH))
      
      ggplot(time20_df, aes(x = Temperature, y = ABS, group = interaction(Temperature, PH))) +
        geom_boxplot(aes(fill = PH), alpha = 0.3) +
        stat_summary(fun = mean, geom = "point", aes(group = Temperature),
                     size = 1.2, fill = "black", shape = 23) +
        stat_summary(fun = mean, geom = "line", aes(group = PH, color = PH),
                     size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1") +
        labs(title = "Temperature and pH at Time = 20", x = 'Temperature',
             y = 'Absorbance (ABS)', theme_minimal() )
      
      # Time = 40 at (A x C)
      time40_df <- emily %>% filter(Time == 40) %>%
        mutate(Temperature = as.factor(Temperature), 
               PH = as.factor(PH))
      
      ggplot(time40_df, aes(x = Temperature, y = ABS, group = interaction(Temperature, PH))) +
        geom_boxplot(aes(fill = PH), alpha = 0.3) +
        stat_summary(fun = mean, geom = "point", aes(group = Temperature),
                     size = 1.2, fill = "black", shape = 23) +
        stat_summary(fun = mean, geom = "line", aes(group = PH, color = PH),
                     size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1") +
        labs(title = "Temperature and pH at Time = 40", x = 'Temperature',
             y = 'Absorbance (ABS)', theme_minimal() )
      
      # Time = 60 at (A x C)
      time60_df <- emily %>% filter(Time == 60) %>%
        mutate(Temperature = as.factor(Temperature), 
               PH = as.factor(PH))
      
      ggplot(time60_df, aes(x = Temperature, y = ABS, group = interaction(Temperature, PH))) +
        geom_boxplot(aes(fill = PH), alpha = 0.3) +
        stat_summary(fun = mean, geom = "point", aes(group = Temperature),
                     size = 1.2, fill = "black", shape = 23) +
        stat_summary(fun = mean, geom = "line", aes(group = PH, color = PH),
                     size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1") +
        labs(title = "Temperature and pH at Time = 60", x = 'Temperature',
             y = 'Absorbance (ABS)', theme_minimal() )
      
    
    # Interaction for BxC at different Temperature levels (25, 50, 80)
    
      # Temperature = 25 at (B x C)
      temperature25_df <- emily %>% filter(Temperature == 25) %>%
        mutate(Time = as.factor(Time), 
               PH = as.factor(PH))
      
      ggplot(temperature25_df, aes(x = Time, y = ABS, group = interaction(Time, PH))) +
        geom_boxplot(aes(fill = PH), alpha = 0.3) + 
        stat_summary(fun = mean, geom = "point", aes(group = Time), size = 1.2, 
                     fill = "black", shape = 23) +
        stat_summary(fun = mean, geom = "line", aes(group = PH, color = PH),
                     size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1") +
        labs(title = "Time and pH at Temperature = 25°C", x = 'Time',
             y = 'Absorbance (ABS)', theme_minimal() )
      
      # Temperature = 50 at (B x C)
      temperature50_df <- emily %>% filter(Temperature == 50) %>%
        mutate(Time = as.factor(Time), 
               PH = as.factor(PH))
      
      ggplot(temperature50_df, aes(x = Time, y = ABS, group = interaction(Time, PH))) +
        geom_boxplot(aes(fill = PH), alpha = 0.3) + 
        stat_summary(fun = mean, geom = "point", aes(group = Time), size = 1.2, 
                     fill = "black", shape = 23) +
        stat_summary(fun = mean, geom = "line", aes(group = PH, color = PH),
                     size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1") +
        labs(title = "Time and pH at Temperature = 50°C", x = 'Time',
             y = 'Absorbance (ABS)', theme_minimal() )
      
      # Temperature = 80 at (B x C)
      temperature80_df <- emily %>% filter(Temperature == 80) %>%
        mutate(Time = as.factor(Time), 
               PH = as.factor(PH))
      
      ggplot(temperature80_df, aes(x = Time, y = ABS, group = interaction(Time, PH))) +
        geom_boxplot(aes(fill = PH), alpha = 0.3) + 
        stat_summary(fun = mean, geom = "point", aes(group = Time), size = 1.2, 
                     fill = "black", shape = 23) +
        stat_summary(fun = mean, geom = "line", aes(group = PH, color = PH),
                     size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1") +
        labs(title = "Time and pH at Temperature = 80°C", x = 'Time',
             y = 'Absorbance (ABS)', theme_minimal() )
    
  
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
  stats_AB <- compute_stats(emily, c("Temperature", "Time"))
  stats_AC <- compute_stats(emily, c("Temperature", "PH"))
  stats_BC <- compute_stats(emily, c("Time", "PH"))
  
  # Compute statistics for three-way interaction
  stats_ABC <- compute_stats(emily, c("Temperature", "Time", "PH"))
  
  # Print results
  print("Mean and SD for Two-Way Interactions:")
  print(stats_AB)
  print(stats_AC)
  print(stats_BC)
  
  print("Mean and SD for Three-Way Interaction:")
  print(stats_ABC)
  print(stats_ABC, n = 27)
  
#Step 2, STD three-way factorial design

  emily$Temperature = as.factor(emily$Temperature)
  emily$Time = as.factor(emily$Time)
  emily$PH = as.factor(emily$PH)  
  
  # three way anova 
  three.way <- aov(ABS ~ A*B*C)
  anova(three.way)

  # Full model
  model <- lm(ABS ~ Temperature*Time*PH, data = emily)
  model
  
  # test the assumptions
  
    #Residuals 
    resd = residuals(model)
    resd
    
    plot(fitted(model), resd, xlab = "fitted values", 
         ylab = "residuals", main = "Full Model Residuals")
    abline(h=0)
    
    # Residuals boxplot 
    boxplot(resd, main = "Full Model Residuals", ylab = "Residuals")
    
    # Residuals histogram
    hist(resd, main = "Histogram of Residuals", ylab = "Residuals")
    
    # normality (shapiro-wilk test) normal vs not normal
    shapiro.test(resd)
    
    # constant variance (Breusch-Pagan test) constant vs non-constant
    bptest(model)
    
    # randomness test (Durbin-Watson test) random vs not random
    dwtest(model)
    
    # non normality 
    qqnorm(resd)
    qqline(resd)
  
# Step 3 (nonparametric three-way factorial design)

# NONPARAMETRIC TESTS
  
  # anova testing
  anova(model)
    
  #post hoc test; tukey's HSD test
  # used only for a valid model; if its not the relationship isn't valid
  # check residuals if its not normal; can't have assumptions on bad test.
  
  # Tukey's HSD test for 3 or more groups
  TukeyHSD(three.way)$A
  TukeyHSD(three.way)$B
  TukeyHSD(three.way)$C
  
# ARTool
  
  library(ARTool)
  library(emmeans)
  
  # convert factors
  library(ARTool)
  library(emmeans)
  
  # Convert factors directly within the data frame
  emily$Temperature <- as.factor(emily$Temperature)
  emily$Time <- as.factor(emily$Time)
  emily$PH <- as.factor(emily$PH)
  
  # use art on threeway
  art.mod <- art(ABS ~ PH*Temperature*Time, emily)
  art.mod

  # anova of three way
  anova(art.mod)  #PH, Time, PH:Temperature, PH:Time are significant
    
  # art.con for PH, Time, PH:Temperature, PH:Time significances
    
    # PH
    art.con(art.mod, "PH" ,adjust= "bonferroni")
    
    # Time
    art.con(art.mod,"Time",adjust= "bonferroni")
    
    # PH:Temperature
    art.con(art.mod, "PH:Temperature",adjust= "bonferroni")
    
    # PH:Time
    art.con(art.mod,"PH:Time",adjust= "bonferroni")
  
  
  

