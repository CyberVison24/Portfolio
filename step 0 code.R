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
library(vtable) #check
library(car) 
library(jtools) #check
library(lmtest) 
library(AICcmodavg) #check
library(ARTool)

knitr::opts_chunk$set(echo = TRUE) 

------------------------
  
  library(readr)
Emily_FF_clean <- read_csv("STA classes/STA 5900/Group Project 2/Emily_FF_clean.csv")
View(Emily_FF_clean)

# get the names
names(Emily_FF_clean)

# convert names to factors 
A <- Emily_FF_clean$Temperature
B <- Emily_FF_clean$Time
C <- Emily_FF_clean$PH
ABS <- Emily_FF_clean$ABS

A
B
C
ABS

# mean and standard deviation
mean(ABS); sd(ABS) # 1.47, 1.26

#plot for all ABS vals
boxplot(ABS, ylab = "ABS", main = "All Observations")
legend("topright",c("Mean: 1.47", "SD: 1.26")) 

# Temperature at 25°C, 50°C and 80°C

  par(mfcol=c(1,3)) 
  
  # Temperature at 25°C
  temp25_df = Emily_FF_clean %>% filter(A == 25) 
  temp25_df
  
  # mean and sd for temperature 25°C
  m_temp25 <- mean(temp25_df$ABS, na.rm = TRUE) 
  sd_temp25 <- sd(temp25_df$ABS, na.rm = TRUE) 
  
  m_temp25; sd_temp25 # 1.44, 1,27
  
  # plot for temperature 25°C
  attach(temp25_df) 
  plot1 <- boxplot(temp25_df$ABS, main="ABS at Temperature 25°C", ylab="Values", col="red" ) 
  legend("topright",c("Mean: 1.44", "SD: 1.27")) 
  detach(temp25_df) 
  
  # Temperature at 50°C
  temp50_df = Emily_FF_clean %>% filter(A == 50) 
  
  # mean and sd for temperature 50°C
  m_temp50 <- mean(temp50_df$ABS, na.rm = TRUE) 
  sd_temp50 <- sd(temp50_df$ABS, na.rm = TRUE) 
  
  m_temp50; sd_temp50 # 1.50, 1.32
  
  attach(temp50_df) 
  plot2 <- boxplot(temp50_df$ABS, main="ABS at Temperature 50°C", ylab="Values", col="blue" ) 
  legend("topright",c("Mean: 1.50", "SD: 1.32")) 
  detach(temp50_df) 
  
  # Temperature at 80°C
  temp80_df = Emily_FF_clean %>% filter(A == 80) 
  
  # mean and sd for temperature 80°C
  m_temp80 <- mean(temp80_df$ABS, na.rm = TRUE) 
  sd_temp80 <- sd(temp80_df$ABS, na.rm = TRUE) 
  
  m_temp80; sd_temp80   # 1.46, 1.24
  
  # plot for temperature 80°C
  attach(temp80_df) 
  plot3 <- boxplot(temp80_df$ABS, main="ABS at Temperature 80°C", ylab="Values", col="green" ) 
  legend("topright",c("Mean: 1.46", "SD: 1.24")) 
  detach(temp80_df) 
  
  # turn off n different plots
  dev.off()

ABS.Temp.25 <- temp25_df$ABS
ABS.Temp.50 <- temp50_df$ABS
ABS.Temp.80 <- temp80_df$ABS

# merged boxplot for temperature
data1 <- data.frame(ABS.Temp.25, ABS.Temp.50, ABS.Temp.80)
names(data1) <- c("25°C", "50°C", "80°C")  # Set proper column names

boxplot(data1, main = "ABS vs Temperature", xlab = "Celsius Temperature",
        ylab = "Values", col= c("red","blue","green"))

#time section 

  # Time (minutes) at 20, 40 and 60
  
  par(mfcol=c(1,3)) 
  
  # Time (minutes) at 20
  time20_df = Emily_FF_clean %>% filter(Time == 20) 
  
  # mean and sd for Time = 20
  m_time20 <- mean(time20_df$ABS, na.rm = TRUE) 
  sd_time20 <- sd(time20_df$ABS, na.rm = TRUE) 
  
  m_time20; sd_time20   # 1.34, 1.14
  
  plot4 <- boxplot(time20_df$ABS, main="ABS at 20 Minutes", ylab="Values", col="red" ) 
  legend("topright",c("Mean: 1.34", "SD: 1.14")) 
  
  
  # Time (minutes) at 40
  time40_df = Emily_FF_clean %>% filter(Time == 40) 
  
  # mean and sd for Time = 40
  m_time40 <- mean(time40_df$ABS, na.rm = TRUE) 
  sd_time40 <- sd(time40_df$ABS, na.rm = TRUE) 
  
  m_time40; sd_time40 # 1.48, 1.29
  
  plot5 <- boxplot(time40_df$ABS, main="ABS at 40 Minutes", ylab="Values", col="blue" ) 
  legend("topright",c("Mean: 1.48", "SD: 1.29")) 
  
  
  # Time (minutes) at 60
  time60_df = Emily_FF_clean %>% filter(Time == 60) 
  
  # mean and sd for Time = 60
  m_time60 <- mean(time60_df$ABS, na.rm = TRUE) 
  sd_time60 <- sd(time60_df$ABS, na.rm = TRUE) 
  
  m_time60; sd_time60 # 1.58, 1.36
  
  plot6<- boxplot(time60_df$ABS, main="ABS at 60 Minutes", ylab="Values", col="green" ) 
  legend("topright",c("Mean: 1.58", "SD: 1.36")) 
  
  # turn off n different plots
  dev.off()

ABS.Time.20 <- time20_df$ABS
ABS.Time.40 <- time40_df$ABS
ABS.Time.60 <- time60_df$ABS

# merged boxplot for time

# merged boxplot for temperature
data2 <- data.frame("20" = ABS.Time.20, "40" = ABS.Time.40, "60" = ABS.Time.60)
names(data2) <- c("20", "40", "60")  # Set proper column names

boxplot(data2, main = "ABS vs Time", xlab = "Time (minutes)",
        ylab = "values", col=c("red","blue","green"))


# pH at 4, 7 and 10 

par(mfcol=c(1,3)) 

  # pH at 4
  ph4_df = Emily_FF_clean %>% filter(PH == 4) 
  
  # mean and sd for ph = 4
  m_ph4 <- mean(ph4_df$ABS, na.rm = TRUE) 
  sd_ph4 <- sd(ph4_df$ABS, na.rm = TRUE) 
  
  m_ph4; sd_ph4  # 0.298, 0.062 
  
  plot7 <- boxplot(ph4_df$ABS, main="ABS at PH 4", ylab="Values", col="red" ) 
  legend("topright",c("Mean: 0.298", "SD: 0.062")) 
  
  # pH at 7
  ph7_df = Emily_FF_clean %>% filter(PH == 7) 
  
  # mean and sd for ph = 7
  m_ph7 <- mean(ph7_df$ABS, na.rm = TRUE) 
  sd_ph7 <- sd(ph7_df$ABS, na.rm = TRUE) 
  
  m_ph7; sd_ph7 # 0.979, 0.103
  
  plot8 <- boxplot(ph7_df$ABS, main="ABS at PH 7", ylab="Values", col="blue" ) 
  legend("topright",c("Mean: 0.979", "SD: 0.103")) 
  
  # pH at 10
  ph10_df = Emily_FF_clean %>% filter(PH == 10) 
  
  # mean and sd for ph = 10
  m_ph10 <- mean(ph10_df$ABS, na.rm = TRUE) 
  sd_ph10 <- sd(ph10_df$ABS, na.rm = TRUE) 
  
  m_ph10; sd_ph10 # 3.133, 0.537 
  
  plot9<- boxplot(ph10_df$ABS, main="ABS at PH 10", ylab="Values", col="green" ) 
  legend("topright",c("Mean, 3.133", "SD: 0.537")) 
  
  # turn off n different plots
  dev.off()

ABS.pH.4 <- ph4_df$ABS
ABS.pH.7 <- ph7_df$ABS
ABS.pH.10 <- ph10_df$ABS

# merged boxplot
data3<- data.frame("4" = ABS.pH.4, "7" = ABS.pH.7, "10" = ABS.pH.10)
names(data3) <- c("4", "7", "10")  # Set proper column names

boxplot(data3, main = "ABS vs pH", xlab = "Time (minutes)",
        ylab = "values", col=c("red","blue","green"))


# Two-Way Interaction and ANOVA model Code: 

temp <- as.factor(A)
time <- as.factor(B)
ph <- as.factor(C) 

par(mfrow=c(1,3), pin=c(2,2)) 

  # temp and time (A x B)
  interaction.plot(A, B, ABS, 
                   xlab = "Temperature (celsius)", ylab= "Mean of ABS",
                   trace.label = "Time (min)", 
                   main="Time and Temperature Interaction Plot", 
                   col = c("red","blue","green"),
                   ylim = c(0,6), cex.main = 1, legend=FALSE) 
  
  legend(x = 2.3, y = 6, legend = levels(temp), 
         col = c( "red", "blue", "green"), lty = 1:4, xpd = TRUE,
         inset = c(-0.1, 0), title = "Time") 
  
  #temp and pH level (A x C)
  
  interaction.plot(A, C, ABS,
                   xlab = "Temperature (celsius)", ylab= "Mean of ABS",
                   trace.label = "pH",main="Temperature and pH Interaction Plot", 
                   col = c("red","blue","green"),ylim = c(0,6), 
                   cex.main = 1,legend=FALSE) 
  
  legend(x = 2.3, y = 6, legend = levels(ph), col = c("red", "blue", "green"), 
         lty = 1:4, xpd = TRUE, inset = c(-0.1, 0), title = "pH") 
  
  # time and pH level (B x C)
  
  interaction.plot(B, C, ABS, xlab = "Time (min)", 
                   ylab= "Mean of ABS",trace.label = "pH",
                   main="Time and pH Interaction Plot" ,
                   col = c("red","blue","green"),
                   ylim = c(0,6), cex.main = 1,legend=FALSE) 
  
  legend(x = 2.3, y = 6, legend = levels(time), col = c("red", "blue", "green"),
         lty = 1:4, xpd = TRUE, inset = c(-0.1, 0), title = "pH") 
  
  # turn off n different plots
  dev.off()

# plot the two way interactions on the boxplots

# Convert to factors for plotting
A2 <- as.factor(Emily_FF_clean$Temperature)  #Temperature
B2 <- as.factor(Emily_FF_clean$Time)   # Time
C2 <- as.factor(Emily_FF_clean$PH)     # pH 

  # Plot boxplot with Temperature and Time interactions
  
  ggplot(Emily_FF_clean, aes(x = A2, y = ABS, fill = as.factor(B2))) +
    geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.6) +  # Boxplots with fill and transparency
    stat_summary(aes(group = B2, color = as.factor(B2)), fun = mean, geom = "line", 
                 position = position_dodge(width = 0.75), size = 1.2) +  # Mean interaction lines
    stat_summary(aes(group = B2, color = as.factor(B2)), fun = mean, geom = "point", 
                 position = position_dodge(width = 0.75), size = 3) +  # Mean points
    labs(title = "Interaction of Temperature and Time", x = "Temperature", y = "ABS", fill = "Time", color = "Time") +
    theme_minimal()
  
  # Plot boxplot with Temperature and pH interactions
  
  ggplot(Emily_FF_clean, aes(x = A2, y = ABS, fill = as.factor(C2))) +
    geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.6) +  # Boxplots with fill and transparency
    stat_summary(aes(group = C2, color = as.factor(C2)), fun = mean, geom = "line", 
                 position = position_dodge(width = 0.75), size = 1.2) +  # Mean interaction lines
    stat_summary(aes(group = C2, color = as.factor(C2)), fun = mean, geom = "point", 
                 position = position_dodge(width = 0.75), size = 3) +  # Mean points
    labs(title = "Interaction of Temperature and pH", x = "Time", y = "ABS", fill = "PH", color = "PH") +
    theme_minimal()
  
  # Plot boxplot with interaction lines for Time and pH
  ggplot(Emily_FF_clean, aes(x = B2, y = ABS, fill = as.factor(C2))) +
    geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.6) +  # Boxplots with fill and transparency
    stat_summary(aes(group = C2, color = as.factor(C2)), fun = mean, geom = "line", 
                 position = position_dodge(width = 0.75), size = 1.2) +  # Mean interaction lines
    stat_summary(aes(group = C2, color = as.factor(C2)), fun = mean, geom = "point", 
                 position = position_dodge(width = 0.75), size = 3) +  # Mean points
    labs(title = "Interaction of Time and pH", x = "Time", y = "ABS", fill = "PH", color = "PH") +
    theme_minimal()

#### 
# geom_boxplot() – Creates boxplots grouped by Temperature/pH and Time.
# geom_point() – Adds jittered points to avoid overlap.
# geom_line() – Draws interaction lines by computing the mean of ABS values within each group.
# scale_fill_brewer() and scale_color_brewer() – Adjusts the color palette.
# theme_minimal() – Provides a clean background.


# Three-Way Interaction Code:  

library(here) 

par(mfrow=c(1,3), pin=c(2,2)) 

  # Time and Temperature (A x B) at pH (C) levels (4, 7, 10)
  
  #pH 4 at (A x B) 
  ph4_df <- Emily_FF_clean %>% filter(PH == 4) 
  
  ABS.pH.4 <- ph4_df$ABS
  ABS.Time.4 <- ph4_df$Time
  ABS.Temp.4 <- ph4_df$Temperature
  
  interaction.plot(response = ABS.pH.4, x.factor = ABS.Time.4,
                   trace.factor = ABS.Temp.4, col = c( "red", "blue", "green"), 
                   lty = 1:4, xlab = "Time", ylab = "ABS", 
                   main = "Temperature and Time at pH = 4", ylim = c(0,6), 
                   cex.main = 1.5, trace.label = "Temp", legend= FALSE ) 
  
  legend(x = 2.5, y = 6, legend = levels(temp), 
         col = c( "red", "blue", "green"), lty = 1:4, xpd = TRUE, 
         inset = c(-0.1, 0), title = "Temp") 
  
  #pH 7 at (A x B) 
  ph7_df = Emily_FF_clean %>% filter(PH == 7) 
  
  ABS.pH.7 <- ph7_df$ABS
  ABS.Time.7 <- ph7_df$Time
  ABS.Temp.7 <- ph7_df$Temperature
  
  interaction.plot(response = ABS.pH.7, x.factor = ABS.Time.7,
                   trace.factor = ABS.Temp.7, col = c( "red", "blue", "green"), 
                   lty = 1:4, xlab = "Time", ylab = "ABS", 
                   main = "Temperature and Time at pH = 7", ylim = c(0,6), 
                   cex.main = 1.5, trace.label = "Temp", legend= FALSE )
  
  legend(x = 2.5, y = 6, legend = levels(temp), col = c( "red", "blue", "green"),
         lty = 1:4, xpd = TRUE, inset = c(-0.1, 0), title = "Temp") 
  
  #pH 10 at (A x B)  
  ph10_df = Emily_FF_clean %>% filter(PH == 10) 
  
  ABS.pH.10 <- ph10_df$ABS
  ABS.Time.10 <- ph10_df$Time
  ABS.Temp.10 <- ph10_df$Temperature
  
  interaction.plot(response = ABS.pH.10, x.factor = ABS.Time.10,
                   trace.factor = ABS.Temp.10, col = c( "red", "blue", "green"), 
                   lty = 1:4, xlab = "Time", ylab = "ABS", 
                   main = "Temperature and Time at pH = 10", ylim = c(0,6), 
                   cex.main = 1.5, trace.label = "Temp", legend= FALSE )
  
  legend(x = 2.5, y = 6, legend = levels(temp), 
         col = c( "red", "blue", "green"), lty = 1:4, xpd = TRUE,
         inset = c(-0.1, 0), title = "Temp") 
  
  # turn off n different plots
  dev.off()

# temperature and pH (A x C) interaction with time (B) at 20, 40, 60  

  par(mfrow=c(1,3), pin=c(2,2)) 
  
  #time = 20 at (A x C)  
  
  time20_df = Emily_FF_clean %>% filter(Time == 20) 
  
  ABS.Time.20 <- time20_df$ABS
  ABS.pH.20 <- time20_df$PH
  ABS.Temp.20 <- time20_df$Temperature
  
  interaction.plot(response = ABS.Time.20, x.factor = ABS.Temp.20, 
                   trace.factor = ABS.pH.20, col = c( "red", "blue", "green"), 
                   lty = 1:4, xlab = "Temperature", ylab = "ABS", 
                   main = "Temperature and PH at Time = 20", ylim = c(0,6),
                   cex.main = 1, legend= FALSE ) 
  
  legend(x = 2.5, y = 6, legend = levels(ph), col = c( "red", "blue", "green"), 
         lty = 1:4, xpd = TRUE, inset = c(-0.1, 0), title = "pH") 
  
  
  #time = 40 at (A x C)  
  time40_df = Emily_FF_clean %>% filter(Time == 40) 
  
  ABS.Time.40 <- time40_df$ABS
  ABS.pH.40 <- time40_df$PH
  ABS.Temp.40 <- time40_df$Temperature
  
  interaction.plot(response = ABS.Time.40, x.factor = ABS.Temp.40, 
                   trace.factor = ABS.pH.40, col = c("red", "blue", "green"), 
                   lty = 1:4, xlab = "Temperature", ylab = "ABS", 
                   main = "Temperature and PH at Time = 40", ylim = c(0,6),
                   cex.main = 1, legend= FALSE ) 
  
  legend(x = 2.5, y = 6, legend = levels(ph), col = c("red", "blue", "green"), 
         lty = 1:4, xpd = TRUE, inset = c(-0.1, 0), title = "pH")  
  
  
  #time = 60 at (A x C)  
  time60_df = Emily_FF_clean %>% filter(Time == 60) 
  
  ABS.Time.60 <- time60_df$ABS
  ABS.pH.60 <- time60_df$PH
  ABS.Temp.60 <- time60_df$Temperature
  
  interaction.plot(response = ABS.Time.60, x.factor = ABS.Temp.60, 
                   trace.factor = ABS.pH.60, col = c( "red", "blue", "green"), 
                   lty = 1:4, xlab = "Temperature", ylab = "ABS", 
                   main = "Temperature and PH at Time = 60", ylim = c(0,6),
                   cex.main = 1, legend= FALSE ) 
  
  legend(x = 2.5, y = 6, legend = levels(ph), col = c( "red", "blue", "green"), 
         lty = 1:4, xpd = TRUE, inset = c(-0.1, 0), title = "pH")  


# Time and pH (B x C) interaction with Temperature (A) at 25, 50, 80  
par(mfrow = c(1,3), pin = c(2,2)) 

  #temperature = 25 at (B x C)  
  temperature25_df = Emily_FF_clean %>% filter(Temperature == 25)
  
  ABS.Temperature.25 <- temperature25_df$ABS
  ABS.Time.25 <- temperature25_df$Time
  ABS.pH.25 <- temperature25_df$PH
  
  interaction.plot(response = ABS.Temperature.25, x.factor = ABS.Time.25,
                   trace.factor = ABS.pH.25,
                   col = c( "red", "blue", "green"), lty = 1:4, xlab = "Time",
                   ylab = "ABS", main = "Time and PH at Temperature = 25", 
                   ylim = c(0,6), cex.main = 1,legend= FALSE ) 
  
  legend(x = 2.5, y = 6, legend = levels(ph), col = c( "red", "blue", "green"), 
         lty = 1:4, xpd = TRUE, inset = c(-0.1, 0), title = "ph") 
  
  #temperature = 50 at (B x C)   
  temperature50_df = Emily_FF_clean %>% filter(Temperature == 50)
  
  ABS.Temperature.50 <- temperature50_df$ABS
  ABS.Time.50 <- temperature50_df$Time
  ABS.pH.50 <- temperature50_df$PH
  
  
  interaction.plot(response = ABS.Temperature.50, x.factor = ABS.Time.50,
                   trace.factor = ABS.pH.50,
                   col = c("red", "blue", "green"), lty = 1:4, xlab = "Time",
                   ylab = "ABS", main = "Time and PH at Temperature = 50", 
                   ylim = c(0,6), cex.main = 1,legend= FALSE ) 
  
  legend(x = 2.5, y = 6, legend = levels(ph), col = c( "red", "blue", "green"), 
         lty = 1:4, xpd = TRUE, inset = c(-0.1, 0), title = "ph") 
  
  #temperature = 80 at (B x C)  
  temperature80_df = Emily_FF_clean %>% filter(Temperature == 80) 
  
  ABS.Temperature.80 <- temperature80_df$ABS
  ABS.Time.80 <- temperature80_df$Time
  ABS.pH.80 <- temperature80_df$PH
  
  
  interaction.plot(response = ABS.Temperature.80, x.factor = ABS.Time.80,
                   trace.factor = ABS.pH.80,
                   col = c( "red", "blue", "green"), lty = 1:4, xlab = "Time",
                   ylab = "ABS", main = "Time and PH at Temperature = 80", 
                   ylim = c(0,6), cex.main = 1,legend= FALSE ) 
  
  legend(x = 2.5, y = 6, legend = levels(ph), col = c( "red","blue", "green"), 
         lty = 1:4, xpd = TRUE, inset = c(-0.1, 0), title = "pH") 
  
  # turn off n different plots
  dev.off()

# pH = 4, 7, 10; Time = 20, 40, 60; Temperature = 25, 50, 80
# Plot boxplots and overlay interaction lines

# Boxplots and interaction for AxB at different pH levels
par(mfrow = c(1, 3), pin = c(2, 2))

  # pH 4 at (A x B)
  ph4_df <- Emily_FF_clean %>% filter(PH == 4)
  
  ggplot(ph4_df, aes(x = Temperature, y = ABS, group = interaction(Temperature, Time))) +
    geom_boxplot(aes(fill = Time), alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", aes(group = Temperature),
                 size = 1.2, fill = "black", shape = 23) +
    stat_summary(fun = mean, geom = "line", aes(group = Time, color = Time),
                 size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Temperature and Time at pH = 4", x = 'Temperature',
         y = 'Absorbance (ABS)', color = 'Time', theme_minimal() )
  
  # pH 7 at (A x B)
  ph7_df <- Emily_FF_clean %>% filter(PH == 7)
  
  ggplot(ph7_df, aes(x = Temperature, y = ABS, group = interaction(Temperature, Time))) +
    geom_boxplot(aes(fill = Time), alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", aes(group = Temperature),
                 size = 1.2, fill = "black", shape = 23) +
    stat_summary(fun = mean, geom = "line", aes(group = Time, color = Time),
                 size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Temperature and Time at pH = 7", x = 'Temperature',
         y = 'Absorbance (ABS)', color = 'Time', theme_minimal() )
  
  # pH 10 at (A x B)
  ph10_df <- Emily_FF_clean %>% filter(PH == 10)
  
  ggplot(ph10_df, aes(x = Temperature, y = ABS, group = interaction(Temperature, Time))) +
    geom_boxplot(aes(fill = Time), alpha = 0.3) +
    stat_summary(fun = mean, geom = "point", aes(group = Temperature),
                 size = 1.2, fill = "black", shape = 23) +
    stat_summary(fun = mean, geom = "line", aes(group = Time, color = Time),
                 size = 1.2, alpha = 0.7, position = position_dodge(width = 0.8)) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Temperature and Time at pH = 10", x = 'Temperature',
         y = 'Absorbance (ABS)', color = 'Time', theme_minimal() )


# Interaction for AxC at different Time levels (20, 40, 60)
par(mfrow = c(1, 3), pin = c(2, 2))

  # Time = 20 at (A x C)
  time20_df <- Emily_FF_clean %>% filter(Time == 20)
  
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
  time40_df <- Emily_FF_clean %>% filter(Time == 40)
  
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
  time60_df <- Emily_FF_clean %>% filter(Time == 60)
  
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
par(mfrow = c(1, 3), pin = c(2, 2))

  # Temperature = 25 at (B x C)
  temperature25_df <- Emily_FF_clean %>% filter(Temperature == 25)
  
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
  temperature50_df <- Emily_FF_clean %>% filter(Temperature == 50)
  
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
  temperature80_df <- Emily_FF_clean %>% filter(Temperature == 80)
  
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










