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
library(qqplotr)
library(ggpubr)
library(nlme)
library(ggplot2)
library(AICcmodavg) 
library(ARTool)

knitr::opts_chunk$set(echo = TRUE) 


library(readr)
USpop <- read_csv("STA 5900/individual project/USpop.csv")
View(USpop)

# name of variables
names(USpop)

# rescale the year
USpop$year_scaled <- USpop$Year - 1790
year_scaled <- USpop$year_scaled

View(USpop)

# rename variables
year <- USpop$Year
population <- USpop$Population

# boxplot of population
boxplot(population, ylab = "Millions", main = "Population Boxplot")
legend("topright",
              legend = c(paste("Mean:", round(mean(population), 2)),
                         paste("SD:", round(sd(population), 2) ) )) 

# summary statistics
summary(population) 
sd(population) #70.8903

# Simple regression linear model (1); Population is dependent, Year is independent
  model1 <- lm(population ~ year_scaled, data = USpop)
  summary(model1)
  
  # conduct anova
  anova(model1)
  
  # plots
  plot(year_scaled, population, xlab = "Year Scaled (0 - 190)",
       ylab = "population (millions)", main = "SLR Model: Population by Year")
  abline(model1)
  
  # residual test
  resid1 <- model1$residuals
  
  # qq plot
  qqnorm(resid1, main = "QQ plot for SLR Model")
  qqline(resid1)
  
  # shapiro test
  shapiro.test(resid1) #p = 0.02443; reject normality
  
  # residual plot
  plot(resid1, xlab = "fitted values", ylab = "residuals", main = "Residual Plot for SLR Model")
  abline(h = 0)
  
  # bp test
  bptest(model1) #p=0.7609; constant variance
  
  # AIC 
  AIC(model1)

#model 2: WII effect

  # Add a dummy variable for 1940 and 1950 (150, 160)
  USpop$WWII_effect <- ifelse(USpop$Year %in% c(1940, 1950), 1, 0)
  View(USpop)
  WWII <- USpop$WWII_effect
  
  # new model with dummy (WII effect)
  model2 <- lm(Population ~ year_scaled + WWII, data = USpop)
  summary(model2)
  
  # conduct anova
  anova(model2)
  
  # Generate predicted values from model2
  USpop$pred2 <- predict(model2)
  
  # Plot actual vs. predicted
  plot(USpop$year_scaled, USpop$Population,
       main = "WWII Model: Population by Year",
       xlab = "Year Scaled (0 - 190)",
       ylab = "Population (millions)",
       pch = 16, col = "black")
  
  # Add fitted line (with WWII effect)
  lines(USpop$year_scaled, USpop$pred2, col = "blue", lwd = 2)

  # residual test
  resid2 <- model2$residuals
  
  # qq plot
  qqnorm(resid2, main = "QQ plot for WWII model")
  qqline(resid2)
  
  # shapiro test
  shapiro.test(resid2) #p = 0.0412; reject normality
  
  # residual plot
  plot(resid2, xlab = "fitted values", ylab = "residuals", main = "Residual Plot for WWII Model")
  abline(h = 0)
  
  # bp test
  bptest(model2) #p=0.3348; constant variance
  
  # AIC 
  AIC(model2)
  
# model 3: quadratic
  model3 <- lm(Population ~ year_scaled + I(year_scaled^2), data = USpop)
  summary(model3)
  
  # ANOVA
  anova(model3)
  
  # Predicted values
  USpop$pred3 <- predict(model3)
  
  # Plot
  plot(USpop$year_scaled, USpop$Population, 
       main = "Quadratic Model: Population by Year", 
       xlab = "Year Scaled (0 - 190)",
       ylab = "Population (millions)", 
       pch = 16)
  lines(USpop$year_scaled, USpop$pred3, col = "darkgreen", lwd = 2)
  
  # MSE and AIC
  mean(model3$residuals^2)
  AIC(model3)
  
  #residuals
  resid3 <- model3$residuals
  
  # qq plot
  qqnorm(resid3, main = "QQ plot for Quadratic Model")
  qqline(resid3)
  
  # shapiro test
  shapiro.test(resid3) #p = 0.001765; reject normality
  
  # residual plot
  plot(resid3, xlab = "fitted values", ylab = "residuals", main = "Residual plot for Quadratic Model")
  abline(h = 0)
  
  # bp test
  bptest(model3) #p = 0.1252; constant variance
  
  # AIC 
  AIC(model3)
  
  
# model 4: quadratic and wwii (best model)
  model4 <- lm(Population ~ year_scaled + I(year_scaled^2) + WWII, data = USpop)
  summary(model4)
  
  # ANOVA
  anova(model4)
  
  # Predicted values
  USpop$pred4 <- predict(model4)
  
  # Plot
  plot(USpop$year_scaled, USpop$Population, 
       main = "Combined Model: Population by Year", 
       xlab = "Year Scaled (0 - 190)",
       ylab = "Population (millions)", 
       pch = 16)
  lines(USpop$year_scaled, USpop$pred4, col = "purple", lwd = 2)
  
  # MSE and AIC
  mean(model4$residuals^2)
  AIC(model4)
  
  #residuals
  resid4 <- model4$residuals
  
  # qq plot
  qqnorm(resid4, main = "Normal QQ plot for Combined Model")
  qqline(resid4)
  
  # shapiro test
  shapiro.test(resid4) #p = 0.8094; DN reject normality
  
  # residual plot
  plot(resid4,xlab = "fitted values", ylab = "residuals", main = "Residual plot for Combined Model" )
  abline(h = 0)
  
  # bp test
  bptest(model4) #p = 0.6155; constant variance
  
  # AIC 
  AIC(model4)
  
# compare aic
  AIC(model1) #181.4217
  AIC(model2) #183.2614
  AIC(model3) #103.285
  AIC(model4) #65.965306
  
# best model to predict population in 2020
predict(model4, newdata = data.frame(year_scaled = 230, WWII = 0))  # 2020 is not WWII era
# 333.084
  
