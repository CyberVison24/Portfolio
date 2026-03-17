library(readr)
NFL_Play_by_Play_3rd_down_2 <- read_csv("Fall 2025/STA 4350/final project/dataset searches/Use/NFL Play by Play 3rd down_2.csv")
View(NFL_Play_by_Play_3rd_down_2)

# any N/A values
any(is.na(NFL_Play_by_Play_3rd_down_2))

# rename
NFL_2014 = NFL_Play_by_Play_3rd_down_2

# first rows
head(NFL_2014)

# third down conversion
Y = NFL_2014$third_down_converted
Y

# predictor variables
yardline_100 = NFL_2014$yardline_100
ydstogo = NFL_2014$ydstogo
game_seconds_remaining = NFL_2014$game_seconds_remaining
score_differential = NFL_2014$score_differential
qtr = NFL_2014$qtr
goal_to_go = NFL_2014$goal_to_go
shotgun = NFL_2014$shotgun
no_huddle = NFL_2014$no_huddle

# quantitative
game_seconds_remaining; 
yardline_100; 
ydstogo; 
score_differential

# qualitative
goal_to_go; 
no_huddle; 
qtr; 
shotgun; 

qtr = as.factor(qtr)

# size of sample
length(Y)
# 7487

#1) describe data

#quantitative: five number summaries
summary(yardline_100)
summary(ydstogo)
summary(game_seconds_remaining)
summary(score_differential)
sd(yardline_100); sd(ydstogo); sd(game_seconds_remaining); sd(score_differential)

par(mfrow=c(2,2))
# Boxplot: yardline_100 vs third_down_converted
boxplot(yardline_100 ~ factor(Y, labels = c("Failed", "Converted")), 
        horizontal = TRUE,
        main = "3rd Down Yardline",
        xlab = "Yardline",
        ylab = "Outcome",
        col = c("green","gold"))

# Boxplot: ydstogo vs third_down_converted
boxplot(ydstogo ~ factor(Y, labels = c("Failed", "Converted")), 
        horizontal = TRUE,
        main = "Yards to Go on 3rd Down ",
        xlab = "Yards to Go",
        ylab = "Outcome",
        col = c("red","purple"))

# Boxplot: game_seconds_remaining vs third_down_converted
boxplot(game_seconds_remaining ~ factor(Y, labels = c("Failed", "Converted")), 
        horizontal = TRUE,
        main = "Time Remaining on 3rd Down",
        xlab = "Seconds Remaining",
        ylab = "Outcome",
        col = c("grey","orange"))

# Boxplot: score_differential vs third_down_converted
boxplot(score_differential ~ factor(Y, labels = c("Failed", "Converted")), 
        horizontal = TRUE,
        main = "3rd Down Score Differential",
        xlab = "Score Differential",
        ylab = "outcome",
        col = c("brown","maroon"))

par(mfrow=c(1,1))


# categorical=======
table(goal_to_go,Y)
table(no_huddle,Y)
table(qtr,Y)
table(shotgun,Y)

prop.table(table(goal_to_go,Y),margin=1)
prop.table(table(no_huddle,Y),margin=1)
prop.table(table(qtr,Y),margin=1)
prop.table(table(shotgun,Y),margin=1)

prop.test(table(goal_to_go,Y),correct=FALSE)
prop.test(table(no_huddle,Y),correct=FALSE)
prop.test(table(qtr,Y),correct=FALSE)
prop.test(table(shotgun,Y),correct=FALSE)

# categorical plots
par(mfrow=c(2,2))

# Barplot for goal to goal
prop_goal <- table(NFL_2014$goal_to_go, Y)

barplot(t(prop_goal),
        col = c("skyblue", "salmon"),
        legend.text = c("Failed","Convert"),
        main = "3rd Down and Goal",
        xlab = "Goal to Go",
        ylab = "Count",
        names.arg = c("> 10 yards", "< 10 yards"))  # 0 = No, 1 = Yes
  # asked chat help for changing the labels

# Barplot of no_huddle vs Y
prop_huddle <- table(NFL_2014$no_huddle, Y)

barplot(t(prop_huddle),
        col = c("skyblue", "salmon"),
        legend.text = c("Failed","Convert"),
        main = "3rd Down No Huddle",
        xlab = "Huddle",
        ylab = "Count",
        names.arg = c("Regular", "No Huddle"))  # 0 = Regular, 1 = No Huddle
# asked chat help for changing the labels

# Barplot of qtr vs Y
prop_qtr <- table(NFL_2014$qtr, Y)
barplot(t(prop_qtr),
        col = c("skyblue", "salmon"),
        legend.text = c("Failed","Convert"),
        main = "3rd Down Quarter",
        xlab = "Quarter",
        ylab = "Count",
        names.arg = c("Q1", "Q2", "Q3", "Q4", "OT"))
# asked chat help for changing the labels

# Barplot of shotgun vs Y
prop_shotgun <- table(NFL_2014$shotgun, Y)

barplot(t(prop_shotgun),
        col = c("skyblue", "salmon"),
        legend.text = c("Failed","Convert"),
        main = "3rd Down Shotgun Formation",
        xlab = "Shotgun",
        ylab = "Count",
        names.arg = c("Under Center", "Shotgun"))  # 0 = Under Center, 1 = Shotgun
# asked chat help for changing the labels

par(mfrow=c(1,1))

# correlation
# variables for correlation matrix
vars <- c("third_down_conversion",
          "game_seconds_remaining",
          "yardline_100",
          "ydstogo",
          "score_differential",
          "goal_to_go",
          "no_huddle",
          "qtr",
          "shotgun")


# corelation matrix
with(NFL_2014, cor(data.frame("Third Down conversion" = Y,
  "Game Seconds Remaining" = game_seconds_remaining,
                              "Yardline 100" = yardline_100,
                              "Yards to Go" = ydstogo,
                              "Score Differential" = score_differential,
                              "Goal To Go" = as.numeric(goal_to_go),
                              "No Huddle" = as.numeric(no_huddle),
                              "Quarter" = as.numeric(qtr),
                              "Shotgun" = as.numeric(shotgun))))
# correlation scatter plot
with(NFL_2014, pairs(data.frame("Third Down conversion" = Y,
                                "Game Seconds Remaining" = game_seconds_remaining,
                                "Yardline 100" = yardline_100,
                                "Yards to Go" = ydstogo,
                                "Score Differential" = score_differential,
                                "Goal To Go" = as.numeric(goal_to_go),
                                "No Huddle" = as.numeric(no_huddle),
                                "Quarter" = as.numeric(qtr),
                                "Shotgun" = as.numeric(shotgun) ) ) )

#=======
# 2) Model selection

# Train/Test model
set.seed(4350)
n_total = nrow(NFL_2014)
n_test = floor(0.25 * n_total) # need a whole number
n_test #1871
test_i = sample(n_total, n_test)

NFL_test = NFL_2014[test_i, ]
NFL_train = NFL_2014[-test_i, ]

#Build the model
mlr1 = glm(third_down_converted ~ yardline_100 + ydstogo + game_seconds_remaining +
             score_differential + goal_to_go + no_huddle + qtr + shotgun,
           family = binomial, data = NFL_train)
summary(mlr1)

# trial and error
mlr2 = glm(third_down_converted ~ yardline_100 + I(yardline_100^2) + I(yardline_100^3) 
           + ydstogo + game_seconds_remaining +
             score_differential + goal_to_go + no_huddle + qtr + shotgun,
           family = binomial, data = NFL_train)
summary(mlr2)

mlr3 = glm(third_down_converted ~ yardline_100 + I(yardline_100^2) + I(yardline_100^3) 
           + ydstogo + I(ydstogo^2) + I(ydstogo^3) + game_seconds_remaining +
             score_differential + goal_to_go + no_huddle + qtr + shotgun,
           family = binomial, data = NFL_train)
summary(mlr3)

mlr4 = glm(third_down_converted ~ yardline_100 + I(yardline_100^2) + I(yardline_100^3) 
           + ydstogo + I(ydstogo^2) + I(ydstogo^3) + game_seconds_remaining +
             I(game_seconds_remaining^2) +
             score_differential + goal_to_go + no_huddle + qtr + shotgun,
           family = binomial, data = NFL_train)
summary(mlr4)

mlr5 = glm(third_down_converted ~ yardline_100 + I(yardline_100^2) + I(yardline_100^3) 
           + ydstogo + I(ydstogo^2) + I(ydstogo^3) + game_seconds_remaining +
             I(game_seconds_remaining^2) + score_differential 
           +  goal_to_go + no_huddle + qtr + shotgun,
           family = binomial, data = NFL_train)
summary(mlr5)

mlr6 = glm(third_down_converted ~ yardline_100 + I(yardline_100^2) + I(yardline_100^3) 
           + ydstogo + I(ydstogo^2) + I(ydstogo^3) + game_seconds_remaining +
             I(game_seconds_remaining^2) + score_differential 
           +  goal_to_go + no_huddle + qtr + shotgun + yardline_100:ydstogo + yardline_100:shotgun,
           family = binomial, data = NFL_train)
summary(mlr6)

mlr7 = glm(third_down_converted ~ yardline_100 + I(yardline_100^2) + I(yardline_100^3) 
           + ydstogo + I(ydstogo^2) + I(ydstogo^3) + game_seconds_remaining +
             I(game_seconds_remaining^2) + score_differential 
           +  goal_to_go + no_huddle + qtr + shotgun + yardline_100:ydstogo 
           + yardline_100:shotgun + ydstogo:game_seconds_remaining + ydstogo:no_huddle
           + ydstogo:shotgun,
           family = binomial, data = NFL_train)
summary(mlr7)

mlr8 = glm(third_down_converted ~ yardline_100 + I(yardline_100^2) + I(yardline_100^3) 
           + ydstogo + I(ydstogo^2) + I(ydstogo^3) + game_seconds_remaining +
             I(game_seconds_remaining^2) + score_differential 
           +  goal_to_go + no_huddle + qtr + shotgun + yardline_100:ydstogo 
           + yardline_100:shotgun + ydstogo:game_seconds_remaining + ydstogo:no_huddle
           + ydstogo:shotgun + game_seconds_remaining:qtr + game_seconds_remaining:shotgun,
           family = binomial, data = NFL_train)
summary(mlr8)

mlr9= glm(third_down_converted ~ yardline_100 + I(yardline_100^2) + I(yardline_100^3) 
           + ydstogo + I(ydstogo^2) + I(ydstogo^3) + game_seconds_remaining +
             I(game_seconds_remaining^2) + score_differential 
           +  goal_to_go + no_huddle + qtr + shotgun + yardline_100:ydstogo 
           + yardline_100:shotgun + ydstogo:game_seconds_remaining + ydstogo:no_huddle
           + ydstogo:shotgun + game_seconds_remaining:qtr + game_seconds_remaining:shotgun
          + score_differential:qtr + score_differential:shotgun,
           family = binomial, data = NFL_train)
summary(mlr9)

mlr10= glm(third_down_converted ~ yardline_100 + I(yardline_100^2) + I(yardline_100^3) 
          + ydstogo + I(ydstogo^2) + I(ydstogo^3) + game_seconds_remaining +
            I(game_seconds_remaining^2) + score_differential 
          +  goal_to_go + no_huddle + qtr + shotgun + yardline_100:ydstogo 
          + yardline_100:shotgun + ydstogo:game_seconds_remaining + ydstogo:no_huddle
          + ydstogo:shotgun + game_seconds_remaining:qtr + game_seconds_remaining:shotgun
          + score_differential:qtr + score_differential:shotgun + qtr:shotgun,
          family = binomial, data = NFL_train)
summary(mlr10)

# mlr10 is the best; looking at the AIC, now will do by mlrtest
library(glmtoolbox)
hltest(mlr10) # p-value = 0.64416

mlr11= glm(third_down_converted ~ yardline_100 + I(yardline_100^2) + I(yardline_100^3) 
           + ydstogo + I(ydstogo^2) + I(ydstogo^3) + game_seconds_remaining +
             I(game_seconds_remaining^2) + score_differential 
           +  goal_to_go + no_huddle + qtr + shotgun + yardline_100:ydstogo 
           + yardline_100:shotgun + ydstogo:game_seconds_remaining + ydstogo:no_huddle
           + ydstogo:shotgun + game_seconds_remaining:qtr + game_seconds_remaining:shotgun
           + score_differential:qtr + qtr:shotgun,
           family = binomial, data = NFL_train)
summary(mlr11)
hltest(mlr11)

mlr12 = glm(third_down_converted ~ yardline_100 + I(yardline_100^2) 
           + ydstogo + I(ydstogo^2) + I(ydstogo^3) + game_seconds_remaining +
             I(game_seconds_remaining^2) + score_differential 
           +  goal_to_go + no_huddle + qtr + shotgun + yardline_100:ydstogo 
           + yardline_100:shotgun + ydstogo:game_seconds_remaining + ydstogo:no_huddle
           + ydstogo:shotgun + game_seconds_remaining:qtr + game_seconds_remaining:shotgun
           + score_differential:qtr + qtr:shotgun,
           family = binomial, data = NFL_train)
summary(mlr12)
hltest(mlr12)

mlr13 = glm(third_down_converted ~ yardline_100 + I(yardline_100^2) 
            + ydstogo + I(ydstogo^2) + I(ydstogo^3) + game_seconds_remaining +
              I(game_seconds_remaining^2) + score_differential 
            +  goal_to_go + no_huddle + qtr + shotgun + yardline_100:ydstogo 
            + yardline_100:shotgun + ydstogo:game_seconds_remaining + ydstogo:no_huddle
            + ydstogo:shotgun + game_seconds_remaining:qtr + game_seconds_remaining:shotgun
            + score_differential:qtr + qtr:shotgun,
            family = binomial, data = NFL_train)
summary(mlr13)
hltest(mlr13)
# p-value =  0.99825

# Model Selection; compare AIC
AIC(mlr1, mlr2, mlr3, mlr4, mlr5,
    mlr6, mlr7, mlr8, mlr9, mlr10,
    mlr11, mlr12, mlr13)

AIC(mlr13)
# 6768.198
# mlr13 due to AIC and hltest

#3) Results

# confidence interval
confint(mlr13)

# likelihood test 
anova(mlr1, mlr13, test = "LRT")

# point estimate approximations
# coefficients of mlr14
length(mlr13$coefficients)
mlr13$coefficients
B0 = mlr13$coefficients[1]
B1 = mlr13$coefficients[2]
B2 = mlr13$coefficients[3]
B3 = mlr13$coefficients[4]
B4 = mlr13$coefficients[5]
B5 = mlr13$coefficients[6]
B6 = mlr13$coefficients[7]
B7 = mlr13$coefficients[8]
B8 = mlr13$coefficients[9]
B9 = mlr13$coefficients[10]
B10 = mlr13$coefficients[11]
B11 = mlr13$coefficients[12]
B12 = mlr13$coefficients[13]
B13 = mlr13$coefficients[14]
B14 = mlr13$coefficients[15]
B15 = mlr13$coefficients[16]
B16 = mlr13$coefficients[17]
B17 = mlr13$coefficients[18]
B18 = mlr13$coefficients[19]
B19 = mlr13$coefficients[20]
B20 = mlr13$coefficients[21]
B21 = mlr13$coefficients[22]

B0; B1; B2; B3; B4;
B5; B6; B7; B8; B9;
B10; B11; B12; B13; B14;
B15; B16; B17; B18; B19;
B20; B21;

B0_hat_pointtest = exp(1 * B0)
B1_hat_pointest = exp(10 * B1)
B2_hat_pointest = exp(100 * B2)
B3_hat_pointest = exp(5 * B3)
B4_hat_pointest = exp(25 * B4)
B5_hat_pointest = exp(125 * B5)
B6_hat_pointest = exp(60 * B6)
B7_hat_pointest = exp(3600 * B7)
B8_hat_pointest = exp(1 * B8)
B9_hat_pointest = exp(1 * B9)
B10_hat_pointest = exp(1 * B10)
B11_hat_pointest = exp(1 * B11)
B12_hat_pointest = exp(1 * B12)
B13_hat_pointest = exp(50*5 * B13)
B14_hat_pointest = exp(10*1 * B14)
B15_hat_pointest = exp(5*60 * B15)
B16_hat_pointest = exp(5*1 * B16)
B17_hat_pointest = exp(5*1 * B17)
B18_hat_pointest = exp(60*1 * B18)
B19_hat_pointest = exp(60*1 * B19)
B20_hat_pointest = exp(1*1 * B20)
B21_hat_pointest = exp(1*1 * B21)

B1_hat_pointest; 
B2_hat_pointest; 
B3_hat_pointest; 
B4_hat_pointest;
B5_hat_pointest; 
B6_hat_pointest; 
B7_hat_pointest; 
B8_hat_pointest; 
B9_hat_pointest; 
B10_hat_pointest; 
B11_hat_pointest; 
B12_hat_pointest;
B13_hat_pointest; 
B14_hat_pointest; 
B15_hat_pointest; 
B16_hat_pointest; 
B17_hat_pointest; 
B18_hat_pointest; 
B19_hat_pointest;
B20_hat_pointest;
B21_hat_pointest;

# 4)  ROC curves
make_roc <- function(y,ph,clr="red",new_plot=TRUE,dt=0.001) {
  
  # sequence of tau values at which to evaluate and plot (fpr(tau),sens(tau)):
  taus <- seq(0,1,dt)
  
  # create vectors to store the sensitivity and fpr values for each tau value:
  sens <- numeric(length(taus))
  fpr <- numeric(length(taus))
  
  # calculate the sensitivity and fpr values:
  for (i in seq_along(taus)) {
    sens[i] <- mean(ph[y==1] > taus[i])
    fpr[i] <- mean(ph[y==0] > taus[i])
  }
  
  # if the user wants a new plotting window, make one:
  if (new_plot) 
  {
    plot(NA,NA,type="n",xlim=c(0,1),ylim=c(0,1),
         xlab="False Positive Rate",ylab="Sensitivity")
    abline(h=c(0,1),v=c(0,1),a=0,b=1)
  }
  
  # draw the ROC curve:
  lines(fpr,sens,col=clr,lwd=3)
  
}

# train data 
t = 0.5

phat_train <- predict(mlr13, newdata = NFL_train, type="response")

# train ROC curve
make_roc(NFL_train$third_down_converted, phat_train) # ROC CURVE
yhat_train <- as.numeric(phat_train > t)

train_acc <- mean(yhat_train == NFL_train$third_down_converted)
train_acc
# 0.6680912

# Test 
phat_test <- predict(mlr13, newdata=NFL_test, type="response")
yhat_test <- as.numeric(phat_test > t)

test_acc <- mean(yhat_test == NFL_test$third_down_converted)
test_acc
# 0.665954

# Test ROC
make_roc(NFL_test$third_down_converted, phat_test, clr="blue", new_plot = FALSE)

# LOOCV plot 
# make a vector to store the predicted probabilities for 7487 observations-
n_train <- nrow(NFL_train)
pi_hat_cv <- numeric(n_train)

system.time(
  for(i in 1:n_train){
    
    # fit the model excluding observation i:
    mlr13 = glm(third_down_converted ~ yardline_100 + I(yardline_100^2) 
                + ydstogo + I(ydstogo^2) + I(ydstogo^3) + game_seconds_remaining +
                  I(game_seconds_remaining^2) + score_differential 
                +  goal_to_go + no_huddle + qtr + shotgun + yardline_100:ydstogo 
                + yardline_100:shotgun + ydstogo:game_seconds_remaining + ydstogo:no_huddle
                + ydstogo:shotgun + game_seconds_remaining:qtr + game_seconds_remaining:shotgun
                + score_differential:qtr + qtr:shotgun,
                family = binomial, data = NFL_train, subset = -i)
    
    # predict on observation i:
    pi_hat_cv[i] <- predict(mlr13, newdata = NFL_train[i, ], type="response")
  }
)

# LOOCV
loocv_acc <- mean(as.numeric(pi_hat_cv > t) == NFL_train$third_down_converted)
loocv_acc
# 0.6654202

make_roc(NFL_train$third_down_converted, pi_hat_cv, clr="orange", new_plot=FALSE)

#=======

# Training, Test, LOOCV Accuracies 
train_acc; test_acc; loocv_acc
# 0.6680912; 0.665954; 0.6654202

# ROC PLOTS SUMMARIZE 

  # ROC FOR TRAIN DATA
  make_roc(NFL_train$third_down_converted, phat_train) 
  
  # ROC FOR TEST DATA
  make_roc(NFL_test$third_down_converted, phat_test, clr="blue", new_plot = FALSE)
  
  # ROC FOR LOOCV
  make_roc(NFL_train$third_down_converted, pi_hat_cv, clr="orange", new_plot=FALSE)

  title(main = "ROC Curves for Train, Test, and LOOCV Data")
  legend("bottomright", inset=0.05, 
         legend = c("Train Data", "Test Data", "LOOCV Data"), 
         text.col = c("red", "blue", "orange"))





