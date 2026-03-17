clean <- read.csv("C:/Users/david/OneDrive/Documents/STA classes/STA 5900/Project 1/cleaned_Data.csv")
clean

# rename table
q01 <- clean$q01
q02 <- clean$q02
q03 <- clean$q03
q04 <- clean$q04
q05 <- clean$q05
q06 <- clean$q06
q06a <- clean$q06a
q07 <- clean$q07
q07a <- clean$q07a
q07b <- clean$q07b
q07c <- clean$q07c
q08 <- clean$q08
q09 <- clean$q09
q10 <- clean$q10
q11 <- clean$q11
q12 <- clean$q12
q13 <- clean$q13
q14 <- clean$q14

# Q1, let p(i) be the probablity of the sudent in genter belonging to i = a,b

# 1.1) test on binomoial distribtion with p(i) = 0.50 for 
#  number of student in 2 categories

# count male and female
male_count <- sum(q01 == "a", na.rm = TRUE) # na.rm = remove NA
female_count <- sum(q01 == "b", na.rm = TRUE) # na.rm = remove NA

male_count                          # 615
female_count                        # 635
sum = male_count + female_count     # 1250

# probablities for p(a) and p(b)
prob <- c(0.50, 0.50)   

# store counts
counts <- c(male_count, female_count)

# chi-sq test for counts based on probabilities 
chisq.test(counts, p = prob, correct = FALSE)

# 1.2) test whether prop of female is greater than 0.50
# prop.test( want, total, probability, alternative = "(less,greater)", correct = False)

prop.test(female_count, sum, p = 0.5, alternative = "greater", correct = FALSE)

# 2) Based on Q04, let p(i) [i = a,b, ... ,e] be the probability why a student 
# came to CPP belong to i. Test binomial distribution with p(i) = 0.20

close2home <- sum(q04 == "a", na.rm = TRUE) # na.rm = remove NA
Some_Rec <- sum(q04 == "b", na.rm = TRUE) # na.rm = remove NA
Beaut_Camp <- sum(q04 == "c", na.rm = TRUE) # na.rm = remove NA
Good_Rep <- sum(q04 == "d", na.rm = TRUE) # na.rm = remove NA
other <- sum(q04 == "e", na.rm = TRUE) # na.rm = remove NA

close2home      # 369
Some_Rec        # 169
Beaut_Camp      # 67
Good_Rep        # 334
other           # 308

# sum of questions
sum2 <- close2home + Some_Rec + Beaut_Camp + Good_Rep+ other
sum2           # 1247

# put counts in table
count2 <- c(close2home, Some_Rec, Beaut_Camp, Good_Rep, other)

# probabilities
prob2 <- c(0.20, 0.20, 0.20, 0.20, 0.20) 

# test 
chisq.test(count2, p = prob2, correct = FALSE)

# 3) Based on Q#13, Let p(i) (𝑖 = 𝑎, 𝑏, 𝑐, 𝑑) be the prob

#3-1. Test whether the specified probabilities p(a) = 0.10 and 
#  p(b) = p(c) = p(d) = 0.30 are suitable for describing the # of students
#  for each category
# counts of responses

faculty <- sum(q13 == "a", na.rm = TRUE) # na.rm = remove NA
education <- sum(q13 == "b", na.rm = TRUE) # na.rm = remove NA
campus <- sum(q13 == "c", na.rm = TRUE) # na.rm = remove NA
DN_Stand <- sum(q13 == "d", na.rm = TRUE) # na.rm = remove NA

faculty            # 115
education            # 409
campus            # 351
DN_Stand            # 371

# sum of responses
sum3 <- faculty + education + campus + DN_Stand
sum3

# counts in tables
count3 <- c(faculty, education, campus, DN_Stand)

# probs in tables
prob3 <- c(0.10, 0.30, 0.30, 0.30)

chisq.test(count3, p = prob3, correct = FALSE)

# prop.test(875, 1246, p = 0.67, alternative = "greater", correct = FALSE)

#3-2. Create a new category “Wow” by merging categories (a), (b), and (c). 
# Test whether more than 2/3 of the students selected belongs to the Wow category.

# number of "wow"
num_wow <- faculty + education + campus

num_wow #875
sum3  #1246

# prop test for one large category
prop.test(num_wow, sum3, p = 2/3, alternative = "greater", correct = FALSE)

#4) Q#01 and Q#03, test to see where two variables are related

# from q03
# yes <- sum(q03 == "a", na.rm = TRUE) # na.rm = remove NA
# no <- sum(q03 == "b", na.rm = TRUE) # na.rm = remove NA

# Male counts for each reason
male_yes  <- sum(q01 == "a" & q03 == "a", na.rm = TRUE)
male_no    <- sum(q01 == "a" & q03 == "b", na.rm = TRUE)

# Female counts for each reason
female_yes  <- sum(q01 == "b" & q03 == "a", na.rm = TRUE)
female_no    <- sum(q01 == "b" & q03 == "b", na.rm = TRUE)

# counts
male_yes; female_yes
male_no; female_no

# create table 
observed4 <- matrix( c(male_yes, male_no, female_yes, female_no), nrow = 2, byrow = TRUE)
observed4

# Perform chi-square test for independence
chisq.test(observed4, correct = FALSE)

# faster table
# table_q01_q03 <- table(q01, q03)

# table_q01_q03

# chisq.test(table_q01_q03, correct = FALSE)

#5. Based on Q#01 and Q#04, test whether the reason why a student came to CPP varies among gender.

# Male counts for each reason
male_close2home  <- sum(q01 == "a" & q04 == "a", na.rm = TRUE)
male_Some_Rec    <- sum(q01 == "a" & q04 == "b", na.rm = TRUE)
male_Beaut_Camp  <- sum(q01 == "a" & q04 == "c", na.rm = TRUE)
male_Good_Rep    <- sum(q01 == "a" & q04 == "d", na.rm = TRUE)
male_other       <- sum(q01 == "a" & q04 == "e", na.rm = TRUE)

# Female counts for each reason
female_close2home  <- sum(q01 == "b" & q04 == "a", na.rm = TRUE)
female_Some_Rec    <- sum(q01 == "b" & q04 == "b", na.rm = TRUE)
female_Beaut_Camp  <- sum(q01 == "b" & q04 == "c", na.rm = TRUE)
female_Good_Rep    <- sum(q01 == "b" & q04 == "d", na.rm = TRUE)
female_other       <- sum(q01 == "b" & q04 == "e", na.rm = TRUE)

# Display results
male_close2home; female_close2home
male_Some_Rec; female_Some_Rec
male_Beaut_Camp; female_Beaut_Camp
male_Good_Rep; female_Good_Rep
male_other; female_other

# create table; 
observed5 <- matrix(c(male_close2home, female_close2home,
                      male_Some_Rec, female_Some_Rec,
                      male_Beaut_Camp, female_Beaut_Camp,
                      male_Good_Rep, female_Good_Rep,
                      male_other, female_other), nrow = 2, ncol = 5)
observed5

# faster table
# table_q01_q04 <- table(q01, q04)
 
# table_q01_q04

# Perform chi-square test for independence
chisq.test(observed5, correct = FALSE)

# chisq.test(table_q01_q04, correct = FALSE)

# 6) Based on Q03 and Q04, test whether the proportion of students who came to CPP because of “a school of
# good academic reputation” among students who answered “yes” on question Q03 is larger than 30%.

# yes counts for each reason
yes_close2home  <- sum(q03 == "a" & q04 == "a", na.rm = TRUE)
yes_Some_Rec    <- sum(q03 == "a" & q04 == "b", na.rm = TRUE)
yes_Beaut_Camp  <- sum(q03 == "a" & q04 == "c", na.rm = TRUE)
yes_Good_Rep    <- sum(q03 == "a" & q04 == "d", na.rm = TRUE)
yes_other       <- sum(q03 == "a" & q04 == "e", na.rm = TRUE)

# no counts for each reason
no_close2home  <- sum(q03 == "b" & q04 == "a", na.rm = TRUE)
no_Some_Rec    <- sum(q03 == "b" & q04 == "b", na.rm = TRUE)
no_Beaut_Camp  <- sum(q03 == "b" & q04 == "c", na.rm = TRUE)
no_Good_Rep    <- sum(q03 == "b" & q04 == "d", na.rm = TRUE)
no_other       <- sum(q03 == "b" & q04 == "e", na.rm = TRUE)

#display results 
yes_close2home; no_close2home
yes_Some_Rec; no_Some_Rec
yes_Beaut_Camp; no_Beaut_Camp
yes_Good_Rep; no_Good_Rep
yes_other; no_other

yestotal <- yes_close2home + yes_Some_Rec + yes_Beaut_Camp + yes_Good_Rep + yes_other
nototal <- no_close2home + no_Some_Rec + no_Beaut_Camp + no_Good_Rep + no_other

yestotal
nototal

# faster table
table_q03_q04 <- table(q03, q04)

table_q03_q04

# prop test for one large category
prop.test(yes_Good_Rep, yestotal, p = 0.3, alternative = "greater", correct = FALSE)

# faster way to make table
# table_q03_q04 <- table(q03, q04)

# table_q03_q04


# 7. Based on Q02 (What is your class standing?) and Q12 (How many times a week do you attend CPP?), 

  #7-1. Test whether the two variables are independent.
  
  # Perform chi-square test for independence
  #chisq.test(observed7, correct = FALSE)
  
  # Create a contingency table
  table_q02_q12 <- table(q02, q12)
  
  table_q02_q12
  
  # Perform Chi-square test
  chisq.test(table_q02_q12, correct = FALSE)
  
  
  # 7-2) Now create the dataset that has entries (a) ~ (d) only for Q#02 and (a) ~ (e) only for Q#12. 
  # Test whether the two variables are related or not.
  
  # Filter dataset: Keep only Q#02 (a-d) and Q#12 (a-e)
  filtered_data <- subset(clean, q02 %in% c("a", "b", "c", "d") & q12 %in% c("a", "b", "c", "d", "e"))
  
  # Create a contingency table
  filtered_table <- table(filtered_data$q02, filtered_data$q12)
  
  filtered_table
  
  # Perform Chi-square test
  chisq.test(filtered_table, correct = FALSE)
  
  
  #7-3. For this part, you will re-create the dataset. 
  # For Q#02 (class standing) and Q12 (number of times a student attends) both are in the ‘ordinal’ measurement
  # scale rather than ‘nominal’ scale. 
  # For the class standing, assign 1 for “(a)”, 2 for “(b)”, 3 for ”(c)”, and 4 for “(d)”.
  # For the number of times a student attends, assign 1 for “(a)”, 2 for “(b)”, 3 for ”(c)”, 4 for “(d)”, and 5 for “(e)”.
  # Using the newly created dataset, compute the (Pearson’s) correlation coefficient between the two variables.
  # Interpret the value of the correlation coefficient: strength and direction in the context of class standing and number
  # of times a student attends.

  # Convert Q#02 to numeric: (a) → 1, (b) → 2, (c) → 3, (d) → 4
  q02_numeric <- as.numeric(factor(q02, levels = c("a", "b", "c", "d"), labels = c(1, 2, 3, 4)))
  
  # Convert Q#12 to numeric: (a) → 1, (b) → 2, (c) → 3, (d) → 4, (e) → 5
  q12_numeric <- as.numeric(factor(q12, levels = c("a", "b", "c", "d", "e"), labels = c(1, 2, 3, 4, 5)))
  
  # Compute Pearson correlation
  correlation <- cor(q02_numeric, q12_numeric, use = "complete.obs", method = "pearson")
  
  # Print correlation coefficient
  correlation

  #7-4. Test whether the correlation coefficient is significantly different from zero. 
  # That is to test H0: the correlation coefficient is equal to zero (i.e., independence).

  cor.test(q02_numeric, q12_numeric, method = "pearson")

# 8. Based on Q07 (Do you use the e-mail stations at school?), Q#7a (How often? Answer this if you chose “yes”
# above), and #Q7b (At what location do you most often use them? Answer this if you chose “yes” above), do the
# following:
  
  #8-1. Test whether the proportion of students who use the e-mail stations at school is significantly greater than
  # the proportion of students who do not use the e-mail stations at school.

  # Count students who use ("yes") vs. don’t use ("no") e-mail stations
  yes_count8 <- sum(q07 == "a", na.rm = TRUE)
  no_count8  <- sum(q07 == "b", na.rm = TRUE)
  total8     <- yes_count8 + no_count8
  
  yes_count8
  no_count8
  total8
  
  # Proportion test (H0: p_yes ≤ p_no vs. H1: p_yes > p_no)
  prop.test(yes_count8, total8, p = 0.5, alternative = "greater", correct = FALSE)
  
  #8-2. Based upon the dataset collected, student’s responses on Q#7a and Q#7b seem to be invalid or
  # inaccurate and cannot be used for any further analysis. Why?  
  
  # Find cases where students answered "no" but still responded to Q#7a or Q#7b
  invalid_cases <- dataset[q07 == "b" & (!is.na(q7a) | !is.na(q7b)), ]
  
  # View invalid cases
  print(invalid_cases)
  

  

  
  
  
  
  
  
