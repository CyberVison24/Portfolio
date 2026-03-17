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

# get values of each of the 17 questions
table1 <- table(q01, exclude = NULL)
table2 <- table(q02, exclude = NULL)
table3 <- table(q03, exclude = NULL)
table4 <- table(q04, exclude = NULL)
table5 <- table(q05, exclude = NULL)
table6 <- table(q06, exclude = NULL)
table7 <- table(q06a, exclude = NULL)
table8 <- table(q07, exclude = NULL)
table9 <- table(q07a, exclude = NULL)
table10 <- table(q07b, exclude = NULL)
table11 <- table(q08, exclude = NULL)
table12 <- table(q09, exclude = NULL)
table13 <- table(q10, exclude = NULL)
table14 <- table(q11, exclude = NULL)
table15 <- table(q12, exclude = NULL)
table16 <- table(q13, exclude = NULL)
table17 <- table(q14, exclude = NULL)

table1
table2
table3
table4
table5
table6
table7
table8
table9
table10
table11
table12
table13
table14
table15
table16
table17

# check sum vals
sum(table1)
sum(table2)
sum(table3)
sum(table4)
sum(table5)
sum(table6)
sum(table7)
sum(table8)
sum(table9)
sum(table10)
sum(table11)
sum(table12)
sum(table13)
sum(table14)
sum(table15)
sum(table16)
sum(table17)

# check relative freq
prop1 <- prop.table(table1)
prop2 <- prop.table(table2)
prop3 <- prop.table(table3)
prop4 <- prop.table(table4)
prop5 <- prop.table(table5)
prop6 <- prop.table(table6)
prop7 <- prop.table(table7)
prop8 <- prop.table(table8)
prop9 <- prop.table(table9)
prop10 <- prop.table(table10)
prop11 <- prop.table(table11)
prop12 <- prop.table(table12)
prop13 <- prop.table(table13)
prop14 <- prop.table(table14)
prop15 <- prop.table(table15)
prop16 <- prop.table(table16)
prop17 <- prop.table(table17)

# output relative frequencies
prop1
prop2
prop3
prop4
prop5
prop6
prop7
prop8
prop9
prop10
prop11
prop12
prop13
prop14
prop15
prop16
prop17


