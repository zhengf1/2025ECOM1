##---------------------------------------------------------------------------------
############### ECOM20001 Econometrics 1 Week 4
# by Zheng Fan; fan.z@unimelb.edu.au
# Contact me if you have any questions.

# this R.script is(will be) available on
# www.zhengfan.site  -> Teaching
# or my github: https://github.com/zhengf1/2025ECOM1

# please refer to Dave's R code for detailed explanations (for your self learning)
# it contains much more than the tutorial questions with extra examples,
# so please be patient when go through the code

# Keep in mind you will have group assignments. 
# You may start to form group and register on Canvas.
# remove everything in the environment to start a new project
rm(list=ls()) 

# new: -- set the working directory based
#      -- on the location of your R code
#      -- This is very handy across device
setwd( dirname(rstudioapi::getActiveDocumentContext()$path) )
# OR as we introduced last two weeks, recall the path
setwd("~/Dropbox/01 UoM-Teaching/2025-S1-Ecom1/Week4") # your path should be different 

# --------- Load Stargazer package for summary statistics and regression tables
library(stargazer)

# --------- load the data set
data1=read.csv(file="tute4_height.csv")


# --------- Q1 
## Summary Statistics
stargazer(data1, 
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),  
          type="text", title="Descriptive Statistics",
          out="sumstats1.txt")

# --------- Q2
## Sample correlation between height and earnings using the cor() function
cor(data1$height,data1$earnings)

## Scatter plot visualising the relationship between height and earnings 
pdf("q1_scat_height_earnings.pdf")
plot(data1$height,data1$earnings,
     main="Relationship Between Height and Earnings",
     xlab="Height in Centimeters",
     ylab="Annual Earnings in $10,000's",
     col="blue",
     pch=16)
dev.off()

# --------- Q3
## Look at the results of the 
## t-test you conducted to determine whether there is a 
## difference between people’s earnings who are under 
## 170 cm tall and those who are 170 cm or taller.
## Write out the hypotheses. Using the results of your 
## test what is your decision and conclusion?

mean(data1$earnings[data1$height>=170]) # introduced last week
# or
mean(data1[data1$height>=170,'earnings'])
# E(earnings|height >= 170)

mean(data1$earnings[data1$height<170])
# E(earnings|height < 170)

(Dbar = mean(data1$earnings[data1$height>=170])-mean(data1$earnings[data1$height<170]))
# The difference in means is $4605/year.


## 2-sample t-test of difference in means for 
# people taller and shorter than 170cm 
t.test(data1$earnings[data1$height>=170],
       data1$earnings[data1$height<170])

# alternatively, calculate manually
s1 = sd(data1$earnings[data1$height>=170])
n1 = length(data1$earnings[data1$height>=170])
s2 = sd(data1$earnings[data1$height<170])
n2 = length(data1$earnings[data1$height<170])
(t_stat = Dbar/sqrt(s1^2/n1+s2^2/n2))

# --------- Q4
## ESTIMATE SIMPLE LINEAR REGRESSION: lm()

## Single linear regression of earnings on height
earn_reg1=lm(earnings~height,data=data1)
summary(earn_reg1)

## Provide an interpretation of the slope coefficient 
## for a one-unit increase in height, and report 
## the R-Squared and Standard Error of the Regression

# --------- Q5
## For a worker with average height, what is the 
## impact of increasing their height by one 
## standard deviation on earnings? 
mean(data1$height)
sd(data1$height)

beta_1 = earn_reg1$coefficients[2]
sd(data1$height) * beta_1 * 10000

## How does this predicted increase compare to 
## the sample mean of earnings?
sd(data1$height) * beta_1 * 10000 / (mean(data1$earnings)*10000)

# --------- Q6
# REGRESSIONS USING SUBSETS OF DATA

# We can also run regressions subsets of data using the indexing commands 
# that allow us to examine subsets of data from tute 4. 

## Single linear regression of earnings on height among males
earn_reg2=lm(earnings[male==1]~height[male==1], data=data1)
summary(earn_reg2)

## Single linear regression of earnings on height among females
earn_reg3=lm(earnings[male==0]~height[male==0], data=data1)
summary(earn_reg3)

# By comparing the results in earn_reg2 and earn_reg3, we can see whether
# the relationship between earnings and height is difference between males and females, for example.

stargazer(earn_reg1,earn_reg2,earn_reg3,
          type = "text",
          digits = 3,
          intercept.bottom = FALSE)

############### Notes ############### 
# this R.script is(will be) available on
# www.zhengfan.site  -> Teaching
# or my github: https://github.com/zhengf1/2025ECOM1

# This inTute code is provided only for the tutorial sessions.

# please refer to Dave's R code for detailed explanations
# it contains much more than the tutorial questions required



