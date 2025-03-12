##---------------------------------------------------------------------------------
############### ECOM20001 Econometrics 1 Week 2 Tutorial 1
# by Zheng Fan; fan.z@unimelb.edu.au
# Contact me if you have any questions.

# this R.script is(will be) available on
# www.zhengfan.site  -> Teaching

# please refer to Canvas R code for detailed explanations
# it contains much more than the tutorial questions required

rm(list=ls()) # remove everything in the environment to start a new project

setwd("/Users/zhengfan/Library/CloudStorage/Dropbox/01 UoM-Teaching/2025-S1-Ecom1/Week2")

# load the data
dt = read.csv("tute2_crime.csv")

# check the data type before any numerical exercise
sapply(dt, class)

# as.numeric() if your data type is not numeric or integer

# -------------------------- Q.1
# summary statistics
dt[,2]
dt$vio
mean(dt$vio)

sapply(dt, mean)
sapply(dt, sd)
sapply(dt, median)

# Quartiles: 0%, 25%, 50%, 75%, 100%
quantile(dt$vio)
# quantile() can also be used to return specifc percentiles of a distribution
quantile(dt$avginc, c(0.32, 0.57, 0.98, 0.1))

summary(dt)

# use the package we introduced before
# install.packages("stargazer")
library(stargazer) # make sure you have installed before load
stargazer(dt, 
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),  
          type="text", 
          title="Descriptive Statistics",
          out="sumstats.txt") 
# this present nice-looking tables

# -------------------------- Q.2
# histogram (need to access the variable from the data set)
hist(dt$vio)

# alternatively, density plot
density(dt$vio)

plot(density(dt$vio))
plot(density(dt$dens))
plot(density(dt$rob))
plot(density(dt$avginc), main = "density plot")

pdf("test.pdf")
plot(density(dt$dens))
dev.off()


# -------------------------- Q.3
# scatter plot
plot(dt$vio, dt$rob)
cor(dt$vio, dt$rob)

plot(dt$avginc, dt$rob)
cor(dt$avginc, dt$rob)

plot(dt$dens, dt$rob)
cor(dt$dens, dt$rob)

# make the scatter plot looks better, example:     
plot(dt$vio, dt$rob,
     xlab = "Violence",
     ylab = "Robbery",
     main = "scatter plot b/w VIO/ROB",
     col = "red",
     pch = 21)
# you can manually save; 
# or save with code (see Dave's code)

# Create box and whisker plot for vio variable
boxplot(dt$vio)
# Box and whisker plots provide a different sort of visualisation of 
# a random variable's central tendency and spread. Specifically, they
# create visualisations based on the following percentiles of the distribution:
# Median or 50th percentile (the middle point of a distribution)
# 25th and 75th percentiles (intermediate low and high values of a distribution)
# Min and Max value         (extreme low and high values of a distribution)

############### Notes ############### 
# this R.script will be uploaded to
# www.zhengfan.site  -> Teaching

# please refer to Canvas R code for detailed explanations
# it may contains much more than the tutorial questions required
