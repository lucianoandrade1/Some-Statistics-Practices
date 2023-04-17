#Luciano Andrade

#Consider the following scenario:
  
#One pharmaceutical company intends to announce a new medicine to control the sleep time of people with sleep disorders.

#Therefore this company wants to validate the efficiency of this new medicine in comparison with a medicine already used by the market.

#This way, the company selected two patient groups and gave medicine, one for the first and two for the second group.

#The objective here is to answer to the following question:

#Is there a significant sleep time difference between the two groups? 
#In other words, is there a significant difference between the two drugs in treating sleep disorders?

# Let's apply the t-Test and answer the question.

#Student t test is a statistical test which is widely used to compare the mean of two groups of samples. 
#It is therefore to evaluate whether the means of the two sets of data are statistically significantly different from each other.

#There are many types of t test :
  
#The one-sample t-test, used to compare the mean of a population with a theoretical value.
#The unpaired two sample t-test, used to compare the mean of two independent samples.
#The paired t-test, used to compare the means between two related groups of samples.

if(!require(car)) install.packages("car")
library(car)
if(!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if(!require(tidyverse)) install.packages("dplyr")
library(dplyr)


#To apply the t-Test, we first need to validate the 5 test assumptions.

# 1- Data are random and representative of the population.
# 2- The dependent variable is continuous.
# 3- Both groups are independent (i.e. exhaustive and exclusive groups).
# 4- The model residuals are normally distributed.
# 5- The residual variance is homogeneous (principle of homoscedasticity).

#For the example in this case study, I will consider assumptions 1 to 3 valid 
#and validate assumptions 4 and 5. For assumption 4, I will use the Shapiro-Wilk
#Test; for assumption 5, I will use the F-Test.

#Let's separate the data from the two groups.
grupo_two <- sleep$group == 2

#-------------------------------------------------------------------------------

#A first step may be to validate assumption 4 using qqPlot.
?qqPlot
qqPlot(sleep$extra[grupo_two])
qqPlot(sleep$extra[! grupo_two])

#Analysis: The data points for the "extra" variable are within the confidence 
#area, indicating that the data follow a normal distribution.

#-------------------------------------------------------------------------------
#One can perform the first hypothesis test as follows:
  
#H0: The set of data is normally distributed.
#H1: The set of data is non-normally distributed.

#The Shapiro-Wilk test is a test of normality in frequentist statistics. The 
#Shapiro-Wilk test tests the null hypothesis that a sample x1, ..., xn came 
#from a normally distributed population. The null hypothesis of this test is 
#that the population is normally distributed. Thus, if the p-value is less than 
#the chosen alpha level (usually 0.05), the null hypothesis is rejected, and 
#there is evidence that the data tested are not normally distributed.

#Validating assumption 4 with normality test Shapiro.test()
#The p-value must be greater than 0.05 to affirm a normal distribution.

?shapiro.test
shapiro.test(sleep$extra[grupo_two]) # p-value = 0.3511 > 0.05 
shapiro.test(sleep$extra[! grupo_two]) # p-value = 0.4079 > 0.05

#The test p-value of each group is greater than 0.05, so we fail to reject H0. 
#One can assume that the data is normally distributed.

#-------------------------------------------------------------------------------

#First we check for missing values.
colSums(is.na(sleep))

#Let's see a statistical summary of the dataset.
sleep %>% group_by(group) %>%
  summarise(
    count = n(),
    mean = mean(extra, na.rm = TRUE),
    sd = sd(extra, na.rm = TRUE))

#Now, one can validate assumption 4 using F-test.

#In statistics, an F-test of equality of variances is a test for the null hypothesis that two normal populations have the same variance.

#One can perform the second hypothesis test as follows:

#H0: The means of data extracted from a normally distributed population have the same variance.
#H1: The means of data extracted from a normally distributed population do not have the same variance.

?var.test
teste_f_result <- var.test(extra ~ group, data = sleep)
teste_f_result 

#The p-value is 0.7427, so greater than 0.05. We failed to reject H0. There is 
#no significant difference between the variances of the 2 groups.

#-------------------------------------------------------------------------------

#At this point, one can consider all assumptions valid. Now we can apply the t-Test.

#One can perform the third hypothesis test as follows:

#H0: There is no significant sleep time difference between the two groups.
#H1: There is significant sleep time difference between the two groups.

?t.test
teste_t_result <- t.test(extra ~ group, data = sleep, var.equal = TRUE)
teste_t_result 

#Final analysis:

#The p-value of the test is 0.07919, therefore greater than 0.05. We failed to 
#reject H0. The two groups have no significant difference. 

#There is no significant difference between the drugs applied to treat sleep disorders.

#This kind of analysis is fascinating. The decision-makers can now determine if the pharmaceutical company has to introduce a new drug to the market or not.





