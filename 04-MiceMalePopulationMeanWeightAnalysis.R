#Luciano Andrade

#Consider the following scenario:

#One research company wants to check the male mice population mean weight.

#They have the previous information that the population mice mean weight is 27 grams, 
#but they have a sample of 20 mice records and they want to chech if this information is correct.

# Let's apply the t-Test and answer the question.

#The one-sample t-test, used to compare the mean of a population with a theoretical value.
#The unpaired two sample t-test, used to compare the mean of two independent samples.
#The paired t-test, used to compare the means between two related groups of samples.

#Therefore we can use the one-sample t-test.

#Before we can do a T-test, we need to make check if we can reasonably treat the mean of this sample as normally distributed.

#dataset obtained from: https://biostat2.uni.lu/practicals/practical07_instruction.html

if(!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)
mice <- read.csv(file = 'mice.csv')

View(mice)

maleWeigths = mice[mice$sex=='M',]

mean(maleWeigths$weight)

#The research company has a small sample of 20 records.
maleWeigthssample = maleWeigths[sample(nrow(maleWeigths),20),c('weight')]

summary(maleWeigthssample)

ggboxplot(maleWeigthssample, 
          ylab = "Weight (g)", xlab = FALSE,
          ggtheme = theme_minimal())

#Since we have a small sample, we let's check if the data comes from a normal distribution using a normal quantile-quantile plot.

ggqqplot(maleWeigthssample, ylab = "Men's weight",
         ggtheme = theme_minimal())

#Since the data lies close the line, and has no notable systematic deviations from line, 
#we conclude that the data may come from normal distributions.

shapiro.test(maleWeigthssample) #p-value = 0.6653 > 0.05

#From the output, the p-value is greater than the significance level 0.05 implying 
#that the distribution of the data are not significantly different from normal #
#distribtion. In other words, we can assume the normality.

#Therefore, one can perform the t-test.

#H0: There is no significant difference between the mice male population mean weight to 27 grams (male population mean weight = 27 grams).
#H1: There is a significant difference between the mice male population mean weight to 27 grams.

t.test(maleWeigthssample, mu = 27, alternative='two.sided') #p-value = 0.6365 > 0.05

#The p-value is greater than the significance interval of 0.05, i.e., We failed to reject H0. 
#Therefore, one can conclude that there is no significant difference between the mice male population mean weight to 27 grams.

