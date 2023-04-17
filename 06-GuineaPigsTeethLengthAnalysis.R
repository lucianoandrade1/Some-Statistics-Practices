#Luciano Andrade

#Scenario:

#The ToothGrowth dataset is available in the datasets package when you start RStudio. 
#This dataset contains records about the length of Guinea pigs' teeth. Researchers 
#administered doses of 2 vitamin supplements to 60 guinea pigs and evaluated the 
#result in the growth of the animal's teeth. Based on these data, we will answer 
#the following question: 


#Is there a significant difference in tooth growth according to the type of supplement used in Guinea pigs?

if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if(!require(GGally)) install.packages("GGally")
library(GGally)

data("ToothGrowth")
str(ToothGrowth)
View(ToothGrowth)
summary(ToothGrowth)

hist(ToothGrowth$len)

qplot(supp,
      len,
      data = ToothGrowth, 
      main = "Teeth Growth in Guinea Pigs By Type of Supplement",
      xlab = "Type of Supplement", 
      ylab = "Tooth Length") + 
  geom_boxplot(aes(fill = supp))


#According to the boxplots, there is a difference in tooth growth associated with 
#the type of supplement. A hypothesis test can validate this information.

#-------------------------------------------------------------------------------
#The first solution is to apply a t-Test of independent samples to verify if the 
#types of supplements impact the growth of the animal's teeth.

#It is essential to mention that the research null hypothesis test is the opposite 
#of the "guess" or "assumption" the research makes.

#Is there a significant difference in tooth growth according to the type of supplement used in Guinea pigs?

#Therefore the hypothesis test is:
  
#H0 (Null Hypothesis) - There is no significant difference between the two groups' means 
#(thus, the supplement type does not impact tooth growth).
#H1 (Alternative Hypothesis) - There is a significant difference between the two groups' means 
#(thus, the type of supplement impacts tooth growth).

#To apply the t-Test, we first need to validate the 5 test assumptions.
#1- Data is random and representative of the population.
#2- The dependent variable is continuous.
#3- Both groups are independent (i.e., exhaustive and excluding groups).
#4- The residuals of the model are normally distributed.
#5- The residual variance is homogeneous (principle of homoscedasticity).

#In this scenario, one can consider assumptions 1 to 3 valid, and one can validate 
#assumptions four and five. For assumption four, one can use the Shapiro-Wilk Test; 
#for assumption five, one can use the F-Test.
 
#Shapiro-Wilk Normality Test
#H0: The data is normally distributed
#H1: Data is not normally distributed

?shapiro.test
shapiro.test(ToothGrowth$len[ToothGrowth$supp == 'OJ']) 
shapiro.test(ToothGrowth$len[ToothGrowth$supp == 'VC']) 

#The p-value of the first group is less than 0.05. We reject H0.
#The p-value of the second group is greater than 0.05. We failed to reject H0.
#The first assumption of the t-Test was not satisfied (we reject H0, that is, 
#the data are not normally distributed). Therefore T-test cannot be used is this scenario.

#Are we assessing the business problem under analysis correctly? Shouldn't we 
#consider the dosage of the supplement and analyze its influence on the growth 
#of teeth in guinea pigs and not just the type of supplement?

#-------------------------------------------------------------------------------
#The second solution is to apply an ANOVA Test to verify whether the dosages of the types of 
#supplements impact the growth of the animal's teeth.

#To use the ANOVA test, we have the following principal assumptions:
#1- Within each sample, observations are sampled randomly and independently.
#2- Each group sample is drawn from a normally distributed population.

#We assume assumption 1 to be true and will test assumption 2.

#The data is divided into three groups according to the dosage of the supplement.
unique(ToothGrowth$dose)
dose_0_5 = ToothGrowth$len[ToothGrowth$dose == 0.5]
dose_1_0 = ToothGrowth$len[ToothGrowth$dose == 1]
dose_2_0 = ToothGrowth$len[ToothGrowth$dose == 2]

#Shapiro-Wilk Normality Test for each group
#H0: The data is normally distributed.
#H1: The data is not normally distributed.
shapiro.test(dose_0_5) #p-value = 0.2466 > 0.05. We failed to reject H0. The data is normally distributed.
shapiro.test(dose_1_0) #p-value = 0.1639 > 0.05. We failed to reject H0. The data is normally distributed.
shapiro.test(dose_2_0) #p-value = 0.9019 > 0.05. We failed to reject H0. The data is normally distributed.

#H0: The means of data extracted from a normally distributed population have the same variance.
#H1: The means of data extracted from a normally distributed population do not have the same variance.

#ANOVA
anova_test = aov(len ~ dose, ToothGrowth)
summary(anova_test)

#The Pr(>F) column is the p value (1.23e-14 < 0.05) of the F statistic. This shows how 
#likely it is that the F value calculated from the test would have occurred if 
#the null hypothesis of no difference among group means were true. 

#Therefore, we reject the null hypothesis, i.e., there is a difference in the length of teeth 
#in guinea pigs for the three different dosage groups. Based on this analysis, dosage has a 
#very significant effect on tooth length.

