#Luciano Andrade

#Consider the following scenario:

#One pharmaceutical company wants to compare the effects of three different medicine on patients.

#This way, the company selected three patient groups and gave three distinct medicines (A, B, and C), one for each of the three groups.

#The objective here is to answer to the following question:

#Are there significant differences in the three medicine effects among the three groups? 
#In other words, is there a significant difference between the three drugs in treating disease?

#One can use the #ANOVA (Analysis of Variance) for this task.  

#to use the ANOVA-test one can consider the following assumptions:

#1. The population from which samples are drawn should be normally distributed.
#2. Independence of cases: the sample cases should be independent of each other.
#3. Homogeneity of variance: Homogeneity means that the variance among the groups should be approximately equal.

#One can create synthetic data to study the use of ANOVA.

# Medicine A = 4 5 4 3 2 4 3 4 4
# Medicine B = 6 8 4 5 4 6 5 8 6
# Medicine C = 6 7 6 6 7 5 6 5 5

#Volunteers grades list
volunteer_grade <- c(4,5,4,3,2,4,3,4,4,6,8,4,5,4,6,5,8,6,6,7,6,6,7,5,6,5,5)
volunteer_grade

#Medicine total amount of tests
medicine <- c(rep("A",9), rep("B",9), rep("C",9))
medicine

#Now one can create the synthetic data frame.
df <- data.frame(volunteer_grade, medicine)
View(df)

grupo_A<- df$medicamento == 'A'
grupo_B<- df$medicamento == 'B'
grupo_C<- df$medicamento == 'C'

#-------------------------------------------------------------------------------

#One can perform the first hypothesis test as follows:

#H0: The set of data is normally distributed.
#H1: The set of data is non-normally distributed.

?shapiro.test
shapiro.test(df$nota_voluntario[grupo_A]) # p-value = 0.1318 > 0.05 
shapiro.test(df$nota_voluntario[grupo_B]) # p-value = 0.1838 > 0.05
shapiro.test(df$nota_voluntario[grupo_C]) # p-value = 0.0548 > 0.05

#-------------------------------------------------------------------------------

#One can perform the second hypothesis test as follows:

#H0: The means of data extracted from a normally distributed population have the same variance.
#H1: The means of data extracted from a normally distributed population do not have the same variance.


res <- bartlett.test(nota_voluntario ~ medicamento, data = PlantGrowth)
res

#From the output, the p-value of 0.1481 is not less than the significance level of
#0.05. It means there is no evidence to suggest that the variance in data is 
#statistically significantly different for the three groups.

#-------------------------------------------------------------------------------

#At this point, one can consider all assumptions valid. Now we can apply the ANOVA-Test.

#One can perform the third hypothesis test as follows:

#H0: There is no significant difference among the groups.
#H1: There is significant difference among the groups.

?aov
teste_anova <-  aov(nota_voluntario ~ medicamento, data = df)
summary(teste_anova)

#Conclusions: The p-value is 0.000256, i.e., p-value < 0.05. 
#Therefore, one can reject H0 (null hypotheses). 
#There is a significant difference among the groups, i.e., the three medicines groups have different effects.

