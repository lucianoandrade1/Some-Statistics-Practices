#Author: Luciano Andrade

#Scenario:

#The dataset under analysis is formed by data about properties (apartment, house 
#with a yard, house without a yard, penthouse, or other). 

#A Chi-Square test of independence is used to determine whether or not there is 
#a significant association between two categorical variables.

#It is a non-parametric test. It does not depend on population parameters, such 
#as mean and variance. The basic principle of this method is to compare proportions, 
#that is, the possible divergences between the observed and expected frequencies 
#for a specific event. Say that two groups behave similarly if the differences 
#between the observed and expected frequencies in each category are minor, close 
#to zero. If the probability is very low, it provides strong evidence that the two 

#There are four assumptions of a Chi-Square test.

#Assumption 1: Both variables are categorical.
#Assumption 2: All observations are independent.
#Assumption 3: Cells in the contingency table are mutually exclusive.
#Assumption 4: Expected value of cells in the contingency table should be 5 or greater in at least 80% of cells.

#One can read and visualize the dataset under analysis.
df = read.csv("PropertiesData.csv")

View(df)

type = df$Property_Type
unique(type)

status = df$Property_Status
unique(status)

#One can see that both variables are categorical. Assumption 1 is satisfied.
#One can also see that bot variables are indepenent. Assumption 2 is satisfied.

#Cross Tabulation
table(type, status)
prop.table(table(type, status))

#One can see that the cells in the contingency table are mutually exclusive. Assumption 3 is satisfied.
#One can see that the values of cells in the contingency table are equal or greater than 5. Assumption 4 is satisfied.

#Therefore, one can user the Chi-Square test of independence to determine whether or not there is 
#a significant association between the two categorical variables status and type.

#Null hypothesis: The observed frequencies are not different from the expected frequencies. 
#There is no difference between the frequencies (counts) of the groups. 
#Therefore, there is no association between the groups.

#Alternative hypothesis: The observed frequencies are different from the expected frequencies. 
#There is a difference between the frequencies. 
#Therefore, there is an association between the groups.

#Using a simpler way:

#H0 = There is no association between the categorical variables type and status.
#H1 = There is an association between the categorical variables type and status.

?chisq.test
chisq.test(table(type, status)) # p-value = 2.2e-16 < 0.05

#The Chi-Square test p-value is smaller than 0.05, so we reject H0. 
#Therefore, the observed frequencies are different and there is an association 
#between the categorical variables type and status.

#-------------------------------------------------------------------------------

#If we do not consider apartment-type properties, is there a difference in the test result?

#Extracts a subset filtering the data according to the condition proposed for this exercise.

fdata = droplevels(subset(df, Property_Type != "apartment"))
View(fdata)

type = fdata$Property_Type
unique(type)

status = fdata$Property_Status
unique(status)

#Cross Tabulation.
table(type, status)

chisq.test(table(type, status)) # p-value = 0.8501 > 0.05

#The Chi-Square test p-value is greater than 0.05, so we fail to reject H0. 
#There is no difference between the frequencies (counts) of the type and status. 
#Therefore, there is no association between the categorical variables type and status.

