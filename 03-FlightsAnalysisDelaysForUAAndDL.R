#Luciano Andrade

#Scenario: 

#Delta Airlines and United Airlines are the two leading flight companies in the USA. 
#One critical question can come up about the two flight companies. 
#Do the flights of Delta Airlines (DL) delay more them the flights of United Airlines (UA)?

# Packages
if(!require(dplyr)) install.packages("dplyr")
library('dplyr')
if(!require(nycflights13)) install.packages("nycflights13")
library('nycflights13')
if(!require(ggplot2)) install.packages("ggplot2")
library('ggplot2')

#Let's consider this dataset as our population of flights.
View(flights)

#Let's remove the missing values
no_missin_values = na.omit(flights)

#Let's slice the dataframe and consider only the relevant data for the described scenario.
pop_data = no_missin_values[(no_missin_values$carrier == 'UA' | no_missin_values$carrier == 'DL') & no_missin_values$arr_delay >= 0, c('carrier','arr_delay')]

#Let's get a sample of UA data from the population.
sample1 = sample_n(pop_data[pop_data$carrier == 'UA',], 1000)
sample1$id = 1

#Let's get a sample of DL data from the population.
sample2 = sample_n(pop_data[pop_data$carrier == 'DL',], 1000)
sample2$id = 2

#-------------------------------------------------------------------------------

#Let's calculate the confidence interval (95%) of both flight companies' samples

#With certain inference conditions (our sample is random, normal, and independent), 
#we can use the following standard deviation calculation to estimate the standard deviation 
#of our population. Since this is just an estimate, it's called the standard error. 
#The condition for using this as an estimate is that the sample size n is greater 
#than 30 (given by the central limit theorem) and meets the independence condition 
#n <= 10% of the population size.

#One can use: std_error = sd(sample$arr_delay) / sqrt(nrow(sample))

#Calculating a Confidence Interval From a Normal Distribution

std_error1 <- qnorm(0.975)*sd(sample1$arr_delay)/sqrt(nrow(sample1))

std_error2 <- qnorm(0.975)*sd(sample2$arr_delay)/sqrt(nrow(sample2))


lower = mean(sample1$arr_delay) - std_error1  
upper = mean(sample1$arr_delay) + std_error1

#Confidence interval for UA.
ci_UA = c(lower,upper)
ci_UA

lower = mean(sample2$arr_delay) - std_error2 
upper = mean(sample2$arr_delay) + std_error2

#Confidence interval for DL.
ci_DL = c(lower,upper)
ci_DL

#Let's create only one dataframe with all relevant data.
samples = rbind(sample1,sample2)
View(samples)

samples$id <- as.factor(samples$id)

#This visualization can help answer whether the samples came from the same population. 
#The answer to this question is most of the data resides in the same confidence interval 
#in the two samples. Therefore they can be from the same population.

toPlot = summarise(group_by(samples, id), mean = mean(arr_delay))
toPlot = mutate(toPlot, lower = ifelse(toPlot$id == 1,ci_UA[1],ci_DL[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$id == 2,ci_UA[2],ci_DL[2]))
ggplot(toPlot, aes(x = id, y=mean, colour=id )) + 
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1)


#One can create a hypothesis test to verify whether Delta Airlines (DL) flights are more delayed than UA (United Airlines) flights.
#H0 and H1 must be mutually exclusive.

#H0 = There is no significant difference between DL and UA delays (mean delay diff = 0).
#H1 = Delta delays more (average diff > 0).

t.test(sample1$arr_delay, sample2$arr_delay, alternative="greater") #p-value = 0.8394 > 0.05

#We failed to reject the null hypothesis, as the p-value is greater than the significance level. 
#Therefore, there is a high probability of no significant difference between the delays. 
#For our data, there is no statistical evidence that DL delays more than UA.

