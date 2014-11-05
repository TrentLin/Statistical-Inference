#Q2
#Suppose that diastolic blood pressures (DBPs) for men aged 35-44 are normally distributed with a mean of 80 (mm Hg) and a standard deviation of 10. About 
#what is the probability that a random 35-44 year old has a DBP less than 70?
pnorm(70, mean = 80, sd = 10, lower.tail=TRUE)

#Q3
#Brain volume for adult women is normally distributed with a mean of 
#about 1,100 cc for women with a standard deviation of 75 cc.
#About what brain volume represents the 95th percentile?
qnorm(0.95, mean = 1100, sd = 75, lower.tail= TRUE)

#Q4
#Refer to the previous question. Brain volume for adult women is
#about 1,100 cc for women with a standard deviation of 75 cc.
#Consider the sample mean of 100 random adult women from this
#population.
#Around what is the 95th percentile of the distribution of 
#that sample mean?
qnorm(0.95, mean = 1100, sd = 75/sqrt(100), lower.tail= TRUE)

#Q5
#You flip a fair coin 5 times, about what's the probability of
#getting 4 or 5 heads?
choose(5,4)*0.5^5 + choose(5,5)*0.5^5

#Q6
#The respiratory disturbance index (RDI), a measure of 
#sleep disturbance, for a specific population has a 
#mean of 15 (sleep events per hour) and a standard deviation of
#10. They are not normally distributed. 
#Give your best estimate of the probability that a sample mean
#RDI of 100 people is between 14 and 16 events per hour?
1-2*(1-pnorm(16, mean = 15, sd=10/sqrt(100)))

#Q8
#Consider a standard uniform density. 
#The mean for this density is .5 and the variance is 1 / 12.
#You sample 1,000 sample means where each sample mean is comprised
#of 100 observations. 
#You take the standard deviation of the 1,000 sample means.
#About what number would you expect it to be?
(1/12)/sqrt(1000)

#Q9
#The number of people showing up at a bus stop is assumed to be 
#Poisson with a mean of 5 people per hour. 
#You watch the bus stop for 3 hours. 
#About what's the probability of viewing 10 or fewer people?
ppois(10, lambda = 5*3)
      