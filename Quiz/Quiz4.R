#Q1
#A pharmaceutical company is interested in testing a potential blood
#pressure lowering medication. Their first examination considers only
#subjects that received the medication at baseline then two weeks later.
#The data are as follows (SBP in mmHg)
Base <- c(140,138,150,148,135)
wk2 <- c(132,135,151,146,130)
t.test(Base,wk2,alternative = "two.sided",paired= TRUE,var.equal=FALSE)

#Q2
#A sample of 9 men yielded a sample average brain volume of 1,100cc 
#and a standard deviation of 30cc. What is the complete set of values
#of £g0 that a test of H0:£g=£g0 would fail to reject the null hypothesis 
#in a two sided 5% Students t-test?
n <- 9
mn <- 1100
s <- 30
z <- qnorm(0.975)
m <- mn + c(-1,1)*z*(s/sqrt(n))

#Q3
#Researchers conducted a blind taste test of Coke versus Pepsi.
#Each of four people was asked which of two blinded drinks given in 
#random order that they preferred. The data was such that 3 of the 4
#people chose Coke. Assuming that this sample is representative, report
#a P-value for a test of the hypothesis that Coke is preferred to Pepsi 
#using a one sided exact test.
binom.test(c(3, 1), p = 0.5, alternative = "greater")

#Q4
#Infection rates at a hospital above 1 infection per 100 person days at
#risk are believed to be too high and are used as a benchmark. 
#A hospital that had previously been above the benchmark recently had 10
#infections over the last 1,787 person days at risk. 
#About what is the one sided P-value for the relevant test of whether
#the hospital is *below* the standard?

#get probability per day
p1 <- 1/100
p2 <- 10/1787
n <- 1787

# When the population size is much larger (at least 10 times larger) than
#the sample size, the standard deviation can be approximated by:
# s = sqrt[ P * ( 1 - P ) / n ] where n is the sample size
s <- sqrt((p1*(1-p1))/n)

# get the releveant quantile(lower) expressed in standard deviations
test_z <- (p1 - p2) / s

pnorm(test_z,lower.tail = FALSE)

#Q7
#Researchers would like to conduct a study of 100 healthy adults 
#to detect a four year mean brain volume loss of .01 mm3. 
#Assume that the standard deviation of four year volume loss in
#this population is .04 mm3. About what would be the power of 
#the study for a 5% one sided test versus a null hypothesis of 
#no volume loss?
n <- 100
delta <- 0.01
sd <- 0.04
power.t.test(n=100, delta = 0.01, sd = 0.04,sig.level=0.05, type="one.sample", alt="one.sided")$power

#Q8
#Researchers would like to conduct a study of n healthy adults to
#detect a four year mean brain volume loss of .01 mm3.
#Assume that the standard deviation of four year volume loss in this 
#population is .04 mm3. About what would be the value of n needded for 
#90% power of type one error rate of 5% one sided test versus a null
#hypothesis of no volume loss?

power.t.test(power=0.9, delta = 0.01, sd = 0.04,sig.level=0.05, type="one.sample", alt="one.sided")


#Q10
#The Daily Planet ran a recent story about Kryptonite poisoning in the
#water supply after a recent event in Metropolis. Their usual field
#reporter, Clark Kent, called in sick and so Lois Lane reported the story.
#Researchers sampled 288 individuals and found mean blood Kryptonite
#levels of 44, both measured in Lex Luthors per milliliter (LL/ml). 
#They compared this to 288 sampled individuals from Gotham city who had 
#an average level of 42.04. About what is the Pvalue for a two sided Z 
#test of the relevant hypothesis? Assume that the standard deviation is 12 
#for both groups.

n <- 288
n_treated <- 288; n_placebo <- 288
mn_treated <- 44; mn_placebo <- 42.04
s <- 12

#get the standard error
std_error <- s*sqrt(1/n_treated + 1/n_placebo)

#get the z value (difference in means expressed in deviations)
z <- (mn_treated-mn_placebo)/std_error

# two sided - what's likely hood of getting those values
2 * pnorm(-abs(z))


#Q11
#Suppose that a researcher performs 10 hypothesis tests and wants 
#a familywise error rate of no more than 5%. What alpha level should
#she compare her p values to in order to ensure the desired error rate?
Q11 <- round((0.05/10),5)
Q11