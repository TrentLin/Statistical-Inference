#Q1.In a population of interest, a sample of 9 men yielded a sample 
#average brain volume of 1,100cc and a standard deviation of 30cc.
#What is a 95% Student's T confidence interval for the mean brain volume 
#in this new population?
m <- 1100
n <- 9
s <- 30
m + c(-1,1)*qt(0.975,n-1)*s/sqrt(n)

#Q2.A diet pill is given to 9 subjects over six weeks. 
#The average difference in weight (follow up - baseline) is -2 pounds. 
#What would the standard deviation of the difference in weight have to 
#be for the upper endpoint of the 95% T confidence interval to touch 0?
n <- 9
m <- -2
#-2 + c(1)qt(0.975,n-1)*s/sqrt(n)=0
#qt(0.975,8)*s/sqrt(9) = 2
s <- 2*sqrt(9)/qt(0.975,8)

#Suppose that standard deviation of 9 paired differences is 1,
#what value would the average difference have to be
#so that the lower endpoint of a 95% students t confidence interval 
#touch zero?
The t interval is x£¼¡Ót.975,8???s/n??????¡Ô
n <- 9
s <- 1
#m + c(-1)*qt(0.975,n-1)*s/sqrt(n)=0
m <- qt(0.975,n-1)*s/sqrt(n)

#Q4. In a study of emergency room waiting times, investigators
#consider a new and the standard triage systems.
#To test the systems, administrators selected 20 nights and randomly
#assigned the new triage system to be used on 10 nights and
#the standard system on the remaining 10 nights. 
#They calculated the nightly median waiting time (MWT) to see a 
#physician. 
#The average MWT for the new system was 3 hours with a variance of
#0.60 while the average MWT for the old system was 5 hours with a
#variance of 0.68. 
#Consider the 95% confidence interval estimate for the differences
#of the mean MWT associated with the new system.
#Assume a constant variance. What is the interval?
#Subtract in this order (New System - Old System).

n1 <- n2 <- 10
m1 <- 3; v1 <- 0.6
m2 <- 5; v2 <- 0.68
sp <- sqrt(((n1-1)*v1+(n2-1)*v2)/(n1 + n2 - 2))

(m1-m2)+c(-1,1)*qt(0.975,n1+n2-2)*sp*sqrt(1/n1+1/n2)

#Q7Suppose that 18 obese subjects were randomized, 9 each,
#to a new diet pill and a placebo. Subjects¡¦ body mass indices 
#(BMIs) were measured at a baseline and again after having 
#received the treatment or placebo for four weeks.
#The average difference from follow-up to the baseline 
#(followup - baseline) was ???3 kg/m2 for the treated group and
#1 kg/m2 for the placebo group. The corresponding standard deviations
#of the differences was 1.5 kg/m2 for the treatment group and
#1.8 kg/m2 for the placebo group.
#Does the change in BMI over the four week period appear to 
#differ between the treated and placebo groups? 
#Assuming normality of the underlying data and a common population
#variance, calculate the relevant *90%* t confidence interval. 
#Subtract in the order of (Treated - Placebo) with the smaller
#(more negative) number first.

tg <- pg <- 9
mtg <- -3; mpg <- 1
stg <- 1.5; spg <- 1.8
df <- (stg^2/tg + spg^2/pg)^2/(((stg^2/tg)^2)/(tg-1)+((spg^2/pg)^2)/(pg-1))
(mtg-mpg)+c(-1,1)*qt(0.95,df)*sqrt(stg^2/tg + spg^2/pg)
