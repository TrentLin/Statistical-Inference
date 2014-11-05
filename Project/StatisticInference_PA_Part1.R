
library(ggplot2)
library(plyr)
###Q1
lambda = 0.2; n = 40; simulation=1000; mu = 1/lambda; sigma = 1/lambda;
sample_dist <- sapply(1:simulation, FUN=function(x) { mean(rexp(n, lambda))})
sd_mean <- mean(sample_dist)
sd_stddev <- sd(sample_dist)
df <- data.frame(mns=sample_dist) #gglot need to input dataframe 
g <- ggplot(df, aes(x=mns, weight=mns/sum(mns)))
g + geom_histogram(aes(y=..density..),fill = "salmon") +
  geom_vline(xintercept = sd_mean, colour = "blue") + 
  geom_vline(xintercept = 5, colour = "Red", size =1 ) +
  labs(title = "1000 sample of distribution means, rexp(40,0.2)",x ="sample means",y="density")
###Q2
sd(sample_dist)
(1/lambda)/sqrt(n)
var(sample_dist)
1/((0.2*0.2) * 40)
###Q3
g <- ggplot(df, aes(x=mns, weight=mns/sum(mns)))
g + geom_histogram(aes(y=..density..),fill = "salmon") +
  geom_vline(xintercept = sd_mean, colour = "blue") + 
  geom_vline(xintercept = 5, colour = "Red", size =1 ) +
  labs(title = "1000 sample of distribution means, rexp(40,0.2)",x ="sample means",y="density") +
  geom_density(fill=NA,colour="black",size = 1)

## 4.Evaluate the coverage of the confidence interval for $1/\lambda = \bar{X} \pm 1.96 \frac{S}{\sqrt{n}}$.
#mean(sample_dist) + c(-1,1)*qnorm(0.975)*sd(sample_dist)/sqrt(n)
mean(sample_dist) + c(-1,1)*qnorm(0.975)*sd(sample_dist)/sqrt(n)

                   