#In Second portion of this project, we're going to analyze the ToothGrowth
#data in the R datasets package

# Basic Setting
library(ggplot2)
echo = TRUE

# load the dataset ToothGrowth
library(datasets)
data(ToothGrowth)

## 1.Load the ToothGrowth data and perform some basic exploratory data   analysis

# Look at the dataset variables
head(ToothGrowth)
tail(ToothGrowth)
str(ToothGrowth)

# converse dose from number to factor
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

# review the dataset variables
str(ToothGrowth)

## 2.Provide a basic summary of the data.

# summary statistics for all variables
summary(ToothGrowth)

# split of cases between different dose levels and delivery methods
table(ToothGrowth$supp, ToothGrowth$dose)

# plot
g <- ggplot(data = ToothGrowth, aes(x = dose, y= len))
g + geom_boxplot(aes(colour = factor(dose))) +
  facet_grid(.~supp) +
  labs(title="Delivery Methods",x = "Vaitamin C in milligrams", y ="Tooth Length")


## 3.Use confidence intervals and hypothesis tests to compare tooth growth by supp and dose.

#The tooth growth was compared by supplement for each dosage under the null hypothesis that each supplement has the same effect at a certain dosage on the tooth.

#Split the dataset upby Vaitamin C dosage
d0.5 <- subset(ToothGrowth, dose == 0.5)
d1.0 <- subset(ToothGrowth, dose == 1.0)
d2.0 <- subset(ToothGrowth, dose == 2.0)

#Conduct a t-test between Delivery Methods(Supplements)
tes0.5 <-t.test(len ~ supp, paired=FALSE, var.equal= FALSE, data=d0.5)
tes1.0 <-t.test(len ~ supp, paired=FALSE, var.equal= FALSE, data=d1.0)
tes2.0 <-t.test(len ~ supp, paired=FALSE, var.equal= FALSE, data=d2.0)

#P-Value of each Vaitamin C dosage
tes0.5$p.value
tes1.0$p.value
tes2.0$p.value

#95 percent confidence interval of each Vaitamin C dosage
tes0.5$conf
tes1.0$conf
tes2.0$conf

