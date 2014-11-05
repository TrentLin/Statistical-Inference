#Q1
mn <- mean(mtcars$mpg)
sd <- sd(mtcars$mpg)
n <- length(mtcars$mpg)
z <- qnorm(0.05,lower.tail=FALSE)
m0 <- mn + z*(sd/sqrt(n))

#Q2
m4 <- mtcars$mpg[mtcars$cyl == 4]
m6 <- mtcars$mpg[mtcars$cyl == 6]
p <- t.test(m4, m6, paired = FALSE, alternative="two.sided", var.equal=FALSE)$p.value

#Q3
n <- 100
mn <- 3
sd <- 1.1
z <- qnorm(0.025,lower.tail=FALSE)
m <- mn + c(-1,1)*z*(sd/sqrt(n))

#Q4
ans <- round(pbinom(54,prob = 0.5,size = 100, lower.tail = FALSE),4)
ans

#Q5
pv <- ppois(15800-1,lambda = 520*30, lower.tail=FALSE)
pv
pn <- pnorm(15800/30, mean = 520, sd=sqrt(520/30),lower.tail=FALSE)
pn



