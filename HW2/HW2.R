# Problem 1
# (a)
pizza <- read.table(file="pizza2.txt", header=TRUE)

a <- pizza[pizza[,"heat"]=="Coal", "rating"]
sum(a) / length(a)
summation  = 0
for (i in a){
  summation = summation + sum(i - mean(a))^2
}
(summation / (length(a)-1)) ^ 0.5

a <- pizza[pizza[,"heat"]=="Wood", "rating"]
sum(a) / length(a)
summation  = 0
for (i in a){
  summation = summation + sum(i - mean(a))^2
}
(summation / (length(a)-1)) ^ 0.5

a <- pizza[pizza[,"heat"]=="Gas", "rating"]
sum(a) / length(a)
summation  = 0
for (i in a){
  summation = summation + sum(i - mean(a))^2
}
(summation / (length(a)-1)) ^ 0.5

# (b)
model <- lm(rating ~ heat, data=pizza)
anova(model)

# (c)
summary(model)


# Problem 2
model1 <- lm(rating ~ heat + area + cost, data=pizza)
summary(model1)
model2 <- lm(rating ~ heat_re + area + cost, data=pizza)
summary(model2)

model1 <- lm(rating ~ heat + area + cost, data=pizza)
predict(model1, data.frame(heat="Coal", area="LittleItaly", cost=2.5))
model2 <- lm(rating ~ heat_re + area + cost, data=pizza)
predict(model2, data.frame(heat_re=0, area="LittleItaly", cost=2.5))


# Problem 3
x1 <- pizza[pizza[,"area"]=="Chinatown", "rating"]
m1 <- mean(x1)
U1 <- m1 + qnorm(0.975) * sd(x1) / sqrt(length(x1))
L1 <- m1 - qnorm(0.975) * sd(x1) / sqrt(length(x1))

x2 <- pizza[pizza[,"area"]=="EVillage", "rating"]
m2 <- mean(x2)
U2 <- m2 + qnorm(0.975) * sd(x2) / sqrt(length(x2))
L2 <- m2 - qnorm(0.975) * sd(x2) / sqrt(length(x2))

x3 <- pizza[pizza[,"area"]=="LES", "rating"]
m3 <- mean(x3)
U3 <- m3 + qnorm(0.975) * sd(x3) / sqrt(length(x3))
L3 <- m3 - qnorm(0.975) * sd(x3) / sqrt(length(x3))

x4 <- pizza[pizza[,"area"]=="LittleItaly", "rating"]
m4 <- mean(x4)
U4 <- m4 + qnorm(0.975) * sd(x4) / sqrt(length(x4))
L4 <- m4 - qnorm(0.975) * sd(x4) / sqrt(length(x4))

x5 <- pizza[pizza[,"area"]=="SoHo", "rating"]
m5 <- mean(x5)
U5 <- m5 + qnorm(0.975) * sd(x5) / sqrt(length(x5))
L5 <- m5 - qnorm(0.975) * sd(x5) / sqrt(length(x5))

y <- c(m1, m2, m3, m4, m5)
U <- c(U1, U2, U3, U4, U5)
L <- c(L1, L2, L3, L4, L5)

require(plotrix)
plotCI(1:5, y, ui=U, li=L, xlab="index", ylab="area")