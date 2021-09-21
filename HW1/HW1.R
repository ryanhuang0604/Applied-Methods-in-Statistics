# Problem 1
# (a)
returning_percent <- c(74, 66, 81, 52, 73, 62, 52, 45, 62, 46, 60, 46, 38)
new_adult <- c(5, 6, 8, 11, 12, 15, 16, 17, 18, 18, 19, 20, 20)
plot(returning_percent, new_adult, xlab="Percentage of Returning", ylab="Number of New Joining")
boxplot(returning_percent, xlab="Percentage of Returning")
boxplot(new_adult, xlab="Number of New Joining")

# (b)
mean(returning_percent)
mean(new_adult)
sd(returning_percent)**2
sd(new_adult)**2

sum = 0.0
for (i in 1:length(returning_percent)){
  count <- ((returning_percent[i]-mean(returning_percent)) * (new_adult[i] - mean(new_adult)))
  sum <- sum + count
}
r <- sum / ((sd(returning_percent)*sqrt(length(returning_percent)-1)) * (sd(new_adult)*sqrt(length(new_adult)-1)))
r

# (c)
cor(returning_percent, new_adult, method="pearson")
cor(returning_percent, new_adult, method="kendall")
cor(returning_percent, new_adult, method="spearman")

# (d)
xy <- data.frame(returning_percent, new_adult)
model <- lm(new_adult ~ returning_percent, data=xy)
model
sigma(model)

# (e)
summary(model)
r**2

# (f)
residuals(model)
plot(model, which=1)


# Problem 2
# (a)
soda <- c(35.1, 35.7, 46.2, 47.4, 47.9, 49.7, 49.3)
milk <- c(27.6, 26.7, 25.7, 23.9, 23.0, 22.9, 23.3)
plot(soda, milk, xlab="Consumption of soda per capita", ylab="Consumption of milk per capita")

# (b)
cor(soda, milk, method="pearson")
cor(soda, milk, method="kendall")
cor(soda, milk, method="spearman")

# (c)
xy <- data.frame(soda, milk)
model <- lm(milk ~ soda, data=xy)
model

# (d)
plot(model, which=1)
plot(model, which=2)