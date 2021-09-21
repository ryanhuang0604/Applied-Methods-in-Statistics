# Problem 1
# (a)
data1 <- read.csv(file="wastewater.csv", header=TRUE)
AF <- data1$AF
FS <- data1$FS
FCC <- data1$FCC
treatment <- c(AF, FS, FCC)
method <- c(rep("AF",10), rep("FS",10), rep("FCC",10))
model <- aov(treatment ~ method, data=data1)
summary(model)

# (b)
boxplot(treatment ~ method, data=data1)


# Problem 2
data2 <- read.csv(file="Fern.csv", header=TRUE)
growth <- data2$Response_area
block <- data2$Block_age
treatment <- data2$wave_light
model <- aov(growth ~ block+treatment, data=data2)
summary(model)

# (a)
boxplot(growth ~ block, data=data2, main="Boxplot of growths and blocks")

# (b)
boxplot(growth ~ treatment, data=data2, main="Boxplot of growths and treatments")


# Problem 3
# (a)
data3 <- read.csv(file="Cotinine.csv", header=TRUE)
cotinine <- data3$cotinine
gender <- as.factor(data3$Gender)
racial <- as.factor(data3$Race)

interaction.plot(x.factor = racial,     # variable to plot on x-axis
                 trace.factor = gender, # variable to specify "traces"; here, lines
                 response = cotinine,   # variable to plot on y-axis
                 fun = mean,            # summary statistic to be plotted for response variable
                 type = "b",            # type of plot, here "l" for lines
                 xlab = "Racial",
                 ylab = "Cotinine")

# (b)
data3 <- data.frame(cotinine, gender, racial)
model <- aov(cotinine ~ gender+racial+gender*racial, data=data3)
summary(model)

# (c)
model <- aov(cotinine ~ gender+racial, data=data3)
summary(model)

model <- aov(cotinine ~ gender, data=data3[which(racial=="White"),])
summary(model)

model <- aov(cotinine ~ gender, data=data3[which(racial=="Black"),])
summary(model)