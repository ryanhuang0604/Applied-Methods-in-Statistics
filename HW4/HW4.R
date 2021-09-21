# Problem 1
# (a)
status <- c("Admit", "Deny")
gender <- c("Male", "Female")
data <- c(490, 210, 280, 220)
table <- expand.grid(status=status, gender=gender)
table <- cbind(table, count=data)
result <- xtabs(count ~ gender+status, table)
addmargins(result)

# (b)
prop.table(result, 1)

# (c)
business <- c(480, 120, 180, 20)
table1 <- expand.grid(status=status, gender=gender)
table1 <- cbind(table1, count=business)
result1 <- xtabs(count ~ gender+status, table1)
prop.table(result1, 1)

law <- c(10, 90, 100, 200)
table2 <- expand.grid(status=status, gender=gender)
table2 <- cbind(table2, count=law)
result2 <- xtabs(count ~ gender+status, table2)
prop.table(result2, 1)


# Problem 2
# (a)
program <- c("Accounting", "Administration", "Economics", "Finance")
gender <- c("Female", "Male")
data <- c(68, 91, 5, 61, 56, 40, 6, 59)
table <- expand.grid(program=program, gender=gender)
table <- cbind(table, count=data)
result <- xtabs(count ~ program+gender, table)
chi <- chisq.test(result)
chi

# (b)
expected <- chi$expected
sum((data-expected)^2 / expected)

# (c)
prop.table(result, 2)

# (d)
observed <- chi$observed
expected <- chi$expected
data <- c((observed-expected)^2 / expected)
table <- expand.grid(program=program, gender=gender)
table <- cbind(table, count=data)
result <- xtabs(count ~ program+gender, table)

observed
expected
result

# (e)
1 - sum(observed) / 722


# Problem 3
# (a)
TA <- c(0.32, 0.41, 0.2, 0.07)
professor <- c(22, 38, 20, 11)
professor / sum(professor)

# (b)
TA * sum(professor)

# (c)
grade <- c(22, 38, 20, 11)
expected <- c(rep(sum(grade)/4, 4))
chi_square <- sum((grade-expected)^2 / expected)

chi_square
pchisq(chi_square, df=3)
1 - pchisq(chi_square, df=3)