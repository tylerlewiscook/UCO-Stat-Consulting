# Stat Fundamentals

# Summary statistics -----------------------------------------------------------

movies <- read.csv(url("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-23/movie_profit.csv"), row.names = 1)
movies$dg_mil <- movies$domestic_gross/1000000
movies$pb_mil <- movies$production_budget/1000000

table(movies$genre)
table(movies$genre, movies$mpaa_rating)

mean(movies$domestic_gross)
sd(movies$domestic_gross)
summary(movies$domestic_gross)

aggregate(domestic_gross ~ genre, data = movies, mean)



# Basic plotting ---------------------------------------------------------------

barplot(table(movies$genre))
barplot(table(movies$genre), col = "blue")
barplot(table(movies$genre), horiz = TRUE)

hist(movies$domestic_gross)
hist(movies$dg_mil, xlab = "Domestic Gross (in millions)", 
     main = "Distribution of Domestic Gross")

boxplot(movies$dg_mil)
boxplot(movies$dg_mil ~ movies$genre)

plot(movies$pb_mil, movies$dg_mil)



# One sample t test ------------------------------------------------------------

t.test(airquality$Temp, mu = 75)
out1 <- t.test(airquality$Temp, mu = 75)

names(out1)
out1$statistic
out1$p.value
out1$conf.int

t.test(airquality$Temp, mu = 79, alternative = "less")



# Two sample t test ------------------------------------------------------------

library(ISLR)
data(College)

# Independent 
t.test(Accept ~ Private, data = College)
t.test(Accept ~ Private, var.equal = TRUE, data = College)
# Note: That code assumes one column of the data is the variable of interest and 
# another column is a grouping variable.  Can also do t.test(group1, group2)

# Paired
before <- c(180, 195, 235, 250, 210, 300)
after <- c(178, 196, 232, 242, 214, 298)
dif <- before - after
t.test(dif)

wt <- data.frame(before, after)
t.test(before, after, paired = TRUE, data = wt)



# One-way ANOVA ----------------------------------------------------------------

doughnut <- read.csv(file = "Data/doughnut.csv")

doughnut$oil <- as.factor(doughnut$oil)
out2 <- aov(fat ~ oil, data = doughnut)
summary(out2)


# Multiple comparisons
TukeyHSD(out2)
plot(TukeyHSD(out2))



# Chi-squared goodness of fit --------------------------------------------------

peas <- c(315, 108, 102, 31)
chisq.test(peas, p = c(9/16, 3/16, 3/16, 1/16))

# The above assumes you already have the observed counts for each category.  
# If don't have observed counts calculated already, then do something like:

data(Credit)

chisq.test(table(Credit$Ethnicity))



# Contingency tables -----------------------------------------------------------

titanic <- matrix(c(202, 118, 178, 212, 123, 167, 528, 696), nrow = 4, ncol = 2)
chisq.test(titanic)

# The above assumes you already have the observed counts for each category.  
# If don't have observed counts calculated already, then do something like:

chisq.test(table(Credit$Gender, Credit$Student))
chisq.test(Credit$Gender, Credit$Student)



# Multiple linear regression ---------------------------------------------------

internet <- read.csv(file = "Data/internet.csv")

internet$gender <- as.factor(internet$gender)
internet$employ <- as.factor(internet$employ)

fit1 <- lm(data ~ ., data = internet)
summary(fit1)


fit2 <- lm(data ~ speed + age + gender, data = internet)
summary(fit2)
speed <- c(20, 30, 10)
age <- c(32, 19, 24)
gender <- as.factor(c(1, 1, 0))
newdat2 <- data.frame(speed, age, gender)
predict(fit2, newdat2)

fit3 <- lm(data ~ speed + gender + speed:gender, data = internet)
summary(fit3)

fit4 <- lm(data ~ speed*gender, data = internet)
summary(fit4)

