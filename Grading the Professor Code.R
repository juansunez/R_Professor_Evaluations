evals <- read.csv("C:/Users/jsunez/Desktop/evals.csv",header=T)
attach(evals)
#Problem1
print("Summary of Variable 'score':"); summary(score);
print(paste("Standard Deviation:", sd(score), sep = " "))
hist(score, xlab = "Eval Score", ylab = "Frequency", col = "green", main = "Histogram of Evaluation Scores")
#Problem2
plot(score, bty_avg, pch=17, xlim=c(0,5), ylim=c(0,10), col="blue", main = "Scores vs Beauty Plot", xlab = "Score", ylab = "Beauty")
library(ggplot2)
dat <- data.frame(score, bty_avg)
#Linear Regression Line
ggplot(dat, aes(x=score, y=bty_avg)) + geom_point(color="red") + geom_smooth(method = lm) +
  labs(x="Score", y="Beauty Average", title="Beauty Average vs. Score")
#Problem3
gndr <- table(gender)
gndr
barplot(gndr, main="Gender", xlab="Gender", col = c("pink", "blue"))
#Problem4
boxplot(score~gender, main="Boxplot of Score by Gender", col=c("pink", "blue"))
par(mfrow=c(2,1))
hist(score[gender=="male"], xlab = "Male", col = "blue", main = "Histogram of Male Scores")
hist(score[gender=="female"], xlab = "Female", col = "pink", main = "Histogram of Female Scores")
t.test(score[gender=="female"], score[gender=="male"])
t.test(score~gender, data=evals)
#Problem5
rank.levels <- table(rank, cls_level)
par(mfrow=c(1,1))
barplot(prop.table(t(rank.levels), 2), beside = T, legend.text = colnames(rank.levels), col=c(2:3), main = "Professor Ranking and Course Level")