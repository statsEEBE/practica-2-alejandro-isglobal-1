#Codigo para problema 2
mis_dades <- iris
mean(mis_dades$Sepal.Length)
sd(mis_dades$Sepal.Length)
dim(mis_dades)
names(mis_dades)
hist(mis_dades$Sepal.Length)

x <- mis_dades$Petal.Length
x
y <- mis_dades$Sepal.Length
y

plot(x, y)

m <- sum( (x-mean(x))*(y-mean(y)) )/sum( (x-mean(x))^2)
m
b <- mean(y) - m*mean(x)
b

m*1.5+b

mod <- lm(y~x)
summary(mod)

xpred <- data.frame(x=1.5)
xpred <- data.frame(x=1:7)
xpred <- data.frame(x=x)
xpred
ypred <- predict(mod, xpred)
ypred


png("plot.png")
plot(x, y)
lines(x, ypred)
dev.off()

Rsq<-sum((ypred-mean(y))^2)/sum((y-mean(y))^2)
Rsq

summary(mod)
