#Codigo para problema 2
mis_dades <- iris
x <- mis_dades$Petal.Length
mean(x)
sd(x)
hist(x)

y <- mis_dades$Sepal.Length
mean(y)

plot(x, y)

m <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
m
b <- mean(y)-m*mean(x)
b

m*1.5+b

####

mod <- lm(y~x)
mod
summary(mod)

data.frame(x=x)

ypred <- predict(mod, data.frame(x=x))

png("plot.png")
 plot(x,y,col="red", pch=16)
 lines(x, ypred)
dev.off()

Rsq <- sum((ypred-mean(y))^2)/sum((y-mean(y))^2)
Rsq

summary(mod)
