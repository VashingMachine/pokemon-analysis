data <- read.table("data2.txt", sep = "" , header = F , nrows = 100,
                   na.strings ="", stringsAsFactors= F)
data[c(1:2, length(data[,1]) - 1, length(data[,1])),1]
c(1:2, length(data[,1]) - 1, length(data[,1]))
#3
v = data[1,]
v[2:9]
#2
N = 50
c = 0
a = 1
b = 10
for(i in 1:N){
  score = sin(i)^i
  if(score > a && score < b) {
    c = c + 1
  }
}
#2

#readfile
N = scan(file="data.txt", what=numeric())
N[4:length(N)]
N[1:3]

#4
b =  matrix(c(1,2,3,4,5,6), ncol = 3, nrow = 2)
print(b)

N = scan(file="data2.txt", what=numeric())

b = matrix(rnorm(6, 0, 1), ncol = 3, nrow = 2)

b = matrix(sample.int(n = 9, size = 6), nrow = 3, ncol = 2)
print(b)
which.max(b[which.max(b),])
which(b==min(b), arr.ind=T)
pmin(b)
max(b)
b[,-1]

n = 13
i = 1:n
j = 2*i
k = 3*i
matrix(c(i,j,k), ncol = 3, nrow = n)

b = matrix(sample.int(n = 9, size = 6), ncol = 2, nrow = 3)
?sum
w = rowSums(b)
print(b)
apply(b, 1, prod)

?data.frame

odp = c("A", "B", "A", "A")
odp = factor(odp)
class(odp) 
levels(odp) = c("Adamowicz", "Płażyński")
as.numeric(odp)
table(odp)
ocena = c(4, 5, 5, 5, 4, 4, 5)
table(ocena)
table(ocena) / length(ocena) * 100

barplot(table(ocena))

pie(table(ocena))
plec = c("m", "f", "?", "m", "m", "k")
plec = factor(plec)
wiec = c(30, 25, 20, 60, 22, 25)
wzrost = c(170, 176, 220, 180, 181, 175)

dane = data.frame(plec, wiec, wzrost)
names(dane) = c("płeć", "wiek", "wzrost")
dane[order(dane$wiek, decreasing = TRUE),]
order(dane$wiek)

library(MASS)
cats

model = lm(cats$Hwt ~ I(cats$Bwt))
plot(cats$Bwt, cats$Hwt)
a = model$coefficients[2]
b = model$coefficients[1]
summary(model)
sum( (cats$Hwt - fitted(model))^2  )
sum(model$residuals^2) / nrow(cats)


abline(b, a)

s = mean(cats$Hwt)
big.cats = cats[cats$Hwt >= s,]
big.model = lm(big.cats$Hwt ~ big.cats$Bwt)
plot(big.cats$Bwt, big.cats$Hwt)
abline(big.model)

#Y ~ -1 + X to Y = aX
# Y ~ X:X to Y = a*X*X


m1 = lm(trees$Girth ~ trees$Height)
summary(m1)
m2 = lm(trees$Girth ~ trees$Volume)
summary(m2)
m3 = lm(trees$Girth ~ trees$Volume + trees$Height)
summary(m3)

m5 = lm(trees$Girth ~ trees$Height * trees$Volume)
sum(resid(m5)^2)

m5 = lm(trees$Girth ~ trees$Height * trees$Volume)
sum(resid(m5)^2)

wzrost = rnorm(30, 170, 20)
egzamin = sample(c(0, 1), 30, replace = T)
dane = data.frame(wzrost, egzamin)
model = lm(egzamin ~ wzrost)
par(mfrow=c(1,2))
plot(wzrost, egzamin)
abline(model)
points(wzrost, fitted(model))
cdplot(wzrost, factor(egzamin), yaxlabels = c("nie zdał", "zdał"))


sums = c()
size = 2000
data = sample(c(-1, 1), size, replace = T, prob = c(0.45, 0.55))
for( i in 1:size ){
  sums = c(sums, sum(data[1:i]))
}
print(sums)
plot(1:size, sums)
abline(1, 0)

dane = Mieszkania
model = lm(dane$cena ~ dane$powierzchnia + dane$dzielnica + dane$typ.budynku)

summary(model)
sum(resid(model)^2)
#### laboratoria 6
dane = read.csv2("wakacje.csv", row.names = 1)
attach(dane)
dane$wakacje = as.numeric(dane$wakacje) - 1
hist(dane$wiek)
qqnorm(wiek)
hist(dochod)
qqnorm(dochod)
logdochod = log(dochod)
hist(logdochod)
dane$logdochod = logdochod
cor(logdochod, wiek)
summary(glm(dane$wakacje ~ wyksztalcenie + plec + wiek + logdochod, dane, family=binomial(link = "logit")))
summary(glm(dane$wakacje ~ wyksztalcenie + plec + logdochod, dane, family=binomial(link = "logit")))
summary(glm(dane$wakacje ~ wyksztalcenie + logdochod, dane, family=binomial(link = "logit")))
model = glm(dane$wakacje ~ wyksztalcenie + logdochod, dane, family=binomial(link = "logit"))
model2 = glm(dane$wakacje ~ logdochod, dane, family=binomial(link = "logit"))
w=function(wyksz, dochod) {
  l = coef(model)[1] + coef(model)[2] * (wyksz=="srednie") + coef(model)[3] * (wyksz=="wyzsze") + coef(model)[4] * log(dochod)
  p = exp(l)/(1+exp(l))
  cat("Kraj: ", 1-p, "\n")
  cat("Zagranica: ", p, "\n")
}
w(c("srednie", "wyzsze"), c(3000,3000))

plot(logdochod, dane$wakacje)
points(logdochod, fitted(model2))

dane = read.csv2("wybory.csv", row.names = 1)
head(dane)
hist(dane$wiek)
dane$partia = as.numeric(dane$partia) - 1 #A = 0
dane$partia
model = glm(dane$partia ~ dane$plec + dane$wiek, family=binomial(link = "logit"))
summary(model)

w=function(plec, wiek) {
  l = coef(model)[1] + coef(model)[2] * (plec=="mezczyzna") + coef(model)[3] * wiek
  p = exp(l)/(1+exp(l))
  cat("A: ", 1-p, "\n")
  cat("B: ", p, "\n")
}
w("kobieta", 1200)

table(factor(dane$partia), factor(dane$poglady))

#lad7
#LDA QDA kNN
library(nnet)
library(MASS)
names(iris)=c("ddk", "sdk", "dp", "sp", "klasa")
hist(iris[iris$klasa == "setosa",]$dp)

modellda = lda(klasa ~ sp + dp, data=iris)
summary(modellda)
modellda
pred = predict(modellda, as.data.frame(cbind(sp, dp)))
plot(sp, dp, type="n", ylab="długość płatka", xlab="szerokość płatka" )
points(cbind(sp, dp), pch=as.numeric(klasa) + 4)
x = seq(min(sp), max(sp), length=100)
y = seq(min(dp), max(dp), length=100)
siatka = expand.grid(sp=x, dp=y)
siatka
pred2=predict(modellda, siatka)
z = as.numeric(pred2$class)
m = matrix(z, ncol = 100, nrow=100)
contour(x, y, m, add=T, levels=c(1.5,2.5))
points(mean(sp[which(klasa=="setosa")]), mean(dp[which(klasa=="setosa")]), cex=3)
#knn
library(class)
modelknn=knn(cbind(sp,dp), cbind(sp,dp), klasa, k=49)
modelknn
blad=1 - sum(modelknn == klasa) / 150
plot(sp, dp, type="n", ylab="długość płatka", xlab="szerokość płatka" )
points(cbind(sp, dp), pch=as.numeric(klasa) + 4)
pred = knn(cbind(sp, dp), siatka, klasa, k=25)
z=as.numeric(pred)
m=matrix(z, ncol=100, nrow=100)
contour(x, y, m, add=T, levels=c(1.5, 2.5))

#zadania
#1
#Czy QDA lepszy od LDA? Analiza QDA
model.qda = qda(klasa ~ sp + dp, data=iris)
model.lda = lda(klasa ~ sp + dp, data=iris)
targets = klasa
sum(predict(model.qda)$class == targets)
sum(predict(model.lda)$class == targets)

plot(sp, dp, type="n")
points(cbind(sp, dp), pch=as.numeric(klasa) + 4)
x = seq(min(sp), max(sp), length=100)
y = seq(min(dp), max(dp), length=100)
my.grid = expand.grid(sp=x, dp=y)
predictions=predict(model.qda, my.grid)
z = as.numeric(predictions$class)
m = matrix(z, ncol = 100, nrow=100)
contour(x, y, m, add=T)

#2

knn.iris = function(k) {
  model = knn(cbind(sp,dp), cbind(sp,dp), klasa, k=k)
  return(150 - sum(targets == model))
}
x = 1:50
y = c()
for(i in x) {
  y = c(y, knn.iris(i))
}
plot(x, y, type = "l")

###### 05.12
library(MASS)
library(class)
train = read.csv2("Uczenie.csv", row.names = 1)
test = read.csv2("Rozpoznanie.csv", row.names = 1)
model.lda = lda(plec ~ wzrost + waga, train)
model.qda = qda(plec ~ wzrost + waga, train)
model.knn = knn(train = cbind(train$wzrost, train$waga), test = cbind(test$wzrost, test$waga), train$plec, k = 20)

#porównanie
pred.lda = predict(model.lda, test)
pred.qda = predict(model.qda, test)
pred.knn = as.numeric(model.knn) - 1
targets = as.numeric(test$plec) - 1
pred.lda = as.numeric(pred.lda$class) - 1
pred.qda = as.numeric(pred.qda$class) - 1
error.lda = sum((targets - pred.lda)^2)
error.qda = sum((targets - pred.qda)^2)
error.knn = sum((pred.knn - targets)^2)

plot(test$wzrost, test$waga, type="n")
points(cbind(test$wzrost, test$waga), pch = as.numeric(test$plec) + 3)
y = seq(min(test$waga), max(test$waga), length=100)
x = seq(min(test$wzrost), max(test$wzrost), length=100)
my.grid = expand.grid(wzrost=x, waga=y)
model.knn = knn(train = cbind(train$wzrost, train$waga), test = my.grid, train$plec, k = 20)
z = as.numeric(model.knn)
m = matrix(z, ncol = 100, nrow=100)
contour(x, y, m, add=T)

#własne knn

my.knn = function(train.data, test.data, groups) {
  test.data = (test.data - mean(test.data)) / sd(test.data) #normalizacja
  train.data = (train.data - mean(train.data)) / sd(train.data) #normalizacja
  results = c()
  for( i in 1:nrow(test.data)) {
    record = test.data[i,]
    temp = (train.data - record)^2
    temp = rowSums(temp)
    min.pos = which(temp == min(temp))
    results = c(results, groups[min.pos])
  }
  return(results - 1)
}
pred = my.knn(cbind(train$wzrost, train$waga), cbind(test$wzrost, test$waga), train$plec)
sum((pred - targets)^2)

###12.05
names(iris) = c("sl","sw","pl","pw","klasa")
library(tree)
drzewo = tree(klasa ~ ., data = iris )
dt
plot(dt)
text(dt)
sum(predict(drzewo, type = "class") == iris$klasa)
drzewo3 = prune.tree(drzewo, best = 4)
plot(drzewo3)
text(drzewo3)
partition.tree(tree = drzewo3)
sum(predict(drzewo3, type = "class") == iris$klasa)
drzewo2 = tree(klasa ~ pl + pw, data = iris)
sum(predict(drzewo2, type = "class") == iris$klasa)

partition.tree(drzewo2)
text(iris$pl, iris$pw, c("s","c","v")[iris$klasa])
library(nnet)
dane=cbind(iris$pl, iris$pw)
head(klasa)
class.ind(iris$klasa)
?nnet
dane
model=nnet(dane, class.ind(iris$klasa), softmax = T, size=30, maxit = 100000)
predict(model)
x = seq(min(iris$pl), max(iris$pl), length=100)
y = seq(min(iris$pw), max(iris$pw), length=100)
my.grid = expand.grid(pl=x, pw=y)
pred = predict(model, my.grid, type="class")
z = as.numeric(as.factor(pred))
m=matrix(z, ncol=100, nrow = 100)
plot(iris$pl, iris$pw, type="n")
text(iris$pl, iris$pw, c("s","c","v")[iris$klasa])
contour(x, y, m, add=T, levels = c(1.5,2.5))
pred = predict(model, type="class")
sum(pred == iris$klasa)

###1
#1
library(MASS)
train = Cars93[1:10,]
test = Cars93[31:nrow(Cars93),]
head(train)
#2
library(tree)
library(nnet)
model = tree(Origin ~ Cylinders + Horsepower + Length + Width, data = train)

model
plot(model)
text(model)
sum(test$Origin == predict(model, test, type = "class"))
table(test$Origin, predict(model, test, type = "class"))

train = Cars93
test = Cars93[61:nrow(Cars93),]
dane = cbind(train$Cylinders, train$Horsepower, train$EngineSize, train$Length, train$Weight)
indicator = class.ind(train$Origin)
model.a = nnet(dane, indicator, size=8 )
model.b = nnet(dane, indicator, softmax = T, size=23, maxit = 100000)
predict(model.a) 
predict(model.a, dane  , type = "class")

a = 1:10
a[-3]
