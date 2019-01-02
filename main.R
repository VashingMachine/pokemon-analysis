#przygotowywanie danych
set.seed(1337) #aby wyniki były jednakowe po każdym uruchomieniu skryptu
data = read.csv2("Pokemon.csv", sep = ",")[,-1]
index = sample.int(nrow(data), 800, replace = F) 
index.train = index[1:600]
index.test = index[601:800]
train = data[index.train,]
test = data[index.test,]
attach(train)

#deklaracji funkcji kosztu i normalizacji danych69
cost = function(y, t) {
  return( sum( (y-t)^2 ) / length(y) )
}

normalize <- function(x) { #znaleziona noralizacja w internecie, dobrze działa z neuralnet
  return ((x - min(x)) / (max(x) - min(x)))
}

normalize.2 <- function(x) { 
  return((x - mean(x))/sd(x))
}
###### Problem regresyjny: Próbujemy ustalić HP pokemona w zależności od jego pozostałych statystyk

### Model liniowy
model.linear.full = lm(HP ~ Attack + Defense + Sp..Atk + Sp..Def + Speed + Type.1 + Legendary, train)
model.linear.optimal = lm(HP ~ Attack + Defense + Sp..Def, train) #tutaj metodą sprawdzania istotności zmiennych doszedłem do tego modelu

prediction.full = predict(model.linear.full, test)
prediction.optimal = predict(model.linear.optimal, test)
cost.linear.optimal = cost(prediction.optimal, test$HP)
cost.linear.full = cost(prediction.full, test$HP)

### Sieci neuronowe
library(neuralnet)
data.nnet = subset(data, select = c("Attack", "Defense", "Sp..Def" , "HP"))
scaled = as.data.frame(lapply(data.nnet, normalize.2))
train_ = scaled[index.train,]
test_ = scaled[index.test,]

n <- names(train_)
f <- as.formula(paste("HP ~", paste(n[!n %in% "HP"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

pr.nn <- compute(nn,test_[,-4])
pr.nn_ <- pr.nn$net.result * sd(data$HP) + mean(data$HP)
cost.nnet.n =  cost(pr.nn_, test$HP) #Koszt z użyciem normalizacji do rozkładu normalnego

maxs <- apply(data.nnet, 2, max)
mins <- apply(data.nnet, 2, min)
scaled = as.data.frame(scale(data.nnet, center = mins, scale = maxs - mins))
train_ = scaled[index.train,]
test_ = scaled[index.test,]
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

pr.nn <- compute(nn,test_[,-4])
pr.nn_ <- pr.nn$net.result*(max(data$HP)-min(data$HP))+min(data$HP)
cost.nnet.nmin = cost(pr.nn_, test$HP) #Kosz z przesklaowaniem do centrum w minimum

### Drzewo decyzyjne
library(tree)
train.dt = data[index.train,]
test.dt = data[index.test,]
model.dt = tree(HP ~ Type.1 + Attack + Defense + Sp..Atk + Sp..Def + Speed + Generation + Legendary, data = train.dt)
plot(model.dt)
text(model.dt)
cost.tree.single =  cost(predict(model.dt, test.dt), test$HP)
### Losowy las? (Random Forest)
library(randomForest)
model.rf = randomForest(HP ~ Type.1 + Attack + Defense + Sp..Def, train.dt, keep.forest=T, ntree=100)
cost.tree.many = cost(predict(model.rf, test.dt), test.dt$HP)

cost.mean = cost(test$HP, rep( mean(test$HP), length(test$HP) ))

cat(paste("Total: ", cost.mean))
cat(paste("Linear full: ", cost.linear.full))
cat(paste("Linear optimal: ", cost.linear.optimal))
cat(paste("NNet normalization: ", cost.nnet.n)) 
cat(paste("NNet normalization min: ", cost.nnet.nmin)) #a ten najlepszy ze wszystkich
cat(paste("Decision tree: ", cost.tree.single))
cat(paste("RandomForest: ", cost.tree.many))

###### Problemy klasyfikacji

### Regresja logistyczna
# Czy pokemon jest legendarny?
model.lr = glm(Legendary ~ Total + HP + Attack + Defense, train, family=binomial(link = "logit"))
A = as.numeric(predict(model.lr, type = "response") > 0.5)
B = as.numeric(train$Legendary) - 1
A = as.factor(A)
levels(A) = c("Non-Legendary", "Legendary")
B = as.factor(B)
levels(B) = c("Non-Legendary", "Legendary")
table(A,B) #Na danych treningowych

A = as.numeric(predict(model.lr, test, type = "response") > 0.5)
B = as.numeric(test$Legendary) - 1
A = as.factor(A)
levels(A) = c("Non-Legendary", "Legendary")
B = as.factor(B)
levels(B) = c("Non-Legendary", "Legendary")
table(A,B) #Na danych testowych


### kNN
#Jakiego typu jest pokemon?
library(class)
model.knn=knn(train = cbind(train$Attack, train$HP, train$Defense, train$Sp..Atk, train$Sp..Def, train$Speed), test = cbind(test$Attack, test$HP, test$Defense, test$Sp..Atk, test$Sp..Def, test$Speed), train$Type.1, k=10)
table(model.knn, test$Type.1)
sum(model.knn == test$Type.1)

records = c()
for(i in 1:10) {
  model.knn=knn(train = cbind(train$Attack, train$HP, train$Defense, train$Sp..Atk, train$Sp..Def, train$Speed), test = cbind(test$Attack, test$HP, test$Defense, test$Sp..Atk, test$Sp..Def, test$Speed), train$Type.1, k=i)
  records = c(records, sum(model.knn == test$Type.1))
}
plot(1:10, records)
k = which(records == max(records))
model.knn=knn(train = cbind(train$Attack, train$HP, train$Defense, train$Sp..Atk, train$Sp..Def, train$Speed), test = cbind(test$Attack, test$HP, test$Defense, test$Sp..Atk, test$Sp..Def, test$Speed), train$Type.1, k=k)
usefullness.knn = records[k]
plot(test$HP, test$Attack)

#Sprawdzam czy statystyki się różnią pomiędzy grupami, a jeśli tak, to jakie statystyki najbardziej
summary(manova(cbind(Attack, HP, Defense, Sp..Atk, Sp..Def) ~ Type.1))
summary(aov(HP ~ Type.1)) 
summary(aov(Attack ~ Type.1)) 
summary(aov(Defense ~ Type.1)) 
summary(aov(Sp..Atk ~ Type.1)) 
summary(aov(Sp..Def ~ Type.1)) 


### LDA - zakładam normalność wszystki statystyk
hist(HP)
library(MASS)

model.lda = lda(Type.1 ~ HP + Attack + Defense + Sp..Atk + Sp..Def + Speed, data = train)
predictions.lda = predict(model.lda, test)$class
table(predictions.lda, test$Type.1)
usefullness.lda = sum(predictions.lda == test$Type.1)

### Sieci neuronowe
library(nnet)

data.nnet.c = subset(data, select = c("HP", "Attack", "Defense" , "Sp..Atk", "Sp..Def", "Speed"))
maxs.c <- apply(data.nnet.c, 2, max)
mins.c <- apply(data.nnet.c, 2, min)
scaled.c = as.data.frame(scale(data.nnet.c, center = mins.c, scale = maxs.c - mins.c))
scaled.c = cbind(scaled.c, class.ind(data$Type.1))
train_.c = scaled.c[index.train,]
test_.c = scaled.c[index.test,]
n.c <- names(train_.c)
levels(data$Type.1)
toPredictLabel = paste(levels(data$Type.1), collapse = " + ")
f.c <- as.formula(paste(toPredictLabel, "~", paste(c("HP", "Attack", "Defense" , "Sp..Atk", "Sp..Def", "Speed"), collapse = " + ")))
nn <- neuralnet(f.c,data=train_.c, hidden=c(2,2), linear.output=F, stepmax = 100000, act.fct = "logistic")
pr.nn <- compute(nn,test_.c[,1:6])
pr.nn = pr.nn$net.result
predictions = levels(data$Type.1)[max.col(pr.nn)]
usefullness.nnet = sum(test$Type.1 == predictions)

### RandomForest

model.rf.c = randomForest(Type.1 ~ HP + Attack + Defense + Sp..Atk + Sp..Def + Speed, train, keep.forest=T, ntree=100)
usefullness.rf = sum(predict(model.rf.c, test) == test$Type.1)

print(paste("KNN: ",usefullness.knn))
print(paste("LDA: ",usefullness.lda))
print(paste("NNET: ",usefullness.nnet))
print(paste("RANDOMFOREST: ",usefullness.rf))




