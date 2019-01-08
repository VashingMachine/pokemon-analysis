library(nnet)
library(neuralnet)
set.seed(1337) #aby wyniki były jednakowe po każdym uruchomieniu skryptu
data = read.csv2("Pokemon.csv", sep = ",")[,-1]
index = sample.int(nrow(data), 800, replace = F) 
index.train = index[1:400]
index.test = index[401:800]

type.1 = class.ind(data$Type.1)
type.1 = cbind(None = rep(0, 800),type.1)
type.2 = class.ind(data$Type.2)
colnames(type.2) = c("None",colnames(type.2)[-1])
type.indicator = type.1 + type.2
data.selected = subset(data, select = c("HP", "Attack", "Defense" , "Sp..Atk", "Sp..Def", "Speed"))
maxs <- apply(data.selected, 2, max)
mins <- apply(data.selected, 2, min)
scaled = as.data.frame(scale(data.selected, center = mins, scale = maxs - mins))
scaled = cbind(scaled, type.indicator)
train = scaled[index.train,]
test = scaled[index.test,]
toPredictLabel = paste(colnames(type.indicator), collapse = " + ")
PredicatorsLabel = paste(c("HP", "Attack", "Defense" , "Sp..Atk", "Sp..Def", "Speed"), collapse = " + ")
f = as.formula(paste(toPredictLabel, " ~ ", PredicatorsLabel))
nn <- neuralnet(f,data=train, hidden=5, linear.output=F, stepmax = 100000, err.fct = "ce")
test.values = test[,1:6]
test.indicator = test[,-(1:6)]
pr.nn = compute(nn, test.values)
results = pr.nn$net.result
first.predicted.type = max.col(results)

results.c = results
for(i in 1:nrow(results.c)) {
  results.c[i,first.predicted.type[i]] = 0
}
second.predicted.type = max.col(results.c)
first.predicted.type.name = colnames(type.indicator)[first.predicted.type]
second.predicted.type.name = colnames(type.indicator)[second.predicted.type]

types = data[index.test,]$Type.1
types.2 = data[index.test,]$Type.2
optimistic.test <- types == first.predicted.type.name | types == second.predicted.type.name | types.2 == first.predicted.type.name | types.2 == second.predicted.type.name
sum(optimistic.test)

