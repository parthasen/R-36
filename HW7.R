load(url("http://eeyore.ucdavis.edu/stat141/Data/linearModelData.rda"))
library(ggplot2)
library(smoothmest)

A = devices[devices$device=='A',1:3]
B = devices[devices$device=='B',1:3]
model_a = lm(y~x , data = A)
model_b = lm(y~x , data = B)
model = lm(y~x, data = devices)
w = residuals(model)
w_a = residuals(model_a)
w_b = residuals(model_b)
#coefficients(model_a) = coef(model_a) = coefficients(model_a)
#model_a$residuals = residuals(model_a) = resi(model_a)
w_a
devices[,3]
Part0 = function(data){
  A = data[data$device=='A',1:2]
  B = data[data$device=='B',1:2]
  w_a = 1/(1/(nrow(A)-2)*sum(residuals(lm(y~x, data = A))^2))
  w_b = 1/(1/(nrow(B)-2)*sum(residuals(lm(y~x, data = B))^2))
  data = data.frame(rbind(A,B), weight = rep(c(w_a, w_b), c(nrow(A), nrow(B))))
  model = lm(y~x, data, weight = data$weight)
  return(list(coef(model), residuals(model)))
}

Part1_sep = function(data, number = 1000){
  dataA = data[data$device=='A',1:2]
  dataB = data[data$device=='B',1:2]
  beta = replicate(number,{
    A = dataA[sample(1:nrow(dataA), nrow(dataA), replace = TRUE), ]
    B = dataB[sample(1:nrow(dataB), nrow(dataB), replace = TRUE), ]
    w_a = 1/(1/(nrow(A)-2)*sum(residuals(lm(y~x, data = A))^2))
    w_b = 1/(1/(nrow(B)-2)*sum(residuals(lm(y~x, data = B))^2))
    data = data.frame(rbind(A,B), weight = rep(c(w_a, w_b), c(nrow(A), nrow(B))))
    model = lm(y~x, data, weight = data$weight)
    coef(model)
  })
  result = matrix(c(var(beta[1,]), cov(beta[1,], beta[2,]), 
                    cov(beta[1,], beta[2,]), 
               var(beta[2,])), nrow = 2, ncol = 2)
   return(result)
}

Part1_all = function(data, number = 1000){
  beta = replicate(number,{
    data = data[sample(1:nrow(data), nrow(data), replace = TRUE), ]
    A = data[data$device=='A',1:2]
    B = data[data$device=='B',1:2]
    w_a = 1/(1/(nrow(A)-2)*sum(residuals(lm(A$y~A$x, data = A))^2))
    w_b = 1/(1/(nrow(B)-2)*sum(residuals(lm(B$y~B$x, data = B))^2))
    data = data.frame(rbind(A,B), weight = rep(c(w_a, w_b), c(nrow(A), nrow(B))))
    model = lm(data$y~data$x, data, weight = data$weight)
    coef(model)
  })
  result = matrix(c(var(beta[1,]), cov(beta[1,], beta[2,]), 
                    cov(beta[1,], beta[2,]), 
                    var(beta[2,])), nrow = 2, ncol = 2)
  return(result)
}


Part1_sep(devices)
Part1_all(devices)
temp
sunny(devices)
Part0(devices)

### Part 2
data = Part0(devices)
beta = data[[1]]
X = devices
e = sample(data[[2]], length(data[[2]]), replace = TRUE)
Y = beta[1] + beta[2]*devices$x + e
X$y = Y

temp = Part1_sep(X)
temp

p3a = function(data){
  A = data[data$device=='A',1:2]
  #B = data[data$device=='B',1:2]
  w_a = 1/(1/(nrow(A)-2)*sum(residuals(lm(y~x, data = A))^2))
  #w_b = 1/(1/(nrow(B)-2)*sum(residuals(lm(y~x, data = B))^2))
  data = data.frame(A, weight = rep(w_a, nrow(A)))
  model = lm(y~x, data, weight = data$weight)
  return(list(coef(model), residuals(model)))
}

p3a = function(data){
  A = data[data$device=='A',1:2]
  #B = data[data$device=='B',1:2]
  w_a = 1/(1/(nrow(A)-2)*sum(residuals(lm(y~x, data = A))^2))
  #w_b = 1/(1/(nrow(B)-2)*sum(residuals(lm(y~x, data = B))^2))
  data = data.frame(A, weight = rep(w_a, nrow(A)))
  model = lm(y~x, data, weight = data$weight)
  return(list(coef(model), residuals(model)))
}


p3b = function(data){
  #A = data[data$device=='A',1:2]
  B = data[data$device=='B',1:2]
  #w_a = 1/(1/(nrow(A)-2)*sum(residuals(lm(y~x, data = A))^2))
  w_b = 1/(1/(nrow(B)-2)*sum(residuals(lm(y~x, data = B))^2))
  data = data.frame(B, weight = rep(w_b, nrow(B)))
  model = lm(y~x, data, weight = data$weight)
  return(list(coef(model), residuals(model)))
}


modelA = p3a(devices)[[2]]
modelB = p3b(devices)[[2]]
modelA
a = rdoublex(4000, mu =0, lambda = 1/ (4000/sum(abs(modelA))))
b = rdoublex(6000, mu =0, lambda = 1/ (6000/sum(abs(modelB))))
#b = rdoublex(6000, mu =0, lambda = 1/ (6000/sum(abs(resid(model_b)))))
X = devices
Y = beta[1] + beta[2]*devices$x + c(a, b)
Y
X$y = Y
temp = Part1_sep(X)
temp


# Part 3
# PART 0 
# Part 0 is taking the original devices data frame, then fitting 
# a linear model for each of the devices. Then find the weights 
# of each device using the residuals of those two linear models, 
# and use those weights to fit another linear model, this time on 
# the all of the data. You will use that final linear model in 
# part 1-3.

# PART 3
# I think you're supposed to use both devices for all 3 parts 
# of the assignment. You only look at them separately in step 
# 0 to find weights. 

# to look at any extreme values or unusual statistics (for example, 
# is the covariance in a particular bootstrap sample really high 
# or low?) and discuss why they are like that. 

sunny = function(data, number = 1000){
  beta = replicate(number,{
    data=data[sample(1:nrow(data), 10000, replace=T),]
    data.A1 <- data[data$device == "A",]
    data.B1 <- data[data$device == "B",]
    mA1 <- lm(data.A1$y~data.A1$x, data = data.A1)
    mB1 <- lm(data.B1$y~data.B1$x, data = data.B1)
    MLE.A1 <- (nrow(data.A1)-1)/(nrow(data.A1)-2)*var(resid(mA1))
    MLE.B1 <- (nrow(data.B1)-1)/(nrow(data.B1)-2)*var(resid(mB1))
    
    data1 <- data[order(data$device),]
    dim(data1)
    weights.reg1 <- rep(c(1/MLE.A1, 1/MLE.B1) , c(dim(data.A1)[1], dim(data.B1)[1]))
    mod1 <- lm(data1$y ~ data1$x , weights = weights.reg1 , data = data1)
    coef(mod1)
  })
  #   result = beta
  result = matrix(c(var(beta[1,]), cov(beta[1,], beta[2,]), 
                    cov(beta[1,], beta[2,]), 
                    var(beta[2,])), nrow = 2, ncol = 2)
  return(result)
  
}

