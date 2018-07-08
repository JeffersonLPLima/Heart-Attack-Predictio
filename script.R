library(e1071)
library(corrplot)
library(Amelia)
library(aod)
library(tidyverse)
library(broom)
library(ggplot2)
library(stats)
library(survey)
library(plyr)
library(easyGgplot2)

data.raw <- read.csv("~/R Projects/Estatistica Hear Attack Detection/data.csv", na.strings="?")
#check for missing values and look how many unique values there are for each variable using the sapply() 
#function which applies the function passed as argument to each column of the dataframe

#plot(sapply(data.raw,function(x) sum(is.na(x))))

sapply(data.raw,function(x) sum(is.na(x)))
sapply(data.raw, function(x) length(unique(x)))


missmap(data.raw, main = "Missing values vs Observação")
data <- subset(data.raw,select=c(1,2,3,4,5,6,7,8,9,10,14))
missmap(data, main = "Missing values vs Observação")
sapply(data,function(x) sum(is.na(x)))

par(mfrow=c(3,4))

hist(data$age,main = paste("Histogram of" , "age"))
hist(data$sex,main = paste("Histogram of" , "sex"))
hist(data$cp,main = paste("Histogram of" , "cp"))
hist(data$trestbps,main = paste("Histogram of" , "trestbps"))
hist(data$chol,main = paste("Histogram of" , "chol"))
hist(data$fbs,main = paste("Histogram of" , "fbs"))
hist(data$restecg,main = paste("Histogram of" , "restecg"))
hist(data$thalach,main = paste("Histogram of" , "thalach"))
hist(data$exang,main = paste("Histogram of" , "exang"))
hist(data$oldpeak,main = paste("Histogram of" , "oldpeak"))
hist(as.numeric(data$num),main = paste("Histogram of" , "heart problem"))

 
 

mean_ <- function(x, y, ...) {
  mean(x,na.rm=T)
}
median_ <- function(x, y, ...) {
  median(x,na.rm=T)
}

data$chol[(data$num==0 & is.na(data$chol))] <- aggregate(data$chol, by=list(data$num), FUN=median_)$x[1]
data$chol[(data$num==1 & is.na(data$chol))] <- aggregate(data$chol, by=list(data$num), FUN=median_)$x[2]

data$trestbps[(data$num==0 & is.na(data$trestbps))] <- aggregate(data$trestbps, by=list(data$num), FUN=median_)$x[1]
data$trestbps[(data$num==1 & is.na(data$trestbps))] <- aggregate(data$trestbps, by=list(data$num), FUN=median_)$x[2]

data$fbs[(data$num==0 & is.na(data$fbs))] <- aggregate(data$fbs, by=list(data$num), FUN=median_)$x[1]
data$fbs[(data$num==1 & is.na(data$fbs))] <- aggregate(data$fbs, by=list(data$num), FUN=median_)$x[2]

data$restecg[(data$num==0 & is.na(data$restecg))] <- aggregate(data$restecg, by=list(data$num), FUN=median_)$x[1]
data$restecg[(data$num==1 & is.na(data$restecg))] <- aggregate(data$restecg, by=list(data$num), FUN=median_)$x[2]

data$thalach[(data$num==0 & is.na(data$thalach))] <- aggregate(data$thalach, by=list(data$num), FUN=median_)$x[1]
data$thalach[(data$num==1 & is.na(data$thalach))] <- aggregate(data$thalach, by=list(data$num), FUN=median_)$x[2]

data$exang[(data$num==0 & is.na(data$exang))] <- aggregate(data$exang, by=list(data$num), FUN=median_)$x[1]
data$exang[(data$num==1 & is.na(data$exang))] <- aggregate(data$exang, by=list(data$num), FUN=median_)$x[2]


sapply(data,function(x) sum(is.na(x)))

#correlação 2
corrplot(cor(data), method="circle")


# ====== Verificando assunções pra regressão logística ========
model <- glm(num ~.,family=binomial(link='logit'),data=data)
summary(model)
plot(model, which = 4, id.n = 3)
#Multicollinearity if > 5
car::vif(model)

#linearidade
probabilities <- predict(model, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "1","0")

predictors <- colnames(data)
data <- data %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
ggplot(data, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") 
#=========== Ajuste do Modelo ============
subset = -c(1,4,8,7,5,6)

model2 <- glm(num ~.,family=binomial(link='logit'),data=data[,-c(1,4,8,7,5,6)])
summary(model2)

dataa<-data[,-c(1,4,8,7,5,6)]
corrplot(cor(dataa), method="circle")

probabilities <- predict(model2, type = "response")
model_svm <- svm(data$num~., data[,-c(1,4,8,7,5,6)], kernel = "radial", cost = 1, gamma=0.01)
summary(model_svm)
predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
head(predicted.classes)
predictors <- colnames(dataa)
# Bind the logit and tidying the data for plot
dataa <- dataa %>%
  mutate(logit = probabilities) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(dataa, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
#anova(model, test="Chisq")
#anova(model2, test="Chisq")
#regTermTest(model, test.terms, method=c("Wald"))
#anova(model,model2)
 
#======== testes =========

##teste
subset = -c(1,4,8,7,5,6)
data <- slice(data, sample(1:n()))
#Captura o tamanho da amostra
n <- nrow(data)

#100 replicas de 10-folds
R <- 30
folds <- 10
data
mean_acc_rate_glm <- rep(0, R)
mean_acc_rate_svm <- rep(0, R)
acc_rate_glm <- rep(0, folds)
acc_rate_svm <- rep(0, folds)
data$num <- as.factor(data$num)
for(r in 1 : R){
  for(i in 1 : folds){
    s <- sample(1 : n, replace = F, floor(.2*n))
    
    train <- data[-s, ]
    test <- data[s, -ncol(data)]
    target_test <- data[s, ncol(data)]
    
    #Aplica o modelo 1
    model_glm <- glm(num ~.,family=binomial(link='logit'),data=train[,subset])
    #Aplica o modelo 2
    model_svm <- svm(num~., train, kernel = "radial", cost = 1, gamma=0.01)
    
    #Previsao dos modelos
    fitted.results <- predict(model_glm,test[,subset] ,type='response')
    predictions_glm <- ifelse(fitted.results > 0.5,1,0)
    
    #misClasificError <- mean(fitted.results != data$num)
    
    predictions_svm <- predict(model_svm, test)
    
    #Matrizes de confusao
    confusion_matrices_glm <- table(predictions_glm, target_test)
    confusion_matrices_svm <- table(predictions_svm, target_test)
    
    #Calcula a taxa de erro dos classificadores
    acc_rate_glm[i] <- sum(diag(confusion_matrices_glm))/nrow(test)
    acc_rate_svm[i] <- sum(diag(confusion_matrices_svm))/nrow(test)
  }
  #Armazena a taxa de erro media dos folds para cada replica
  mean_acc_rate_glm[r] <- mean(acc_rate_glm)
  mean_acc_rate_svm[r] <- mean(acc_rate_svm)
}
print("glm")
mean(mean_acc_rate_glm)
print("svm")
mean(mean_acc_rate_svm)
summary(mean_acc_rate_glm)
summary(mean_acc_rate_svm)


t.test(mean_acc_rate_glm)
t.test(mean_acc_rate_svm)
res <- wilcox.test(mean_acc_rate_glm, mean_acc_rate_svm,paired = TRUE, alternative = "greater") 
res

summary(mean_acc_rate_glm)
#> erro = 1.96*(sqrt(var(mean_acc_rate_glm))/sqrt(n))
#> erro
#[1] 0.005762322
#> plotCI(x, mean_acc_rate_glm, ui=mean_acc_rate_glm-erro, li=mean_acc_rate_glm+erro)
#> x <- 1:n
#> plotCI(x, mean_acc_rate_glm, ui=mean_acc_rate_glm-erro, li=mean_acc_rate_glm+erro)


pad = function(x, k, n = 1L, append = TRUE)
{
  dims = replicate(length(dim(x)), substitute(), simplify = FALSE)
  if(append) dims[[k]] = c((n + 1):dim(x)[[k]], rep_len(NA, n))
  else dims[[k]] = c(rep_len(NA, n), 1:(dim(x)[[k]] - n))
  do.call("[", c(list(x), dims))
}

 ###### PLOT HIST ############
date = data.frame(data[(data$num==0),-c(1)] , group="0")
date1 = data.frame(data[(data$num==1),-c(1)] , group="1")
x = 1:nrow(data)
dat = rbind(date, date1)
plot1<-ggplot(dat, aes(x=dat$age, fill=dat$num, color=dat$num)) +
  geom_histogram(position="identity", alpha=0.5)

date = data.frame(data[(data$num==0),-c(2)] , group="0")
date1 = data.frame(data[(data$num==1),-c(2)] , group="1")
x = 1:nrow(data)
dat = rbind(date, date1)
plot2<-ggplot(dat, aes(x=dat$sex, fill=dat$num, color=dat$num)) +
  geom_histogram(position="identity", alpha=0.5)


date = data.frame(data[(data$num==0),-c(3)] , group="0")
date1 = data.frame(data[(data$num==1),-c(3)] , group="1")
x = 1:nrow(data)
dat = rbind(date, date1)
plot3<-ggplot(dat, aes(x=dat$cp, fill=dat$num, color=dat$num)) +
  geom_histogram(position="identity", alpha=0.5)

date = data.frame(data[(data$num==0),-c(4)] , group="0")
date1 = data.frame(data[(data$num==1),-c(4)] , group="1")
x = 1:nrow(data)
dat = rbind(date, date1)
plot4<-ggplot(dat, aes(x=dat$trestbps, fill=dat$num, color=dat$num)) +
  geom_histogram(position="identity", alpha=0.5)

date = data.frame(data[(data$num==0),-c(5)] , group="0")
date1 = data.frame(data[(data$num==1),-c(5)] , group="1")
x = 1:nrow(data)
dat = rbind(date, date1)
plot5<-ggplot(dat, aes(x=dat$chol, fill=dat$num, color=dat$num)) +
  geom_histogram(position="identity", alpha=0.5)

date = data.frame(data[(data$num==0),-c(6)] , group="0")
date1 = data.frame(data[(data$num==1),-c(6)] , group="1")
x = 1:nrow(data)
dat = rbind(date, date1)
plot6<-ggplot(dat, aes(x=dat$fbs, fill=dat$num, color=dat$num)) +
  geom_histogram(position="identity", alpha=0.5)

date = data.frame(data[(data$num==0),-c(7)] , group="0")
date1 = data.frame(data[(data$num==1),-c(7)] , group="1")
x = 1:nrow(data)
dat = rbind(date, date1)
plot7<-ggplot(dat, aes(x=dat$restecg, fill=dat$num, color=dat$num)) +
  geom_histogram(position="identity", alpha=0.5)

date = data.frame(data[(data$num==0),-c(8)] , group="0")
date1 = data.frame(data[(data$num==1),-c(8)] , group="1")
x = 1:nrow(data)
dat = rbind(date, date1)
plot8<-ggplot(dat, aes(x=dat$thalach, fill=dat$num, color=dat$num)) +
  geom_histogram(position="identity", alpha=0.5)


date = data.frame(data[(data$num==0),-c(9)] , group="0")
date1 = data.frame(data[(data$num==1),-c(9)] , group="1")
x = 1:nrow(data)
dat = rbind(date, date1)
plot9<-ggplot(dat, aes(x=dat$oldpeak, fill=dat$num, color=dat$num)) +
  geom_histogram(position="identity", alpha=0.5)


summary(dat)
grid.arrange(plot1, plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9, ncol=3, nrow=3)



model.data <- augment(model)
model.data
