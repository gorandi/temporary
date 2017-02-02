closeAllConnections()
rm(list=ls())
options(stringsAsFactors = FALSE)
library(nycflights13)
library(tidyverse)
library(simputation)
library(missForest)
library(magrittr)
library(caret)
Donor <- iris[1:75,1:5] #Donor file
Recipient <- select(iris[76:150,1:5],1,2,3,4) #Recipient file
ComRecipient <- as.data.frame(lapply(intersect(names(Donor), names(Recipient)), function(name) Recipient[name])) #Makes a dataframe of the recipient containing only common variables.
FusedData<-bind_rows(ComRecipient,Donor) #binds the donor with recipient except for entries with <NA> which is the non-common variable
#Imputation <- function(arg1, arg2){
 # if (is.factor(arg1)==TRUE) {impute_cart(arg1 ~.)}
#  else{impute_lm(arg1 ~ arg2)}
#}
FusedData %>% impute_rf(Species ~ .)
#FusedData %>% impute_cart(Species ~ .) 
set.seed(1)
Flyg <- flights %>% sample_n(100)
Flyg2 <- flights %>% sample_n(100)
Riktigavardet <- 
Flyg.sml <- select(Flyg, year:day, ends_with("delay"), distance, air_time)
Riktigavardet <- select(Riktigavardet, year:day, ends_with("delay"), distance, air_time)
Test1 <- mutate(Flyg.sml, speed = distance / air_time * 60, gain = arr_delay - dep_delay)
Riktigavardet <- mutate(Riktigavardet, speed = distance / air_time * 60, gain = arr_delay - dep_delay)
Test2 <- select(Flyg2, year:day, arr_delay, carrier, tailnum, flight)
Test12<-as.data.frame(lapply(intersect(names(Test1), names(Test2)), function(name) Test2[name]))
Test3
Test3 <- select(Test3, year:day, arr_delay, dep_delay, distance:gain)
#trainControl(method = "boot", number = 100)

Testknn <-Test3 %>% impute_knn( gain ~ year + month + day + arr_delay, "multivariate", k=3) #KNN, RF and lm seem to work. Cart and shd works also works but gives wierd resutlts.
Testrf <-Test3 %>% impute_rf(speed + dep_delay + distance + air_time + gain ~ year + month + day + arr_delay)
Testlm <-Test3 %>% impute_lm(speed + dep_delay + distance + air_time + gain ~ year + month + day + arr_delay)
Testcart <-Test3 %>% impute_cart(speed + dep_delay + distance + air_time + gain ~ year + month + day + arr_delay)
mutate(Test4, logspeed = log(speed))
testfunc <- function(...){c1 <- c(...)
return(c1)}
################
################ Functioning code
closeAllConnections()
rm(list=ls())
library(plyr)
library(dplyr)
library(nycflights13)
library(tidyverse)
library(simputation)
library(missForest)
library(magrittr)
library(caret)
library(ISLR)
library(rlist)
library(distEx)
library(topicmodels)
library(emdist)
library(fpc)
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
############# Fusion Process
Reference <- iris %>% sample_n(100)
Donor1<-head(Reference, 50)
Studie11 <- dplyr::select(tail(Reference, 50), -(Petal.Width))
CommonVar <-  as.data.frame(lapply(intersect(names(Donor1), names(Studie11)), function(name) Studie11[name]))
unimpset<-bind_rows(Donor1,Studie11)
impknn <- unimpset %>% impute_knn( Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length + Species, "complete", k=3)
implm <- unimpset %>% impute_lm(Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length+ Species)
impcart <- unimpset %>% impute_cart( Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length + Species)
imprf <- unimpset %>% impute_rf( Petal.Width ~ Sepal.Length + Sepal.Width + Petal.Length + Species)
############# Evaluate Fusions
my_funcNum <-function(Ref1,...){
  x <- list(...)
  for(a in 1:length(x)){x[[a]] <- as.numeric(x[[a]])}
  y <- list(Ref1)
result <- c(1:length(x)*0)
 for(i in 1:length(x)){
  result[i] <-  RMSE(x[[i]],y[[1]])
 }

  print(result)
}
my_funcNum(Reference$Petal.Width,impknn$Petal.Width,implm$Petal.Width,imprf$Petal.Width,impcart$Petal.Width)
my_funcCat <- function(Ref2,...){
  x1 <- list(...)
  y1 <- list(Ref2)
  for(b in 1:length(x1)){
   res <- table(x1[[b]], y1[[1]])
    confm <- confusionMatrix(res)
    print(confm$overall)
  }
}
my_funcCat(Reference$Species, impknn$Species, implm$Species, imprf$Species, impcart$Species)
##### Feature Selection
my_funcCorr <- function(Donorarg){
  CorrMatrix <- cor(Donorarg[,!sapply(Donorarg,is.factor)])
  HighCorr <- findCorrelation(CorrMatrix)
  print(CorrMatrix)
  print(HighCorr)
}
############---This is a more sophisticated way of measuring feature importance than just simple Random Forest.
my_funcImpo <- function(respondent, dat){
  form <- as.formula(paste0(respondent, '~ .'))
  controll <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  model <- train(form, data = dat, method = "lvq", preProcess = "scale", trControl =controll)
  importance <- varImp(model, scale = "FALSE")
  plot(importance)
}
############# -----------------------------------------------------------------------------------------
my_funcImp <- function(resp, dat){ ###Simple way of measuring feature importance
  form <- as.formula(paste0(resp, '~ .'))
  fit1 <- randomForest::randomForest(form, data = dat)
  impfit <- randomForest::importance(fit1)
  print(impfit)
}  
my_funcFeatsel <- function(respo, dat){ ###Automatic method of selecting feature that build an accurate model. Not working atm
  control <- rfeControl(functions = rfFuncs, method = "cv", number = 10, repeats = 3)
  featsel <- rfe(dat[,-grep(respo,colnames(dat))], dat[,grep(respo,colnames(dat))], sizes = c(1:length(dat[,-grep(respo,colnames(dat))])), rfeControl = control)
  goodvar <- predictors(featsel)
  return(goodvar)
  ##########We can plot and print the results of the selection if needed. 
}
SubCar <- select(Carseats, Sales:ShelveLoc)
SubCar <- SubCar %>% sample_n(100)
Studie3 <- select(SubCar, -Price, -ShelveLoc, -Population, -Sales)
Studie4 <- select(SubCar, -ShelveLoc, -Population, -Price)
Studie3 <- mutate(Studie3, Media = Income*Advertising)
Studie4 <- mutate(Studie4, Netto = CompPrice/Sales)
ivv <- c("Population", "Price")
