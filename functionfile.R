###All functions written so far
########### FEATURE SELECTION---------------------------------------------------------------------
my_funcFeatsel <- function(respo, dat){ ###Automatic method of selecting feature that build an accurate model. Not working atm
  control <- rfeControl(functions = rfFuncs, method = "cv", number = 10, repeats = 3)
  type <- sapply(dat[,grep(paste("^",respo,"$", sep=""),colnames(dat))], is.factor)
  if(is.element(TRUE,type)==TRUE){
  featsel <- rfe(dat[,-grep(paste("^",respo,"$", sep=""),colnames(dat))], droplevels(dat[,grep(paste("^",respo,"$", sep=""),colnames(dat))]), sizes = c(1:length(dat[,-grep(paste("^",respo,"$", sep=""),colnames(dat))])), rfeControl = control)}
  else{
    featsel <- rfe(dat[,-grep(paste("^",respo,"$", sep=""),colnames(dat))], dat[,grep(paste("^",respo,"$", sep=""),colnames(dat))], sizes = c(1:length(dat[,-grep(paste("^",respo,"$", sep=""),colnames(dat))])), rfeControl = control)
    }
  goodvar <- predictors(featsel)
  return(goodvar)
  ##########We can plot and print the results of the selection if needed. 
}

# exemepl1 ----------------------------------------------------------------


my_funcImp <- function(resp, dat){ ###Simple way of measuring feature importance
  form <- as.formula(paste0(resp, '~ .'))
  print(form)
  type <- sapply(dat, is.factor)
  if(is.element(TRUE,type)==TRUE){
  fit1 <- randomForest::randomForest(form, data = droplevels(dat))}
  else{fit1 <- randomForest::randomForest(form, data = dat)}
  impfit <- randomForest::importance(fit1)
  print(impfit)
  randomForest::varImpPlot(fit1, type = 2)
} 

my_funcImpo <- function(respondent, dat){
  x1 <- dat
  for(i in respondent){x1 <- x1[,-grep(paste("^",i,"$", sep=""), colnames(dat)), drop = FALSE]}
  x2 <- colnames(x1)
  form0 <- paste(paste(paste(respondent,collapse = "+"), " ~", sep = ""), paste(x2, collapse = "+"))
  form <- as.formula(form0)
  controll <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  res <- dat[, grep(paste("^",respondent,"$", sep=""),colnames(dat))]
  type <- sapply(dat, is.factor)
  if(is.element(TRUE,type)==TRUE){
  model <- train(form, data = dat, method = "pls", preProcess = "scale", trControl =controll)}
  else{
    model <- train(form, data = dat, method = "pls", preProcess = "scale", trControl =controll)}
  importance1 <- varImp(model, scale = FALSE)
  print(importance1)
  plot(importance1)
}

my_funcCorr <- function(Donorarg){
  CorrMatrix <- cor(Donorarg[,!sapply(Donorarg,is.factor)])
  HighCorr <- findCorrelation(CorrMatrix)
  print(CorrMatrix)
  print(HighCorr)
}

my_funcGA <- function(dat,y){
  ga_ctrl <- gafsControl(functions = rfGA, method = "cv", repeats = 3)
  set.seed(10)
  rf_ga <- gafs(dat[,-grep(paste("^",y,"$", sep=""),colnames(dat))], dat[,grep(paste("^",y,"$", sep=""),colnames(dat))], iters = 3,gafsControl = ga_ctrl)
  bestFeatures <- rf_ga$ga$final
  return(bestFeatures)
}
#################################-----------------------------------------------------------------------------------------
my_funcCat <- function(ref,...){
  
  #store the imputed values in list for each imputation method
  imputed_values <- list(...)
  
  #store reference values in a list
  reference_values <- list(ref)
  
  #vector to store accuracy values
  accuracy_values <- c()
  
  #vector to store kappa values
  kappa_values <- c()
  
  
  #loop that calculated the statistical measures for the imputed values for each imputation method
  for(b in 1:length(imputed_values)){
    
    #evaluated the nr of matching predictions in table format
    matching_predictions <- table(imputed_values[[b]], reference_values[[1]]) #table of predictions
    
    #confusion matrix of the matching tables
    confm <- confusionMatrix(matching_predictions) #confusion matrix of matching predictions
    
    #extract accuracy values
    accuracy_values <- append(accuracy_values, confm$overall['Accuracy']) #
    
    #extract kappa values
    kappa_values <- append(kappa_values,confm$overall['Kappa'])
    
  }
  
  #return as vector
  return(accuracy_values)
  
}

my_funcNum <-function(ref,...){
  
  #list of imputed values
  imputed_values <- list(...)
  
  
  #loop to make sure values are numeric
  for(a in 1:length(imputed_values)){
    
    imputed_values[[a]] <- as.numeric(imputed_values[[a]])
    
  }
  
  #reference values of imputed variable
  reference_values <- list(ref)
  
  #vector to store rmse in
  result <- c(1:length(imputed_values)*0)
  
  
  #calculate rmse for imputed values for each imputation method
  for(i in 1:length(imputed_values)){
    
    result[i] <-  RMSE(imputed_values[[i]],reference_values[[1]])
    
  }
  
  
  #return as vector
  return(result)
}

RMSE = function(m, o){
  
  sqrt(mean((m - o)^2)) #Simple calcuations of rmse
  
}


Imputefunc <- function(dat, intrVar,l,training){
  #Function called to choose imputation method
  index <- SplitTrain(training,intrVar)
  
  #Subsetting the explanatory variables from the response variable in the unimputed data set
  explanatoryVar <- dat[,-grep(paste("^",intrVar,"$", sep=""), colnames(dat)), drop = FALSE] #
  
  #Assign the names of the explanatory variables to an object
  explanatoryVar <- colnames(explanatoryVar)
  
  #A nested use of paste function to obtain the right expression for the formula
  form0 <- paste(paste(paste(intrVar,collapse = "+"), " ~", sep = ""), paste(explanatoryVar, collapse = "+"))
  
  #we make a formula from previous expression
  form <- as.formula(form0)
  
  #We store all imputation functions in a list
  listimpfunc <- list(impute_knn, impute_cart, impute_rf, impute_lm)
  
  #If loof is necessary to insert the right arguments in each imputation function
  if(index ==1){
    
    ChoseImp <- dat %>% listimpfunc[[index]](form, "complete", k=3)} #if knn
  
  else{
    
    ChoseImp <-  dat %>% listimpfunc[[index]](form) #if remaining
    
    }
  
  #Seperate study values from donor values
  ImpStudy <- ChoseImp[(l+1):nrow(ChoseImp),]
  
  return(ImpStudy)
  
}


Evalfunc <- function(dat,intrVar,...){
  
  #vector to contain RMSE values wrt imputed data for each imputation method
  rmse_values <- c()
  
  #vector to contain accuracy and kappa values wrt imputed data for each imputation method
  accuracy_kappa_values <-c()
  
  #Reference values for the imputed variable
  reference <- dat[, grep(paste("^",intrVar,"$", sep=""), colnames(dat))]
  
  #list of imputed data sets from different methods to be evaluated
  lista <- list(...)
  
  #list to store EMD values
  EMDdistvec <- c()
  
  #for loop to calculate the RMSE, accuracy, Kappa, EMD, hellinger etc for each imputation method
  for(x in lista){ #loop through each imputed set in list
    
    imputed_values <- x[, grep(paste("^",intrVar,"$", sep=""), colnames(x))] #take out imputed values for imputed variable 
    
    
    if(is.factor(imputed_values)==TRUE){ #if categorical values
      
      accuracy_kappa_values <- append(accuracy_kappa_values, my_funcCat(reference,imputed_values)) #
      
    }
    else{ #if numerical values
      
     rmse_values <- append(rmse_values, my_funcNum(reference,imputed_values))
     
    }
    
  }
  
  #vector with the minimuim rmse and its index which refers to a certain imputation method
  rmse_values <- c(min(rmse_values), which.min(rmse_values))
  
  #same but for kappa
  accuracy_kappa_values <- c(min(accuracy_kappa_values), which.min(accuracy_kappa_values))
  
  #return the statistical measure appropiate for imputed data
  if(is.factor(imputed_values)==TRUE){
    
      return(accuracy_kappa_values)}
  
  else{
    
      return(rmse_values)
    
      }
}

SplitTrain <- function(dat,intrVar){
  
  #select a training set from the data
  trainDonor <- dat[1:(nrow(dat)/2),]
  
  #select a test set from the data
  testDonor <- dat[((nrow(dat)/2)+1):nrow(dat),]
  
  #Object used to store reference data
  Refer <-  dat[((nrow(dat)/2)+1):nrow(dat),]
  
  #we remove the values to be imputed so that in fact get a "test" data
  testDonor[,intrVar] <- NA
  
  #We compose the data so that we can implement our imputation methods
  unimp <- bind_rows(trainDonor,testDonor)
  
  #Subsetting the explanatory variables
  explanatoryVar <- testDonor[,-grep(paste("^",intrVar,"$", sep=""), colnames(testDonor)), drop = FALSE]
  
  #Grab the names of the explanatory variables
  explanatoryVar <- colnames(explanatoryVar)
  
  #a nested use of paste to get the right expression for the formula
  form0 <- paste(paste(paste(intrVar,collapse = "+"), " ~", sep = ""), paste(explanatoryVar, collapse = "+"))
  
  #We make a formula of previous expression
  form <- as.formula(form0)
  
  #See if data contains category variables
  type <- sapply(dat, is.factor)
  
  
   if(is.element(TRUE,type)==TRUE){ #if there are categorical variables then use following imputation methods
     
    impknn <- unimp %>% impute_knn(form, "complete", k=3) #knn imputation
    
    imprf <- unimp %>% impute_rf(form) #randomforest imputation
    
    impcart <- unimp %>% impute_cart(form) #cart imputation
    
    list_of_imp_method <- list('knn','cart','randforest') #used to print which methods was ultimatley chosen
    
    Result <- Evalfunc(dat,intrVar,impknn,impcart,imprf) #Result is presented in terms of an index 
 
  }
  else{#if numerical, same reasoning follows
    
    impknn <- unimp %>% impute_knn(form, "complete", k=3)
    
    implm <- unimp %>% impute_lm(form)
    
    impcart <- unimp %>% impute_cart(form)
    
    imprf <- unimp %>% impute_rf(form)
    
    list_of_imp_method <- list('knn','cart','randforest','linear model')
    
    Result <- Evalfunc(dat,intrVar,impknn,impcart,imprf,implm)
  }
  
 #print the chosen method as well as its measured performance  
 print(paste(as.character(list_of_imp_method[Result[2]]),",",as.character(Result[1])))
 
 #return the index which represents a specific imputation method
 return(Result[2])
 
}

Hellinger <- function(vec1,vec2){ ##Change this------------------------------------------------------
  if(is.factor(vec1)==TRUE){
    vec1 <- as.numeric(vec1)
    vec2 <- as.numeric(vec2)
  } 
  matrix1 <- matrix(vec1,1,length(vec1))
  matrix2 <- matrix(vec2,1,length(vec2))
  Resultmatrix <-distHellinger(matrix1,matrix2)
  return(Resultmatrix[[1]])
}#########################################----------------------------------------------------------

EarthMD <- function(vec1, vec2){
  if(is.factor(vec1)==TRUE){
    vec1 <- as.numeric(vec1)
    vec2 <- as.numeric(vec2)    
  }
  matrix1 <- matrix(vec1,1,length(vec1))
  matrix2 <- matrix(vec2,1,length(vec2))
  Resultmatrix <- emd2d(matrix1,matrix2)
  return(Resultmatrix[[1]])
}

Hellinger <- function(dat1,dat2,vararg){
  numVar <-colnames(dat1[,which(sapply(dat1,is.numeric))])
  catind <- grep(FALSE,vararg %in% numVar)
  catVar <- c(numeric(length(catind))*0)
  for(a in 1:length(catind)){catVar[a] <- vararg[catind[a]]}
  segm1 <-  dat1 %>% mutate_each_(funs(cut(., breaks=quantile(.,probs = seq(0,1,by = 0.25),na.rm=TRUE,inlcude.lowest=TRUE))), numVar)  %>% group_by_(.dots=vararg) 
  totnrow <- nrow(segm1)
  ram <- as.data.frame(segm1)
  print(levels(ram$Sepal.Width))
  segm1 <- segm1 %>% count()
  segm2 <-  dat2 %>% mutate_each_(funs(cut(., breaks=quantile(.,probs = seq(0,1,by = 0.25),na.rm=TRUE,inlcude.lowest=TRUE))), numVar)  %>%group_by_(.dots=vararg) %>% count()
  distrvec1 <- segm1$n/totnrow
  distrvec2 <- segm2$n/totnrow
  absterm <- c()
  for(i in 1:min(length(distrvec2),length(distrvec1))){
       absterm <- append(absterm, (abs(sqrt(distrvec1[i])-sqrt(distrvec2[i])))^2)
  }
  h <- (1/sqrt(2))*sqrt(sum(absterm))
  print(distrvec1)
  print(distrvec2)
  return(h)
}


# absterm <- c()
# for(i in 1:length(distrvec1)){
#   absterm <- append(absterm, (abs(sqrt(distrvec1[i])-sqrt(distrvec2[i])))^2)
# }
# h <- (1/sqrt(2))*sqrt(sum(absterm))
# return(h)
#listaresp <- list()
#for(j in 1:length(y)){
# resp <- y[j]
# print(paste(resp, "accuracy"))
# listaresp[[j]] <- Ref[, grep(resp, colnames(Ref))]
# listimp <- list(impknn[, grep(resp,colnames(impknn))],implm[, grep(resp,colnames(implm))],imprf[, grep(resp,colnames(imprf))],impcart[, grep(resp,colnames(impcart))])
#if(is.factor(listaresp[[j]]) == TRUE){my_funcCat(listaresp[[j]],listimp[[1]],listimp[[2]], listimp[[3]], listimp[[4]])}
#else{my_funcNum(listaresp[[j]],listimp[[1]],listimp[[2]], listimp[[3]], listimp[[4]] )}

#print(paste(y,"accuracy"))
#listimp <- list(impknn[, grep(y,colnames(impknn))],implm[, grep(y,colnames(implm))],imprf[, grep(y,colnames(imprf))],impcart[, grep(y,colnames(impcart))])
#3if(is.factor(listaresp[[j]]) == TRUE){my_funcCat(listaresp[[j]],listimp[[1]],listimp[[2]], listimp[[3]], listimp[[4]])}
#else{my_funcNum(listaresp[[j]],listimp[[1]],listimp[[2]], listimp[[3]], listimp[[4]] )}