closeAllConnections()
rm(list=ls())
#INPUT
INPUT <- function(Donor,intrVar,...){
listA<-list(...)
listSelFeat <- list()
for(i in 1:length(intrVar)){
  resp <- intrVar[i]
  SelFeat <- my_funcFeatsel(resp,Donor)
  print(paste("Important features for ",resp," are ", paste(SelFeat, collapse = ", ")))
  listSelFeat[[i]] <- select_(Donor,.dots = SelFeat)
  my_funcImp(resp, Donor)
  my_funcImpo(resp, Donor)
}
my_funcCorr(Donor)
listFuseddata <- list()
Relevantdonorlist <-list()
for(j in 1:length(listSelFeat)){
  SelElem <- listSelFeat[[j]]
  for(jj in 1:length(listA)){
  SelStudie <- listA[[jj]]  
  CommonFeat <-as.data.frame(lapply(intersect(names(SelElem), names(SelStudie)), function(name) SelStudie[name]))
 # for(a in intrVar){CommonFeat[,a] = NA} This is for combining columnwise.
  Relevantdonor <- select_(Donor, .dots = c(colnames(CommonFeat),intrVar[j])) #IMPORTANT IS IT COMMONFEAT OR SELELEM
  Relevantdonorlist<-list.append(Relevantdonorlist, Relevantdonor) #Relevant donor list and matrix are for training in impute function
  FusedData <- bind_rows(Relevantdonor,CommonFeat)
  listFuseddata <- append(listFuseddata,list(FusedData))
  }
}
  MatrixReldonor <- matrix(Relevantdonorlist,nrow = length(intrVar),ncol = length(listA),byrow = TRUE)
  Matrixsaver <- matrix(listFuseddata,nrow = length(intrVar), ncol = length(listA), byrow = TRUE)
  ListofCompl <- list()
  for(a in 1:nrow(Matrixsaver)){
    for(b in 1:ncol(Matrixsaver)){
      compl <- Imputefunc(Matrixsaver[a,b][[1]],intrVar[a],nrow(Relevantdonor),MatrixReldonor[a,b][[1]]) #Length parameter included to split the fused data.
      ListofCompl <- list.append(ListofCompl,compl)
    }
  }
  Matrixcompl <- matrix(ListofCompl, nrow = length(intrVar), ncol=length(listA),byrow=TRUE)
  return(Matrixcompl[1,1][[1]])
}
#-cleaning
#-QC
#-identify linking variables
#transform
#FEATURE SELECTION
#informative linking variables
#multicollinearity
#rf, pca, genetic alg
#IMPUTERING/FUSION - parameter tuning/regulization
#bayesian
#knn
#ml (random forest)
#FUSION PERFORMANCE
#Accuracy
#kappa
#hellinger etc