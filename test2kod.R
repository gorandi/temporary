DATA_FUSION <- function(Donor,intrVar,...){
  
  #List to save all reciever studies 
  list_of_Reciever_Studies<-list(...)
  
  #List to save all selected important features
  list_of_selected_features <- list()
  
  #List to save all linking variables. Each elemement correspond to linking var between donor and each studie 
  list_of_linking_Variables <- list()
  
  #List to save Donors where uncommon variables for each studie is cut.
  list_of_Donors_Common_Variabels <-list()
  
  #A for loop to find linking variables between donor and each studie
  for (ii in 1:length(list_of_Reciever_Studies)) {  # ii goes from 1 to number of studies 
   
     studie <- list_of_Reciever_Studies[[ii]]  # Assinging each study to an object
    
    list_of_linking_Variables[[ii]] <- as.data.frame(lapply(intersect(names(Donor), #Nested function to find linking variables and cut out the rest from each studie  
                                                        names(studie)), function(name) studie[name]))
    
    
    #Cut out the intresting variables and the common variables for each studie 
    for (i in 1:length(intrVar)) {  #i goes from 1 to number of intresting variables
      
      list_of_Donors_Common_Variabels <-  list.append(list_of_Donors_Common_Variabels, select_(Donor,
        .dots = c(colnames(list_of_linking_Variables[[ii]]), intrVar[i]))) #Subset all the donors wrt linking variables and feature variables
    }
    
  }
  
   #Put all the donors where uncommon variables has been cut out in a matrix     
   subset_of_donors <- matrix(list_of_Donors_Common_Variabels,length(intrVar),length(list_of_Reciever_Studies))
   
   
   #Double for loop to find important features for each intresting variable 
   for(i in 1:length(intrVar)){
     
     resp <- intrVar[i] #assign each intresting variable to an object
     
     for(j in 1:length(list_of_Reciever_Studies)){
       
      selected_import_feat <- my_funcFeatsel(resp,subset_of_donors[i,j][[1]]) #Recursive feature elemination for each studie and intresting variable
       
      print(paste("Important features for ",resp," are ", paste(selected_import_feat, collapse = ", "))) #print the important features
       
      list_of_selected_features <- list.append(list_of_selected_features, #subset all studies wrt these features, which are also linking 
                                                select_(list_of_linking_Variables[[j]],.dots = selected_import_feat))
     }
     
   }
   
  #Put all the subsetted studies in a matrix
  subset_of_Studies <- matrix(list_of_selected_features,length(intrVar),length(list_of_Reciever_Studies),byrow = TRUE)
  
  
  #List to insert all imputed (subsetted) studie sets
  imputed_studies <-list()
  
  
  #For loop iterate over all subsetted studies and donor sets stored in matrices
  for(a in 1:length(intrVar)){
    
    for(b in 1:length(list_of_Reciever_Studies)){ 
      
      subset_of_donor_impvar <- select_(subset_of_donors[a,b][[1]], #Subset each donor again wrt to the variable we want to impute
                             .dots = c(colnames(subset_of_Studies[a,b][[1]]),intrVar[a]))
      
      fused_data <-bind_rows(subset_of_donor_impvar,subset_of_Studies[a,b]) #Combine all each subsetted donor with corresponding subsetted studie
      
      imputed_studie <- Imputefunc(fused_data,intrVar[a], nrow(Donor), subset_of_donor_impvar) #impute data for the intresting varaible for each subsetted studie
      
      print(imputed_studie)
      
      imputed_studies <- list.append(imputed_studies, imputed_studie) #store all the imputed studies in a list 
   
    }
    
  }
  
  #make matrix from that list where each where entry ij is intresting variable i imputed in study j
  imputed_studies <- matrix(imputed_studies,length(intrVar),length(list_of_Reciever_Studies)) 

  }
