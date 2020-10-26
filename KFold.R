###K-Fold Cross-Validation Sampling functions

#'classical random sampling
#'and random sampling specilized for the small dataset trying to keep the data range of subsets possibly
#'input should be a dataframe
#'
#'Siwei, 20201025

CV_subset <- function(data, target, fold, KeepRange = FALSE, seed = 1){
  
  #initialization
  set.seed(seed)
  fn <- ceiling(nrow(data)/fold) #number of samples in every fold
  test_set <- list()
  train_set <- list()
  
  if(KeepRange == FALSE){ 
    
    ##classical k-fold cross-validation sampling
    
    sampling_series <- sample.int(nrow(data))
    for(i in 1:(fold-1)){
      test_set[[i]] <- data[sampling_series[(fn*(i-1)+1):(fn*i)],]
      train_set[[i]] <- data[-sampling_series[(fn*(i-1)+1):(fn*i)],]
    }
    test_set[[fold]] <- data[sampling_series[(fn*(fold-1)+1):length(sampling_series)],]
    train_set[[fold]] <- data[-sampling_series[(fn*(fold-1)+1):length(sampling_series)],]
  }
  
  else{ 
    
    ##range-keeping with partitional random sampling
    
    #sort the data
    data = data[order(data[,target]),]
    #generate random series for sampling
    sampling_series <- list()
    for(i in 1:(fn-1)){
      sampling_series[[i]] <- sample.int(fold)
    }
    sampling_series[[fn]] <- sample.int(nrow(data)-fold*(fn-1))
    #K-fold sampling
    for(i in 1:fold){
      k_testset <- data.frame(matrix(nrow = 0, ncol = ncol(data)))
      colnames(k_testset) <- colnames(data)
      k_trainset <- data.frame(matrix(nrow = 0, ncol = ncol(data)))
      colnames(k_trainset) <- colnames(data)
      for(j in 1:(fn-1)){
        #divide the data into fn partions, fn-1 partions has a length of fold
        n_subset <- data[(fold*(j-1)+1):(fold*j),]
        #select 1 sample from each partion as test set and rest of the partition as training set
        k_testset <- rbind(k_testset,n_subset[sampling_series[[j]][i],])
        k_trainset <- rbind(k_trainset,n_subset[sampling_series[[j]][-i],])
      }
      #special for the small remainder subset
      n_subset <- data[(fold*(fn-1)+1):nrow(data),]
      if(i<=(nrow(data)-fold*(fn-1))){
        k_testset <- rbind(k_testset,n_subset[sampling_series[[fn]][i],])
        k_trainset <- rbind(k_trainset,n_subset[sampling_series[[fn]][-i],])
      }
      else{
        k_trainset <- rbind(k_trainset,n_subset[sampling_series[[fn]],])
      }
      test_set[[i]] <- k_testset
      train_set[[i]] <- k_trainset
    }
  }
  
  #return results
  result_set <- list()
  result_set[['train']] <- train_set
  result_set[['test']] <- test_set
  return(result_set)
}

