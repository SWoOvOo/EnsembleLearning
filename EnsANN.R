###Ensemble ANN function

#'based on R package "neuralnet"
#'
#'Siwei, 20201023

#load packages
library(neuralnet)


##Part 1
##Average ensemble of neuralnets trained with identical hyperparameters and different random starting weights
#Training function
ensANNa_train <- function(nNN, seed=1, formula, data, hidden, act.fct="logistic", algorithm="rprop+", stepmax=1e6, linear.output=T, rep=1){
  #transform target into activation
  target <- strsplit(paste(deparse(formula),collapse = ""),split = " ~")[[1]][1]
  target_min <- min(data[target])
  target_max <- max(data[target])
  data[target] <- 0.1 + 0.8 * (data[target] - target_min) / (target_max - target_min)
  #Training neuralnets
  list_NN <- list()
  for(i in 1:nNN){
    set.seed(seed-1+i)
    list_NN[[i]] <- neuralnet(formula, data=data, hidden=hidden, act.fct=act.fct, algorithm=algorithm, stepmax=stepmax, linear.output=linear.output, rep=rep)
  }
  #save target transformation data
  list_NN[[nNN+1]] <- c(target_min,target_max)
  return(list_NN)
}

#Prediction function
ensANN_predict <- function(list_NN, newdata,singlePred = FALSE){
  #get number of neuralnets
  nNN = length(list_NN)-1
  #recall transformation data
  target_min <- list_NN[[nNN+1]][1]
  target_max <- list_NN[[nNN+1]][2]
  #Prediction
  pred <- matrix(nrow = nrow(newdata), ncol = nNN)
  for(i in 1:nNN){
    pred[,i] <- predict(list_NN[[i]],newdata = newdata)
    pred[,i] <- ((pred[,i]-0.1)/0.8)*(target_max-target_min)+target_min
  }
  #average ensemble
  meanPred <- rowSums(pred)/ncol(pred)
  #Output results
  if(singlePred == FALSE){
    return(meanPred)
  }
  else{
    df <- data.frame(pred)
    df[,"MeanPred"] <- meanPred
    return(df)
  }
}

##Part 2
##Random ANN structures based on bootstraped data
#Training function
ensANNr_train <- function(nNN, seed=1, formula, data, min_nodes = 1, fraction = 0.8, replace = TRUE, act.fct="logistic", algorithm="rprop+", stepmax=1e6, linear.output=T, rep=1){
  #transform target into activation
  target <- strsplit(paste(deparse(formula),collapse = ""),split = " ~")[[1]][1]
  target_min <- min(data[target])
  target_max <- max(data[target])
  data[target] <- 0.1 + 0.8 * (data[target] - target_min) / (target_max - target_min)
  #Calculate number of model variables
  nvar <- length(unlist(strsplit(strsplit(paste(deparse(formula),collapse = ""),split = " ~")[[1]][2],split = "\\+")))
  #Training neuralnets
  list_NN <- list()
  for(i in 1:nNN){
    set.seed(seed-1+i)
    #generate bootstraped training dataset (no bootstrapping - set fraction = 1, replace = FALSE)
    data_b <- sample_frac(data,size = fraction,replace = replace)
    #generate random structure of ANN
    nodes1 <- round(runif(1,min = min_nodes,max = nvar+1))
    nodes2 <- round(runif(1,min = min(min_nodes,nodes1),max = nodes1))
    #Train ANN
    list_NN[[i]] <- neuralnet(formula, data=data_b, hidden=c(nodes1,nodes2), act.fct=act.fct, algorithm=algorithm, stepmax=stepmax, linear.output=linear.output, rep=rep)
  }
  #save target transformation data
  list_NN[[nNN+1]] <- c(target_min,target_max)
  return(list_NN)
}

