###Goodness of fit function for predictive models###
#Nellie Hobley, July 2017

#'calculates bias (mean error), mean squared error, 
#'root means squared error, normalised root mean square error (to range), coefficient of variation of RMSE, 
#'mean absolute error, normalised root mean absolute error (to range), coefficient of variation of MAE, 
#'ratio of standard deviation to error, residual prediction deviation and coefficient of determination
#'Explanation updated by Siwei, August 2019

#Do the fit calcs
GOF<-function(x,y){
  ME<-sum(x-y)/length(x) #mean error
  MSE<-sum((x-y)^2)/length(x) # mean squared error
  RMSE<-sqrt(MSE) #calculate root mean square error
  NRMSE<-RMSE/(max(x)-min(x)) #RMSE normalised to range of x
  CVRMSE<-RMSE/(mean(x)) #RMSE normalised to mean of x
  MAE<-sum(abs(x-y))/length(x)#mean absolute error
  NMAE<-MAE/(max(x)-min(x)) #MAE normalised to range of x
  CVMAE<-MAE/(mean(x)) #MAE normalised to mean of x
  SDE<-sqrt(MSE-ME^2) #standard deviation of error
  RPD<-sd(x)/RMSE #residual prediction deviation (redundant to R2)
  R2<-1-MSE/var(x) #coefficient of determination
  GOF<-data.frame(cbind(ME,MSE,RMSE,NRMSE,CVRMSE,MAE,NMAE,CVMAE,SDE,RPD,R2))
  colnames(GOF)<-c('ME','MSE','RMSE','normRMSE','cvRMSE','MAE','normMAE','cvMAE','SDE','RPD','R2')
  return(GOF)
}