
linreg<-function(formula,data)
{ 
  x<-model.matrix(formula,data)
  y<-all.vars(formula)[1]
  RC<-c((solve((t(x)%*%x))%*%t(x))%*%data[,y])
  FV<-as.vector(x%*%RC)
  RD<-as.matrix(data[,y]-FV)
  DF<- nrow(data)-length(data)
  RV<- c(t(RD)%*%RD)%/%DF
  VRC<-RV*(solve(t(x)%*%x))
  tvalues<-RC/sqrt(diag(VRC))
  tvalues<-round(x=tvalues,digits = 2)


 
 
   {
  
  formulas <- lin_class$new(formula = as.character(formula),
                                 beta=RC ,
                                 fitted=FV, 
                                 resd =c(RD),
                                 df=DF,
                                 resvariance=RV,
                              varcoef = c(diag(VRC)),
                              tvalues=tvalues)
                      ls<-list(beta,fitted,resd,df,resvariance,varcoef,tvalues)
  }
return(formulas)
}



        