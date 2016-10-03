#'@import ggplot2
#'@export

linreg<-function(formula,data)
{ 
  x<-model.matrix(formula,data)
  y<-all.vars(formula)[1]
  RC<-c((solve((t(x)%*%x))%*%t(x))%*%data[,y])
  FV<-as.vector(x%*%RC)
  RD<-c(as.matrix(data[,y]-FV))
  DF<- nrow(data)-ncol(x)
  RV<- c(t(RD)%*%RD)%/%DF
  VRC<-RV*(solve(t(x)%*%x))
  tvalues<-RC/sqrt(diag(VRC))
  tvalues<-round(x=tvalues,digits = 2)
 
  list <- list()
  
  formulas <- lin_class$new(formula = as.character(formula),
  beta=RC ,
  fitted=FV, 
  resd =c(RD),
  df=DF,
  resvariance=RV,
  varcoef = c(diag(VRC)),
  tvalues=tvalues)
  
  D1<- data.frame(x = FV, y = RD)
  D2<- data.frame(x = FV, y = as.numeric(sqrt(abs(RD))))
  print(D1)
  print(D2)
                      
  plot <- plot_class$new(D1 = D1,D2 = D2)
    
    list$plot <- plot
    
    list$formulas <- formulas
  
  
  return(list)                           
}



        