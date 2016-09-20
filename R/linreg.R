linreg<-function(formula,data)
{
  x<-model.matrix(formula,data)
  y<-all.vars(formula)[1]
  class(linreg)="linreg"
  beta<- as.vector((solve((t(x)%*%x))%*%t(x))%*%data[,y])
  fitted<-x%*%beta
  resd<-as.vector(data[,y]-fitted)
  df<- nrow(data)-length(data)
  resvariace<- as.vector((t(resd)%*%resd)%/%df)
  varcoef<-resvariace*(solve(t(x)%*%x))
  tvalues<-beta/(sqrt(varcoef))
  
  return(fitted)
  
}
data("iris")
linreg(Petal.Length~Sepal.Width,iris)
