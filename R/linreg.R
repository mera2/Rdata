linreg<-function(formula,data){
  
  x<-model.matrix(formula,data)
  y<-all.vars(formula)[1]
  
  regcof<-as.vector(solve(t(x)%*%x)%*%t(x)%*%data[,y])
  fitted_values<-x%*%regcof
  resi<-y-fitted_values
  
}
data("iris")
linreg(Petal.Length~Petal.Width,iris)