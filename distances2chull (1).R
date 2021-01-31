distances2chull<-function(x,y){
  df0<-data.frame(x=x, y=y)
  chu<-chull(df0$x, df0$y)

  ver<-df0[cuh,]
  
  for(i in 1:(nrow(ver)-1)){
    ver$x2[i]<- ver$x[i+1]
    ver$y2[i]<- ver$y[i+1]
    ver$dx[i]<-ver$x2[i] - ver$x[i]
    ver$dy[i]<-ver$y2[i] - ver$y[i]
    ver$m[i] <- ver$dy[i]/ver$dx[i]
    ver$b[i] <- ver$y[i]
  }
  
  ver<- ver[-nrow(ver),]
  
  distances<-matrix(nrow = nrow(df0), ncol=nrow(ver))
  
  for(i in 1:nrow(df0)){
    for(j in 1:nrow(ver)){
      
      
      if(ver$m[j] == 0){df0$m2[j] <- 1} #slope eq point
      if(ver$m[j] != 0){df0$m2[j] <- -1/ver$m[j]} #slope eq point
      
      df0$b2[j]= df0$y[i] + df0$x[i]*df0$m2[i]*-1
      
      
      df0$x_int_1[i] = (df0$b2[j] + -1*ver$b[j])/(ver$m[j] + -1*df0$m2[j])
      df0$y_int_1[i] = ver$m[j]* df0$x_int[i] + ver$b[j]
      distances[i,j] = sqrt((df0$x_int_1[i]-df0$x[i])^2+(df0$y_int_1[i]-df0$y[i])^2 )
      
      
    }
    
  }
}