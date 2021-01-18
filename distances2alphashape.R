distances2alphashape<-function(x,y, alpha.shape){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(grDevices, alphahull)
  
  dfx<-data.frame(x=x, y=y)

  ver<- ashape(x=dfx$x, y=dfx$y, alpha=alpha.shape)
  ver<- as.data.frame(ver$edges)
  for(m in 1:nrow(ver)){
    ver$m[m] <-(ver$y2[m] - ver$y1[m])/(ver$x2[m] - ver$x1[m])
    ver$b[m] <- -ver$x1[m]*ver$m[m] + ver$y1[m]
  }
  
  
  distances<-matrix(nrow = nrow(dfx), ncol=nrow(ver))
  x_int<-matrix(nrow = nrow(dfx), ncol=nrow(ver))
  y_int<-matrix(nrow = nrow(dfx), ncol=nrow(ver))
  slopes_m2 <- matrix(nrow = nrow(dfx), ncol=nrow(ver))
  intercept_b2<-matrix(nrow = nrow(dfx), ncol=nrow(ver))
  
  for(i in 1:nrow(dfx)){
    for(j in 1:nrow(ver)){
      
      if(ver$m[j] == 0){slopes_m2[i,j] <- 1} #slope eq point
      if(ver$m[j] != 0){slopes_m2[i,j] <- -1/ver$m[j]} #slope eq point
      
      intercept_b2[i,j]<-dfx$y[i] - dfx$x[i]*slopes_m2[i,j]
      
      
      xint <- x_int[i,j] <- (intercept_b2[i,j] - ver$b[j])/(ver$m[j] - slopes_m2[i,j])
      yint <- y_int[i,j]<- slopes_m2[i,j]*x_int[i,j] + intercept_b2[i,j]
      
      
      if(xint >= max(ver[j,c("x1", "x2")]) & yint <= min(ver[j,c("y1", "y2")])){
        x_int[i,j] <-max(ver[j,c("x", "x2")]); y_int[i,j] <-min(ver[j,c("y1", "y2")])}
      
      if(xint >= max(ver[j,c("x1", "x2")]) & yint >= max(ver[j,c("y1", "y2")])){
        x_int[i,j] <-max(ver[j,c("x1", "x2")]); y_int[i,j] <-max(ver[j,c("y1", "y2")])}
      
      if(xint <= min(ver[j,c("x1", "x2")]) & yint <= min(ver[j,c("y1", "y2")])){
        x_int[i,j] <-min(ver[j,c("x1", "x2")]); y_int[i,j] <-min(ver[j,c("y1", "y2")])}
      
      if(xint <= min(ver[j,c("x1", "x2")]) & yint >= min(ver[j,c("y1", "y2")])){
        x_int[i,j] <-min(ver[j,c("x1", "x2")]); y_int[i,j] <-max(ver[j,c("y1", "y2")])}
      
      
      
      distances[i,j] = sqrt((x_int[i,j]-dfx$x[i])^2+(y_int[i,j]-dfx$y[i])^2 )
      
    }
    
  }
  listRet<- list(distances=distances,vertex.alphashape=ver, ,x_int=x_int, y_int=y_int,
                 slopes_m2=slopes_m2,intercept_b2=intercept_b2)
  return(listRet)
}#ElFin
