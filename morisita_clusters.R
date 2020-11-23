morisita_clusters <- function(df, types = c("n.C", "n.L"), unique.rm = TRUE){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(vegan)
  
  "%ni%"  <- Negate("%in%")
  
  for(i in length(types)){
    if(types[i] %ni% colnames(df)){stop(paste0("type -> ", types[i] ," <- does not exist in the input dataframe"))}
  }
  

  morisita_clusters <- dispindmorisita(data[,types])
  
  return(morisita_clusters)
}