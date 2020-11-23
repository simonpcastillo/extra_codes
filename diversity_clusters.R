diversity_clusters <- function(df, types = c("n.C", "n.L")){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(vegan)
  
  "%ni%"  <- Negate("%in%")
  
  for(i in length(types)){
    if(types[i] %ni% colnames(df)){stop(paste0("type -> ", types[i] ," <- does not exist in the input dataframe"))}
  }
  
  if(length(types) < 2 ){warning("if input dataframe have 1 sp, a global diversity is computed")}
  
  H <- diversity(data[,types])
  simp <- diversity(data[,types], "simpson")
  
  diversity_clusters <- data.frame(Shannon = H, Simpson = simp)
  
  return(diversity_clusters)
}
