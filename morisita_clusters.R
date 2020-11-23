require(vegan)
#remove data cluster 0 
morisitaClusters <- dispindmorisita(data[,c("n.C", "n.L")], unique.rm = TRUE)