require(vegan)
data <- read.csv("POET0109B.csv") # rows are communities columns are cell types
#remove rows under the cluster 0

H <- diversity(data[,c("n.C", "n.L")])
simp <- diversity(data[,c("n.C", "n.L")], "simpson")

diversity_clusters <- data.frame(Shannon = H, Simpson = simp)