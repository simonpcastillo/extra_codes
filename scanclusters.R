
scanClusters <- function(df=df,neighbourhood.radius = 20, neighbourhood.size = 3, neighbourhood.classes = c("p", "n", "l", "s")) {
  pacman::p_load(fpc, factoextra, dbscan, sp)

  df <-df[df$class %in% neighbourhood.classes,]
  # Compute DBSCAN using fpc package
  db <- fpc::dbscan(df[,c("x", "y")], eps = neighbourhood.radius, MinPts = neighbourhood.size)

  df$cluster <- db$cluster
  g <- unique(df$cluster)

  df.cluster <- data.frame()

  for (i in g) {
    box.coords <- df[df$cluster==i,c("x", "y")]
    box.hpts <- chull(x=box.coords$x, y=box.coords$y)
    box.hpts <- c(box.hpts, box.hpts[1])
    box.chull.coords <- box.coords[box.hpts,]
    chull.poly <- Polygon(box.chull.coords, hole=F)
    chull.area <- chull.poly@area
    n.L <- nrow(df[df$cluster==i & df$class == "l",])
    n.C <- nrow(df[df$cluster==i & df$class == "p",])
    df0 <- data.frame(sample.id = sample.id, cluster = i, n.C = n.C, n.L = n.L, chull.area = chull.area)
    df.cluster <- rbind(df.cluster, df0)
    #print(chull.area)
    }

    return(df.cluster)
}