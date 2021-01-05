spatialdata.coldness.WSI <- function(input.folder = input.folder, output.folder = output.folder){
  
  files <- Sys.glob(paste0(input.folder, "/PO*.csv"))
  source("https://raw.githubusercontent.com/simonpcastillo/extra_codes/main/diversity_clusters.R") 
  spatial.coldness.WSI <- data.frame()
  
  for(j in 1:length(files)){#1:length(files)
    print(j)
    sample.id <- substr(files[j],(nchar(files[j])-12), (nchar(files[j])-4))
    stage <-  substr(sample.id, 9,9)
    data <- read.csv(files[j])
    
    div0 <- diversity_clusters(df = data , types = c("n.C", "n.L"))
    data0 <- cbind(data, div0)
    data1 <- data0[data0$cluster != 0, ]
    data1$LC <- data1$n.L/data1$n.C
    data1$coldness <- NA
    medL <- median(data1[data1$n.L>0,]$n.L)
   
    for(l in 1:nrow(data1)){
     if(data1$n.L[l]< medL){data1$coldness[l] <- "cold"}
     if(data1$n.L[l]>= medL){data1$coldness[l] <- "hot"}
     if(data1$n.L[l] == 0){data1$coldness[l] <- "depleted"}
   }

     for(l in 1:nrow(data1)){
     if(data1$LC[l] < 1 & data1$LC[l] >0){data1$coldness2[l] <- "cold"}
     if(data1$LC[l]>= 1){data1$coldness2[l] <- "hot"}
     if(data1$LC[l] == 0){data1$coldness2[l] <- "depleted"}
   }
    
    
    #cor <- cor.test(data1$n.C, data1$n.L)
    
    
    g.cluster <- data.frame(nclusters = nrow(data1),
                            hotcluster =  nrow(data1[data1$coldness == "hot",]),
                            coldcluster =  nrow(data1[data1$coldness == "cold",]),
                            depletedcluster =  nrow(data1[data1$coldness == "depleted",]),
                            hotcluster2 =  nrow(data1[data1$coldness2 == "hot",]),
                            coldcluster2 =  nrow(data1[data1$coldness2 == "cold",]),
                            depletedcluster2 =  nrow(data1[data1$coldness2 == "depleted",])
                            
                        )

    
    
    ecospace <- data.frame(j=j, sample.id, stage, g.cluster)
    
    spatial.coldness.WSI <- rbind(spatial.coldness.WSI, ecospace) 
    
    
  }
  
return(spatial.coldness.WSI)  
  
}#Fin


#input.folder = "E:/COMPATH_SPANET_FINAL/SpatialOncosystems/results_20201208"
#output.folder = "E:/COMPATH_SPANET_FINAL/SpatialOncosystems"
#spatialdata.coldness.WSI<- spatialdata.coldness.WSI(input.folder = input.folder, output.folder = output.folder)
