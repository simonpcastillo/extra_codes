if (!require("pacman")) install.packages("pacman")
pacman::p_load(RCurl)
img.id <- "Path000008_201604141207"
url <- paste0("sftp://polyscope.icr.ac.uk/var/www/polyzoomer/",img.id, "/page/*/*/*/annotations.txt" )
text_data <- getURL(url, userpwd = "username:password", connecttimeout = 60)
df <- read.csv(text = text_data)