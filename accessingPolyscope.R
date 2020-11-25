if (!require("pacman")) install.packages("pacman")
pacman::p_load(RCurl)

url <- "ftp://ftppath/www_logs/testfolder/test.csv"
text_data <- getURL(url, userpwd = "username:password", connecttimeout = 60)
df <- read.csv(text = text_data)