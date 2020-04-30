# 30 14,15,16,00 * * *
source("/Users/smazeri/Documents/GitHub/covid19_scotland/upload_new_data2dropbox.R")
Sys.sleep(300)
rsconnect::restartApp("Covid19_Scotland")
