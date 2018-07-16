#################################
#prepare pressure of chl anomalies
#################################

rm(list = ls(all = T))

library(tidyverse)

goal <- "fis"
region <- "bali"
year <- 2016

filename <- "chl_anomaly"


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat <- read.table(file = paste(filename, ".txt", sep = ""), header = T, sep = ",")
	
	
	#dat[3] <- sapply(dat[3],as.numeric)
	#dat <- dat%>%arrange(rgn_id, year)
	
	
	#rescaled with normalized pressure (0-1),
	#using equation: zi = xi - max(x)/(min(x) - max(x)) 
	#highest anomaly = lowest pressure to FIS
	#lowest anomaly = highest pressure to FIS
	##############################################################
	
	dat$pressure_score <- ifelse(is.na(dat[,3]), NA, (dat[,3] - max(dat[,3], na.rm = T))/(min(dat[,3], na.rm = T) - max(dat[,3], na.rm = T)))
	
	
	dat <- dat%>%select(rgn_id, year, pressure_score)%>%arrange(rgn_id, year)
	
		# save data layer
		#################
		write_csv(dat, paste(directory, "prep/", toupper(goal), "/", "cc", "_", filename, "_", region, year, ".csv", sep = ""))
		write_csv(dat, paste(directory, "region2017/layers/", "cc", "_", filename, "_", region, year, ".csv", sep = ""))	
