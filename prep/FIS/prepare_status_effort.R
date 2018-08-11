###########################################
#prepare status & trend fisheries variable
##########################################

rm(list = ls(all = T))

library(tidyverse)
library(reshape2)

goal <- "fis"
region <- "bali"
year <- 2016

#data filename
##############
filename <- "nb_vessel"

outfilename <- c("catch", "effort")
outfilename <- outfilename[2]


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ",")
	
	#convert wide form to long dataframe form
	##########################################
	dat <- melt(dat, id = c("region"))
	
	#rename coloumn names
	#####################
	names(dat) <- c("region", "year", "values")
	
	
	#extract year
	#############
	dat$year <- substr(dat$year, 3, 6)
	
	#assign rgn_id
	##############
	
	#assign subregion code, e.g. regiion "Denpasar" = 1
	###################################################
	dat$rgn_id <- ifelse(dat$region == "denpasar", 1, ifelse(dat$region == "badung", 2, ifelse(dat$region == "tabanan", 3, ifelse(dat$region == "jembrana", 4, 
				 ifelse(dat$region == "buleleng", 5, ifelse(dat$region == "karangasem", 6, ifelse(dat$region == "klungkung", 7, 8)))))))
				 
	dat <- dat%>%select(rgn_id, year, values)%>%rename(rgn_id = rgn_id, year = year, vessel = values)%>%arrange(rgn_id, year)
	
			## save data layer
			##################
	
			write_csv(dat, paste(directory, "prep/", toupper(goal), "/", goal, "_", outfilename, "_", region, year, ".csv", sep = ""))
			write_csv(dat, paste(directory, "region2017/layers/", goal, "_", outfilename, "_", region, year, ".csv", sep = ""))
	
	
	
	
	
	
