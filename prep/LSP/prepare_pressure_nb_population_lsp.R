###############################################
#prepare pressure of LSP from nb of population
###############################################

rm(list = ls(all = T))

library(tidyverse)
library(reshape2)

goal <- "lsp"
region <- "bali"
year <- 2016

#data input
###########
filename <- "nb_population"



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
	
	
	#convert year as numeric
	########################
	dat$year <- as.numeric(substr(dat$year, 3, 6))
	
	#assign subregion code, e.g. regiion "Denpasar" = 1
	###################################################
	dat$rgn_id <- ifelse(dat$region == "Denpasar", 1, ifelse(dat$region == "Badung", 2, ifelse(dat$region == "Tabanan", 3, ifelse(dat$region == "Jembrana", 4, 
				 ifelse(dat$region == "Buleleng", 5, ifelse(dat$region == "Karangasem", 6, ifelse(dat$region == "Klungkung", 7, 8)))))))
			
	dat <- dat%>%select(rgn_id, year, values)%>%arrange(rgn_id, year)
	
	#normalized pressure score between 0-1
	#using equation: zi = xi - min(x)/(max(x) - min (x)) 
	####################################################
		
	dat$pressure_score <- ifelse(is.na(dat$value), NA, (dat$value - min(dat$value, na.rm = T))/(max(dat$value, na.rm = T) - min(dat$value, na.rm = T)))
	
	dat <- dat%>%select(rgn_id, year, pressure_score)
	
	## save data layer
	##################
	
	write_csv(dat, paste(directory, "prep/", toupper(goal), "/", "hd_", filename, "_", region, year, ".csv", sep = ""))
	write_csv(dat, paste(directory, "region2017/layers/", "hd_", filename, "_", region, year, ".csv", sep = ""))
	
	
	
