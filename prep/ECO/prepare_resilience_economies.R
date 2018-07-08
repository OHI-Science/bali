##############################
#prepare resilience of economy
##############################

library(tidyverse)

goal <- "eco"
region <- "bali"
year <- 2016

filename <- "labour_force"


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ",")
	
	
	#fill missing region and year with = NA
	#######################################
	dat <- dat %>% complete(year = full_seq(x = c(min(dat$year, na.rm = T), max(dat$year, na.rm = T)), period = 1), rgn_id, fill = list(labour_force = NA)) %>%
		   complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1), year, fill = list(labour_force = NA))
	
	
	#normalized pressure score between 0-1
	#using equation: zi = xi - min(x)/(max(x) - min (x)) 
	####################################################
		
	dat$resilience_score <- ifelse(is.na(dat$labour_force), NA, (dat$labour_force - min(dat$labour_force, na.rm = T))/(max(dat$labour_force, na.rm = T) - min(dat$labour_force, na.rm = T)))
	
	dat <- dat%>%select(rgn_id, year, resilience_score)
	
			
	## save data layer
	##################
	
	write_csv(dat, paste(directory, "prep/", toupper(goal), "/","g", "_", filename, "_", region, year, ".csv", sep = ""))
	write_csv(dat, paste(directory, "region2017/layers/", "g", "_", filename, "_", region, year, ".csv", sep = ""))
	
	
	
	
	
	
