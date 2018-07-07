#######################################
#prepare pressure of coastal livelihood
#######################################

library(tidyverse)

goal <- "liv"
region <- "bali"
year <- 2016

filename <- "captures_marine_fisheries_2011_2015"
outfilename <- "fp_marine_fisheries"

directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ",")
	
	
	#fill missing region and year with = NA
	#######################################
	dat <- dat %>% complete(year = full_seq(x = c(2011, 2015), period = 1), rgn_id, fill = list(ton = NA)) %>%
		   complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1), year, fill = list(ton = NA))
	
	
	#normalized pressure score between 0-1
	#using equation: zi = xi - min(x)/(max(x) - min (x)) 
	####################################################
		
	dat$pressure_score <- ifelse(is.na(dat$ton), NA, (dat$ton - min(dat$ton, na.rm = T))/(max(dat$ton, na.rm = T) - min(dat$ton, na.rm = T)))
	
	dat <- dat%>%select(rgn_id, year, pressure_score)
	
		
	## save data layer
	##################
	
	write_csv(dat, paste(directory, "prep/", toupper(goal), "/", outfilename, "_", region, year, ".csv", sep = ""))
	write_csv(dat, paste(directory, "region2017/layers/", outfilename, "_", region, year, ".csv", sep = ""))
	
	
	
	
	
	
