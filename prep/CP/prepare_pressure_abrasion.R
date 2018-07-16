###################################
#prepare pressure CP ue to abrasion
###################################

rm(list = ls(all = T))

library(tidyverse)

goal <- "cp"
region <- "bali"
year <- 2016

filename <- "cp_abrasion"


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ";")
	
	
	#fill NA when data unavailable
	##############################
	dat <- dat%>%rename(rgn_id = rgn_id, year = years, abrasion_km = abrasion_km)%>%
			complete(year = full_seq(x = c(min(year, na.rm = T), max(year, na.rm = T)), period = 1), rgn_id, fill = list(abrasion_km = NA)) %>%
		   complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1), year, fill = list(abrasion_km = NA))
	
	
	#normalized pressure score between 0-1
	#using equation: zi = xi - min(x)/(max(x) - min (x)) 
	####################################################
		
	dat$pressure_score <- ifelse(is.na(dat$abrasion_km), NA, (dat$abrasion_km - min(dat$abrasion_km, na.rm = T))/(max(dat$abrasion_km, na.rm = T) - min(dat$abrasion_km, na.rm = T)))
	
	
	
	dat <- dat%>%select(rgn_id, year, pressure_score)
	
	
	
		# save data layer
		#################
		write_csv(dat, paste(directory, "prep/", toupper(goal), "/", "hd", "_", filename, "_", region, year, ".csv", sep = ""))
		write_csv(dat, paste(directory, "region2017/layers/", "hd", "_", filename, "_", region, year, ".csv", sep = ""))	
