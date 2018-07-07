#################################################################
#pressure of tourism, calculated from index of mortality of coral
#################################################################


library(tidyverse)

year <- 2016
region <- "bali"

#select goal, change to goal[1] when select "cs"
goal <- "tr"

filename <- "index_mortality_coral"
outfilename <- "hd_coral_reef"

#reverse function
#higher index mortality relates to more coral reef destruction, so for the OHI pressure
#those index are required to be reversed


	revscore <- function(x) {
		
		ab <- seq(0, 1, 0.01)
		ak <- rev(ab)
		dat <- data.frame(ab, ak)
		
		dat[match(x, dat[, "ab"]), 2]
	
	}


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ",")
	
	#add pressure score as reverse from index_mortality
	###################################################
	dat <- dat %>% mutate(pressure_score = revscore(dat$index_mortality)) %>% select(rgn_id, year, pressure_score)
	
	
		
	#fill missing region and year with = NA
	#######################################
	dat <- dat %>% complete(year = full_seq(x = c(2011, 2016), period = 1), rgn_id, fill = list(index_mortality = NA)) %>%
		   complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1), year, fill = list(index_mortality = NA))
		   
		
	## save data layer
	##################
	
	write_csv(dat, paste(directory, "prep/", toupper(goal), "/", outfilename, "_", region, year, ".csv", sep = ""))
	write_csv(dat, paste(directory, "region2017/layers/", outfilename, "_", region, year, ".csv", sep = ""))
	
	
	
	
