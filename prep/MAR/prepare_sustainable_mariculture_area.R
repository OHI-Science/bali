#####################################################
#prepare extent of sustainable mariculture area (km2)
######################################################

##############################################
#dataset only available for Bali province, so
#the script divides equally each annual data
#into eight subregions
##############################################

rm(list = ls(all = T))

library(tidyverse)
library(dplyr)

goal <- "mar"
region <- "bali"
year <- 2016

filename <- "aquaculture_sustainable_area"


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ",")
	

	
	#repeat each row into eight rows
	################################
	dat <- dat[rep(seq_len(nrow(dat)), each = 8),]
	
	#equally divided into eight sub-region and converted to km2
	###########################################################
	dat$km2 <- dat$sustainable_ha/8 * 0.01
	
	#add new variable that will be used as rgn_id
	#############################################	
	dat <- dat%>%mutate(rg = seq(1,8,1))%>%select(rg, km2)%>%
			rename(rgn_id = rg, km2 = km2)
			
		
			## save data layer
			##################
	
			write_csv(dat, paste(directory, "prep/", toupper(goal), "/", goal, "_", filename, "_", region, year, ".csv", sep = ""))
			write_csv(dat, paste(directory, "region2017/layers/", goal, "_", filename, "_", region, year, ".csv", sep = ""))
			
		
			
