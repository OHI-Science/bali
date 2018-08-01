###########################################
#prepare msy and fopt
##########################################

rm(list = ls(all = T))

library(tidyverse)
library(reshape2)

goal <- "fis"
region <- "bali"
year <- 2016

#data contains nb catch, nb fishermen, & nb fishing gear
########################################################
filename <- "marine_production"



directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ",")
	
	
	
	#convert wide form to long dataframe form
	##########################################
	dat <- melt(dat, id = c("rgn_id", "variable"))
	
	#rename coloumn names
	#####################
	names(dat) <- c("rgn_id", "variable", "year", "values")
	

	#convert year as numeric
	########################
	dat$year <- as.numeric(substr(dat$year, 3, 6))
	
	#extract catch data
	###################
	dat_catch <- dat[dat$variable == "catch",]
	
		dat_catch <- dat_catch[,c("rgn_id", "year", "values")]
		names(dat_catch) <- c("rgn_id", "year", "catch")
	
	#extract effort
	###############
	dat_effort <- dat[dat$variable %in% c("fishermen", "fishing_gear"),]
	
	#average effort
	###############
	dat_effort <- dat_effort%>%group_by(rgn_id, year)%>%summarize(effort = mean(values, na.rm = T))%>%ungroup()
	
	#merge catch and effort data
	############################
	dat_all <- merge(dat_catch, dat_effort, by.x = c("rgn_id", "year"), all= T)
	
	#calculate cpue
	###############
	dat_all$cpue <- dat_all$catch/dat_all$effort
	
	#calculate msy and fopt 
	#######################
	
		model <- lm(dat_all$cpue~dat_all$effort)
		
		intercept <- abs(model[[1]][[1]])
		slope <- abs(model[[1]][[2]])
		
		msy <- (intercept^2)/(4*slope)
		
		fopt <- (intercept)/(2*slope)
	
		
			rgn_id <- seq(min(dat$rgn_id, na.rm = T),max(dat$rgn_id, na.rm = T),1)
			year_dat <-  seq(min(dat$year, na.rm = T), max(dat$year, na.rm = T), 1)
		
		#save msy file
		##############	
		dat_msy <- expand.grid(x = rgn_id, y = year_dat)
		dat_msy$values <- rep(msy, nrow(dat_msy))	
		dat_msy <- dat_msy%>%rename(rgn_id = x, year = y, values = values)
		
		
		
				write_csv(dat_msy, paste(directory, "prep/", toupper(goal), "/", goal, "_msy_", region, year, ".csv", sep = ""))
				write_csv(dat_msy, paste(directory, "region2017/layers/", goal, "_msy_", region, year, ".csv", sep = ""))
				
				
		#save fopt file
		##############	
		dat_fopt <- expand.grid(x = rgn_id, y = year_dat)
		dat_fopt$values <- rep(fopt, nrow(dat_fopt))	
		dat_fopt <- dat_fopt%>%rename(rgn_id = x, year = y, values = values)
		
				write_csv(dat_fopt, paste(directory, "prep/", toupper(goal), "/", goal, "_fopt_", region, year, ".csv", sep = ""))
				write_csv(dat_fopt, paste(directory, "region2017/layers/", goal, "_fopt_", region, year, ".csv", sep = ""))
		
		
	
	
