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
	
	#calculate msy and fopt for each subregion
	###########################################
	
	a_msy <- NULL
	a_fopt <- NULL
	
	for (aa in unique(dat_all$rgn_id)){
		
		al <- which(dat_all$rgn_id == aa)
		
	if (length(al > 0)){
		
		ald <- dat_all[al,]
		
	if (length(ald > 0)){
		
		
		model <- lm(ald$cpue~ald$effort)
		
		intercept <- abs(model[[1]][[1]])
		slope <- abs(model[[1]][[2]])
		
		msy <- (intercept^2)/(4*slope)
		
		fopt <- (intercept)/(2*slope)
		
		
		dat_msy <- data.frame(rgn_id = ald$rgn_id, year = ald$year, values = msy)
		
		dat_fopt <- data.frame(rgn_id = ald$rgn_id, year = ald$year, values = fopt)
		
		
	a_msy <- rbind(a_msy, dat_msy)
	a_fopt <- rbind(a_fopt, dat_fopt)	
		
		
	
	}
	}
	}
	
	
	
	## save data layer
	##################
	
		write_csv(a_msy, paste(directory, "prep/", toupper(goal), "/", goal, "_msy_", region, year, ".csv", sep = ""))
		write_csv(a_msy, paste(directory, "region2017/layers/", goal, "_msy_", region, year, ".csv", sep = ""))
	
		write_csv(a_fopt, paste(directory, "prep/", toupper(goal), "/", goal, "_fopt_", region, year, ".csv", sep = ""))
		write_csv(a_fopt, paste(directory, "region2017/layers/", goal, "_fopt_", region, year, ".csv", sep = ""))
	
	
	
	
	
	
	
	
