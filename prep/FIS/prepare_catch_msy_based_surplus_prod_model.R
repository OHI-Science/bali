############################################
#calculate catch msy based surplus prod model
############################################

rm(list = ls(all = T))

library(tidyverse)
library(reshape2)

goal <- "fis"
region <- "bali"
year <- 2016

#input filename
###############
fl_catch <- paste(goal, "_catch_", region, year, sep = "")
fl_effort <- paste(goal, "_effort_", region, year, sep = "")



directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat_catch <- readr::read_delim(file = paste(fl_catch, ".csv", sep = ""), delim = ",")
	dat_effort <- readr::read_delim(file = paste(fl_effort, ".csv", sep = ""), delim = ",")
	
	
	#merge catch and effort data
	############################
	dat <- merge(dat_catch, dat_effort, by.x = c("rgn_id", "year"), all= T)
	
	#calculate cpue
	###############
	dat$cpue <- dat$ton/dat$vessel
	
	#calculate msy
	##############
	
		model <- lm(dat$cpue~dat$vessel)
		
		intercept <- abs(model[[1]][[1]])
		slope <- abs(model[[1]][[2]])
		
		msy <- (intercept^2)/(4*slope)
		
		dat$msy <- c(msy)
		
		dat <- dat%>%select(rgn_id, year, msy)
			
		
		
				write_csv(dat, paste(directory, "prep/", toupper(goal), "/", goal, "_msy_", region, year, ".csv", sep = ""))
				write_csv(dat, paste(directory, "region2017/layers/", goal, "_msy_", region, year, ".csv", sep = ""))
	
	
