###########################################
#prepare status & trend fisheries variable
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
	
	
	#looping for each variable to get status and trend
	##################################################
	
	for (aa in unique(dat$variable)){
		
		al <- which(dat$variable == aa)
		
	if (length(al > 0)){
		
		ald <- dat[al,]
		
	if (length(ald > 0)){
		
		
		outfilename <- aa
		
		
			
	#fill missing region and year with = NA
	#######################################
	ald <- ald %>% complete(year = full_seq(x = c(min(ald$year, na.rm = T), max(ald$year, na.rm = T)), period = 1), rgn_id, fill = list(values = NA)) %>%
		   complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1), year, fill = list(values = NA))%>%select(rgn_id, year, values)
		   
		   
	#data divided 10000 for simplifying during trend calculation
	############################################################
	ald$values_divided <- ald$values/10000
	
	
	#calculate trend
	################
	r.trend <- ald%>%filter(year >= min(ald$year))%>%
				filter(!is.na(values_divided))%>%
				group_by(rgn_id)%>%arrange(year)%>%
				top_n(5, year)%>%ungroup()
				
	r.trend <- r.trend%>%group_by(rgn_id)%>%do(mdl=lm(values_divided~year, data=.))%>%
				summarize(rgn_id = rgn_id, trend = coef(mdl)['year']*5)%>%ungroup()
		
		
	#merge dat with trend
	#####################
	dat_all <- merge(ald, r.trend, by.x = c("rgn_id"), all= T)
	
	dat_all <- dat_all %>% arrange(rgn_id, year)
	
	
		#save status data
		#################
		dat_stat <- dat_all%>%select(rgn_id, year, values)
		
		names(dat_stat) <- c("rgn_id", "year", aa)
		
		
			## save data layer
			##################
	
			write_csv(dat_stat, paste(directory, "prep/", toupper(goal), "/", goal, "_", outfilename, "_", region, year, ".csv", sep = ""))
			write_csv(dat_stat, paste(directory, "region2017/layers/", goal, "_", outfilename, "_", region, year, ".csv", sep = ""))
			
		#save trend data
		################
		dat_trend <- dat_all%>%select(rgn_id, year, trend)
			
			## save data layer
			##################
	
			write_csv(dat_trend, paste(directory, "prep/", toupper(goal), "/", goal, "_", outfilename, "_trend_", region, year, ".csv", sep = ""))
			write_csv(dat_trend, paste(directory, "region2017/layers/", goal, "_", outfilename, "_trend_", region, year, ".csv", sep = ""))

			
		
		
	}
	}
	}	
	
	
	
