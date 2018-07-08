##########################################
#prepare status & trend coastal livelihood
##########################################

library(tidyverse)

goal <- "liv"
region <- "bali"
year <- 2016

filename <- "nb_marine_industries"


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ",")
	
	
	#fill missing region and year with = NA
	#######################################
	dat <- dat %>% complete(year = full_seq(x = c(min(dat$year, na.rm = T), max(dat$year, na.rm = T)), period = 1), rgn_id, fill = list(nb_marine_industries = NA)) %>%
		   complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1), year, fill = list(nb_marine_industries = NA))
	
	
	#convert character data to numeric, only for third coloumn (nb_marine_industries)
	#################################################################################
	dat[,3] <- sapply(dat[,3],as.numeric)	
	
	
	#calculate trend
	################
	r.trend <- dat%>%filter(year >= min(dat$year))%>%
				filter(!is.na(nb_marine_industries))%>%
				group_by(rgn_id)%>%arrange(year)%>%
				top_n(5, year)%>%ungroup()
				
	r.trend <- r.trend%>%group_by(rgn_id)%>%do(mdl=lm(nb_marine_industries~year, data=.))%>%
				summarize(rgn_id = rgn_id, trend = coef(mdl)['year']*5)%>%ungroup()
				
		
	#merge dat with trend
	#####################
	dat_all <- merge(dat, r.trend, by.x = c("rgn_id"), all= T)
	
	
		#save status data
		#################
		dat_stat <- dat_all%>%select(rgn_id, year, nb_marine_industries)
		
		
			## save data layer
			##################
	
			write_csv(dat_stat, paste(directory, "prep/", toupper(goal), "/", goal, "_", filename, "_", region, year, ".csv", sep = ""))
			write_csv(dat_stat, paste(directory, "region2017/layers/", goal, "_", filename, "_", region, year, ".csv", sep = ""))
			
		#save trend data
		################
		dat_trend <- dat_all%>%select(rgn_id, year, trend)
			
			## save data layer
			##################
	
			write_csv(dat_trend, paste(directory, "prep/", toupper(goal), "/", goal, "_", filename, "_trend_", region, year, ".csv", sep = ""))
			write_csv(dat_trend, paste(directory, "region2017/layers/", goal, "_", filename, "_trend_", region, year, ".csv", sep = ""))
	   
		   
	
	
	
	
