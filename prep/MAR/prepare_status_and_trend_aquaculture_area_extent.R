##############################################
#prepare status & trend aquaculture production
##############################################

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

filename <- "aquaculture_area_extent"


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
	dat$km2 <- dat$extent_ha/8 * 0.01
	
	#add new variable that will be used as rgn_id
	#############################################	
	dat <- dat%>%group_by(year)%>%mutate(rg = seq(1,8,1))%>%select(rg, year, km2)%>%
			rename(rgn_id = rg, year = year, km2 = km2)
			
			
			
			
			
	#calculate trend
	################
	r.trend <- dat%>%filter(year >= min(dat$year))%>%
				filter(!is.na(km2))%>%
				group_by(rgn_id)%>%arrange(year)%>%
				top_n(5, year)%>%ungroup()
				
	r.trend <- r.trend%>%group_by(rgn_id)%>%do(mdl=lm(km2~year, data=.))%>%
				summarize(rgn_id = rgn_id, trend = coef(mdl)['year']*5)%>%ungroup()
				
				
				
		
	#merge dat with trend
	#####################
	dat_all <- merge(dat, r.trend, by.x = c("rgn_id"), all= T)
	
	dat_all <- dat_all %>% arrange(rgn_id, year)
			
			
	
		#save status data
		#################
		dat_stat <- dat_all%>%select(rgn_id, year, km2)
		
		
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
