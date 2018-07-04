###########################################
#calculate number of tourist in each region
###########################################


library(tidyverse)
library(dplyr)

year <- 2016
region <- "bali"

#select goal, change to goal[1] when select "cs"
goal <- "tr"

filename <- "tr_number_tourist"


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ",")
	
	
	#calculate total nb tourist each region and year
	###############################################
	dat <- dat %>% group_by(rgn_id, years) %>% summarize(nb_tourist = sum(unit, na.rm = T))%>% ungroup()
	
	#convert zero to NA
	###################
	dat$nb_tourist[dat$nb_tourist <= 0] <- NA
	
			
	#order data based on region id and years
	########################################
	dat <- dat %>% rename(year = years) %>% arrange(rgn_id, year)
	
	#fill missing region with unit = NA
	###################################
	dat <- dat %>% complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1), year, fill = list(nb_tourist = NA))
	
	## save data layer
	##################
	write_csv(dat, paste(directory, "prep/", toupper(goal), "/", filename, "_", region, year, ".csv", sep = ""))
	write_csv(dat, paste(directory, "region2017/layers/", filename, "_", region, year, ".csv", sep = ""))



	#calculate trend
	r.trend <- dat %>%
				filter(year >= min(year)) %>%
				filter(!is.na(nb_tourist))%>%
				group_by(rgn_id) %>%
				arrange(year) %>%
				top_n(5, year) %>%
				ungroup()
			
				
	r.trend <- r.trend %>%
				group_by(rgn_id) %>%
				do(mdl = lm(nb_tourist~year, data=.))%>%
				summarize(rgn_id = rgn_id, trend = coef(mdl)['year']*5) %>%
				ungroup()
				
	
	#fill missing region with unit = NA
	###################################
	r.trend <- r.trend %>% complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1), fill = list(trend = NA))
	
	dat <- merge(dat, r.trend, by = c("rgn_id"))
	
	dat <- dat %>% select(rgn_id, year, trend) 
	
	## save data layer
	##################
	write_csv(dat, paste(directory, "prep/", toupper(goal), "/", filename, "_trend_", region, year, ".csv", sep = ""))
	write_csv(dat, paste(directory, "region2017/layers/", filename, "_trend_", region, year, ".csv", sep = ""))

