#prepare coral_reef, mangrove, and seagrass data

library(tidyverse)
library(dplyr)

#prepare mangrove health

variable <- "mangrove"
year <- 2016
region <- "bali"

directory <- paste("/home/twibawa/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/HAB/", sep = "")
setwd(dir_scripts)

indir <- dir_scripts

				
		#read_data
				
		dat <- readr::read_delim(file = paste(indir, variable, "_health_ndvi.csv", sep = ""), delim = ",")
				

		
		#order data based on region id and years
		dat <- dat %>% rename(year = years, health=ndvi) %>% arrange(rgn_id, year)
		
		
		#fill missing region with health = NA
		dat <- dat %>% complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1), year, fill = list(health = NA))
		
		#add habitat name based on variable name
		dat <- dat %>% mutate(habitat = variable) %>% select(rgn_id, habitat, year, health)
		
		

		## save data layer
		write_csv(dat, paste(directory, "prep/HAB/hab_", variable, "_health_", region, year, ".csv", sep = ""))
		write_csv(dat, paste(directory, "region2017/layers/hab_", variable, "_health_", region, year, ".csv", sep = ""))
		
				



