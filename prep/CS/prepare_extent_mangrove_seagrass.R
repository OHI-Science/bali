#prepare coral_reef, mangrove, and seagrass data

library(tidyverse)



year <- 2016
region <- "bali"

#select goal, change to goal[1] when select "cs"
goal <- c("cs", "cp", "hab")
goal <- goal[1]


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


indir <- dir_scripts

variables <- c("mangrove", "seagrass")


	for (i in 1:length(variables)){
		
			#read each data variable	
			if (variables[i] == "coral"){
			
				
				dat <- readr::read_delim(file = paste(indir, goal, "_", variables[i], "_reef.csv", sep = ""), delim = ";")
				
			}else{
				
				
				dat <- readr::read_delim(file = paste(indir, goal, "_", variables[i], ".csv", sep = ""), delim = ";")
				
			}
		
		
		#order data based on region id and years
		dat <- dat %>% rename(year = years) %>% arrange(rgn_id, year)
		
		#fill missing region with area_ha = NA
		dat <- dat %>% complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1), year, fill = list(area_ha = NA))
		
		#add habitat name based on variable name
		dat <- dat %>% mutate(habitat = variables[i]) %>% select(rgn_id, habitat, year, area_ha)
		
		## convert hectares to square kilometers
		dat <- dat %>% mutate(km2 = area_ha * 0.01) %>% select(rgn_id, habitat, year, km2)

		## save data layer
		write_csv(dat, paste(directory, "prep/", toupper(goal), "/", goal, "_", variables[i], "_extent_", region, year, ".csv", sep = ""))
		write_csv(dat, paste(directory, "region2017/layers/", goal, "_", variables[i], "_extent_", region, year, ".csv", sep = ""))
		
				
	}


