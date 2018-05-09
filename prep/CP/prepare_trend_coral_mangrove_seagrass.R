#############################################################
#calculate trend from extent of mangrove, seagrass, and coral
#############################################################


library(tidyverse)
library(dplyr)

year <- 2016
region <- "bali"

#select goal, change to goal[1] when select "cs"
goal <- c("cs", "cp", "hab")
goal <- goal[2]


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)



#variables
variables <- c("coral", "mangrove", "seagrass")


	for (i in 1:length(variables)){
		
		
	#read extent data		
	dat <- readr::read_delim(file = paste(goal, "_", variables[i], "_extent_bali2016.csv", sep = ""), delim = ",")
	
		
		#calculate trend for each subregion using linear model
		#if extent contains NA, trend = NA
		
		#I'm sorry, I attempted to calculate slope of linear model using dplyr style, but not succeed
				
		
		jm <- NULL
		for (j in unique(dat$rgn_id)){
			jl <- which(dat$rgn_id == j)
			
		if (length(jl > 0)){
			jld <- dat[jl,]
			
		if (length(jld > 0)){
		
			if (is.na(unique(jld$km2))){
			
				jld$trend <- NA
				
				jm <- rbind(jm, jld)
				
			}else{
				
				#calculate linear model and extract slope
				trendline <- lm(jld$km2~jld$year)
				
				
				trendline <- trendline[[1]][[2]]
				
				jld$trend <- trendline
				
				jm <- rbind(jm, jld)
				
			}
			
		}
		}
		}
		
		
	
		#order data based on region id and years
		jm <- jm %>% arrange(rgn_id, year)
		
		#select rgn_id, year, habitat, and trend
		jm <- jm %>% select(rgn_id, year, habitat, trend)
		
		## save data layer
		write_csv(jm, paste(directory, "prep/", toupper(goal), "/", goal, "_", variables[i], "_trend_", region, year, ".csv", sep = ""))
		write_csv(jm, paste(directory, "region2017/layers/", goal, "_", variables[i], "_trend_", region, year, ".csv", sep = ""))
		
				

}
