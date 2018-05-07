#############################################################
#calculate trend from extent of mangrove, seagrass, and coral
#############################################################
#Im sorry, I tried to use dplyr but not succeeded 

library(tidyverse)
library(dplyr)

year <- 2016
region <- "bali"


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/HAB/", sep = "")
setwd(dir_scripts)

#input directory
indir <- dir_scripts

#variables
variables <- c("coral", "mangrove", "seagrass")


	for (i in 1:length(variables)){
		
				
	dat <- read.table(file = paste(indir, "hab_", variables[i], "_extent_bali2016.csv", sep = ""), sep = ",", header = T)
	
		
		#calculate trend for each subregion using linear model
		#if extent are NA, trend = NA
		
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
				
				#calculate linear model and extract trend
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
		write_csv(jm, paste(directory, "prep/HAB/hab_", variables[i], "_trend_", region, year, ".csv", sep = ""))
		write_csv(jm, paste(directory, "region2017/layers/hab_", variables[i], "_trend_", region, year, ".csv", sep = ""))
		
				
	}


