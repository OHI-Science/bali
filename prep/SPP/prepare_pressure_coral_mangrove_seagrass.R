#############################################################
#calculate pressure of habitat indicated by decreasing extent
#############################################################

rm(list = ls(all = T))

library(tidyverse)
library(dplyr)

year <- 2016
region <- "bali"


goal <- "spp"


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)



#variables
variables <- c("coral", "mangrove", "seagrass")


	for (i in 1:length(variables)){
		
		
	#read extent data		
	dat <- readr::read_delim(file = paste("hab_", variables[i], "_extent_bali2016.csv", sep = ""), delim = ",")
	
		
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
				
				jld$pressure_score <- NA
				
				jm <- rbind(jm, jld)
				
			}else{
				
				#calculate linear model and extract slope
				trendline <- lm(jld$km2~jld$year)
				
				
				trendline <- trendline[[1]][[2]]
				
				jld$trend <- trendline
				
				
					#calculate pressure score
					#########################
					
					#jld <- data.frame(jld)
					
					if (unique(jld$trend) < -0.01){
						
						#normalized pressure score between 0-1
						#0: largest extent, 1: smallest extent
						#using equation: zi = (xi - max(x))/(min(x) - max(x)) 
						####################################################
						
						
						
							km <- NULL
							for (k in 1:nrow(jld)){
								
								kld <- jld[k,]
		
								kld$pressure_score <- (kld$km2 - max(jld$km2, na.rm = T))/(min(jld$km2, na.rm = T) - max(jld$km2, na.rm = T))
						
							km <- rbind(km, kld)
								
							}
							
							km[km[,c("pressure_score")] == -0,] <- NA
							
						jm <- rbind(jm, km)
						
						
						
						
					}else{
						
						
						jld$pressure_score <- NA
						
						jm <- rbind(jm, jld)
						
					}
					
				
								
			}
			
		}
		}
		}
		
		
	
		#order data based on region id and years
		jm <- jm %>% arrange(rgn_id, year)
		
		#select rgn_id, year, habitat, and trend
		jm <- jm %>% select(rgn_id, year, habitat, pressure_score)
		
		
		jm <- data.frame(jm)
		jm <- jm[!is.na(jm$rgn_id),]
		
		
		
		## save data layer
		write_csv(jm, paste(directory, "prep/", toupper(goal), "/", "hd_", variables[i], "_", region, year, ".csv", sep = ""))
		write_csv(jm, paste(directory, "region2017/layers/", "hd_", variables[i], "_", region, year, ".csv", sep = ""))
		
				

}
