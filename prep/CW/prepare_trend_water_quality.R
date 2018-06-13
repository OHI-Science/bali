#########################################
#calculate trend of clean water variables
#########################################


library(tidyverse)
library(dplyr)

goal <-"cw"
year <- 2016
region <- "bali"

filename <- "cw_water_quality.csv"

#set working directory
######################
directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)

#read data
##########
dat <- read.csv(file = filename, header = T, sep = ",")


#replacing comma with point for decimal mark, the data start from fifth coloumn
###############################################################################
dat[,5:ncol(dat)] <- apply(apply(dat[,5:ncol(dat)], 2, gsub, patt=",", replace="."), 2, as.numeric)

#rename coloum names
####################
names(dat) <- c("rgn_id", "year", "location", "region", "turbidity", "temperature", "tss", "tds", "ph", "salinity", "do", "bod",
				"cod", "ammonia", "nitrat","sulfat", "fosfat", "sulfida", "crude_oil", "detergent", "cadmium", "plumbum", 				
				"iron", "chlorin", "coliform")
				
#calculate mean of value in a region and year
##############################################

dat <- setNames(aggregate(cbind(dat$turbidity, dat$tss, dat$tds, dat$do, dat$bod, dat$cod, dat$ammonia, dat$nitrat, dat$sulfat, dat$fosfat, dat$sulfida, dat$crude_oil, 
		dat$detergent, dat$cadmium, dat$plumbum, dat$iron, dat$chlorin, dat$coliform) ~ dat$rgn_id + dat$year, FUN = mean, na.rm = T, na.action="na.pass"), 
		c("rgn_id", "year", "turbidity","tss", "tds", "do", "bod", "cod", "ammonia", "nitrat","sulfat", "fosfat", "sulfida", "crude_oil", "detergent", "cadmium", "plumbum", 				
		"iron", "chlorin", "coliform"))

#replace NAN to NA
##################
dat[is.na(dat)] <- NA

#calculate trend for each variable and rgn_id
#############################################

for (i in 3:ncol(dat)){
	
	dat1 <- dat[, c(1, 2, i)]
		
	
		jm <- NULL
		for (j in unique(dat1$rgn_id)){
			
			jl <- which(dat1$rgn_id == j)
			
		if (length(jl > 0)){
			
			jld <- dat1[jl,]
			
		if (length(jld > 0)){
			
					
			if (is.na(unique(jld[,3]))){
				
				#all value are NA, so trend will be NA
			
				jld$trend <- NA
				
				jld <- jld[,c("rgn_id", "year", "trend")]
				
				jm <- rbind(jm, jld)
				
			}else{
			if (length(!is.na(unique(jld[,3]))) < 2){
				
				#non-NA variables values less then 2, trend will be NA
				
				jld$trend <- NA
				
				jld <- jld[,c("rgn_id", "year", "trend")]
				
				jm <- rbind(jm, jld)
				
			}else{
				
								
				#calculate linear model and extract slope
				trendline <- lm(jld[,3]~jld$year)
								
				trendline <- trendline[[1]][[2]]
				
				jld$trend <- trendline
				
				jld <- jld[,c("rgn_id", "year", "trend")]
				
				jm <- rbind(jm, jld)
				
			}
			}
		}
		}
		}
		
		
		#set output file name and save as file
		######################################
		
		if (names(dat[i]) == "do"){
			
			# save data layer
			write_csv(jm, paste(directory, "prep/", toupper(goal), "/", goal, "_", names(dat[i]), "_trend_", region, year, ".csv", sep = ""))
			write_csv(jm, paste(directory, "region2017/layers/", goal, "_", names(dat[i]), "_trend_", region, year, ".csv", sep = ""))
			
						
		}else{
			
			
			# save data layer
			write_csv(jm, paste(directory, "prep/", toupper(goal), "/", "po", "_", names(dat[i]), "_trend_", region, year, ".csv", sep = ""))
			write_csv(jm, paste(directory, "region2017/layers/", "po", "_", names(dat[i]), "_trend_", region, year, ".csv", sep = ""))
		
			
		}
		
			
}
	
	
	




