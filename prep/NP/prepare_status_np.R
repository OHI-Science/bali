###############################
#prepare status natural product
###############################

rm(list = ls(all = T))

library(tidyverse)
library(reshape2)

goal <- "np"
region <- "bali"
year <- 2016

#data contains exort values of various species NP 
#################################################
filename <- "export_marine_commodities"



directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ",")
	
	#convert wide form to long dataframe form
	##########################################
	dat <- melt(dat, id = c("rgn_id", "species"))
	
	
	#rename coloumn names
	#####################
	names(dat) <- c("rgn_id", "species", "year", "values")
	
	
	
	#convert year as numeric
	########################
	dat$year <- as.numeric(substr(dat$year, 3, 6))
	
		
	dat <- dat%>%arrange(year)
	
	
	#save as file for each species
	##############################
	
	for (aa in unique(dat$species)){
		
		al <- which(dat$species == aa)
		
	if (length(al > 0)){
		
		ald <- dat[al,]
		
	if (length(ald > 0)){
		
	
		ald <- ald[,c("rgn_id", "year", "values")]
		
				
			## save data layer
			##################
	
			write_csv(ald, paste(directory, "prep/", toupper(goal), "/", goal, "_", aa, "_", region, year, ".csv", sep = ""))
			write_csv(ald, paste(directory, "region2017/layers/", goal, "_", aa, "_", region, year, ".csv", sep = ""))
	
	
		
		
	}
	}
	}
	
