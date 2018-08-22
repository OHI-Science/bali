##################################
#prepare mariculture extent layer 
##################################

##############################################
#dataset only available for Bali province, so
#the script divides propprtionally each annual 
#data into eight subregions
##############################################

rm(list = ls(all = T))

library(tidyverse)
library(dplyr)

goal <- "mar"
region <- "bali"
year <- 2016

filename <- "mariculture_area_extent"

fl_prod <- paste(goal, "_mariculture_production_", region,year, sep = "")


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read extent data
	#################		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ",")
	
		#repeat each row into eight rows
		################################
		dat <- dat[rep(seq_len(nrow(dat)), each = 8),]
		
		#convert to km2
		################
		dat$km2 <- dat$extent_ha * 0.01
		
		#add new variable that will be used as rgn_id
		#############################################	
		dat <- dat%>%group_by(year)%>%mutate(rg = seq(1,8,1))%>%select(rg, year, km2)%>%
			rename(rgn_id = rg, year = year, km2 = km2)
	
	
	
	
	#read mariculture production data for weighting the extent
	###########################################################
	datprod <- readr::read_delim(file = paste(fl_prod, ".csv", sep = ""), delim = ",")
	
		total_prod <- datprod%>%group_by(year)%>%summarize(total_prod = sum(ton, na.rm = T))%>%ungroup()
		
		datprod <- merge(datprod, total_prod, by = c("year"))
		
		datprod$weight <- datprod$ton/datprod$total_prod
		
		datprod <- datprod[, c("rgn_id", "year", "weight")]
	
	
	
	dat_ext <- merge(dat, datprod, by = c("rgn_id", "year"))
	
	dat_ext$km2 <- round(dat_ext$weight*dat_ext$km2, 4)
	
	dat_ext <- dat_ext[,c("rgn_id", "year", "km2")]
	
	
		
			## save data layer
			##################
	
			write_csv(dat_ext, paste(directory, "prep/", toupper(goal), "/", goal, "_", filename, "_", region, year, ".csv", sep = ""))
			write_csv(dat_ext, paste(directory, "region2017/layers/", goal, "_", filename, "_", region, year, ".csv", sep = ""))
			
