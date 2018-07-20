######################
#prepare extent of MPA
######################

rm(list = ls(all = T))

library(tidyverse)


goal <- "lsp"
region <- "bali"
year <- 2016

#data input
###########
filename <- "extent_mpa"

directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)

	#read data
	##########		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ",")
	
	#fill missing region and year with = NA
	#######################################
	dat <- dat %>%complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1), year, fill = list(extent_ha = NA))
	
	#repeat each row into eight rows
	################################
	dat <- dat[rep(seq_len(nrow(dat)), each = 6),]
	
	dat <- dat%>%group_by(rgn_id)%>%mutate(km2 = extent_ha * 0.01)%>%mutate(year = seq(2011, 2016, 1))
	
	dat <- dat%>%select(rgn_id, year, km2)
	
	
	
	#fill missing region and year with = NA
	#######################################
	#dat <- dat %>%complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1), year, fill = list(km2 = NA))
			  

	## save data layer
	##################
		write_csv(dat, paste(directory, "prep/", toupper(goal), "/", goal, "_", filename, "_", region, year, ".csv", sep = ""))
		write_csv(dat, paste(directory, "region2017/layers/", goal, "_", filename, "_", region, year, ".csv", sep = ""))






