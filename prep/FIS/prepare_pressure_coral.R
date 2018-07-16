#################################################
#prepare pressure FIS due to coral reef condition
################################################

rm(list = ls(all = T))

library(tidyverse)

goal <- "fis"
region <- "bali"
year <- 2016

filename <- "coral_index_mortality"


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ",")
	
	#summarize data
	###############
	dat <- dat%>%group_by(rgn_id, year)%>%summarize(mean_im = mean(index_mortality, na.rm = T))%>%ungroup()
	
	#select variable and rename coloumn
	###################################
	dat <- dat%>%select(rgn_id, year, mean_im)%>%rename(rgn_id = rgn_id, year = year, index_mortality = mean_im)
	
	#gap filling - calculate intercept and slope
	############################################
	intercept_slope <- dat%>%group_by(rgn_id)%>%filter(length(index_mortality) > 1)%>%do(mdl= lm(index_mortality~year, data=.))%>%
					summarize(rgn_id = rgn_id, intercept = coef(mdl)[1], slope = coef(mdl)[2])%>%ungroup()
	
		
	#fill missing region and year with = NA
	#######################################
	dat <- dat %>% complete(year = full_seq(x = c(min(dat$year, na.rm = T), max(dat$year, na.rm = T)), period = 1), rgn_id, fill = list(index_mortality = NA)) %>%
		   complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1), year, fill = list(index_mortality = NA))
		   
		   
	#merge dat with intercept_slope
	###############################
	dat_all <- merge(dat, intercept_slope, by.x = c("rgn_id"), all= T)
	
	#fill missing value based on intercept and slope
	################################################
	dat_all$index_mortality_new <- ifelse(!is.na(dat_all$intercept), (dat_all$slope*dat_all$year)+dat_all$intercept, NA)
	
	#replace with new result of index mortality
	#######################################
	dat_all$index_mortality <- ifelse(is.na(dat_all$index_mortality), dat_all$index_mortality_new, dat_all$index_mortality)
	
	dat_all <- dat_all%>%select(rgn_id, year, index_mortality)%>%arrange(rgn_id, year)
	
	
	#rescaled with normalized pressure (0-1),
	#using equation: zi = xi - min(x)/(max(x) - min(x)) 
	##############################################################
	
	
	dat_all$pressure_score <- ifelse(is.na(dat_all[,3]), NA, (dat_all[,3] - min(dat_all[,3], na.rm = T))/(max(dat_all[,3], na.rm = T) - min(dat_all[,3], na.rm = T)))
	
	
	dat_all <- dat_all%>%select("rgn_id", "year", "pressure_score")
		
		# save data layer
		#################
		write_csv(dat_all, paste(directory, "prep/", toupper(goal), "/", "fp", "_", filename, "_", region, year, ".csv", sep = ""))
		write_csv(dat_all, paste(directory, "region2017/layers/", "fp", "_", filename, "_", region, year, ".csv", sep = ""))
		
		
