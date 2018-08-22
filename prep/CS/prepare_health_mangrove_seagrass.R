###################################################
#prepare health of mangroves and seagrass
#data are derived from Central Bureau of Statistic
###################################################

rm(list = ls(all = T))

library(tidyverse)
library(dplyr)


year <- 2016
region <- "bali"

goal <- "cs"


variable <- c("mangrove", "seagrass")
condition <- "good"

filename <- "habitat_extent_health.csv"

file_extent_mangrove <- "cs_mangrove_extent_bali2016.csv"
file_extent_seagrass <- "cs_seagrass_extent_bali2016.csv"

directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


				
		#read_data
		###########
				
		dat <- readr::read_delim(file = filename, delim = ",")
		
		
		#extract habitat & condition
		############################
		dat <- dat%>%mutate(habitat = sapply(strsplit(as.character(variable), "\\_"), "[[", 1))%>%
				mutate(condition = sapply(strsplit(as.character(variable), "\\_"), "[[", 2))

		#select habitat and good condition
		##################################			
		dat <- dat[dat$habitat %in% c(variable) & dat$condition %in% c(condition),]
				
				
		dat <- dat%>%arrange(habitat, year)
		
		
			#fill missing year observation with = NA
			#########################################
			dat <- dat %>% group_by(habitat)%>%complete(year = full_seq(x = c(2007, 2016), period = 1), habitat, fill = list(percentage = NA))%>%ungroup()
			
						
					
		#temporal filling gap for each habitat, if any NA data
		#######################################################
	
		#gap filling - calculate intercept and slope
		############################################
		dat_no_na <- dat[!is.na(dat$percentage),]
		dat_no_na <- dat_no_na%>%arrange(habitat, year)
		
		intercept_slope <- dat_no_na%>%group_by(habitat)%>%filter(length(percentage) > 1)%>%do(mdl= lm(percentage~year, data=.))%>%
					summarize(habitat = habitat, intercept = coef(mdl)[1], slope = coef(mdl)[2])%>%ungroup()
	
		intercept_slope$intercept[intercept_slope$intercept == 0] <- NA
		
		
		
		#merge dat_all with intercept_slope
		###############################
		dat_all <- merge(dat, intercept_slope, by.x = c("habitat"), all= T)
		dat_all <- dat_all%>%arrange(habitat, year)
		
		#fill missing value based on intercept and slope
		################################################
		dat_all$percentage_new <- ifelse(!is.na(dat_all$intercept),(dat_all$slope*dat_all$year)+dat_all$intercept, NA)
		
		
			
		#replace with new result of catch
		#################################
		dat_all$percentage[dat_all$percentage == 0] <- NA
		
		dat_all$percentage <- ifelse(is.na(dat_all$percentage), dat_all$percentage_new, dat_all$percentage)
	
		dat_all <- dat_all%>%select(habitat, year, percentage)%>%arrange(habitat, year)
		
				
		#remove data still have NA catch and lower than zero catch
		##########################################################
		dat_all <- dat_all[!is.na(dat_all$percentage),]
		
		dat_all <- dat_all[dat_all$percentage > 0,]
		
		
		#distribute equally the health condition over subregions
		#########################################################
		
				#repeat each row to eight subregion
				###################################
				dat_all <- dat_all[rep(seq_len(nrow(dat_all)), each = 8),]
				dat_all <- dat_all%>%group_by(habitat, year)%>%mutate(rgn_id = seq(1,8,1))%>%ungroup()
				
				#for mangrove
				##############
				dat_all_mangrove <- dat_all[dat_all$habitat == "mangrove",]
			
					#extent mangrove
					################
					dat_mangrove <- readr::read_delim(file = file_extent_mangrove, delim = ",")
					
					dat_all_mangrove <- merge(dat_all_mangrove, dat_mangrove, by = c("rgn_id", "year"), all = T)
					
					dat_all_mangrove_km <- dat_all_mangrove%>%group_by(rgn_id)%>%summarize(mean_km = mean(km2, na.rm = T))%>%ungroup()
					
					dat_all_mangrove <- left_join(dat_all_mangrove, dat_all_mangrove_km)
										
					dat_all_mangrove <- dat_all_mangrove%>%group_by(rgn_id)%>%mutate(percentage = ifelse(is.na(mean_km), NA, percentage))%>%ungroup()
					
					dat_all_mangrove <- dat_all_mangrove%>%select(rgn_id, habitat.x, year, percentage)%>%rename(rgn_id = rgn_id, habitat = habitat.x, year = year, health = percentage)
					
								
					## save data layer, for the bali project, health mangrove was derived from ndvi
					#write_csv(dat_all_mangrove, paste(directory, "prep/", toupper(goal), "/", goal, "_mangrove_health_", region, year, ".csv", sep = ""))
					#write_csv(dat_all_mangrove, paste(directory, "region2017/layers/", goal, "_mangrove_health_", region, year, ".csv", sep = ""))
					
					
				#for seagrass
				#############
				
				dat_all_seagrass <- dat_all[dat_all$habitat == "seagrass",]
				
				
					
					
					#extent seagrass
					################	
					dat_seagrass <- readr::read_delim(file = file_extent_seagrass, delim = ",")
					
					dat_all_seagrass <- merge(dat_all_seagrass, dat_seagrass, by = c("rgn_id", "year"), all = T)
					
					dat_all_seagrass_km <- dat_all_seagrass%>%group_by(rgn_id)%>%summarize(mean_km = mean(km2, na.rm = T))%>%ungroup()
					
					dat_all_seagrass <- left_join(dat_all_seagrass, dat_all_seagrass_km)
					
								
					dat_all_seagrass <- dat_all_seagrass%>%group_by(rgn_id)%>%mutate(percentage = ifelse(is.na(mean_km), NA, percentage))%>%ungroup()
					
					dat_all_seagrass <- dat_all_seagrass%>%select(rgn_id, habitat.x, year, percentage)%>%rename(rgn_id = rgn_id, habitat= habitat.x, year = year, health = percentage)
					
										
					## save data layer
					write_csv(dat_all_seagrass, paste(directory, "prep/", toupper(goal), "/", goal, "_seagrass_health_", region, year, ".csv", sep = ""))
					write_csv(dat_all_seagrass, paste(directory, "region2017/layers/", goal, "_seagrass_health_", region, year, ".csv", sep = ""))
					
					
		
		
				



