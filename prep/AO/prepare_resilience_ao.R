##############################
#prepare resilience of economy
##############################

rm(list = ls(all = T))

library(reshape2)
library(tidyverse)

goal <- "ao"
region <- "bali"
year <- 2016

filename <- "gear_diversity"


directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read data
	##########		
	dat <- read.table(file = paste(dir_scripts, "/", filename, ".csv", sep = ""), header = T, sep = ",")
	
	#convert 0 to NA
	###############
	dat[, -2][dat[,-2]== 0] <- NA
	
	#convert wide form to long dataframe form
	#########################################
	dat <- melt(dat, id = c("region", "year"))
	
	#calculate diversity index using Hannon-index approach
	######################################################
	
	dat$loop <- paste(dat$region, dat$year, sep = "_")
	
		am <- NULL
		for (aa in unique(dat$loop)){
			
			al <- which(dat$loop == aa)
			
		if (length(al > 0)){
			
			ald <- dat[al,]
			
		if (length(ald > 0)){
			
			ald$pi <- ifelse(!is.na(ald$value), ald$value/sum(ald$value, na.rm = T), NA)
			ald$pi_2 <- ifelse(!is.na(ald$pi), ald$pi^2, NA)
			ald$log_pi <- ifelse(!is.na(ald$value), log(ald$pi), NA)
			ald$pi_log_pi <- ifelse(!is.na(ald$pi), ald$pi*ald$log_pi, NA)
			
			diversity_index <- -1*(sum(ald$pi_log_pi, na.rm = T))
			
			alk <- data.frame(region = unique(ald$region), year = unique(ald$year), diversity_index = diversity_index)
			
			am <- rbind(am,alk)
			
		}
		}
		}
	


	#assign subregion code, e.g. regiion "Denpasar" = 1
	###################################################
	am$rgn_id <- ifelse(am$region == "Denpasar", 1, ifelse(am$region == "Badung", 2, ifelse(am$region == "Tabanan", 3, ifelse(am$region == "Jembrana", 4, 
				 ifelse(am$region == "Buleleng", 5, ifelse(am$region == "Karangasem", 6, ifelse(am$region == "Klungkung", 7, 8)))))))
				 
		
	#normalized resilience score between 0-1
	#using equation: zi = xi - min(x)/(max(x) - min (x)) 
	####################################################
		
	am$resilience_score <- ifelse(is.na(am$diversity_index), NA, (am$diversity_index - min(am$diversity_index, na.rm = T))/(max(am$diversity_index, na.rm = T) - min(am$diversity_index, na.rm = T)))
	
	am <- am%>%select(rgn_id, year, resilience_score)%>%arrange(rgn_id, year)
	
	## save data layer
	##################
	
	write_csv(am, paste(directory, "prep/", toupper(goal), "/","g", "_",goal, "_", region, year, ".csv", sep = ""))
	write_csv(am, paste(directory, "region2017/layers/", "g", "_", goal, "_", region, year, ".csv", sep = ""))
	
	
