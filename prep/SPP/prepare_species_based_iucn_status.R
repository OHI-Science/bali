######################
#prepare status of SPP
######################

rm(list = ls(all = T))

library(tidyverse)


goal <- "spp"
region <- "bali"
year <- 2016

#data input
###########
filename <- "species_iucn_status"

directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)

	#read data
	##########		
	dat <- readr::read_delim(file = paste(filename, ".csv", sep = ""), delim = ",")
	
	#score based on IUCN status
	#Not Evaluated (NE):NA; Data Deficient (DD):1;  Least Concern (LC):2; Near Threatened (NT):3;
	#Vulnerable(VU):4; Edangered (EN):5; Critically Endangered (CR):6; Extinct in the Wild (EW):7;
	#Extinct (EX):8.
	##############################################################################################
	dat$iucn_score <- ifelse(dat$iucn == "NE", NA, ifelse(dat$iucn == "DD", 1, ifelse(dat$iucn == "LC", 2, ifelse(dat$iucn == "NT", 3, 
					  ifelse(dat$iucn == "VU", 4, ifelse(dat$iucn == "EN", 5, ifelse(dat$iucn == "CR", 6, ifelse(dat$iucn == "EW", 7, 8))))))))
					  
	#assign subregion code, e.g. regiion "Denpasar" = 1
	###################################################
	dat$rgn_id <- ifelse(dat$region == "denpasar", 1, ifelse(dat$region == "badung", 2, ifelse(dat$region == "tabanan", 3, ifelse(dat$region == "jembrana", 4, 
				 ifelse(dat$region == "buleleng", 5, ifelse(dat$region == "karangasem", 6, ifelse(dat$region == "klungkung", 7, ifelse(dat$region == "gianyar", 8, 9))))))))
	
	#normalized iucn score between 0-1
	#using equation: zi = xi - min(x)/(max(x) - min (x)) 
	####################################################
		
	dat$iucn_score <- ifelse(is.na(dat$iucn_score), NA, (dat$iucn_score - min(dat$iucn_score, na.rm = T))/(max(dat$iucn_score, na.rm = T) - min(dat$iucn_score, na.rm = T)))
	
	#for bali region: iucn_score is distributed into eight subregion
	################################################################
	
	am <- NULL
	for (aa in unique(dat$region)){
		
		al <- which(dat$region == aa)
		
	if (length(al > 0)){
		
		ald <- dat[al,]
	
	if (length(ald > 0)){
		
		
		if (unique(ald$region == "bali")){
			
			bm <- NULL
			for (bb in 1:nrow(ald)){
				
				dat1 <- ald[bb,]
				
				dat1 <- dat1[rep(seq_len(nrow(dat1)), each = 8),]
				dat1$rgn_id <- seq(1,8,1)
				
				bm <- rbind(bm, dat1)
				
				
			}
			
			
			am <- rbind(am, bm)
			
			
			
		}else{
			
			
		am <- rbind(am, ald)
			
			
		}
		
		
		
	}
	}
	}
		
		
	#calculate mean iucn score for each region
	##########################################
	dat_new <- setNames(aggregate(am$iucn_score~am$rgn_id, FUN = mean ,na.rm = T), c("rgn_id", "iucn_score"))
	
	## save data layer
	##################
		write_csv(dat_new, paste(directory, "prep/", toupper(goal), "/", goal, "_iucn_score_", region, year, ".csv", sep = ""))
		write_csv(dat_new, paste(directory, "region2017/layers/", goal, "_iucn_score_", filename, "_", region, year, ".csv", sep = ""))
	
	




