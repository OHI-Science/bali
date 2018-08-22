########################################################################
#prepare artisanal fishery catch
#two dataset: 1.artisanal_catch_bali_nusra_2000_2012_separated.csv
#2.artisanal_catch_bali_nusra_2007_2015_merged.csv
#calculate mean annual percentage of bali for each species
#within the first file to extract exact bali catch from the second file 
########################################################################

rm(list = ls(all = T))

library(tidyverse)
library(reshape2)

goal <- "ao"
region <- "bali"
year <- 2016

#data filename
##############
filename1 <- "artisanal_catch_bali_nusra_2000_2012_separated" 	#catch_separated
filename2 <- "artisanal_catch_bali_nusra_2007_2015_merged"		#catch_merge
filename3 <- "nb_artisanal_fishing_boat"						#effort



directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)


	#read first data
	#################		
	dat1 <- readr::read_delim(file = paste(filename1, ".csv", sep = ""), delim = ",")
	
		
		#calculate percentage Bali's catch for each species
		###################################################
		dat12 <- dat1%>%group_by(provinces, species)%>%summarize(catch = sum(ton, na.rm = T))%>%ungroup()
		
		dat13 <- dat12%>%group_by(species)%>%mutate(catch_percentage = catch/sum(catch, na.rm = T)*100)%>%ungroup()
		
		#select only Bali's catch with percentage higher than 0
		#######################################################
		dat13 <- dat13[dat13$provinces == "Bali",]
		dat13 <- dat13[dat13$catch_percentage > 0,]
		
		#The Kerapu species contain 3 species: kerapu karang, bebek & sunu, convert to single percentage value
		######################################################################################################
		dat14 <- dat1[dat1$species %in% c("Kerapu bebek", "Kerapu karang", "Kerapu sunu", "Kerapu balong", "Kerapu bebek", "Kerapu lumpur"),]
		
		dat15 <- dat14%>%group_by(provinces)%>%summarize(catch = sum(ton, na.rm = T))%>%ungroup()
		dat15 <- dat15%>%mutate(catch_percentage = catch/sum(catch, na.rm = T)*100)
		dat15 <- dat15%>%mutate(species = "Kerapu")%>%select(provinces,species, catch, catch_percentage)
		dat15 <- dat15[dat15$provinces == "Bali",]
		
		#combine with dat13
		###################
		dat13 <- bind_rows(dat13, dat15)
		
		dat13 <- dat13%>%select(species, catch_percentage)
		
		
				
	#read second data
	#################
	dat2 <- readr::read_delim(file = paste(filename2, ".csv", sep = ""), delim = ",")
	
	
		#convert to long format dataframe
		##################################
		dat2 <- melt(dat2, id = c("species"))
		
		#rename coloumn names
		#####################
		names(dat2) <- c("species", "year", "catch")
	
	
		#extract year, and only data > 2012 used
		########################################
		dat2$year <- as.numeric(substr(dat2$year, 3, 6))
		dat2 <- dat2[dat2$year > 2012,]
		
		
			
		#combine dat2 with catch percentage from dat13
		#to calculate only Bali's catch
		###############################################
		dat2 <- left_join(dat2, dat13)
			
		#select only species with catch percentage, no percentage means
		#no catch species in the Bali
		################################################################
		dat2 <- dat2[!is.na(dat2$catch_percentage),]	
		
		#calculate Bali's catch
		#######################
		dat2$catch <- dat2$catch*dat2$catch_percentage/100
		dat2 <- dat2%>%select(species, year, catch)%>%rename(species = species, year = year, ton = catch)
		
		
		
	#combine first data(dat1) & second data(dat2)
	#############################################
	dat1 <- dat1[dat1$provinces == "Bali",]
	
	dat1 <- dat1%>%select(species, year, ton)
	
		#merge all kerapu, and calculate annual catch
			
		dat1a <- dat1[dat1$species %in% c("Kerapu bebek", "Kerapu karang", "Kerapu sunu", "Kerapu balong", "Kerapu bebek", "Kerapu lumpur"),]
		dat1a <- dat1a%>%group_by(year)%>%summarize(ton = sum(ton, na.rm = T))%>%ungroup()
		dat1a <- dat1a%>%mutate(species = "Kerapu")%>%select(species, year, ton)%>%arrange(year)
		
	#remove kerapu species and commbine results of merge kerapu	
	dat1 <- dat1[!dat1$species %in% c("Kerapu bebek", "Kerapu karang", "Kerapu sunu", "Kerapu balong", "Kerapu bebek", "Kerapu lumpur"),]
	dat1 <- rbind(dat1, dat1a)	
	
	dat_all <- rbind(dat1, dat2)
	
	
	
	
	#temporal filling gap for each species, if any NA data
	#######################################################
	
		#gap filling - calculate intercept and slope
		############################################
		dat_no_na <- dat_all[!is.na(dat_all$ton),]
		dat_no_na <- dat_no_na%>%arrange(year)
		
		intercept_slope <- dat_no_na%>%group_by(species)%>%filter(length(ton) > 1)%>%do(mdl= lm(ton~year, data=.))%>%
					summarize(species = species, intercept = coef(mdl)[1], slope = coef(mdl)[2])%>%ungroup()
	
		intercept_slope$intercept[intercept_slope$intercept == 0] <- NA
		
		#merge dat_all with intercept_slope
		###############################
		dat_all <- merge(dat_all, intercept_slope, by.x = c("species"), all= T)
		dat_all <- dat_all%>%arrange(species, year)
		
		#fill missing value based on intercept and slope
		################################################
		dat_all$ton_new <- ifelse(!is.na(dat_all$intercept),(dat_all$slope*dat_all$year)+dat_all$intercept, NA)
		
		
			
		#replace with new result of catch
		#################################
		dat_all$ton[dat_all$ton == 0] <- NA
		
		dat_all$ton <- ifelse(is.na(dat_all$ton), dat_all$ton_new, dat_all$ton)
	
		dat_all <- dat_all%>%select(species, year, ton)%>%arrange(species, year)
		
		
		
		#remove data still have NA catch and lower than zero catch
		##########################################################
		dat_all <- dat_all[!is.na(dat_all$ton),]
		
		dat_all <- dat_all[dat_all$ton > 0,]
		
	
	
	#read third data (effort)
	#########################
	dat3 <- readr::read_delim(file = paste(filename3, ".csv", sep = ""), delim = ",")
	
		
		#convert to long format dataframe
		##################################
		dat3 <- melt(dat3, id = c("vessel_type", "region"))
		
		#extract year
		#############
		dat3$year <- as.numeric(substr(dat3$variable, 3, 6))
		
		#calculate annual number of vessel for each region
		###################################################
		dat3 <- dat3%>%group_by(region, year)%>%summarize(nb_vessel = sum(value, na.rm = T))%>%ungroup()
		
		#assign subregion code, e.g. region "Denpasar" = 1
		###################################################
		dat3$rgn_id <- ifelse(dat3$region == "denpasar", 1, ifelse(dat3$region == "badung", 2, ifelse(dat3$region == "tabanan", 3, ifelse(dat3$region == "jembrana", 4, 
					   ifelse(dat3$region == "buleleng", 5, ifelse(dat3$region == "karangasem", 6, ifelse(dat3$region == "klungkung", 7, 8)))))))
		
		dat3 <- dat3%>%select(rgn_id, year, nb_vessel)%>%arrange(rgn_id, year)
	
	
		#calculate number gears annually for all bali
		#############################################
		dat4 <- dat3%>%group_by(year)%>%summarize(effort = sum(nb_vessel, na.rm = T))%>%ungroup()
		
	
	#calculate MSY
	##############
	dat_all <- left_join(dat_all, dat4)
	
	dat_all <- as.tibble(dat_all)
	
		#calculate cpue and msy
		#######################
		dat_all$cpue <- dat_all$ton/dat_all$effort
		
		dat_msy <- dat_all%>%group_by(species)%>%do(mdl=lm(cpue ~ effort, data = .))%>%summarize(species = species, 
				   intercept = abs(mdl[[1]][[1]]), slope = abs(mdl[[1]][[2]]))%>%ungroup()
				   
		dat_msy$msy <- (dat_msy$intercept^2)/(4*dat_msy$slope)
		
		#write as file
		##############
		dat_msy <- dat_msy[rep(seq_len(nrow(dat_msy)), each = 8*(length(seq(2000, 2015, 1)))),]
		
		dat_msy <- dat_msy%>%group_by(species)%>%mutate(rgn_id = rep(seq(1,8,1), length(seq(2000, 2015, 1))))%>%ungroup()
		
		dat_msy <- dat_msy%>%group_by(species, rgn_id)%>%mutate(year = seq(2000,2015,1))%>%ungroup()
		
		
		dat_msy <- dat_msy%>%select(rgn_id, year, species, msy)
		
		
		#replace white space, "/" with ("_")
		dat_msy$species <- gsub("([[:punct:]])|\\s+","_",dat_msy$species)
		
		
	
			for (i in unique(dat_msy$species)){
				il <- which(dat_msy$species == i)
			if (length(il > 0)){
				ild <- dat_msy[il,]
			if (length(ild < 0)){
				write_csv(ild, paste(directory, "prep/", toupper(goal), "/", goal, "_", i, "_msy_", region, year, ".csv", sep = ""))
				write_csv(ild, paste(directory, "region2017/layers/", goal, "_", i,"_msy_", region, year, ".csv", sep = ""))
				
			}
			}
			}
			
			
		
	#Distribute annual catch to the subregion proportionally based on number gear (effort)
	######################################################################################
	
	dat_all <- dat_all%>%select(species, year, ton)
	
		#repeat each row to eight subregion
		###################################
		dat_all <- dat_all[rep(seq_len(nrow(dat_all)), each = 8),]
		dat_all <- dat_all%>%group_by(species, year)%>%mutate(rgn_id = seq(1,8,1))%>%ungroup()
		
		
	
		
							   
		dat3 <- dat3%>%group_by(year)%>%mutate(vessel_percentage = nb_vessel/sum(nb_vessel, na.rm = T)*100)%>%ungroup()
		
		dat3 <- dat3%>%select(rgn_id, year, vessel_percentage)
		
		dat4 <- left_join(dat_all, dat3)
		
		dat4$catch <- round(dat4$vessel_percentage*dat4$ton/100,3)
		
		dat4 <- dat4%>%select(rgn_id, species, year, catch)%>%arrange(species, rgn_id, year)
			
		dat4$species <- gsub("([[:punct:]])|\\s+","_",dat4$species)
			
			## save data layer
			##################
			
			for (j in unique(dat4$species)){
				jl <- which(dat4$species == j)
			if (length(jl > 0)){
				jld <- dat4[jl,]
			if (length(jld < 0)){
				
				jld <- jld%>%select(rgn_id, year, species, catch)
				
				write_csv(jld, paste(directory, "prep/", toupper(goal), "/", goal, "_", j, "_", region, year, ".csv", sep = ""))
				write_csv(jld, paste(directory, "region2017/layers/", goal, "_", j,"_", region, year, ".csv", sep = ""))
				
			}
			}
			}
	
