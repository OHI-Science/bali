###########################
#prepare water clarity data
###########################

library(reshape2)

goal <- "cw"
region <- "bali"
year <- 2016

filename <- "cw_water_quality.csv"

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

#save each variable as file in prep and layer folder
###################################################

for (i in 3:ncol(dat)){
	
	
	dat1 <- dat[, c(1,2, i)]
	
	
	#prefix filename of do will be cw, while the others will be po (pollution-pressure)
	###################################################################################
	
		names(dat1) <- c("rgn_id", "year", "mg/l")
		
		# save data layer
		write_csv(dat1, paste(directory, "prep/", toupper(goal), "/", goal, "_", names(dat[i]), "_", region, year, ".csv", sep = ""))
		write_csv(dat1, paste(directory, "region2017/layers/", goal, "_", names(dat[i]), "_", region, year, ".csv", sep = ""))
		
		
	}else{
		
			
		#normalized pressure score between 0-1
		#using equation: zi = xi - min(x)/(max(x) - min (x)) 
		####################################################
		
		dat1$pressure_score <- ifelse(is.na(dat1[,3]), NA, (dat1[,3] - min(dat1[,3], na.rm = T))/(max(dat1[,3], na.rm = T) - min(dat1[,3], na.rm = T)))
		
			
		dat1 <- dat1[,c("rgn_id", "year", "pressure_score")]
		
		# save data layer
		write_csv(dat1, paste(directory, "prep/", toupper(goal), "/", "po", "_", names(dat[i]), "_", region, year, ".csv", sep = ""))
		write_csv(dat1, paste(directory, "region2017/layers/", "po", "_", names(dat[i]), "_", region, year, ".csv", sep = ""))
			
	}
	}
	






