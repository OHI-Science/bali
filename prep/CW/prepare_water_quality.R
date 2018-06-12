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
	
	if (names(dat[i]) == "do"){
		
		
		names(dat1) <- c("rgn_id", "year", "mg/l")
		
		# save data layer
		write_csv(dat1, paste(directory, "prep/", toupper(goal), "/", goal, "_", names(dat[i]), "_", region, year, ".csv", sep = ""))
		write_csv(dat1, paste(directory, "region2017/layers/", goal, "_", names(dat[i]), "_", region, year, ".csv", sep = ""))
		
		
	}else{
		
	if (names(dat[i]) == "turbidity"){
		
		#unit of turbidity is TBU
		#########################
		names(dat1) <- c("rgn_id", "year", "tbu")
		
		# save data layer
		write_csv(dat1, paste(directory, "prep/", toupper(goal), "/", "po", "_", names(dat[i]), "_", region, year, ".csv", sep = ""))
		write_csv(dat1, paste(directory, "region2017/layers/", "po", "_", names(dat[i]), "_", region, year, ".csv", sep = ""))
		
	}else{
	if (names(dat[i]) == "coliform"){
		
		#unit of coliform is cell/100ml
		################################
		names(dat1) <- c("rgn_id", "year", "cell/100ml")
		
		# save data layer
		write_csv(dat1, paste(directory, "prep/", toupper(goal), "/", "po", "_", names(dat[i]), "_", region, year, ".csv", sep = ""))
		write_csv(dat1, paste(directory, "region2017/layers/", "po", "_", names(dat[i]), "_", region, year, ".csv", sep = ""))
		
		
		
	}else{
		
			
		names(dat1) <- c("rgn_id", "year", "mg/l")
		
		# save data layer
		write_csv(dat1, paste(directory, "prep/", toupper(goal), "/", "po", "_", names(dat[i]), "_", region, year, ".csv", sep = ""))
		write_csv(dat1, paste(directory, "region2017/layers/", "po", "_", names(dat[i]), "_", region, year, ".csv", sep = ""))
	}
	}
	}
	
	
}









stop()

#convert wide data to long format
#################################
dat_long <- melt(dat, id = c("years"))

#convert coloum name
####################
names(dat_long) <- c("year", "rgn_id", "meter")

#convert rgn_id to number
#########################
dat_long$rgn_id <- gsub("X", "", dat_long$rgn_id)

#take number of region
######################
dat_long$rgn_id <- as.numeric(substr(dat_long$rgn_id, 1, 1))

#calculate mean of depth in a region when there are more than a value in a year
###############################################################################
dat_long <- setNames(aggregate(dat_long$meter ~ dat_long$rgn_id + dat_long$year, FUN = mean), c("rgn_id", "year", "meter"))

#order data based on rgn_id and year
####################################
dat_long <- dat_long[order(dat_long$rgn_id, dat_long$year),]

## save data layer
write_csv(dat_long, paste(directory, "prep/", toupper(goal), "/", goal, "_water_clarity_", region, year, ".csv", sep = ""))
write_csv(dat_long, paste(directory, "region2017/layers/", goal, "_water_clarity_", region, year, ".csv", sep = ""))

