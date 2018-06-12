###########################
#prepare water clarity data
###########################

library(reshape2)

goal <- "cw"
region <- "bali"
year <- 2016

filename <- "water_clarity_bpol.csv"

directory <- paste("~/github/", region, "/", sep = "")
dir_scripts <- paste(directory, "/prep/", toupper(goal) , "/", sep = "")
setwd(dir_scripts)

#read data
##########
dat <- read.csv(file = filename, header = T, sep = ",")

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

