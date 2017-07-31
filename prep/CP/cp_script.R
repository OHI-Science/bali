# load packages
library(dplyr)
library(tidyr) # install.packages('tidyr')
library(readr) # install.packages('readr')
library(stringr) # install.packages('stringr')

# set directory
dir_cp <- '~/github/bali/prep/cp'
setwd(dir_cp)

dir_layers <- '~/github/bali/prep/Cp'

# read in raw data file
cp_raw <- read_csv(file.path(dir_cp,'abration.csv'))
