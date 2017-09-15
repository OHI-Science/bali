# load packages
library(dplyr)
library(tidyr) # install.packages('tidyr')
library(readr) # install.packages('readr')
library(stringr) # install.packages('stringr')

# set directory
dir_cp <- '~/github/bali/prep/CP'
setwd(dir_cp)

dir_layers <- '~/github/bali/prep/cp'

# read in raw data file
cp_raw <- read_csv(file.path(dir_cp,'cp_abrasion.csv'))


## save in layers folder
d <- read_csv2(file.path(dir_cp, 'cp_abrasion.csv')) %>%
  dplyr::rename(year = years)
write_csv(d, '~/github/bali/region2017/layers/cp_abrasion_bali.csv')
# and now register it in layers.csv!
