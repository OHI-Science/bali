# load packages
library(dplyr)
library(tidyr) # install.packages('tidyr')
library(readr) # install.packages('readr')
library(stringr) # install.packages('stringr')


# set directory
dir_hab <- '~/github/bali/prep/hab'
setwd(dir_hab)


dir_layers <- '~/github/bali/prep/hab'

# read in raw data file
cp_raw <- read_csv(file.path(dir_hab,'hab_coral_reef.csv'))
cp_raw <- read_csv(file.path(dir_hab,'hab_mangrove.csv'))
cp_raw <- read_csv(file.path(dir_hab,'hab_seagrass.csv'))

## Hanggar: you'll need to save these in layers and register them in layers.csv.
## Remember to replace `;` with `,` and change the 'years' column to 'year'. See the CP prep script.
