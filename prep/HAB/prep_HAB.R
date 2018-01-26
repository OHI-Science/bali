## prep mangrove data

## set working directory to bali home
setwd("~/github/bali")


## load libraries
library(tidyverse) # install.package('tidyverse')

## read in data; read_csv2 uses `;` as separators
d <- readr::read_delim(file = 'prep/HAB/hab_coral_reef.csv',
                       delim = ";")

## look at the data
head(d)
unique(d$rgn_id)

## arrange data by rgn_id and year
d <- d %>%
  rename(year = years) %>%
  arrange(rgn_id, year)

## missing regions should be assigned to NA.
## to understand `complete()`, see: http://ohi-science.org/data-science-training/tidyr.html#complete
d <- d %>%
  complete(rgn_id = full_seq(x = c(rgn_id, 8), period = 1),
           year,
           fill = list(area_ha = NA))

## look at the data now
head(d)
unique(d$rgn_id)
summary(d)
View(d)

## add a habitat column to match the global `hab_mangrove_extent` layer
d <- d %>%
  mutate(habitat = "coral") %>%
  select(rgn_id, habitat, year, area_ha)

## convert hectares to square kilometers
d <- d %>%
  mutate(km2 = area_ha * 0.01) %>%
  select(rgn_id, habitat, year, km2)

## save data layer
write_csv(d, "prep/HAB/hab_coral_complete.csv")
write_csv(d, "region2017/layers/hab_coral_complete.csv")
