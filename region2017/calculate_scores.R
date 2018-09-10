## calculate_scores.R


## This script calculates OHI scores with the `ohicore` package.
## - configure_toolbox.r ensures your files are properly configured for `ohicore`.
## - The `ohicore` function CalculateAll() calculates OHI scores.

## set working directory for all OHI calculations
#setwd("~/github/bali/region2017/")

## run the configure_toolbox.r script to check configuration
source("configure_toolbox.R")

## calculate scenario scores
scores <- ohicore::CalculateAll(conf, layers)


#determine date-hour when running the script
#to be included in the output filename of scores
#datetime <- format(Sys.time(), "%d%b%Y_%X")

## save scores as scores.csv
write.csv(scores, paste("scores.csv", sep = ""), na='', row.names=FALSE)


# visualizations ---

 ## source script (to be incorporated into ohicore)
 source("plot_flower_local.R")

 PlotFlower(assessment_name = "Bali",
            dir_fig_save    = "~/github/bali/region2017/reports/figures/")
  
