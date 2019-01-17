library(data.table)

# You will need to change your working directory
setwd("/git/xx_01")

fileSources = file.path("code", list.files("code", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

#CreateFakeData()
d <- readRDS("data_raw/individual_level_data.RDS")
d 
