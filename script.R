# load libraries
library(xlsx)
library(tidyverse)
library(lubridate)
library(data.table)
library(devtools)
library(makeR)
library(chron)
library(lattice)
library(grid)
library(gridExtra)
library(ggthemes)
library(viridis)

# Create destination folders
if (!dir.exists("./data")) dir.create("./data")
if (!dir.exists("./plots")) dir.create("./plots")
# Name file as srft.xlsx and move manually to /data

list.files("./data", pattern = "srft.xlsx")
ds_srft <-  read.xlsx("./data/srft.xls", sheetIndex = 6, startRow = 1, header = TRUE, 
                         colClasses = rep("character", 50), stringsAsFactors = FALSE)
