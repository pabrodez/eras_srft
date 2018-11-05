# load libraries
library(readxlsx)
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
# Anonymised spreadsheet
# Name file as srft.xlsx and move manually to /data

list.files("./data", pattern = "eras_srft.xlsx")
ds_srft <-  read_excel("./data/srft.xls", sheet = 6, n_max = 300, range = cell_cols("E:BR")),
                        col_types = TRUE, col_types = "text", trim_ws = TRUE)

