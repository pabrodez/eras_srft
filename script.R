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
df_srft <-  read_excel("./data/srft.xls", sheet = 6, n_max = 300, range = cell_cols("E:BR")),
                        col_types = TRUE, col_types = "text", trim_ws = TRUE)

# To lower caps
df_srft[] <- lapply(df_srft, tolower)

# Remove rows with empty values in all columns
rem_row <- function(x) {
        x[rowSums(is.na(x)) != ncol(x), ]
}

# Remove columns with empty values in all rows
rem_col <- function(x) {
        x[, colSums(is.na(x)) != nrow(x)]
}

df_srft <- rem_row(df_srft)
rem_col <- rem_col(df_srft)








