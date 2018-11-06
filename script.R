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

# Missing values
df_srft[df_srft == ""] <- NA
df_srft[df_srft == "n/a"] <- NA
df_srft[df_srft == "na"] <- NA
df_srft[df_srft == "not known/not recorded"] <- NA
df_srft[df_srft == "not done"] <- NA
df_srft[df_srft == "not recorded"] <- NA
df_srft[df_srft == "u/k"] <- NA
df_srft[df_srft == "unkown"] <- NA

# Remove rows with empty values in all columns
rem_row <- function(x) {
        x[rowSums(is.na(x)) != ncol(x), ]
}

# Remove columns with empty values in all rows
rem_col <- function(x) {
        x[, colSums(is.na(x)) != nrow(x)]
}

df_srft <- rem_row(df_srft)
df_srft <- rem_col(df_srft)

# Set fields types
df_srft$admission_date <- lubridate::dmy(df_srft$admission_date)
df_srft$surgery_date <- lubridate::dmy(df_srft$surgery_date)
df_srft$discharge_date <- lubridate::dmy(df_srft$discharge_date)
df_srft$date_15 <- lubridate::dmy(df_srft$date_15)
df_srft$date_15 <- lubridate::hms(df_srft[grep("$hour[s]", df_srft$date_15, values = FALSE, ignore.case = TRUE), ]$date_15)


















# NAs plot to check visually
plotNa <- function(dataFrame) {
        tempDf <- as.data.frame(ifelse(is.na(dataFrame), 0, 1))
        tempDf <- tempDf[, order(colSums(tempDf))]
        tempData <- expand.grid(list(x = 1:nrow(tempDf), y = colnames(tempDf)))
        tempData$v <- as.vector(as.matrix(tempDf))
        tempData <- data.frame(x = unlist(tempData$x), y = unlist(tempData$y), v = unlist(tempData$v))
        ggplot(tempData) + geom_tile(aes(x=x, y=y, fill=factor(v))) +
                scale_fill_manual(values=c("white", "black"), name="Missing value\n1=No, 0=Yes") +
                theme_light() + ylab("") + xlab("Rows of data set") + ggtitle("")
        
}

adults_nas <- plotNa(df_srft)









