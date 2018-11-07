setwd("v:/Desktop/R")

library(readxl)
library(tidyverse)
library(lubridate)

# Create destination folders
if (!dir.exists("./data")) dir.create("./data")
if (!dir.exists("./plots")) dir.create("./plots")

# Name file as eras_srft and move manually to /data
 
data_path <- list.files("./data", pattern = "eras_srft", full.names = TRUE, ignore.case = TRUE)[[1]]
df_srft <-  read_excel(data_path, sheet = 6, range = cell_limits(c(1, 5), c(300, 70)), col_types = "text", trim_ws = TRUE)
str(df_srft)

# lower caps
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
# dates and times
df_srft$discharge_date <- as.numeric(df_srft$discharge_date)
df_srft$discharge_date <- as.Date(df_srft$discharge_date, origin = "1899-12-30")

df_srft$admission_date <- as.numeric(df_srft$admission_date)
df_srft$admission_date <- as.Date(df_srft$admission_date, origin = "1899-12-30")

df_srft$surgery_date <- as.numeric(df_srft$surgery_date)
df_srft$surgery_date <- as.Date(df_srft$surgery_date, origin = "1899-12-30")

df_srft$discharge_date <- as.numeric(df_srft$discharge_date)
df_srft$discharge_date <- as.Date(df_srft$discharge_date, origin = "1899-12-30")

df_srft$date_15 <- as.numeric(df_srft$date_15)
df_srft$date_15 <- as.Date(df_srft$date_15, origin = "1899-12-30")

df_srft$surgery_duration <- lubridate::hms(df_srft[!grep("$hour", df_srft$surgery_duration, values = FALSE, ignore.case = TRUE), ]$surgery_duration)

# days
df_srft$hospital_stay <- as.numeric(df_srft$hospital_stay)

# strange dates and times
df_srft[lubridate::year(df_srft$admission_date) < 2017, ]$admission_date <- NA
df_srft[lubridate::year(df_srft$surgery_date) < 2017, ]$surgery_date <- NA
df_srft[lubridate::year(df_srft$discharge_date) < 2017, ]$discharge_date <- NA
df_srft[lubridate::year(df_srft$date_15) < 2017, ]$date_15 <- NA
df_srft[lubridate::year(df_srft$hospital_stay) < 0, ]$hospital_stay <- NA

# biomarkers
df_srft[df_srft$hb %in% c("not measured", "not done", "not recorded"), ]$hb <- NA
df_srft$hb <- as.numeric(df_srft$hb)

df_srft[df_srft$creatinine %in% c("not measured", "not done", "not recorded"), ]$creatinine <- NA
df_srft$creatinine <- as.numeric(df_srft$creatinine)

df_srft[df_srft$albumin %in% c("not measured", "not done", "not recorded"), ]$albumin <- NA
df_srft$albumin <- as.numeric(df_srft$albumin)

df_srft[df_srft$creatinine_base %in% c("not measured", "not done", "not recorded"), ]$creatinine_base <- NA
df_srft$creatinine_base <- as.numeric(df_srft$creatinine_base)

df_srft[df_srft$creatinine_high %in% c("not measured", "not done", "not recorded"), ]$creatinine_high <- NA
df_srft$creatinine_high <- as.numeric(df_srft$creatinine_high)
