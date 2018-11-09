library(readr)
library(tidyverse)
library(lubridate)
library(stringr)

# Create destination folders
if (!dir.exists("./data")) dir.create("./data")
if (!dir.exists("./plots")) dir.create("./plots")

# Name file as eras_srft and move manually to /data
na_list <- c(
  0, "0", "", "NA", "na", "n/a", "not known/not recorded",
  "not measured", "Not Measured", "none measured", "None Measured", "not done", "Not Done",
  "n/d", "Not Recorded", "not recorded", "u/k", "unknown"
)

data_path <- list.files("./data", pattern = "eras_srft", full.names = TRUE, ignore.case = TRUE)[[1]]
df_srft <- read_csv(data_path, trim_ws = TRUE, na = na_list, n_max = 300, col_types = cols(.default = "c"))
# lower caps
df_srft[] <- lapply(df_srft, tolower)
# n/d is not removed in na argument. Try this maybe:
df_srft[] <- lapply(df_srft, function(x) ifelse(x %in% na_list, NA, x))

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
# df_srft$discharge_date <- as.numeric(df_srft$discharge_date)
# df_srft$discharge_date <- as.Date(df_srft$discharge_date, origin = "1899-12-30")
df_srft$discharge_date <- as.Date(df_srft$discharge_date, format = "%m/%d/%Y")

# df_srft$admission_date <- as.numeric(df_srft$admission_date)
# df_srft$admission_date <- as.Date(df_srft$admission_date, origin = "1899-12-30")
df_srft$admission_date <- as.Date(df_srft$admission_date, format = "%m/%d/%Y")

# df_srft$surgery_date <- as.numeric(df_srft$surgery_date)
# df_srft$surgery_date <- as.Date(df_srft$surgery_date, origin = "1899-12-30")
df_srft$surgery_date <- as.Date(df_srft$surgery_date, format = "%m/%d/%Y")

# df_srft$date_15 <- as.numeric(df_srft$date_15)
# df_srft[df_srft$date_15 == 0, ]$date_15 <- NA
df_srft$date_15 <- as.Date(df_srft$date_15, format = "%m/%d/%Y")

df_srft$date_school <- as.Date(df_srft$date_school, format = "%m/%d/%Y")

# df_srft[df_srft$surgery_duration == 0, ]$surgery_duration <- NA
df_srft[grep("hour[s]$", df_srft$surgery_duration), ]$surgery_duration <- NA
df_srft$surgery_duration <- lubridate::hms(df_srft$surgery_duration)
df_srft$surgery_duration <- lubridate::time_length(df_srft$surgery_duration, unit = "minutes")

# days
df_srft$hospital_stay <- as.numeric(df_srft$hospital_stay)
df_srft$hospital_stay <- ifelse(df_srft$hospital_stay < 0, NA, df_srft$hospital_stay)

# strange dates and times
# df_srft[lubridate::year(df_srft$admission_date) < 2017, ]$admission_date <- NA
# df_srft[lubridate::year(df_srft$surgery_date) < 2017, ]$surgery_date <- NA
# df_srft[lubridate::year(df_srft$discharge_date) < 2017, ]$discharge_date <- NA
# df_srft[lubridate::year(df_srft$date_15) < 2017, ]$date_15 <- NA

# biomarkers
df_srft$hb <- sub(",", ".", df_srft$hb, fixed = TRUE)
df_srft$hb <- as.numeric(df_srft$hb)

df_srft$creatinine <- sub(",", ".", df_srft$creatinine, fixed = TRUE)
df_srft$creatinine <- as.numeric(df_srft$creatinine)

df_srft$albumin <- sub(",", ".", df_srft$albumin, fixed = TRUE)
df_srft$albumin <- as.numeric(df_srft$albumin)

df_srft$creatinine_base <- sub(",", ".", df_srft$creatinine_base, fixed = TRUE)
df_srft$creatinine_base <- as.numeric(df_srft$creatinine_base)

df_srft$creatinine_high <- sub(",", ".", df_srft$creatinine_high, fixed = TRUE)
df_srft$creatinine_high <- as.numeric(df_srft$creatinine_high)

df_srft$creatinine_high <- sub(",", ".", df_srft$creatinine_high, fixed = TRUE)
df_srft$creatinine_high <- as.numeric(df_srft$creatinine_high)

df_srft$troponin <- sub(",", ".", df_srft$troponin, fixed = TRUE)
df_srft$troponin <- sub("<", "-", df_srft$troponin)
df_srft$troponin <- as.numeric(df_srft$troponin)

df_srft$troponin_7 <- sub(",", ".", df_srft$troponin_7, fixed = TRUE)
df_srft$troponin_7 <- sub("<", "-", df_srft$troponin_7, fixed = TRUE)
df_srft$troponin_7 <- as.numeric(df_srft$troponin_7)

# other fields
df_srft$discharge_loc <- sub("critical care (level 2)", "ccu (level 2)", df_srft$discharge_loc, fixed = TRUE)
df_srft$discharge_loc <- sub("ccu (level 2 and 3)", "ccu (level 2/3)", df_srft$discharge_loc, fixed = TRUE)
df_srft$discharge_loc <- str_replace(df_srft$discharge_loc, "^hdu$", "ccu")

df_srft$current_activity <- sapply(strsplit(df_srft$current_activity, ","), `[`, 1)

df_srft$anaesthesia <- sapply(strsplit(df_srft$anaesthesia, ",\\s"), `[`, 1)

df_srft$tidal_vol <- sub(",", ".", df_srft$tidal_vol, fixed = TRUE)

# vector with all anaemia treatments 
anae_treat_vec <- unlist(strsplit(df_srft$anaemia_treat, ",\\s"))
# also:
# df_srft$anaemia_treat2 <- vector(mode = "character", length = nrow(df_srft))
# df_srft$anaemia_treat2 <- sapply(strsplit(df_srft$anaemia_treat, ",\\s"), `[`, 2)
# df_srft$anaemia_treat <- sapply(strsplit(df_srft$anaemia_treat, ",\\s"), `[`, 1)
# 
# df_srft %>% select("anaemia_treat", "anaemia_treat2") %>% gather()

sapply(df_srft, unique)










