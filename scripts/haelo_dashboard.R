####################################################################################
## Purpose: Clean and format eras_srft.csv for use in haelo Dashboard
###################################################################################

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(survival)
library(survminer)
library(survMisc)
library(stringr)
library(ggthemes)
library(reshape2)
# 1 Read ---------------------------------------------------------------
data_path <- list.files("./data", pattern = "\\.csv", full.names = TRUE, ignore.case = TRUE)[[1]]  ## save "collated" sheet as a single .csv file and move manually to folder "data"
df_srft <- read.csv(data_path, strip.white = TRUE, stringsAsFactors = FALSE, nrow = 310)           ## specify number of rows to read as excel tends to generate blank rows, slowing this step      
df_srft[, sapply(df_srft, is.character)] <- sapply(df_srft[, sapply(df_srft, is.character)],
                                                   iconv, from = "WINDOWS-1252", to = "UTF-8")

# 2 Data cleaning -----------------------------------------------------------
df_srft[] <- lapply(df_srft, function(x)
  if (is.character(x) == TRUE) {
    tolower(x)
  } else {
    x
  })

df_srft[] <- lapply(df_srft, function(x)
  if (is.character(x) == TRUE) {
    trimws(x)
  } else {
    x
  })

# Replace not recorded and missing values with NAs (to be discussed)
na_list <- c(
  0, "0", "", "NA", "na", "n/a", "<NA>", "not known/not recorded", "n/r",
  "not measured", "Not Measured", "none measured", "None Measured", "not done", "Not Done",
  "n/d", "Not Recorded", "not recorded", "u/k", "unknown", "#ref!"
)
df_srft[] <- lapply(df_srft, function(x) ifelse(x %in% na_list, NA, x))

# Remove rows with empty values in all columns
remove_blankr <- function(x) {
  x[rowSums(is.na(x)) != ncol(x), ]
}

df_srft <- remove_blankr(df_srft)

# Remove columns with empty values in all rows
remove_blankc <- function(x) {
  x[, colSums(is.na(x)) != nrow(x)]
}

df_srft <- remove_blankc(df_srft)

# Set columns classes
df_srft$admission_date <- as.Date(df_srft$admission_date, format = "%d/%m/%Y")

df_srft$surgery_date <- as.Date(df_srft$surgery_date, format = "%d/%m/%Y")

df_srft$discharge_date <- as.Date(df_srft$discharge_date, format = "%d/%m/%Y")

df_srft$date_15 <- as.Date(df_srft$date_15, format = "%d/%m/%Y")

df_srft$date_school <- as.Date(df_srft$date_school, origin = "1899-12-30")

df_srft[] <- lapply(df_srft, function(x) {
  if (class(x) == "Date") {
    dplyr::if_else(lubridate::year(x) < 2018, as.Date(NA), x)
  }
  else {
    (x)
  }
})

df_srft$hospital_stay <- as.numeric(df_srft$hospital_stay)
df_srft$hospital_stay <- ifelse(df_srft$hospital_stay < 0, NA, df_srft$hospital_stay)

df_srft$hb <- as.numeric(df_srft$hb)
df_srft$creatinine <- as.numeric(df_srft$creatinine)
df_srft$albumin <- as.numeric(df_srft$albumin)
df_srft$creatinine_base <- as.numeric(df_srft$creatinine_base)
df_srft$creatinine_high <- as.numeric(df_srft$creatinine_high)
df_srft$tidal_vol <- as.numeric(df_srft$tidal_vol)

df_srft$discharge_loc <- sub("critical care (level 2)", "ccu (level 2)", df_srft$discharge_loc, fixed = TRUE)
df_srft$discharge_loc <- sub("ccu (level 2 and 3)", "ccu (level 2/3)", df_srft$discharge_loc, fixed = TRUE)
df_srft$discharge_loc <- str_replace(df_srft$discharge_loc, "^hdu$", "ccu")

df_srft$anaesthesia <- sapply(strsplit(df_srft$anaesthesia, ",\\s"), `[`, 1)

df_srft$surgeon <- gsub("/", ",", df_srft$surgeon, fixed = TRUE)
df_srft[df_srft$surgeon == "bilal khaffaf",]$surgeon <- "bilal alkhaffaf"

df_srft[df_srft$area_surgery == "urology",]$area_surgery <- "urology and endocrinology"
df_srft[df_srft$area_surgery == "nephrology",]$area_surgery <- "urology and endocrinology"
df_srft[df_srft$area_surgery == "endocrinology",]$area_surgery <- "urology and endocrinology"
df_srft[df_srft$area_surgery == "head and neck",]$area_surgery <- "other"

df_srft[df_srft$surgery == "adrenalectomy (unilateral)",]$surgery <- "adrenalectomy"
df_srft[df_srft$surgery == "anterior resection of rectum",]$surgery <- "anterior resection"
df_srft[df_srft$surgery == "cystoscopy, stents laparotomy, adhesiolysis, small bowel resection, formation of ileostomy, abdominal wall reconstruction",]$surgery <- "abdominal wall reconstruction"
df_srft[df_srft$surgery == "gastrectomy (partial / total) with excision of surrounding tissue",]$surgery <- "gastrectomy (total or partial) with excision of surrounding tissue"
df_srft[df_srft$surgery == "ileo-caecal resection (with anastamosis or ileostomy formation)",]$surgery <- "ileocaecal resection"
df_srft[df_srft$surgery == "laparoscopic left hemicolectomy",]$surgery <- "left hemicolectomy (with anastomosis /colostomy)"
df_srft[df_srft$surgery == "laparoscopic left nephrectomy +/- open",]$surgery <- "nephrectomy (non-transplant)"
df_srft[df_srft$surgery == "left hemicolectomy (with colostomy)",]$surgery <- "left hemicolectomy (with anastomosis /colostomy)"
df_srft[df_srft$surgery == "left radical laparoscopic nephrectomy",]$surgery <- "nephrectomy (non-transplant)"
df_srft[df_srft$surgery == "nephrectomy and excision of perirenal tissue",]$surgery <- "nephrectomy (non-transplant)"
df_srft[df_srft$surgery == "oesophagectomy",]$surgery <- "oesophagectomy (partial /total)/oesophagogastrectomy"
df_srft[df_srft$surgery == "oesophagectomy (total)/oesophagogastrectomy",]$surgery <- "oesophagectomy (partial /total)/oesophagogastrectomy"
df_srft[df_srft$surgery == "open gastrectomy",]$surgery <- "gastrectomy (partial / total) with excision of
surrounding tissue"
df_srft[df_srft$surgery == "laparoscopic right radical nephrectomy",]$surgery <- "nephrectomy (non-transplant)"
df_srft[df_srft$surgery == "open partial nephrectomy",]$surgery <- "nephrectomy (non-transplant)"
df_srft[df_srft$surgery == "open right adrenalectomy",]$surgery <- "adrenalectomy"
df_srft[df_srft$surgery == "open right radical nephrectomy with lymphadenectomy",]$surgery <- "nephrectomy (non-transplant)"
df_srft[df_srft$surgery == "partial gastrectomy (+/- excision of surrounding tissue)",]$surgery <- "gastrectomy (partial / total) with excision of
surrounding tissue"
df_srft[df_srft$surgery == "right adrenalectomy",]$surgery <- "adrenalectomy"
df_srft[df_srft$surgery == "right hemicolectomy (with ileostomy)",]$surgery <- "right hemicolectomy (with anastomosis /colostomy)"
df_srft[df_srft$surgery == "subtotal gastrectomy",]$surgery <- "gastrectomy (partial / total) with excision of
surrounding tissue"
df_srft[df_srft$surgery == "gastrectomy (total or partial) with excision of surrounding tissue",]$surgery <- "gastrectomy (partial / total) with excision of
surrounding tissue"
df_srft$surgery <- sub("\\\n", " ", df_srft$surgery)
df_srft[df_srft$surgery == "right hemicolectomy (with anastamosis)",]$surgery <- "right hemicolectomy (with anastomosis /colostomy)"

df_srft$tidal_vol <- as.numeric(df_srft$tidal_vol)

df_srft$gastro <- ifelse(!is.na(df_srft$gastro) & df_srft$gastro == "experienced nausea, vomiting or distension, unable to tolerate enteral diet", "unable to tolerate enteral diet, experienced nausea, vomiting or distension", df_srft$gastro)

# 3 Transform --------------------------------------------------------------
df_srft <- rename(df_srft, comment_pre = comment)  # Change col name and remove cancelled and non elegible patients
df_srft <- df_srft[(!df_srft$comment_pre %in% c("cancelled", "down's syndrome") | is.na(df_srft$comment_pre)) &
                     (df_srft$gm_eras_ele != "no" | is.na(df_srft$gm_eras_ele)), ]
df_srft$week_start <- cut.Date(df_srft$surgery_date, breaks = "week")  ## week of surgery
df_srft$month_surgery <- format(as.Date(df_srft$surgery_date), "%Y-%m")  ## month of surgery
df_srft$discharge_my <- format(as.Date(df_srft$discharge_date), "%Y-%m")  ## month of discharge

# Data frame with relevant columns for haelo dashboard
df_haelo <-
  df_srft %>%
  filter(eras == "yes" | is.na(eras)) %>%   ## only eras patients
  dplyr::select(
    inc_spiro, t_brushes, m_washes, oral_diet, mobilised,
    surgery_school, chest_physio, week_start, month_surgery, area_surgery, hospital_stay, infec_7, pulm_supp_7,
    readmit_30, surgery_date, admission_date, discharge_date, discharge_my, surgery
  ) %>%  ## select columns relevant for haelo dashboard
  mutate(nas_columns = rowSums(is.na(dplyr::select(., 1:5)))) %>%  ## this will allow to filter in iCough measures
  mutate(inc_spiro = if_else(inc_spiro %in% c(">two", "once", "twice"), 1, 0)) %>%
  mutate_at(c("t_brushes", "m_washes"), function(x) {
    if_else(x == "twice", 1, 0)
  }) %>%
  mutate_at(c("oral_diet", "mobilised"), function(x) {
    if_else(x == "yes", 1, 0)
  }) %>%
  mutate(
    compliance_n = rowSums(dplyr::select(., 1:5), na.rm = TRUE),
    compliant = if_else(compliance_n >= 4, "yes", "no")
  )

# 4 Haelo measures ----------------------------------------------------------------
# 1.4.1 Patients that attended Surgery School
count_surgery <- function(data, time_group) {
  dplyr::select(data, area_surgery, surgery_school, chest_physio, week_start, month_surgery) %>%
    group_by(!!time_group) %>%
    summarise(
      n_patients = n(),
      school_n = sum(
        surgery_school %in% c("yes - hospital session", "yes -  online") |
          chest_physio %in% c("nurse", "doctor", "surgery school")
      ),
      y = school_n / n_patients
    )
}

# 1.4.2 Number of readmissions within 30 days by month of discharge
count_readm <- function(data, time_group = discharge_my) {
  data %>%
  select(discharge_my, area_surgery, readmit_30) %>%
  filter(!is.na(!! time_group)) %>% 
  group_by(!! time_group) %>%
  summarise(
    n_patients = n(), ## number of pts discharged that month
    readmit_na = sum(is.na(readmit_30)), ## number of patients for whom 30 days after discharge haven't passed
    y = sum(readmit_30 == "yes", na.rm = TRUE)
  )
}

# 1.4.3 Number of patients with Postoperative Pulmonary Complication by week of surgery
count_ppc <- function(data, time_group = week_start) {
  dplyr::select(data, week_start, infec_7, pulm_supp_7, area_surgery, month_surgery) %>%
    group_by(!! time_group) %>%
    summarise(
      n_patients = n(),
      patients_ppc = sum(infec_7 == "chest" |
                           pulm_supp_7 %in% c("moderate", "mild", "severe"), na.rm = TRUE),
      y = patients_ppc / n_patients
    )
}

# 1.4.4 Average Length of Stay grouped by month of discharge
avg_los <- function(data, time_group = discharge_my) {
  data %>%
    select(discharge_my, hospital_stay, area_surgery) %>%
    filter(!is.na(!! time_group)) %>%
    group_by(!! time_group) %>%
    summarise(
      y = mean(hospital_stay, na.rm = TRUE),
      n_patients = n()
    )
}

# 1.4.5 Satisfaction score
foo_satis <- data.frame(
  score = sample(1:5, 200, replace = TRUE),
  week_start = df_haelo$week_start[1:200]
) ## sample df to test on

foo_satis %>%
  group_by(week_start, score) %>%
  summarise(n = n())

# 1.4.6 Number of patients compliant with iCough bundle
count_icough <- function(data, time_group = week_start) {
  dplyr::select(data, area_surgery, week_start, nas_columns, compliant, month_surgery) %>%
    filter(nas_columns != 5) %>% ## Remove pts without all 5 items recorded
    group_by(!! time_group) %>%
    summarise(
      n_patients = n(),
      n_compliant = sum(compliant == "yes", na.rm = TRUE),
      y = n_compliant / n_patients
    )
}

# 1.4.7 Number of patients that had their teeth brushed twice
count_tbrush <- function(data, time_group = week_start) {
  dplyr::select(data, area_surgery, week_start, t_brushes, nas_columns, month_surgery) %>%
    filter(nas_columns != 5) %>%
    group_by(!! time_group) %>%
    summarise(
      n_patients = n(),
      n_tbrushes = sum(t_brushes, na.rm = TRUE),
      n_nas = sum(is.na(t_brushes)),
      y = n_tbrushes / n_patients
    )
}

# 1.4.8 Number of patients mobilised
count_mob <- function(data, time_group = week_start) {
  dplyr::select(data, area_surgery, week_start, mobilised, nas_columns, month_surgery) %>%
    filter(nas_columns != 5) %>%
    group_by(!! time_group) %>%
    summarise(
      n_patients = n(),
      n_mob = sum(mobilised, na.rm = TRUE),
      n_nas = sum(is.na(mobilised)),
      y = n_mob / n_patients
    )
}

# 1.4.9 Number of patients that used incentive spirometer
count_spiro <- function(data, time_group = week_start) {
  dplyr::select(data, area_surgery, week_start, inc_spiro, nas_columns, month_surgery) %>%
    filter(nas_columns != 5) %>%
    group_by(!! time_group) %>%
    summarise(
      n_patients = n(),
      n_is = sum(inc_spiro, na.rm = TRUE),
      n_nas = sum(is.na(inc_spiro)),
      y = n_is / n_patients
    )
}

# 1.4.10 Number of patients that used mouth wash twice
count_mwash <- function(data, time_group = week_start) {
  dplyr::select(data, area_surgery, week_start, m_washes, nas_columns, month_surgery) %>%
    filter(nas_columns != 5) %>%
    group_by(!! time_group) %>%
    summarise(
      n_patients = n(),
      n_mwash = sum(m_washes, na.rm = TRUE),
      n_nas = sum(is.na(m_washes)),
      y = n_mwash / n_patients
    )
}

# 1.4.11 Number of patients who re-started oral diet
count_diet <- function(data, time_group = week_start) {
  dplyr::select(data, area_surgery, week_start, oral_diet, nas_columns, month_surgery) %>%
    filter(nas_columns != 5) %>%
    group_by(!!time_group) %>%
    summarise(
      n_patients = n(),
      n_diet = sum(oral_diet, na.rm = TRUE),
      n_nas = sum(is.na(oral_diet)),
      y = n_diet / n_patients
    )
}

# 1.4.12 Function to conveniently get most of the measures above by area of surgery
print_measure <- function(data, measure_fun, area = "all", time = week_start) {
  ## time = week_start or discharge_my
  group <- enquo(time)
  if (area == "all") {
    measure_fun(data = data, time_group = group)
  } else {
    df <- filter(data, area_surgery == area)
    measure_fun(data = df, time_group = group)
  }
}

