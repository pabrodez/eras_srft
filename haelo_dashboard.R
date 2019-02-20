####################################################################################
## Purpose 1: Clean and format data for use in haelo Dashboard
## Purpose 2: Read and format baseline colorectal data
## Purpose 3: Read oesophagectomy data and merge with oesophagectomy patients in eras dataset
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
# 1.1 Read data ---------------------------------------------------------------
data_path <- list.files("./data", pattern = "\\.csv", full.names = TRUE, ignore.case = TRUE)[[1]]  ## save "collated" sheet as a single .csv file and move manually to folder "data"
df_srft <- read.csv(data_path, strip.white = TRUE, stringsAsFactors = FALSE, nrow = 310)           ## specify number of rows to read as excel tends to generate blank rows, slowing this step      
df_srft[, sapply(df_srft, is.character)] <- sapply(df_srft[, sapply(df_srft, is.character)],
                                                   iconv, from = "WINDOWS-1252", to = "UTF-8")

# 1.2 Data cleaning -----------------------------------------------------------
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

# 1.3 Transform --------------------------------------------------------------
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
    readmit_30, surgery_date, discharge_date, discharge_my
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

# 1.4 Haelo measures ----------------------------------------------------------------
# 1.4.1 Patients that attended Surgery School
count_surgery <- function(data, time_group) {
  dplyr::select(data, area_surgery, surgery_school, chest_physio, week_start, month_surgery) %>%
    group_by(!! time_group) %>%
    summarise(
      n = n(),
      school_n = sum(
        surgery_school %in% c("yes - hospital session", "yes -  online") |
          chest_physio %in% c("nurse", "doctor", "surgery school")
      )
    )
}

# 1.4.2 Number of readmissions within 30 days by month of discharge
df_haelo %>%
  select(discharge_my, area_surgery, readmit_30) %>%
  group_by(discharge_my) %>%
  summarise(
    n_patients = n(), ## number of pts discharged that month
    readmit_na = sum(is.na(readmit_30)), ## number of patients for whom 30 days after discharge haven't passed
    number_readmit = sum(readmit_30 == "yes", na.rm = TRUE)
  )

# 1.4.3 Number of patients with Postoperative Pulmonary Complication by week of surgery
count_ppc <- function(data, time_group) {
  dplyr::select(data, week_start, infec_7, pulm_supp_7, area_surgery, month_surgery) %>%
    group_by(!! time_group) %>%
    summarise(
      n_patients = n(),
      patients_ppc = sum(infec_7 == "chest" |
                           pulm_supp_7 %in% c("moderate", "mild", "severe"), na.rm = TRUE)
    )
}

# 1.4.4 Average Length of Stay grouped by month of discharge
df_haelo %>% 
  select(discharge_my, hospital_stay, area_surgery) %>%
  group_by(discharge_my) %>%
  summarise(
    mean_los = mean(hospital_stay, na.rm = TRUE),
    n_patients = n()
  )

# 1.4.5 Satisfaction score
foo_satis <- data.frame(
  score = sample(1:5, 200, replace = TRUE),
  week_start = df_haelo$week_start[1:200]
) ## sample df to test on

foo_satis %>%
  group_by(week_start, score) %>%
  summarise(n = n()) %>%
  print.data.frame()

# 1.4.6 Number of patients compliant with iCough bundle
count_icough <- function(data, time_group) {
  dplyr::select(data, area_surgery, week_start, nas_columns, compliant, month_surgery) %>%
    filter(nas_columns != 5) %>% ## Remove pts without all 5 items recorded
    group_by(!! time_group) %>%
    summarise(
      n_patients = n(),
      n_compliant = sum(compliant == "yes", na.rm = TRUE)
    )
}

# 1.4.7 Number of patients that had their teeth brushed twice
count_tbrush <- function(data, time_group) {
  dplyr::select(data, area_surgery, week_start, t_brushes, nas_columns, month_surgery) %>%
    filter(nas_columns != 5) %>%
    group_by(!! time_group) %>%
    summarise(
      n_patients = n(),
      n_tbrushes = sum(t_brushes, na.rm = TRUE),
      n_nas = sum(is.na(t_brushes))
    )
}

# 1.4.8 Number of patients mobilised
count_mob <- function(data, time_group) {
  dplyr::select(data, area_surgery, week_start, mobilised, nas_columns, month_surgery) %>%
    filter(nas_columns != 5) %>%
    group_by(!! time_group) %>%
    summarise(
      n_patients = n(),
      n_mob = sum(mobilised, na.rm = TRUE),
      n_nas = sum(is.na(mobilised))
    )
}

# 1.4.9 Number of patients that used incentive spirometer
count_spiro <- function(data, time_group) {
  dplyr::select(data, area_surgery, week_start, inc_spiro, nas_columns, month_surgery) %>%
    filter(nas_columns != 5) %>%
    group_by(!! time_group) %>%
    summarise(
      n_patients = n(),
      n_is = sum(inc_spiro, na.rm = TRUE),
      n_nas = sum(is.na(inc_spiro))
    )
}

# 1.4.10 Number of patients that used mouth wash twice
count_mwash <- function(data, time_group) {
  dplyr::select(data, area_surgery, week_start, m_washes, nas_columns, month_surgery) %>%
    filter(nas_columns != 5) %>%
    group_by(!! time_group) %>%
    summarise(
      n_patients = n(),
      n_mwash = sum(m_washes, na.rm = TRUE),
      n_nas = sum(is.na(m_washes))
    )
}

# 1.4.11 Number of patients who re-started oral diet
count_diet <- function(data, time_group) {
  dplyr::select(data, area_surgery, week_start, oral_diet, nas_columns, month_surgery) %>%
    filter(nas_columns != 5) %>%
    group_by(!!time_group) %>%
    summarise(
      n_patients = n(),
      n_diet = sum(oral_diet, na.rm = TRUE),
      n_nas = sum(is.na(oral_diet))
    )
}

# 1.4.12 Function to conveniently get most of the measures above by area of surgery
print_measure <- function(data, measure_fun, area = "all", time = month_surgery) {
  time_group <- enquo(time)
  if (area == "all") {
    measure_fun(data = data, time = time_group)
  } else {
    df <- filter(data, area_surgery == area)
    measure_fun(data = df, time = time_group)
  }
}

print_measure(df_haelo, measure_fun = count_diet, time=week_start, area = "colorectal")  ## week_start or month_surgery

# 2 Read data -------------------------------------------------------------
df_colorectal <- read_xlsx("./data/colorectal_5_years.xlsx", sheet = 2,
                           col_types = c("text", "skip", "skip", "date", "date", "text", "text", "skip", 
                                         "numeric", "numeric", "numeric", "numeric", "text"),
                           trim_ws = TRUE)

# 2.2 Transform --------------------------------------------------------------------
colnames(df_colorectal) <- c("patient_number", "admission_date", "discharge_date", "opcs_code", "surg_name", "los",
                             "readm_30", "mort_30", "mort_90", "cc_stay")

df_colorectal[] <- lapply(df_colorectal, function(x)
  if (is.character(x)) {
    tolower(x)
  } else {
    x
  })

df_colorectal$admission_date <- as.Date(df_colorectal$admission_date, format = "%Y-%m-%d")
df_colorectal$discharge_date <- as.Date(df_colorectal$discharge_date, format = "%Y-%m-%d")
df_colorectal$month_adm <- as.Date(cut.Date(df_colorectal$admission_date, breaks = "months"))
df_colorectal$week_adm <- as.Date(cut.Date(df_colorectal$admission_date, breaks = "weeks"))
df_colorectal$week_dis <- as.Date(cut.Date(df_colorectal$discharge_date, breaks = "weeks"))

double_surg <- df_colorectal %>% ## Index of entries with same patient undergoing two surgeries on same day
  select(patient_number, admission_date) %>%
  {
    which(duplicated(.))
  }

df_colorectal$id <- with(df_colorectal, match(patient_number, unique(patient_number)))  ## add patient identifier

# 2.3  Merge df_srft and df_colorectal --------------------------------------------------------------------
colorectal_merged <- df_srft %>%
  filter(area_surgery == "colorectal" & (eras == "yes" | is.na(eras))) %>%  ## eras and colorectal pts
  select(admission_date, discharge_date,
         los = hospital_stay,
         readm_30 = readmit_30, surg_name = surgery
  ) %>% ## only common columns
  mutate(
    baseline = "no"
  ) %>%
  {
    bind_rows(., df_colorectal %>%
                select(admission_date, discharge_date, los, readm_30, surg_name) %>%
                slice(-double_surg) %>%  ## remove double surgeries 
                mutate(baseline = "yes", readm_30 = if_else(readm_30 == 1, "yes", "no")))
  } %>%
  as_tibble()

colorectal_merged <- mutate(colorectal_merged,
                            week_adm = as.Date(cut.Date(admission_date, "week")),
                            month_adm =  format.Date(admission_date, "%Y-%m"),
                            month_dis = format.Date(discharge_date, "%Y-%m")
)

# 2.4 Plot ----------------------------------------------------------------
# Merged: Number of patients by month
colorectal_merged %>% group_by(month_adm) %>% 
  summarise(n = n(), baseline = if_else(any(baseline == "yes"), "yes", "no")) %>% 
  {
    ggplot(., aes(x = .$month_adm, y = .$n, group = 1, colour = baseline)) +
      geom_line() +
      scale_y_continuous(breaks = seq(min(.$n), max(.$n), 3)) +
      scale_x_discrete(breaks = .$month_adm[seq(1, length(.$month_adm), 4)]) +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }

# Merged: Mean and median of length of stay grouped by month of discharge
colorectal_merged %>%
  select(month_dis, los, baseline) %>%
  group_by(month_dis) %>%
  summarise(
    median = median(los, na.rm = TRUE),
    mad = mad(los, na.rm = TRUE),
    mean = mean(los, na.rm = TRUE),
    sd = sd(los, na.rm = TRUE),
    count = n(),
    baseline = if_else(any(baseline == "yes"), "yes", "no")
  ) %>%
  ungroup() %>%
  gather(summary, value, -month_dis, -mad, -sd, -count, -baseline) %>%
  na.omit() %>% ## omit pts with LoS = NA
  {
    ggplot(.) +
      geom_line(aes(x = month_dis, y = value, colour = baseline, group = 1)) +
      scale_x_discrete(
        breaks = unique(.$month_dis)[seq(1, length(.$month_dis), 1)]
      ) +
      scale_y_continuous(
        limits = c(min(.$value), max(.$value)),
        breaks = seq(0, max(.$value), 5)
      ) +
      geom_segment(aes(x = "2018-03", xend = "2018-03", y = 20, yend = 15),
                   arrow = arrow(length = unit(0.03, "npc"))
      ) +
      geom_segment(aes(x = "2018-07", xend = "2018-07", y = 20, yend = 15),
                   arrow = arrow(length = unit(0.03, "npc"))
      ) +
      annotate("text", x = "2018-04", y = 21, label = "Gap") +
      facet_wrap(vars(summary), ncol = 1) +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }

# Merged: Box plot LoS by month
colorectal_merged %>%
  select(month_dis, los, baseline) %>%
  group_by(month_dis) %>%
  na.omit() %>%
  {
    ggplot(.) +
      geom_boxplot(aes(x = month_dis, y = los, colour = baseline)) +
      scale_x_discrete(
        breaks = unique(.$month_dis)[seq(1, length(.$month_dis), 3)]
      ) +
      scale_y_continuous(
        limits = c(0, 96),
        breaks = seq(min(.$los), max(.$los), 3)
      ) + ## change interactively if want to keep all observations
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }

# Merged: Histogram LoS by month
colorectal_merged %>%
  select(month_dis, los, baseline) %>%
  group_by(month_dis) %>%
  na.omit() %>%
  {
    ggplot(.) +
      geom_histogram(aes(x = los), binwidth = 1, colour = "grey") +
      scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(min(.$los), max(.$los), 3)
      ) +
      theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }

# 3. Read data ------------------------------------------------------------
df_oeso <- read_xlsx("./data/ppt_oesophagectomies.xlsx",
                     sheet = 1,
                     col_types = c("date", "text", "text", "text", "text", "text"),
                     trim_ws = TRUE,
                     na = "n/a"
)

colnames(df_oeso) <- c("surgery_date", "pneumonia", "oxigen_req", "ventilated", "abx", "note")
df_oeso$pneumonia <- sub("^([[:digit:]])\\.0", "\\1", df_oeso$pneumonia)
df_oeso$oxigen_req <- sub("^([[:digit:]])\\.0", "\\1", df_oeso$oxigen_req)
df_oeso$ventilated <- sub("^([[:digit:]])\\.0", "\\1", df_oeso$ventilated)
df_oeso$abx <- sub("^([[:digit:]])\\.0", "\\1", df_oeso$abx)

