# Libraries and import---------------------------------------------------------------

library(readr)
library(tidyverse)
library(gridExtra)
library(lubridate)
library(readxl)
library(ggthemes)

Sys.setlocale("LC_ALL", "uk")
# Create destination folders
if (!dir.exists("./data")) dir.create("./data")
if (!dir.exists("./plots")) dir.create("./plots")

# file_url <- "https://raw.githubusercontent.com/pabrodez/eras_data/master/eras_srft_16_nov.csv"
# download.file(file_url, destfile = "./data/eras_srft.csv", method = "auto")

# if not downloaded:
# save as .csv collated sheet, name file as eras_srft and move manually to /data
# remember to remove unpopulated rows
na_list <- c(
  0, "0", "", "NA", "na", "n/a", "<NA>", "not known/not recorded",
  "not measured", "Not Measured", "none measured", "None Measured", "not done", "Not Done",
  "n/d", "Not Recorded", "not recorded", "u/k", "unknown"
)
# .csv encoding is WINDOWS-1252. Change to UTF-8
data_path <- list.files("./data", pattern = "\\.csv", full.names = TRUE, ignore.case = TRUE)[[1]]
df_srft <- read.csv(data_path, strip.white = TRUE, stringsAsFactors = FALSE, nrows = 196)
df_srft[, sapply(df_srft, is.character)] <- sapply(df_srft[, sapply(df_srft, is.character)], 
                                                   iconv, from = "WINDOWS-1252", to = "UTF-8")

# Data prep ---------------------------------------------------------------

# lower caps and trim ws
df_srft[] <- lapply(df_srft, tolower)
df_srft[] <- lapply(df_srft, trimws)

# group columns per timing
vars_pre <- c("troponin", "hb", "creatinine", "albumin", "tidal_vol", "chest_physio", "surgery_school", "date_school", 
              "school_satisfaction", "app_down", "website", "base_activity", "current_activity",
              "activity_type", "anaemia_treat", "anaesthesia", "area_surgery", "surgery", "surgeon", 
              "medical_hist", "smoke_hist", "smoke_cess", "asa_grade", "frailty")
vars_2 <- c("inc_spiro", "t_brushes", "m_washes", "iv_disc", "drinking", "tolerate_15l",
            "oral_diet", "mobilised", "resp_supp", "elev_head")
vars_7 <- c("in_hospital", "location_7", "infec_24", "infec_7", "gastro", "pulm_supp_24", "pulm_supp_24", "pulm_pharm", "cardio_24", "troponin", "creatinine_base", "creatinine_high", "rrt", "renal_24", "neuro", "wound", "haemato", "pain",
            "mobility", "why_inpatient_7", "comment_7", "date_15", "inpatient_15", "comment_15", "discharge_date", "discharge_dest", "complication")
vars_adm <- c("age", "gender", "gm_eras_ele", "eras", "pqip_consent", "discharge_loc", "surgery_duration",
              "admission_date", "surgery_date", "discharge_date", "hospital_stay", "readmit_30")

# visualize missing data before replacing miss values with NA
plotmiss <- function(dataFrame, na_values) {
  tempDf <- as.data.frame(lapply(dataFrame, function(x) ifelse(x %in% na_values, 0, 1)))
  tempDf <- tempDf[, order(colSums(tempDf))]
  tempData <- expand.grid(list(x = 1:nrow(tempDf), y = colnames(tempDf)))
  tempData$v <- as.vector(as.matrix(tempDf))
  tempData <- data.frame(x = unlist(tempData$x), y = unlist(tempData$y), v = unlist(tempData$v))
  ggplot(tempData) + geom_tile(aes(x=x, y=y, fill=factor(v))) +
    scale_fill_manual(values=c("white", "black"), name="Not measured\n1=No, 0=Yes") +
    theme_light() + ylab("") + xlab("Rows of data set") + ggtitle("")
  
}

not_rec_list <- c("not known/not recorded",
                  "not measured", "Not Measured", "none measured", "None Measured", "not done", "Not Done",
                  "n/d", "Not Recorded", "not recorded", "u/k", "unknown")

plotmiss(df_srft[, vars_pre], na_values = not_rec_list)
# ggsave(filename = "nr_pre.png", path = "./plots", device = "png", units = "cm", height = 25, width = 20, dpi = 700)

plotmiss(df_srft[, vars_2], na_values = not_rec_list)
# ggsave(filename = "nr_2.png", path = "./plots", device = "png", units = "cm", height = 25, width = 20, dpi = 700)

plotmiss(df_srft[, vars_7], na_values = not_rec_list)
# ggsave(filename = "nr_7.png", path = "./plots", device = "png", units = "cm", height = 25, width = 20, dpi = 700)

plotmiss(df_srft[, vars_adm], na_values = not_rec_list)
# ggsave(filename = "nr_adm.png", path = "./plots", device = "png", units = "cm", height = 25, width = 20, dpi = 700)

# Pure NA values
na_pure <- c(NA, 0, "0", "", "NA", "na", "n/a", "<NA>")
plotmiss(df_srft[, vars_pre], na_values = na_pure)
# ggsave(filename = "na_pre.png", path = "./plots", device = "png", units = "cm", height = 25, width = 20, dpi = 700)

plotmiss(df_srft[, vars_2], na_values = na_pure)
# ggsave(filename = "na_2.png", path = "./plots", device = "png", units = "cm", height = 25, width = 20, dpi = 700)

plotmiss(df_srft[, vars_7], na_values = na_pure)
# ggsave(filename = "na_7.png", path = "./plots", device = "png", units = "cm", height = 25, width = 20, dpi = 700)

plotmiss(df_srft[, vars_adm], na_values = na_pure)
# ggsave(filename = "na_adm.png", path = "./plots", device = "png", units = "cm", height = 25, width = 20, dpi = 700)

# replace not recorded and missing values with NA
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

df_srft <- rename(df_srft, comment_pre = comment)

# remove cancelled patients
df_srft <- df_srft[df_srft$comment_pre != "cancelled" | is.na(df_srft$comment_pre), ]

# Set fields types
df_srft$age <- as.numeric(df_srft$age)

# dates and times
# df_srft$discharge_date <- as.numeric(df_srft$discharge_date)
# df_srft$discharge_date <- as.Date(df_srft$discharge_date, origin = "1899-12-30")
df_srft$discharge_date <- as.Date(df_srft$discharge_date, format = "%d/%m/%Y")

# df_srft$admission_date <- as.numeric(df_srft$admission_date)
# df_srft$admission_date <- as.Date(df_srft$admission_date, origin = "1899-12-30")
df_srft$admission_date <- as.Date(df_srft$admission_date, format = "%d/%m/%Y")

# df_srft$surgery_date <- as.numeric(df_srft$surgery_date)
# df_srft$surgery_date <- as.Date(df_srft$surgery_date, origin = "1899-12-30")
df_srft$surgery_date <- as.Date(df_srft$surgery_date, format = "%d/%m/%Y")

# df_srft$date_15 <- as.numeric(df_srft$date_15)
# df_srft[df_srft$date_15 == 0, ]$date_15 <- NA
df_srft$date_15 <- as.Date(df_srft$date_15, format = "%d/%m/%Y")

df_srft$date_school <- as.Date(df_srft$date_school, format = "%d/%m/%Y")

# df_srft[grep("hour[s]$", df_srft$surgery_duration), ]$surgery_duration <- NA
df_srft$surgery_duration <- lubridate::hms(df_srft$surgery_duration)
df_srft$surgery_duration <- lubridate::time_length(df_srft$surgery_duration, unit = "minutes")

# days
df_srft$hospital_stay <- as.numeric(df_srft$hospital_stay)
df_srft$hospital_stay <- ifelse(df_srft$hospital_stay < 0, NA, df_srft$hospital_stay)

# check year
# ifelse doesn't return dates as numeric. dplyr::if_else does the trick. 
# true and false args must be same type, that's why as.Date(NA)

df_srft[] <- lapply(df_srft, function(x) {
  if (class(x) == "Date") {
    dplyr::if_else(lubridate::year(x) < 2018, as.Date(NA), x)
  }
  else {
    (x)
  }
})

# biomarkers
# df_srft$hb <- sub(",", ".", df_srft$hb, fixed = TRUE) 
df_srft$hb <- as.numeric(df_srft$hb)

# df_srft$creatinine <- sub(",", ".", df_srft$creatinine, fixed = TRUE)
df_srft$creatinine <- as.numeric(df_srft$creatinine)

# df_srft$albumin <- sub(",", ".", df_srft$albumin, fixed = TRUE)
df_srft$albumin <- as.numeric(df_srft$albumin)

# df_srft$creatinine_base <- sub(",", ".", df_srft$creatinine_base, fixed = TRUE)
df_srft$creatinine_base <- as.numeric(df_srft$creatinine_base)

# df_srft$creatinine_high <- sub(",", ".", df_srft$creatinine_high, fixed = TRUE)
df_srft$creatinine_high <- as.numeric(df_srft$creatinine_high)

# df_srft$creatinine_high <- sub(",", ".", df_srft$creatinine_high, fixed = TRUE)
df_srft$creatinine_high <- as.numeric(df_srft$creatinine_high)

# df_srft$troponin <- sub(",", ".", df_srft$troponin, fixed = TRUE)
df_srft$troponin <- sub("<", "-", df_srft$troponin)
df_srft$troponin <- as.numeric(df_srft$troponin)

# df_srft$troponin_7 <- sub(",", ".", df_srft$troponin_7, fixed = TRUE)
df_srft$troponin_7 <- sub("<", "-", df_srft$troponin_7, fixed = TRUE)
df_srft$troponin_7 <- as.numeric(df_srft$troponin_7)

# other fields
df_srft$discharge_loc <- sub("critical care (level 2)", "ccu (level 2)", df_srft$discharge_loc, fixed = TRUE)
df_srft$discharge_loc <- sub("ccu (level 2 and 3)", "ccu (level 2/3)", df_srft$discharge_loc, fixed = TRUE)
df_srft$discharge_loc <- str_replace(df_srft$discharge_loc, "^hdu$", "ccu")

# df_srft$current_activity <- sapply(strsplit(df_srft$current_activity, ","), `[`, 1)

df_srft$anaesthesia <- sapply(strsplit(df_srft$anaesthesia, ",\\s"), `[`, 1)

df_srft$surgeon <- gsub("/", ",", df_srft$surgeon, fixed = TRUE)
df_srft[df_srft$surgeon == "bilal khaffaf",]$surgeon <- "bilal alkhaffaf"
df_srft[df_srft$surgeon == "chaparala",]$surgeon <- "ramakrishna chaparala"
df_srft[df_srft$surgeon == "demonic slade, gordon lawrence carlson",]$surgeon <- "dominic slade, gordon lawrence carlson"

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
df_srft[df_srft$surgery == "percutaneous nephrolithotomy",]$surgery <- "percutaneous nephrolithotomy (including cystoscopy
and retrograde catheterisation)"
df_srft[df_srft$surgery == "right adrenalectomy",]$surgery <- "adrenalectomy"
df_srft[df_srft$surgery == "right hemicolectomy (with ileostomy)",]$surgery <- "right hemicolectomy (with anastomosis /colostomy)"
df_srft[df_srft$surgery == "subtotal gastrectomy",]$surgery <- "gastrectomy (partial / total) with excision of
surrounding tissue"
df_srft[df_srft$surgery == "gastrectomy (total or partial) with excision of surrounding tissue",]$surgery <- "gastrectomy (partial / total) with excision of
surrounding tissue"
df_srft$surgery <- sub("\\\n", " ", df_srft$surgery)
df_srft[df_srft$surgery == "percutaneous nephrolithotomy (including cystoscopy\nand retrograde catheterisation)",]$surgery <- "percutaneous nephrolithotomy (including cystoscopy
and retrograde catheterisation)"
df_srft[df_srft$surgery == "right hemicolectomy (with anastamosis)",]$surgery <- "right hemicolectomy (with anastomosis /colostomy)"

df_srft$tidal_vol <- as.numeric(df_srft$tidal_vol)

df_srft$infec_24 <- sub("\\?", "", df_srft$infec_24)
# df_srft$infec_24 <- ifelse(!is.na(df_srft$infec_24) & df_srft$infec_24 == "patient had temperature >38c, patient currently on iv antibiotics", "patient currently on iv antibiotics, patient had temperature >38c", df_srft$infec_24)

df_srft$gastro <- ifelse(!is.na(df_srft$gastro) & df_srft$gastro == "experienced nausea, vomiting or distension, unable to tolerate enteral diet", "unable to tolerate enteral diet, experienced nausea, vomiting or distension", df_srft$gastro)


# Plots -------------------------------------------------------------------

# surgeries per week (starting on monday)
df_srft$week_start <- cut.Date(df_srft$surgery_date, breaks = "week")
table_weeks <- transform(table(df_srft$week_start))
table_weeks$Var1 <- as.Date(table_weeks$Var1, format = "%Y-%m-%d")

ggplot(data = table_weeks, aes(x = Var1, y = Freq)) + 
  geom_point(size = 0.5) +
  geom_line(aes(group = 1), colour = "#FF2700") +
  scale_y_continuous(limits = c(0, 20)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b") +
  labs(title = "Aggregated number of surgeries per week", 
       caption = "Starting on monday") +
  ylab("Count") + xlab("Week") +
  theme_fivethirtyeight()

# ggsave(filename = "surgey_week.png", path = "./plots", device = "png", units = "cm", height = 15, width = 30, dpi = 700)

# aggregated number of surgeries per day of the week
df_srft %>% 
  mutate(day_week = lubridate::wday(df_srft$surgery_date, label = TRUE)) %>% 
  ggplot(data = ., aes(x = day_week)) +
  geom_bar() +
  scale_y_continuous(limits = c(0, 50)) +
  labs(title = "Aggregated surgeries per\nday of the week",
       caption = paste("From", min(df_srft$surgery_date, na.rm = TRUE), "to", max(df_srft$surgery_date, na.rm = TRUE))) +
  ylab("Count") + xlab("Day of week") +
  theme_fivethirtyeight()

# ggsave(filename = "surgery_day.png", path = "./plots", device = "png", units = "cm", height = 9, width = 15, dpi = 700)

# same as above but proportions
df_srft %>%
  mutate(day_week = lubridate::wday(df_srft$surgery_date, label = TRUE)) %>%
  group_by(day_week) %>%
  summarise(n = n()) %>%
  mutate(rel_freq = n / sum(n)) %>%
  ggplot(data = ., aes(x = day_week, y = rel_freq)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 0.35)) +
  labs(
    title = "Rel. freq. surgeries per\nday of the week",
    caption = paste("From", min(df_srft$surgery_date, na.rm = TRUE), "to", max(df_srft$surgery_date, na.rm = TRUE))
  ) +
  theme_fivethirtyeight()

# ggsave(filename = "surgery_day_freq.png", path = "./plots", device = "png", units = "cm", height = 9, width = 15, dpi = 700)


# aggregated number of discharges per day of the week
df_srft %>%
  mutate(dis_day = lubridate::wday(df_srft$discharge_date, week_start = 1, label = TRUE)) %>%
  ggplot(data = ., aes(x = dis_day)) +
  geom_bar() +
  scale_y_continuous(limits = c(0, 30)) +
  labs(title = "Aggregated number of D/C per day of week",
       caption = paste("From", min(df_srft$discharge_date, na.rm = TRUE), "to", max(df_srft$discharge_date, na.rm = TRUE))) +
  theme_fivethirtyeight()

# ggsave(filename = "discharge_day.png", path = "./plots", device = "png", units = "cm", height = 10, width = 20, dpi = 700)

# aggregated readmissions grouped by month of discharge
# table(lubridate::month(df_srft[df_srft$readmit_30 == "yes", "discharge_date"], label = TRUE), useNA = "ifany")

df_srft %>% 
  filter(readmit_30 == "yes") %>% 
  select(discharge_date) %>% 
  mutate(month_dc = lubridate::month(discharge_date, label = TRUE)) %>% 
  ggplot(data = ., aes(x = month_dc)) +
  geom_bar() +
  labs(title = "Aggregated R/A grouped by month of D/C") +
  theme_fivethirtyeight()

# ggsave(filename = "readmit_month.png", path = "./plots", device = "png", units = "cm", height = 10, width = 20, dpi = 700)

# what proportion of patients were readmitted grouped by month of discharge?

df_srft %>%
  select(discharge_date, readmit_30) %>%
  mutate(dc_month = lubridate::month(discharge_date, label = TRUE)) %>%
  na.omit() %>%
  group_by(dc_month, readmit_30) %>%
  summarise(n = n()) %>%
  mutate(rel_freq = n / sum(n)) %>%
  ggplot(data = ., aes(x = dc_month, y = rel_freq, fill = readmit_30)) +
  geom_col() +
  labs(title = "Rel. freq. R/A grouped by month of D/C",
       fill = "Readmitted?") +
  theme_fivethirtyeight()

# ggsave(filename = "readmit_month_freq.png", path = "./plots", device = "png", units = "cm", height = 10, width = 20, dpi = 700)


# How many patients are compliant with at least 4 (out of 5) items of icough bundle?
bundle_yes <- c(">two", "once", "twice", "yes")

df_srft %>%
  select(inc_spiro, t_brushes, m_washes, oral_diet, mobilised) %>%
  sapply(., function(x) {
    if_else(x %in% bundle_yes, 1, 0)
  }) %>%
  as.data.frame(.) %>%
  mutate(compliant = rowSums(.)) %>%
  mutate(
    compliant = if_else(compliant >= 4, "yes", "no"),
    area_surgery = df_srft$area_surgery
  ) %>%
  ggplot(data = ., aes(x = compliant)) +
  stat_count() +
  labs(title = "Patients compliant with iCough bundle by area",
       subtitle = "Compliant with 4 in 5: mobilisation, diet, mouth wash, teeth brush or IS") +
  theme_fivethirtyeight() +
  facet_wrap(vars(area_surgery), ncol = 2)

# ggsave(filename = "icough_area.png", path = "./plots", device = "png", units = "cm", height = 25, width = 20, dpi = 700)

# what icough item patients are less compliant with by area
df_srft %>%
  select(inc_spiro, t_brushes, m_washes, oral_diet, mobilised) %>%
  sapply(., function(x) {
    if_else(x %in% bundle_yes, 1, 0)
  }) %>%
  as.data.frame(.) %>%
  mutate(compliant = rowSums(.)) %>%
  mutate(
    compliant = if_else(compliant >= 4, "yes", "no"),
    area_surgery = df_srft$area_surgery
  ) %>%
  rename(is = inc_spiro, tb = t_brushes, mob = mobilised, 
         mw = m_washes, diet = oral_diet) %>% 
  group_by(compliant, area_surgery) %>% 
  summarise_at(., .vars = 1:5, .funs = sum) %>% 
  gather(key, value, -compliant, -area_surgery) %>% 
  ggplot(data = .) + 
  geom_col(aes(x = key, y = value)) +
  facet_wrap(vars(area_surgery, compliant), scales = "free_x", ncol = 2) +
  labs(title = "icough items compliance by area") +
  theme_fivethirtyeight()

# ggsave(filename = "icough_items_area.png", path = "./plots", device = "png", units = "cm", height = 40, width = 25)

# Avg LOS per area (aggregate(x = df_srft$hospital_stay, by = list(df_srft$area_surgery), FUN = mean, na.rm = TRUE))
df_srft %>% 
  select(area_surgery, hospital_stay) %>%
  group_by(area_surgery) %>% 
  summarise(avg_los = mean(hospital_stay, na.rm = TRUE), 
            sdev = sd(hospital_stay, na.rm = TRUE)) %>%
  ggplot(data = .) + 
  geom_col(aes(x = area_surgery, y = avg_los)) +
  geom_errorbar(aes(x = area_surgery, ymin = avg_los - sdev, ymax = avg_los + sdev), 
                na.rm = TRUE,
                width=.08) +
  # scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 5)) +
  labs(title = "Avg. LOS per area", 
       caption = paste("Last D/C:", max(df_srft$discharge_date, na.rm = TRUE))) +
  theme_fivethirtyeight()

# Aggregated LOS
df_srft %>%
  select(area_surgery, hospital_stay) %>%
  ggplot(data = .) +
  stat_bin(aes(x = hospital_stay), binwidth = 1) +
  scale_x_continuous(breaks = seq(min(df_srft$hospital_stay, na.rm = TRUE), max(df_srft$hospital_stay, na.rm = TRUE), 3)) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  labs(
    title = "Aggregated LOS",
    caption = paste("Last D/C:", max(df_srft$discharge_date, na.rm = TRUE))
  ) +
  theme_fivethirtyeight()

# LOS distribution per area
df_srft %>%
  select(area_surgery, hospital_stay) %>%
  ggplot(data = .) + stat_bin(aes(x = hospital_stay), binwidth = 1) +
  scale_x_continuous(breaks = seq(min(df_srft$hospital_stay, na.rm = TRUE), max(df_srft$hospital_stay, na.rm = TRUE), 2)) +
  labs(
    title = "LOS per area",
    caption = paste("Last D/C:", max(df_srft$discharge_date, na.rm = TRUE))
  ) +
  facet_wrap(vars(area_surgery), ncol = 2) +
  theme_fivethirtyeight()

# What proportion of patients who were still in hospital on day 7
# had any complication by week?
compl_yes <- c("abdominal leak", "chest", "surgical site infection", "empirical", "urine")
df_srft %>% 
  select(infec_7, week_start) %>% 
  filter(!is.na(infec_7)) %>%
  mutate(complication = if_else(infec_7 %in% compl_yes, 1, 0),
         week_start = as.Date(week_start, format = "%Y-%m-%d")) %>%
  group_by(week_start) %>% 
  summarise(n_pts = n(), freq_compl = sum(complication)/n(), n_compl = sum(complication)) %>% 
  ggplot(data = .,aes(x = week_start, y = freq_compl)) +
  geom_line(aes(group = 1), colour = "#FF2700") +
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b") +
  geom_point(size = 0.8) +
  labs(title = "[0, 1] prop. of pts. who were in hosp. at day 7 and\nhad any type of complication") +
  geom_text(aes(label = n_pts), size = 3, vjust = 0, nudge_y = 0.05) +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.1))

# Top 5 surgeons by number of surgeries
surgeons_df <- transform(table(unlist(strsplit(df_srft$surgeon, ",\\s"))))

surgeons_df %>% 
  arrange(., desc(Freq)) %>% 
  filter(between(row_number(), 1, 5)) %>% 
  mutate(Var1 = factor(.$Var1,levels = .$Var1[order(.$Freq)])) %>% 
  ggplot(data = ., aes(x = Var1, y = Freq)) + 
  geom_col() +
  scale_y_continuous(breaks = seq(min(surgeons_df$Freq, na.rm = TRUE), max(surgeons_df$Freq, na.rm = TRUE), 2)) +
  labs(title = "Top 5 surgeons by n. of surgeries") +
  theme_fivethirtyeight() +
  coord_flip()

# functions to plot numeric and categorical columns 
df_num <- df_srft[, sapply(df_srft, is.numeric)]
df_char <- df_srft[, sapply(df_srft, is.character)]  
# columns like comment, surgery or surgeon shouldn't be plotted with second func
plotNum <- function(datain, ii) {
  plots_list <- list()
  for (i in ii) {
    plot <- ggplot(data = data.frame(x = datain[[i]]), aes(x = x)) + 
      stat_bin(na.rm = TRUE) +
      theme_fivethirtyeight() +
      labs(subtitle = colnames(datain)[i],
           caption = paste("NAs: ", sum(is.na(datain[[i]])),"/", nrow(datain)))
    plots_list <- c(plots_list, list(plot))
  }
  
  do.call("grid.arrange", c(plots_list, ncol = 2))
}

plotCat <- function(datain, ii) {
  plots_list <- list()
  for (i in ii) {
    plot <- ggplot(data = data.frame(x = datain[[i]]), aes(x = x)) + 
      geom_bar(na.rm = TRUE, position = "dodge") +
      theme_fivethirtyeight() +
      labs(subtitle = colnames(datain)[i]) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    plots_list <- c(plots_list, list(plot))
  }
  
  do.call("grid.arrange", c(plots_list, ncol = 2))
}

# ERAS+ Dashboard measures for Haelo
# What proportion of pts. attended Surgery School prior to admission?
df_srft %>% 
  select(chest_physio, week_start) %>%
  mutate(chest_physio = if_else(chest_physio %in% c("nurse", "surgery school", "doctor"), 1, 0)) %>% 
  group_by(week_start) %>% 
  summarise(school_prop = sum(chest_physio) / n()) %>% 
  ggplot(data = ., aes(x = as.Date(week_start, format = "%Y-%m-%d"), y = school_prop)) +
  geom_line() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") +
  theme_fivethirtyeight()
# Proportion of colorectal pts. with PPC within 7 days
# Proportion of pts. compliant with icough bundle
# Prop pts teeth brushed twice
# Prop pts mobilised
# Prop pts used IS
# Prop pts mouth washed twice
# Prop pts oral diet

# https://stackoverflow.com/questions/32398427/r-split-a-character-string-on-the-second-underscore
# vector with all anaemia treatments 
# anae_treat_vec <- unlist(strsplit(df_srft$anaemia_treat, ",\\s"))
# also:
# df_srft$anaemia_treat2 <- vector(mode = "character", length = nrow(df_srft))
# df_srft$anaemia_treat2 <- sapply(strsplit(df_srft$anaemia_treat, ",\\s"), `[`, 2)
# df_srft$anaemia_treat <- sapply(strsplit(df_srft$anaemia_treat, ",\\s"), `[`, 1)
# 
# df_srft %>% select("anaemia_treat", "anaemia_treat2") %>% gather()
