####################################################################################
## Purpose: Clean and format colorectal_5_years.xlsx
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

# Source haelo_dashboard.R ------------------------------------------------
source("./scripts/haelo_dashboard.R", echo = FALSE)

# 1 Read -------------------------------------------------------------
df_colorectal <- read_xlsx("./data/colorectal_5_years.xlsx", sheet = 2,
                           col_types = c("text", "skip", "skip", "date", "date", "text", "text", "skip", 
                                         "numeric", "numeric", "numeric", "numeric", "text"),
                           trim_ws = TRUE)

# 2 Clean/ Transform --------------------------------------------------------------------
colnames(df_colorectal) <- c("patient_number", "admission_date", "discharge_date", "opcs_code", "surg_name", "los",
                             "readm_30", "mort_30", "mort_90", "cc_stay")

df_colorectal <- purrr::modify_if(df_colorectal, is.character, ~ tolower(.x))

df_colorectal$admission_date <- as.Date(df_colorectal$admission_date, format = "%Y-%m-%d")
df_colorectal$discharge_date <- as.Date(df_colorectal$discharge_date, format = "%Y-%m-%d")
df_colorectal$month_adm <- as.Date(cut.Date(df_colorectal$admission_date, breaks = "months"))
df_colorectal$week_adm <- as.Date(cut.Date(df_colorectal$admission_date, breaks = "weeks"))
df_colorectal$week_dis <- as.Date(cut.Date(df_colorectal$discharge_date, breaks = "weeks"))

double_surg <- select(df_colorectal, patient_number, admission_date) %>% ## Index of entries with same patient undergoing two surgeries on same day
  {
    which(duplicated(.))
  }
  

df_colorectal$id <- with(df_colorectal, match(patient_number, unique(patient_number)))  ## add patient identifier

# 3  Merge df_haelo and df_colorectal --------------------------------------------------------------------
colorectal_merged <- df_haelo %>%
  filter(area_surgery == "colorectal") %>%
  select(admission_date, discharge_date,
         los = hospital_stay,
         readm_30 = readmit_30, surg_name = surgery
  ) %>% ## only common columns with baseline data
  mutate(
    baseline = "no"
  ) %>%
  {
    bind_rows(., df_colorectal %>%
                select(admission_date, discharge_date, los, readm_30, surg_name, -patient_number) %>%
                slice(-double_surg) %>%  ## remove double surgeries 
                mutate(baseline = "yes", readm_30 = if_else(readm_30 == 1, "yes", "no")))
  } %>%
  as_tibble()

colorectal_merged <- mutate(colorectal_merged,
                            week_adm = as.Date(cut.Date(admission_date, "week")),
                            month_adm =  format.Date(admission_date, "%Y-%m"),
                            month_dis = format.Date(discharge_date, "%Y-%m")
)

# 4 Plot ----------------------------------------------------------------
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
