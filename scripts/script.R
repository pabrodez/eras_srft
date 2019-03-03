# Libraries and import---------------------------------------------------------------

library(readr)
library(tidyverse)
library(stringr)
library(grid)
library(gridExtra)
library(lubridate)
library(readxl)
library(ggthemes)
library(survival)
library(survminer)
library(reshape2)
library(qualityTools)

Sys.setlocale("LC_ALL", "uk")
# Create destination folders
if (!dir.exists("./data")) dir.create("./data")
if (!dir.exists("./plots")) dir.create("./plots")

# file_url <- "https://raw.githubusercontent.com/pabrodez/eras_data/master/eras_srft.csv"
# download.file(file_url, destfile = "./data/eras_srft.csv", method = "auto")

# if not downloaded:
# save "collated" sheet as .csv, name file as eras_srft and move manually to /data
# remember to remove unpopulated rows
na_list <- c(
        0, "0", "", "NA", "na", "n/a", "<NA>", "not known/not recorded",
        "not measured", "Not Measured", "none measured", "None Measured", "not done", "Not Done",
        "n/d", "Not Recorded", "not recorded", "u/k", "unknown"
)
# .csv encoding is WINDOWS-1252. Change to UTF-8
data_path <- list.files("./data", pattern = "\\.csv", full.names = TRUE, ignore.case = TRUE)[[1]]
df_srft <- read.csv(data_path, strip.white = TRUE, stringsAsFactors = FALSE, nrows = 300)
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
              "medical_hist", "smoke_hist", "smoke_cess", "asa_grade", "frailty", "cancer")
vars_2 <- c("inc_spiro", "t_brushes", "m_washes", "iv_disc", "drinking", "tolerate_15l",
            "oral_diet", "mobilised", "resp_supp", "elev_head")
vars_7 <- c("in_hospital", "location_7", "infec_24", "infec_7", "gastro", "pulm_supp_24", "pulm_supp_24", "pulm_pharm", "cardio_24", "troponin", "creatinine_base", "creatinine_high", "rrt", "renal_24", "neuro", "wound", "haemato", "pain",
            "mobility", "why_inpatient_7", "comment_7", "date_15", "inpatient_15", "comment_15", "discharge_date", "discharge_dest", "complication")
vars_adm <- c("age", "gender", "gm_eras_ele", "eras", "pqip_consent", "discharge_loc", "surgery_duration",
              "admission_date", "surgery_date", "discharge_date", "hospital_stay", "readmit_30", "days_readmit", "reason_readmit")

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

not_rec_list <- c("not known/not recorded", "n/r",
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
df_srft[df_srft$surgery == "right hemicolectomy (with anastamosis)",]$surgery <- "right hemicolectomy (with anastomosis /colostomy)"

df_srft$tidal_vol <- as.numeric(df_srft$tidal_vol)

df_srft$infec_24 <- sub("\\?", "", df_srft$infec_24)
# df_srft$infec_24 <- ifelse(!is.na(df_srft$infec_24) & df_srft$infec_24 == "patient had temperature >38c, patient currently on iv antibiotics", "patient currently on iv antibiotics, patient had temperature >38c", df_srft$infec_24)

df_srft$gastro <- ifelse(!is.na(df_srft$gastro) & df_srft$gastro == "experienced nausea, vomiting or distension, unable to tolerate enteral diet", "unable to tolerate enteral diet, experienced nausea, vomiting or distension", df_srft$gastro)


# Plots (general) -------------------------------------------------------------------

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
        theme_fivethirtyeight() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.1))

# aggregated number of surgeries per day of the week
df_srft %>% 
        mutate(day_week = lubridate::wday(df_srft$surgery_date, label = TRUE, week_start = 1)) %>% 
        ggplot(data = ., aes(x = day_week)) +
        geom_bar() +
        scale_y_continuous(limits = c(0, 70)) +
        labs(title = "Aggregated surgeries per\nday of the week",
             caption = paste("From", min(df_srft$surgery_date, na.rm = TRUE), "to", max(df_srft$surgery_date, na.rm = TRUE))) +
        ylab("Count") + xlab("Day of week") +
        theme_fivethirtyeight()

# same as above but proportions
df_srft %>%
        mutate(day_week = lubridate::wday(df_srft$surgery_date, label = TRUE, week_start = 1)) %>%
        group_by(day_week) %>%
        summarise(n = n()) %>%
        mutate(rel_freq = n / sum(n)) %>%
        ggplot(data = ., aes(x = day_week, y = rel_freq)) +
        geom_col() +
        scale_y_continuous(limits = c(0, 0.3)) +
        labs(
                title = "Rel. freq. surgeries per\nday of the week",
                caption = paste("From", min(df_srft$surgery_date, na.rm = TRUE), "to", max(df_srft$surgery_date, na.rm = TRUE))
        ) +
        theme_fivethirtyeight()

# aggregated number of discharges per day of the week
df_srft %>%
        mutate(dis_day = lubridate::wday(df_srft$discharge_date, week_start = 1, label = TRUE)) %>%
        ggplot(data = ., aes(x = dis_day)) +
        geom_bar() +
        # scale_y_continuous(limits = c(0, 30)) +
        labs(title = "Aggregated number of D/C per day of week",
             caption = paste("From", min(df_srft$discharge_date, na.rm = TRUE), "to", max(df_srft$discharge_date, na.rm = TRUE))) +
        theme_fivethirtyeight()

# aggregated readmissions grouped by month of discharge
# table(lubridate::month(df_srft[df_srft$readmit_30 == "yes", "discharge_date"], label = TRUE), useNA = "ifany")
df_srft$monthyr_disc <- format(as.Date(df_srft$discharge_date), "%Y-%m")

df_srft %>%
        filter(readmit_30 == "yes") %>% 
        dplyr::select(discharge_date, area_surgery, monthyr_disc) %>%
        ggplot(data = ., aes(x = monthyr_disc)) +
        geom_bar() +
        theme_bw()

# what proportion of patients were readmitted grouped by month of discharge?
df_srft %>%
        dplyr::select(surgery_date, readmit_30, area_surgery, monthyr_disc) %>%
        na.omit() %>%
        group_by(monthyr_disc, readmit_30) %>%
        summarise(n = n()) %>%
        mutate(rel_freq = n / sum(n)) %>%
        ggplot(data = ., aes(x = monthyr_disc, y = rel_freq, fill = readmit_30)) +
        geom_col() +
        labs(
                fill = "Readmitted?",
                x = "", y= "") +
        theme_bw()

# How many patients are compliant with at least 4 (out of 5) items of icough bundle?
bundle_yes <- c(">two", "once", "twice", "yes")

df_srft %>%
        dplyr::select(inc_spiro, t_brushes, m_washes, oral_diet, mobilised) %>%
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

# what icough item patients are less compliant with by area
df_srft %>%
        dplyr::select(inc_spiro, t_brushes, m_washes, oral_diet, mobilised) %>%
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

# What proportion of patients who were still in hospital on day 7 had any infectious complication by week?
compl_yes <- c("abdominal leak", "chest", "surgical site infection", "empirical", "urine")
df_srft %>% 
        dplyr::select(infec_7, week_start) %>% 
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
        mutate(Var1 = factor(.$Var1, levels = .$Var1[order(.$Freq)])) %>% 
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

# Plots LOS ---------------------------------------------------------------
# Avg LOS per month of surgery
df_srft$monthyr_surg <- format(as.Date(df_srft$surgery_date), "%Y-%m")
df_srft %>%
        dplyr::select(monthyr_surg, hospital_stay, area_surgery) %>%
        filter(area_surgery == "urology and endocrinology") %>% 
        mutate(stay = hospital_stay) %>%
        na.omit() %>%
        group_by(monthyr_surg) %>%
        summarise(avg_los = mean(stay, na.rm = TRUE),
                  n = n()) %>%
        ggplot(data = ., aes(x = monthyr_surg, y = avg_los)) +
        geom_line(aes(group = 1), colour = "#FF2700") +
        geom_point(size = 0.2) +
        scale_y_continuous(limits = c(0, 20)) +
        geom_text(aes(label = n), size = 3, vjust = -0.5, nudge_y = 0.1) +
        ylab("Average") +
        xlab("Month of surgery") +
        # labs(
        #      caption = paste("Last D/C:", max(df_srft$discharge_date, na.rm = TRUE))) +
        theme_bw()

# Avg LOS per area (aggregate(x = df_srft$hospital_stay, by = list(df_srft$area_surgery), FUN = mean, na.rm = TRUE))
df_srft %>% 
        dplyr::select(area_surgery, hospital_stay) %>%
        group_by(area_surgery) %>% 
        summarise(avg_los = mean(hospital_stay, na.rm = TRUE), 
                  sdev = sd(hospital_stay, na.rm = TRUE)) %>%
        ggplot(data = .) + 
        geom_col(aes(x = area_surgery, y = avg_los)) +
        geom_errorbar(aes(x = area_surgery, ymin = avg_los - sdev, ymax = avg_los + sdev), 
                      na.rm = TRUE,
                      width = .08) +
        scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
        labs(title = "Mean LoS and SD per area", 
             caption = paste("Last D/C:", max(df_srft$discharge_date, na.rm = TRUE))) +
        theme_fivethirtyeight()

# Same as above but median absolute deviation
df_srft %>% 
        dplyr::select(area_surgery, hospital_stay) %>%
        group_by(area_surgery) %>% 
        summarise(med_los = median(hospital_stay, na.rm = TRUE), 
                  mad_los = mad(hospital_stay, na.rm = TRUE)) %>%
        ggplot(data = .) + 
        geom_col(aes(x = area_surgery, y = med_los)) +
        geom_errorbar(aes(x = area_surgery, ymin = med_los - mad_los, ymax = med_los + mad_los), 
                      na.rm = TRUE,
                      width = .08) +
        scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
        labs(title = "Median LoS and median absolute deviation per area", 
             caption = paste("Last D/C:", max(df_srft$discharge_date, na.rm = TRUE))) +
        theme_fivethirtyeight()

# LoS distribution
df_srft %>%
        dplyr::select(area_surgery, hospital_stay) %>%
        ggplot(data = .) +
        stat_bin(aes(x = hospital_stay), binwidth = 1) +
        scale_x_continuous(breaks = seq(min(df_srft$hospital_stay, na.rm = TRUE), max(df_srft$hospital_stay, na.rm = TRUE), 3)) +
        scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
        labs(
                title = "LoS distribution",
                caption = paste("Last D/C:", max(df_srft$discharge_date, na.rm = TRUE))
        ) +
        theme_fivethirtyeight()

# LOS distribution per area
los_back <- subset(df_srft, select = -area_surgery)

ggplot(data = df_srft, aes(x = hospital_stay, fill = area_surgery)) +
        geom_histogram(colour = "black", binwidth = 1) +
        geom_histogram(data = los_back, fill = "grey", alpha = .5, binwidth = 1) +
        scale_x_continuous(breaks = seq(min(df_srft$hospital_stay, na.rm = TRUE), max(df_srft$hospital_stay, na.rm = TRUE), 2)) +
        labs(
                title = "LOS per area",
                caption = paste("Last D/C:", max(df_srft$discharge_date, na.rm = TRUE))
        ) +
        facet_wrap(vars(area_surgery), ncol = 2, scales = "free_y") +
        theme_fivethirtyeight() +
        guides(fill = FALSE)
# geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust = -.5)

# LOS grouped by icough items complied with
df_srft %>% dplyr::select(inc_spiro, m_washes, t_brushes, oral_diet, mobilised, hospital_stay) %>% 
        mutate(inc_spiro = if_else(inc_spiro %in% bundle_yes, 1, 0),
               m_washes = if_else(m_washes == "twice", 1, 0),
               t_brushes = if_else(t_brushes == "twice", 1, 0),
               oral_diet = if_else(oral_diet == "yes", 1, 0),
               mobilised = if_else(mobilised == "yes", 1, 0)
        ) %>% 
        na.omit() %>% 
        mutate(compl_group = rowSums(dplyr::select(., 1:5))) %>%
        mutate_at(vars(compl_group), as.character) %>% 
        ggplot(., aes(y = hospital_stay, x = compl_group, colour = compl_group)) +
        scale_y_continuous(
                breaks = seq(min(df_srft$hospital_stay, na.rm = TRUE),
                             max(df_srft$hospital_stay, na.rm = TRUE), 3)) +
        geom_jitter(width = 0.25) +
        theme_fivethirtyeight() +
        labs(title = "Number of iCough items complied with and hospital stay\nwithin 24h after surgery",
             caption = paste("From", min(df_srft$surgery_date, na.rm = TRUE), "to", max(df_srft$surgery_date, na.rm = TRUE))) +
        theme(legend.position = "none")

# Same as above but area of surgery is given a colour
df_srft %>% dplyr::select(inc_spiro, m_washes, t_brushes, oral_diet, mobilised, hospital_stay, area_surgery) %>% 
        mutate(inc_spiro = if_else(inc_spiro %in% bundle_yes, 1, 0),
               m_washes = if_else(m_washes == "twice", 1, 0),
               t_brushes = if_else(t_brushes == "twice", 1, 0),
               oral_diet = if_else(oral_diet == "yes", 1, 0),
               mobilised = if_else(mobilised == "yes", 1, 0)
        ) %>% 
        na.omit() %>% 
        mutate(compl_group = rowSums(dplyr::select(., 1:5))) %>%
        mutate_at(vars(compl_group), as.character) %>% 
        ggplot(., aes(y = hospital_stay, x = compl_group, colour = area_surgery)) +
        scale_y_continuous(
                breaks = seq(min(df_srft$hospital_stay, na.rm = TRUE),
                             max(df_srft$hospital_stay, na.rm = TRUE), 3)) +
        geom_jitter(width = 0.25) +
        theme_fivethirtyeight() +
        labs(title = "Number of iCough items complied with and hospital stay\nwithin 24h after surgery",
             caption = paste("From", min(df_srft$surgery_date, na.rm = TRUE), "to", max(df_srft$surgery_date, na.rm = TRUE))) +
        theme(legend.position = "right",
              legend.direction = "vertical")

# Day of the week of surgery and LOS
# makes sense that complex surgeries are not usually placed on fridays as there is less medical resources available
# seems reasonable to think that complex cases, therefore with higher risk of morbidities, will require more LoS
# complex surgeries are obviously placed on days of the week from where there's a long uninterrupted care with high resources available
df_srft %>% 
        dplyr::select(surgery_date, 
                      hospital_stay) %>% 
        mutate(day_week = lubridate::wday(surgery_date, label = TRUE, week_start = 1)) %>% 
        na.omit() %>% 
        ggplot(., aes(x = day_week, y = hospital_stay, colour = day_week)) +
        scale_y_continuous(
                breaks = seq(min(df_srft$hospital_stay, na.rm = TRUE),
                             max(df_srft$hospital_stay, na.rm = TRUE), 3)) +
        geom_jitter(width = 0.25) +
        theme_fivethirtyeight() +
        theme(legend.position = "none")

# Infectious complication within 7 days and LOS
compl_yes <- c("abdominal leak", "chest", "surgical site infection", "empirical", "urine")
df_srft %>% 
        dplyr::select(infec_7, hospital_stay) %>% 
        na.omit() %>% 
        mutate(complication = if_else(infec_7 %in% compl_yes, "yes", "no")) %>%
        ggplot(., aes(x = complication, y = hospital_stay)) +
        scale_y_continuous(
                breaks = seq(min(df_srft$hospital_stay, na.rm = TRUE),
                             max(df_srft$hospital_stay, na.rm = TRUE), 3)) +
        geom_jitter(width = 0.25) +
        theme_fivethirtyeight() +
        theme(text = element_text(size = 25)) +
        coord_flip()

# POMS and LOS
df_srft %>%
        dplyr::select(
                pulm_supp_24, infec_24, renal_24, gastro, cardio_24, neuro,
                haemato, wound, pain, hospital_stay
        ) %>% 
        na.omit() %>%  ## there are pts with day 7 items completed and not d/c
        mutate_at(
                .vars = vars(-hospital_stay),
                .funs = function(x) 
                        if_else(x == "none of the above", 0, 1)
                
        ) %>%
        mutate(morbid = rowSums(dplyr::select(., 1:9))) %>%
        ggplot(., aes(x = as.factor(morbid), y = hospital_stay, colour = as.factor(morbid))) +
        geom_jitter(width = 0.25) +
        scale_y_continuous(breaks = seq(min(df_srft$hospital_stay, na.rm = TRUE),
                                        max(df_srft$hospital_stay, na.rm = TRUE), 3)) +
        theme_fivethirtyeight() +
        theme(legend.position = "none")

# ERAS+ Dashboard measures for Haelo --------------------------------------
measures_plots <- list()
# What proportion of pts. attended Surgery School prior to admission?
measures_plots <- c(measures_plots, list(
        df_srft %>%
                dplyr::select(chest_physio, week_start, area_surgery) %>%
                filter(area_surgery == "urology and endocrinology") %>% 
                mutate(chest_physio = if_else(chest_physio %in% c("nurse", "surgery school", "doctor"), 1, 0)) %>%
                group_by(week_start) %>%
                summarise(school_prop = sum(chest_physio) / n(),
                          n = n()) %>%
                ggplot(data = ., aes(x = as.Date(week_start, format = "%Y-%m-%d"), y = school_prop)) +
                geom_line(colour = "#FF2700") +
                geom_point(size = 1) +
                geom_line(colour = "#FF2700", group = 1) +
                geom_point(size = 1) +
                geom_text(aes(label = n), size = 3,nudge_x = 0, nudge_y = 0.08) +
                scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") +
                scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) +
                labs(
                        x="Week starting", y = "Proportion"
                ) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
))
# Proportion of pts. with PPC within 7 days
df_srft %>% dplyr::select(area_surgery, surgery_date, infec_7, pulm_supp_7, week_start) %>% 
        filter(area_surgery == "urology and endocrinology" & !is.na(infec_7)) %>% 
        mutate(infec_7 = if_else(infec_7 == "chest", 1, 0),
               pulm_supp_7 = if_else(pulm_supp_7 %in% c("mild", "moderate", "severe"), 1, 0)) %>% 
        mutate(ppc = if_else(infec_7 == 1 | pulm_supp_7 == 1, 1, 0)) %>% 
        group_by(week_start) %>%
        summarise(denominator = n(), 
                  numerator = sum(ppc),
                  prop = sum(ppc) / n(),
                  n=n()) %>% 
        ggplot(data = ., aes(x = as.Date(week_start, format = "%Y-%m-%d"), y = prop)) +
        geom_line(colour = "#FF2700") +
        geom_point(size = 1) +
        # geom_hline(aes(yintercept = median(prop)), linetype = 2) +
        geom_text(aes(label = n), size = 3,nudge_x = 0, nudge_y = 0.08) +
        scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") +
        scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) +
        labs(
                x="Week starting", y = "Proportion"
        ) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Proportion of pts. compliant with icough bundle
measures_plots <- c(measures_plots, list(
        df_srft %>%
                dplyr::select(inc_spiro, t_brushes, m_washes, oral_diet, mobilised, week_start, area_surgery) %>%
                filter(area_surgery == "urology and endocrinology") %>% 
                filter(rowSums(is.na(dplyr::select(., 1:5))) < 4) %>%  # omit recent surgeries
                mutate_at(
                        .vars = vars(1:5),
                        .funs = function(x) {
                                if_else(x %in% bundle_yes, 1, 0)
                        }
                ) %>%
                mutate(compliant = rowSums(dplyr::select(., 1:5))) %>%
                mutate(
                        compliant = if_else(compliant >= 4, "yes", "no")
                ) %>%
                group_by(week_start) %>%
                summarise(prop = sum(compliant == "yes") / n(),
                          n=n()) %>% 
                ggplot(data = ., aes(x = as.Date(week_start), y = prop)) +
                geom_line(colour = "#FF2700", group = 1) +
                geom_point(size = 1) +
                # geom_hline(aes(yintercept = median(prop)), linetype = 2) +
                geom_text(aes(label = n), size = 3,nudge_x = 0, nudge_y = 0.08) +
                scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") +
                scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) +
                labs(
                        x="Week starting", y = "Proportion"
                ) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
))

# Prop pts teeth brushed twice
measures_plots <- c(measures_plots, list(
        df_srft %>%
                dplyr::select(t_brushes, week_start, area_surgery) %>%
                filter(area_surgery == "urology and endocrinology") %>% 
                na.omit() %>%
                mutate(t_brushed = if_else(t_brushes == "twice", 1, 0)) %>%
                group_by(week_start) %>%
                summarise(freq = sum(t_brushed) / n(),
                          n = n()) %>%
                ggplot(data = ., aes(x = as.Date(week_start), y = freq)) +
                geom_line(colour = "#FF2700", group = 1) +
                geom_point(size = 1) +
                scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") +
                scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) +
                geom_text(aes(label = n), size = 3,nudge_x = 0, nudge_y = 0.08) +
                labs(
                        y = "Proportion",
                        x = "Week starting"
                ) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
))

# Prop pts mobilised
measures_plots <- c(measures_plots, list(
        df_srft %>%
                dplyr::select(mobilised, week_start, area_surgery) %>%
                filter(area_surgery == "urology and endocrinology") %>% 
                na.omit() %>%
                mutate(mobilised = if_else(mobilised == "yes", 1, 0)) %>%
                group_by(week_start) %>%
                summarise(freq = sum(mobilised) / n(), n = n()) %>%
                ggplot(data = ., aes(x = as.Date(week_start), y = freq)) +
                geom_line(colour = "#FF2700", group = 1) +
                geom_point(size = 1) +
                geom_text(aes(label = n), size = 3,nudge_x = 0, nudge_y = 0.08) +
                scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") +
                scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) +
                labs(
                        x = "Week starting", y = "Proportion"
                ) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
))

# Prop pts used IS
measures_plots <- c(measures_plots, list(
        df_srft %>%
                dplyr::select(inc_spiro, week_start, area_surgery) %>%
                filter(area_surgery == "urology and endocrinology") %>% 
                na.omit() %>%
                mutate(inc_spiro = if_else(inc_spiro %in% c("once", "twice", ">two"), 1, 0)) %>%
                group_by(week_start) %>%
                summarise(freq = sum(inc_spiro) / n(),
                          n = n()) %>%
                ggplot(data = ., aes(x = as.Date(week_start), y = freq)) +
                geom_line(colour = "#FF2700", group = 1) +
                geom_point(size = 1) +
                geom_text(aes(label = n), size = 3,nudge_x = 0, nudge_y = 0.08) +
                scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") +
                scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) +
                labs(
                        y = "Proportion", x = "Week starting"
                ) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
))

# Prop pts mouth washed twice
measures_plots <- c(measures_plots, list(
        df_srft %>%
                dplyr::select(m_washes, week_start, area_surgery) %>%
                filter(area_surgery == "urology and endocrinology") %>% 
                na.omit() %>%
                mutate(m_washed = if_else(m_washes == "twice", 1, 0)) %>%
                group_by(week_start) %>%
                summarise(freq = sum(m_washed) / n(),
                          n = n()) %>%
                ggplot(data = ., aes(x = as.Date(week_start), y = freq)) +
                geom_line(colour = "#FF2700", group = 1) +
                geom_point(size = 1) +
                geom_text(aes(label = n), size = 3,nudge_x = 0, nudge_y = 0.08) +
                scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") +
                scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) +
                labs(
                        x = "Week starting", y = "Proportion"
                ) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
))


# Prop pts oral diet
measures_plots <- c(measures_plots, list(
        df_srft %>%
                dplyr::select(oral_diet, week_start, area_surgery) %>%
                filter(area_surgery == "urology and endocrinology") %>% 
                na.omit() %>%
                mutate(oral_diet = if_else(oral_diet == "yes", 1, 0)) %>%
                group_by(week_start) %>%
                summarise(freq = sum(oral_diet) / n(),
                          n = n()) %>%
                ggplot(data = ., aes(x = as.Date(week_start), y = freq)) +
                geom_line(colour = "#FF2700", group = 1) +
                geom_point(size = 1) +
                geom_text(aes(label = n), size = 3,nudge_x = 0, nudge_y = 0.08) +
                scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") +
                scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) +
                labs(
                        x="Week starting", y = "Proportion"
                ) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
))

do.call("grid.arrange", c(measures_plots, ncol = 2, 
                          top = "Proportion of patients who", 
                          bottom = paste("From ", min(as.Date(df_srft$week_start)), "to ", max(as.Date(df_srft$week_start)))))


# extract most used words in medical history -------------------------------------
# https://www.gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf
med_hist_v <- df_srft[, "medical_hist"]

# med_hist_v <- unlist(strsplit(x = med_hist_v, split = ",\\s"))
# extract asa grade from medical history before asa grade was introduced as new column
asa_grades <- str_extract(string = med_hist_v, pattern = "asa\\s[:digit:]")

df_srft$asa_grade <- ifelse(is.na(df_srft$asa_grade), 
                            str_extract(asa_grades, pattern = "[:digit:]"), 
                            df_srft$asa_grade)

# https://stackoverflow.com/questions/32398427/r-split-a-character-string-on-the-second-underscore
# vector with all anaemia treatments 
# anae_treat_vec <- unlist(strsplit(df_srft$anaemia_treat, ",\\s"))
# also:
# df_srft$anaemia_treat2 <- vector(mode = "character", length = nrow(df_srft))
# df_srft$anaemia_treat2 <- sapply(strsplit(df_srft$anaemia_treat, ",\\s"), `[`, 2)
# df_srft$anaemia_treat <- sapply(strsplit(df_srft$anaemia_treat, ",\\s"), `[`, 1)
# 
# df_srft %>% select("anaemia_treat", "anaemia_treat2") %>% gather()


# trying time to event  ---------------------------------------------------
# test for normality of distribution of LOS as outcome measure using Shapiro-Wilk’s method 
ggplot(data = df_srft, aes(sample = hospital_stay)) + stat_qq() + stat_qq_line()  # normal dist
shapiro.test(df_srft$hospital_stay)
qqPlot(x = df_srft$hospital_stay, y = "weibull")

# data prep for survival
df_suv <- df_srft %>%
        dplyr::select(
                surgery_date, discharge_date,
                pulm_supp_24, infec_24, renal_24,
                gastro, cardio_24, neuro,
                haemato, wound, pain, area_surgery,
                inc_spiro, t_brushes, m_washes, oral_diet, mobilised
        ) %>%
        mutate_at(
                .vars = vars(13:17),
                .funs = function(x) {
                        if_else(x %in% bundle_yes, 1, 0)
                }
        ) %>%
        mutate(compliant = rowSums(dplyr::select(., 13:17))) %>%
        mutate(
                compliant = factor(if_else(compliant >= 4, "yes", "no"))
        ) %>%
        mutate(
                discharge = if_else(!is.na(discharge_date), 1, 0),
                time_event = if_else(is.na(discharge_date) == TRUE, Sys.Date() - surgery_date, discharge_date - surgery_date)
        ) %>%
        mutate_at(
                .vars = vars(3:11),
                .funs = function(x)
                        if_else(x == "none of the above" | is.na(x) == TRUE, 0, 1) ## pts d/c before 7 days are considered as no morb
        ) %>%
        mutate(poms = factor(if_else(rowSums(dplyr::select(., 3:11)) == 0, "no", "yes")))

los_km <- survfit(Surv(time = time_event, event = discharge) ~ 1, data = df_suv, type = "kaplan-meier")

# data.frame(time = los_km$time, n.risk = los_km$n.risk, n.event = los_km$n.event,
#            n.censor = los_km$n.censor, surv = los_km$surv)

ggsurvplot(los_km,
           conf.int = TRUE,
           risk.table = "nrisk_cumevents",
           legend = "none",
           surv.median.line = "hv",
           linetype = 1,
           palette = "lancet",
           break.x.by = 3,
           ggtheme = theme_fivethirtyeight()
)

# add poms yes no groups considering pts d/c before day 7  as not showing any morbidity
los_km_poms <- survfit(Surv(time = time_event, event = discharge) ~ poms, data = df_suv)
suvplot_poms_all <- ggsurvplot(los_km_poms,
                               conf.int = TRUE,
                               risk.table = FALSE,
                               surv.median.line = "hv",
                               linetype = 1,
                               palette = "lancet",
                               break.x.by = 3,
                               ggtheme = theme_fivethirtyeight()
)

# combined poms yes/no and general curves
ggsurvplot_combine(list(poms = los_km_poms, general = los_km),
                   conf.int = TRUE,
                   data = df_suv,
                   surv.median.line = "hv",
                   linetype = 1,
                   palette = "lancet",
                   break.x.by = 3,
                   ggtheme = theme_fivethirtyeight()
)
# Compare 2 curves for POMS: (i) consider all patients and those d/c before 7 days are considered not to have morb
# (ii) consider only pts where POMS was administered, this is, pts not d/c before day 7
# (i)
suvplot_poms_all
# (ii)
df_suv_7 <- df_srft %>%
        dplyr::select(
                surgery_date, discharge_date,
                pulm_supp_24, infec_24, renal_24,
                gastro, cardio_24, neuro,
                haemato, wound, pain
        ) %>%
        filter_at(vars(3:11), any_vars(!is.na(.))) %>%
        mutate(
                discharge = if_else(!is.na(discharge_date), 1, 0),
                time_event = if_else(is.na(discharge_date) == TRUE, Sys.Date() - surgery_date, discharge_date - surgery_date)
        ) %>%
        mutate_at(
                .vars = vars(3:11),
                .funs = function(x)
                        if_else(x == "none of the above", 0, 1)
        ) %>% 
        mutate(poms = if_else(rowSums(dplyr::select(., 3:11)) == 0, "no", "yes"))

los_km_poms_7 <- survfit(Surv(time = time_event, event = discharge) ~ poms, data = df_suv_7)

suvplot_poms_7 <- ggsurvplot(los_km_poms_7,
                             conf.int = TRUE,
                             risk.table = FALSE,
                             surv.median.line = "hv",
                             linetype = 1,
                             palette = "lancet",
                             break.x.by = 3,
                             ggtheme = theme_fivethirtyeight()
)

# In this Kaplan-Meier curve, comparing strata of patients who had a morbidity and who did not, patients who were discharged before day 7 are not considered to not have any morbidity according to POMS (whereas the previous curve did include patients discharged before day 7 by giving a zero score in POMS). This is: this curve only shows patients who were administered the POMS (see next plot). The POMS, in the data collection process, is only administered on day 7, and the time reference is the previous 24 hours. For example, \marginpar{Kaplan-Meier estimate: $$\hat{S}_i =\left( \frac{n_i-d_i}{n_i} \right) \hat{S}_{i-1}.$$}it might be the case that the patient could qualify as showing a morbidity according to POMS on day 3 or 5, and this is not recorded if the patients is discharged before the follow-up on day 7. In other words, we do not know if the patient had or not a morbidity if it is not recorded. It would be reasonable to assume that if that was the case, the patient would stay for 7 or more days as they would need extra treatment. However, [administering POMS on the very first days after surgery might confuse  prophylactic interventions with actual treatment for a morbidity](https://pqip.org.uk/FilesUploaded/Library/Postoperative/7.3%20complications%203%20Grocott%20-%20Postoperative%20morbidity%20survey%20validation.pdf). 

## Probability of staying in hospital over time grouped by compliance with iCough bundle
df_suv_compl <- df_srft %>%
        dplyr::select(inc_spiro, t_brushes, m_washes, oral_diet, mobilised, discharge_date, surgery_date) %>%
        mutate(
                inc_spiro = if_else(inc_spiro %in% c(">two", "twice", "once"), 1, 0), 
                t_brushes = if_else(t_brushes == "twice", 1, 0),
                m_washes = if_else(m_washes == "twice", 1, 0),
                oral_diet = if_else(oral_diet == "yes", 1, 0),
                mobilised = if_else(mobilised == "yes", 1, 0)
        ) %>%
        mutate(compliant = rowSums(dplyr::select(., 1:5))) %>%
        mutate(
                compliant = factor(if_else(compliant >= 4, "yes", "no"))
        ) %>%
        mutate(
                discharge = if_else(!is.na(discharge_date) == TRUE, 1, 0),
                time_event = if_else(is.na(discharge_date) == TRUE, Sys.Date() - surgery_date, discharge_date - surgery_date))

los_km_cough <- survfit(Surv(time = time_event, event = discharge) ~ compliant, data = df_suv)
ggsurvplot(los_km_cough,
           conf.int = TRUE,
           risk.table = FALSE,
           surv.median.line = "hv",
           linetype = 1,
           palette = "lancet",
           break.x.by = 3,
           ggtheme = theme_fivethirtyeight()
)

# KM grouped by compliant and poms (pts d/c before 7 days are considered as no morb)
survfit(Surv(time = time_event, event = discharge) ~ compliant + strata(poms), data = df_suv, type = "kaplan-meier") %>% 
        ggsurvplot(.,
                   conf.int = TRUE,
                   risk.table = FALSE,
                   surv.median.line = "hv",
                   linetype = 1,
                   palette = "lancet",
                   break.x.by = 3,
                   ggtheme = theme_fivethirtyeight()
        )
# Same as above but considering only patients who were administered POMS
compl_poms_7 <- df_srft %>%
        dplyr::select(
                surgery_date, discharge_date,
                pulm_supp_24, infec_24, renal_24,
                gastro, cardio_24, neuro,
                haemato, wound, pain, area_surgery,
                inc_spiro, t_brushes, m_washes, oral_diet, mobilised
        ) %>%
        filter_at(vars(3:11), any_vars(!is.na(.))) %>%
        mutate_at(
                .vars = vars(13:17),
                .funs = function(x) {
                        if_else(x %in% bundle_yes, 1, 0)
                }
        ) %>%
        mutate(compliant = rowSums(dplyr::select(., 13:17))) %>%
        mutate(
                compliant = factor(if_else(compliant >= 4, "yes", "no"))
        ) %>%
        mutate(
                discharge = if_else(!is.na(discharge_date), 1, 0),
                time_event = if_else(is.na(discharge_date) == TRUE, Sys.Date() - surgery_date, discharge_date - surgery_date)
        ) %>%
        mutate_at(
                .vars = vars(3:11),
                .funs = function(x)
                        if_else(x == "none of the above", 0, 1)
        ) %>%
        mutate(poms = factor(if_else(rowSums(dplyr::select(., 3:11)) == 0, "no", "yes"))) 

compl_poms_7_km <- survfit(Surv(time = time_event, event = discharge) ~ compliant + strata(poms), data = compl_poms_7, type = "kaplan-meier")


ggsurvplot(compl_poms_7_km,
           conf.int = TRUE,
           risk.table = FALSE,
           surv.median.line = "hv",
           linetype = 1,
           palette = "lancet",
           break.x.by = 3,
           ggtheme = theme_fivethirtyeight()
)





# weibull model
los_wb <- survreg(Surv(time = time_event, event = discharge) ~ 1, data = df_suv)
# compute quantiles of distribution function
predict(object = los_wb, type = "quantile", p = c(1 - 0.9, 1 - 0.75, 1 - 0.5, 1 - 0.25, 1 - 0.1), newdata = data.frame(1))
# Returns vector with time points in number of days at which the probability that 
# patients remain in hospital beyond it is 90, 75, 50, 25 and 10%
# Survival curve from weibull distribution model using quantiles computed from model
surv <- seq(0.99, 0.01, by = -0.01)
t <- predict(los_wb, type = "quantile", p = 1 - surv, newdata = data.frame(1))
srv_wb <- data.frame(time = t, surv = surv,
                     upper = NA, lower = NA, std.err = NA)

ggsurvplot_df(fit = srv_wb, surv.geom = geom_line)

# adding poms and icough bundle compliance
srv_wb_covs <- survreg(Surv(time = time_event, event = discharge) ~ poms + compliant, data = df_suv, dist = "weibull")
newdat <- expand.grid(
        poms = levels(df_suv$poms),
        compliant = levels(df_suv$compliant)
)
# compute survival function
t_newdat <- predict(srv_wb_covs, type = "quantile", p = 1- surv, newdata = newdat, se.fit = TRUE)
srv_wb_wide <- cbind(t_newdat, newdat)
surv_wbmod <-  melt(srv_wb_wide, id.vars = c("poms", "compliant"), value.name = "time", variable.name = "surv_id")
surv_wbmod$surv <- surv[as.numeric(surv_wbmod$surv_id)]
surv_wbmod[, c("upper", "lower", "std.err", "strata")] <- NA
ggsurvplot_df(surv_wbmod, linetype = "poms", color = "compliant", surv.geom = geom_line, legend.title = NULL, break.x.by = 3,
              ggtheme = theme_fivethirtyeight())

# compare distributions
compliantlev <- data.frame(compliant = levels(df_suv$compliant))
wbmod <- survreg(Surv(time_event, discharge) ~ compliant, data = df_suv, dist = "weibull")
wbt <- predict(wbmod, type = "quantile", p = 1- surv, newdata = compliantlev)
expmod <- survreg(Surv(time = time_event, event = discharge) ~ compliant, data = df_suv, dist = "exponential")
expt <- predict(expmod, type = "quantile", p = 1- surv, newdata = compliantlev)
logmod <- survreg(Surv(time = time_event, event = discharge) ~ compliant, data = df_suv, dist = "lognormal")
logt <- predict(logmod, type = "quantile", p = 1- surv, newdata = compliantlev)

dists_wide <- cbind(rbind(wbt, expt, logt), compliantlev, 
                    distribution = c(rep("weibull", 2), rep("exponential", 2), rep("lognormal", 2)))
dists_long <- melt(dists_wide, id.vars = c("compliant", "distribution"), variable.name = "surv_id", value.name = "time")
dists_long$surv <- surv[as.numeric(dists_long$surv_id)]
dists_long[, c("upper", "lower", "std.err", "strata")] <- NA
ggsurvplot_df(dists_long, surv.geom = geom_line, linetype = "compliant", color = "distribution")

# cox model
cxmod <- coxph(Surv(time_event, discharge) ~ compliant + poms, data = df_suv)
cox_levels <- expand.grid(
        compliant = levels(df_suv$compliant),
        poms = levels(df_suv$poms)
)
rownames(cox_levels) <- letters[1:4]

coxsf <- survfit(cxmod, newdata = cox_levels)
surv_cxmod <- surv_summary(coxsf)
pid <- as.character(surv_cxmod$strata)  # character vector of patients letters
m_newdat <- cox_levels[pid, ]  # create df that shows patient characteristics corresponding to the rows in surv_cxmod
surv_cxmod <- cbind(surv_cxmod, m_newdat)  # combine patient info
ggsurvplot(surv_cxmod, color = "poms", linetype = "compliant", legend.title = NULL, censor = FALSE, fun = "cumhaz")  # cumulative hazard function


# Data summary table outcomes and indicators for colorectal pts------------------------------
# code to work out indicators and other measures of the Data Summary Table per Site
# Attendance at surgery school
df_srft %>%
        dplyr::select(chest_physio, surgery_date, area_surgery) %>%
        filter(area_surgery == "colorectal") %>% 
        mutate(chest_physio = if_else(chest_physio %in% c("nurse", "surgery school", "doctor"), 1, 0)) %>%
        group_by(month = lubridate::month(surgery_date, label = TRUE)) %>%
        summarise(school_sum = sum(chest_physio), 
                  n = n(),
                  school_prop = sum(chest_physio) / n())

# Prop of pts treated for anaemia management
df_srft %>%
        dplyr::select(anaemia_treat, hb, surgery_date, area_surgery) %>%
        filter(area_surgery == "colorectal") %>% 
        mutate(
                anaemia_treat = if_else(!anaemia_treat %in% c("none", "not applicable"), 1, 0),
                hb = if_else(hb < 13, 1, 0)) %>% 
        group_by(month = lubridate::month(surgery_date, label = TRUE)) %>%
        summarise(denominator = sum(hb) + sum(anaemia_treat), 
                  numerator = sum(anaemia_treat),
                  anae_prop = sum(anaemia_treat) / (sum(hb) + sum(anaemia_treat)))

# iCOUGH bundle compliance  (Patients that were eligible and patients that were compliant)
df_srft %>%
        dplyr::select(inc_spiro, t_brushes, m_washes, oral_diet, mobilised, surgery_date, area_surgery) %>%
        filter(rowSums(is.na(dplyr::select(., 1:5))) < 4 & area_surgery == "colorectal") %>%  # omit recent surgeriies
        mutate_at(
                .vars = vars(1:5),
                .funs = function(x) {
                        if_else(x %in% bundle_yes, 1, 0)
                }
        ) %>%
        mutate(compliant = rowSums(dplyr::select(., 1:5))) %>%
        mutate(
                compliant = if_else(compliant >= 4, "yes", "no")
        ) %>%
        group_by(month = lubridate::month(surgery_date, label = TRUE)) %>%
        summarise(numerator = sum(compliant == "yes"),
                  denominator = n(),
                  prop = sum(compliant == "yes") / n())

# Pt satisfaction with ERAS
df_srft %>%
        dplyr::select(school_satisfaction, surgery_date, area_surgery, eras) %>%
        mutate(school_satisfaction = as.numeric(school_satisfaction)) %>% 
        filter(area_surgery == "colorectal" & !is.na(school_satisfaction)) %>%
        group_by(month = lubridate::month(surgery_date, label = TRUE)) %>%
        summarise(denominator = 10 * n(), 
                  numerator = sum(school_satisfaction),
                  prop = (sum(school_satisfaction) / n()))

# Development of PPC by 7 days after surgery
df_srft %>% dplyr::select(area_surgery, surgery_date, infec_7, pulm_supp_7) %>% 
        filter(area_surgery == "colorectal" & !is.na(infec_7)) %>% 
        mutate(infec_7 = if_else(infec_7 == "chest", 1, 0),
               pulm_supp_7 = if_else(pulm_supp_7 %in% c("mild", "moderate", "severe"), 1, 0)) %>% 
        mutate(ppc = if_else(infec_7 == 1 | pulm_supp_7 == 1, 1, 0)) %>% 
        group_by(month = lubridate::month(surgery_date, label = TRUE)) %>%
        summarise(denominator = n(), 
                  numerator = sum(ppc),
                  prop = sum(ppc) / n())

# Hospital length of stay following surgery (Median per month by discharge month)
df_srft %>% dplyr::select(area_surgery, discharge_date, hospital_stay) %>% 
        filter(area_surgery == "colorectal" & !is.na(discharge_date)) %>% 
        group_by(month = lubridate::month(discharge_date, label = TRUE)) %>%
        summarise(n_pts = n(),
                  median_los = median(hospital_stay, na.rm = TRUE))

# Readmission rates in 30 days (Discharges per month that were readmitted less than 31 days later)
df_srft %>% dplyr::select(area_surgery, discharge_date, readmit_30) %>% 
        filter(area_surgery == "colorectal" & !is.na(readmit_30)) %>% 
        group_by(month = lubridate::month(discharge_date, label = TRUE)) %>%
        summarise(denominator = n(),
                  numerator = sum(readmit_30 == "yes"),
                  prop = sum(readmit_30 == "yes") / n())

# Post-operative morbidity survey (POMS) recorded on day 7
df_srft %>%
        dplyr::select(
                surgery_date, area_surgery,
                pulm_supp_24, infec_24, renal_24,
                gastro, cardio_24, neuro,
                haemato, wound, pain
        ) %>%
        filter_at(vars(3:11), any_vars(!is.na(.))) %>%
        filter(area_surgery == "colorectal") %>%
        mutate_at(
                .vars = vars(3:11),
                .funs = function(x)
                        if_else(x == "none of the above", 0, 1)
        ) %>% 
        mutate(poms = if_else(rowSums(dplyr::select(., 3:11)) == 0, 0, 1)) %>% 
        group_by(lubridate::month(surgery_date, label = TRUE)) %>% 
        summarise(denominator = n(),
                  numerator = sum(poms),
                  prop = sum(poms) / n())

# Surgical complications recorded on patient discharge using the Clavien-Dindo grading system
df_srft %>% dplyr::select(area_surgery, complication, discharge_date) %>% 
        filter(area_surgery == "colorectal" & !is.na(discharge_date)) %>% 
        group_by(lubridate::month(discharge_date, label = TRUE))