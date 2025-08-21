########################### PACKAGES ############################################

rm(list=ls())
getwd()
setwd("C:/Users/boche/OneDrive/Bureau/JDA MODULE/Projet final")

# Set French locale for dates
Sys.setlocale("LC_TIME", "fr_BE.UTF-8")

# Install and load required packages
required_packages <- c("RMariaDB", "factoextra", "FactoMineR", "getPass", 
                      "mice", "readxl", "car", "data.table", "ggplot2", 
                      "bit64", "tidyr", "dplyr","lmtest","lmtest", "broom", "lubridate")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

####################################### DATA MANAGEMENT ########################################

# Connect to MariaDB database
mgr <- MariaDB()
con <- dbConnect(mgr, host="34.198.2.135", dbname="BikeClimate", 
                user="JDA2025", password=getPass())

########################## DATA MONITOR CB02411 ################################################

# Extract data for specific monitor CB02411
CB02411 <- dbGetQuery(con,"select * from monitor where FeatureID='CB02411'")

CB02411 <- CB02411 %>%
  mutate_if(~ inherits(.x, "integer64"), as.numeric)

CB02411 <- CB02411[CB02411$DateUTC1 < as.Date("2025-01-01"), ]

setDT(CB02411)



######################### WEATHER DATA FROM UCCLE ###########################################################

# Extract weather data from Uccle station
weather_uccle <- dbGetQuery(con, "SELECT * FROM uccle")


setDT(weather_uccle)

######################### STUDENT HOLIDAYS DATA ###########################################################

# Load student holidays data
student_holidays <- read_excel("Vacances FR+NL.xlsx")

# Convert date column to proper date format
student_holidays$Date <- as.Date(student_holidays$Date, format = "%d-%m-%y")

# Convert binary variables to numeric
student_holidays$Students_Holidays_NL <- as.numeric(student_holidays$Students_Holidays_NL)
student_holidays$Students_Holidays_FR <- as.numeric(student_holidays$Students_Holidays_FR)

# Create combined student holiday variable (either NL or FR)
student_holidays$student_holiday <- ifelse(
  student_holidays$Students_Holidays_NL == 1 | student_holidays$Students_Holidays_FR == 1, 1, 0
)

######################### PART 1 - ALL MONITORS DATA ANALYIS #################################### 

# Extract all monitor data for comparative analysis
all_monitor_data <- dbGetQuery(con, "SELECT * FROM monitor")
setDT(all_monitor_data)

all_monitor_data[, year := format(as.Date(DateUTC1), "%Y")]
all_monitor_data <- all_monitor_data[year != "2025"]


all_monitor_data$Count <- as.numeric(all_monitor_data$Count)

# Define Belgian public holidays (2022-2024)
public_holidays <- as.Date(c(
  # 2022
  "2022-01-01", "2022-04-18", "2022-05-01", "2022-05-26", "2022-06-06",
  "2022-07-21", "2022-08-15", "2022-11-01", "2022-11-11", "2022-12-25",
  # 2023
  "2023-01-01", "2023-04-10", "2023-05-01", "2023-05-18", "2023-05-29",
  "2023-07-21", "2023-08-15", "2023-11-01", "2023-11-11", "2023-12-25",
  # 2024
  "2024-01-01", "2024-04-01", "2024-05-01", "2024-05-09", "2024-05-20",
  "2024-07-21", "2024-08-15", "2024-11-01", "2024-11-11", "2024-12-25"
))

# Create temporal variables
all_monitor_data[, day := weekdays(DateUTC1)]
all_monitor_data[, is_holiday := DateUTC1 %in% public_holidays]

# Define weekdays correctly (Monday=1, Sunday=7 with lubridate)
all_monitor_data[, weekday_num := lubridate::wday(DateUTC1, week_start = 1)]
all_monitor_data[, is_weekday := !(weekday_num %in% c(6, 7) | is_holiday)] # Sat=6, Sun=7

all_monitor_data[, month := months(DateUTC1, abbreviate = FALSE)]
all_monitor_data[, time_period := ifelse(hourUTC1 >= 7 & hourUTC1 < 19, "Day", "Night")]
all_monitor_data[, year := year(DateUTC1)]

########################################## SEPARATE DAY/NIGHT ANALYSIS #############################

# Counts for weekdays - DAY period (7h-19h)
weekday_day_counts <- all_monitor_data[
  is_weekday == TRUE & hourUTC1 >= 7 & hourUTC1 < 19,
  .(total_passages = sum(Count, na.rm = TRUE)),
  by = .(FeatureID, year, month)
]

# Counts for weekdays - NIGHT period (19h-7h)
weekday_night_counts <- all_monitor_data[
  is_weekday == TRUE & (hourUTC1 < 7 | hourUTC1 >= 19),
  .(total_passages = sum(Count, na.rm = TRUE)),
  by = .(FeatureID, year, month)
]


# Define month order for proper sequencing
month_order <- c("janvier", "février", "mars", "avril", "mai", "juin", 
                 "juillet", "août", "septembre", "octobre", "novembre", "décembre")

# Function to prepare data for PCA analysis
prepare_pca_data <- function(data, period_name) {
# Calculate monthly averages per station
  monthly_means <- data[
    , .(mean_passages = mean(total_passages, na.rm = TRUE)),
    by = .(FeatureID, month)
  ]
  
  # Transform to wide format for PCA
  wide_data <- monthly_means %>%
    pivot_wider(
      id_cols = FeatureID,
      names_from = month,
      values_from = mean_passages,
      names_prefix = paste0(period_name, "_")
    )
  
  # Replace NA values with 0
  wide_data[is.na(wide_data)] <- 0
  
  # Round to 2 decimal places
  wide_data <- wide_data %>%
    mutate(across(where(is.numeric), ~round(., 2)))
  
  # Set FeatureID as rownames
  wide_data <- as.data.frame(wide_data)
  rownames(wide_data) <- wide_data$FeatureID
  wide_data$FeatureID <- NULL
  
  return(wide_data)
}

##################################### PCA FOR WEEKDAYS #####################################

# Prepare data for PCA - Weekdays day period
weekday_day_wide <- prepare_pca_data(weekday_day_counts, "weekday_day")
colnames(weekday_day_wide) <- gsub("weekday_day_", "", colnames(weekday_day_wide))

# PCA for Weekdays - DAY period
pca_weekday_day <- PCA(weekday_day_wide, scale.unit = TRUE, graph = FALSE)

cat("=== PCA WEEKDAYS - DAY PERIOD ===\n")
print(pca_weekday_day$eig)

# PCA visualizations for day period
fviz_eig(pca_weekday_day, main = "PCA Weekdays - Day Period")

fviz_pca_var(pca_weekday_day,
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Variables - Weekdays Day Period")

fviz_pca_ind(pca_weekday_day,
             repel = TRUE,
             geom = c("point", "text"),
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             pointsize = 3,
             title = "Individuals - Weekdays Day Period") +
  theme_minimal()

# Prepare data for PCA - Weekdays night period
weekday_night_wide <- prepare_pca_data(weekday_night_counts, "weekday_night")
colnames(weekday_night_wide) <- gsub("weekday_night_", "", colnames(weekday_night_wide))

# PCA for Weekdays - NIGHT period
pca_weekday_night <- PCA(weekday_night_wide, scale.unit = TRUE, graph = FALSE)

cat("=== PCA WEEKDAYS - NIGHT PERIOD ===\n")
print(pca_weekday_night$eig)

# PCA visualizations for night period
fviz_eig(pca_weekday_night, main = "PCA Weekdays - Night Period")

fviz_pca_var(pca_weekday_night,
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Variables - Weekdays Night Period")

fviz_pca_ind(pca_weekday_night,
             repel = TRUE,
             geom = c("point", "text"),
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             pointsize = 3,
             title = "Individuals - Weekdays Night Period") +
  theme_minimal()

####################  HIERARCHICAL CLUSTERING HCPC #######################

# Hierarchical clustering for weekdays - DAY period
hcpc_weekday_day <- HCPC(pca_weekday_day, graph = FALSE)

fviz_dend(hcpc_weekday_day, main = "Dendrogram - Weekdays Day Period")

# Coordinates for CB02411 station
coord_day <- as.data.frame(pca_weekday_day$ind$coord)
coord_day$FeatureID <- rownames(coord_day)

fviz_cluster(hcpc_weekday_day,
             repel = TRUE,
             labelsize = 10,
             pointshape = 19,
             pointsize = 3,
             palette = "jco",
             main = "Station Clustering - Weekdays Day Period",
             ggtheme = theme_minimal()) +
  geom_text(data = subset(coord_day, FeatureID == "CB02411"),
            aes(x = Dim.1, y = Dim.2, label = FeatureID),
            color = "red", fontface = "bold", size = 5, vjust = -1)

# Hierarchical clustering for weekdays - NIGHT period
hcpc_weekday_night <- HCPC(pca_weekday_night, graph = FALSE)

fviz_dend(hcpc_weekday_night, main = "Dendrogram - Weekdays Night Period")

coord_night <- as.data.frame(pca_weekday_night$ind$coord)
coord_night$FeatureID <- rownames(coord_night)

fviz_cluster(hcpc_weekday_night,
             repel = TRUE,
             labelsize = 10,
             pointshape = 19,
             pointsize = 3,
             palette = "jco",
             main = "Station Clustering - Weekdays Night Period",
             ggtheme = theme_minimal()) +
  geom_text(data = subset(coord_night, FeatureID == "CB02411"),
            aes(x = Dim.1, y = Dim.2, label = FeatureID),
            color = "red", fontface = "bold", size = 5, vjust = -1)

# Cluster interpretation
cat("=== DAY PERIOD CLUSTERS ===\n")
print(hcpc_weekday_day$desc.var)
cat("Station CB02411 in cluster:", hcpc_weekday_day$data.clust["CB02411", "clust"], "\n")

cat("=== NIGHT PERIOD CLUSTERS ===\n")
print(hcpc_weekday_night$desc.var)
cat("Station CB02411 in cluster:", hcpc_weekday_night$data.clust["CB02411", "clust"], "\n")


################################### PART 2 - REGRESSION MODELING  #########################

################ CB02411 DATASET CLEANING ################

# Create season variable
CB02411 <- CB02411 %>%
  mutate(
    season = case_when(
      format(DateUTC1, "%m-%d") >= "12-21" | format(DateUTC1, "%m-%d") < "03-21" ~ "winter",
      format(DateUTC1, "%m-%d") >= "03-21" & format(DateUTC1, "%m-%d") < "06-21" ~ "spring",
      format(DateUTC1, "%m-%d") >= "06-21" & format(DateUTC1, "%m-%d") < "09-21" ~ "summer",
      format(DateUTC1, "%m-%d") >= "09-21" & format(DateUTC1, "%m-%d") < "12-21" ~ "autumn"))

# Handle missing values
median_23_autumn <- CB02411 %>%
  filter(hourUTC1 == 23, season == "autumn", !is.na(Count)) %>%
  summarise(med = median(Count)) %>%
  pull(med)

CB02411 <- CB02411 %>%
  mutate(Count = ifelse(hourUTC1 == 23 & season == "autumn" & is.na(Count),
                        median_23_autumn, Count))

# Replace missing speed values with 0
CB02411$Speed[is.na(CB02411$Speed)] <- 0

# Treatment of NA values for UTCI & Tmrt
CB02411$UTCI[is.na(CB02411$UTCI)] <- median(CB02411$UTCI, na.rm = TRUE)
CB02411$Tmrt[is.na(CB02411$Tmrt)] <- median(CB02411$Tmrt, na.rm = TRUE)

sum(is.na(CB02411$UTCI))
sum(is.na(CB02411$Tmrt))

####### MISSING VALUE TREATMENT IN WEATHER DATA - MULTIPLE IMPUTATION ##########
sum(is.na(weather_uccle))
colSums((is.na(weather_uccle)))

sum(is.na(CB02411))
colSums((is.na(CB02411)))

# Select variables for imputation
weather_vars <- weather_uccle %>%
  select(Ta_ucc, wind_speed_ucc, press_ucc, cloud_ucc, rain_ucc, solar_bxl,
         hourUTC1, DateUTC1)

# Select variables for imputation
weather_with_outcome <- weather_vars
weather_with_outcome$count <- CB02411$Count

# MULTIPLE IMPUTATION with MICE
method_vector <- make.method(weather_with_outcome)
method_vector["count"] <- ""

set.seed(123)
imputed_data_full <- mice(
  weather_with_outcome,
  method = method_vector,
  m = 5,                    
  maxit = 10,              
  printFlag = FALSE
)

print(imputed_data_full)
plot(imputed_data_full, main = "Convergence avec variable dépendante")

# Fit the model on each imputation
fitted_models <- with(imputed_data_full, lm(count ~ Ta_ucc + wind_speed_ucc + press_ucc + cloud_ucc + rain_ucc + solar_bxl))

# Pooling
pooled_results <- pool(fitted_models)

cat("\n=== RÉSULTATS DE RÉGRESSION POOLÉS ===\n")
summary(pooled_results, conf.int = TRUE)

# Complete imputed datas
weather_imputed <- complete(imputed_data_full, action ="all")



###################### Merging and creating datasets for multiple regression ##############

daily_datasets <- list()

for (i in 1:5) {
  weather_i <- complete(imputed_data_full, i)
  
  # merge
  df <- merge(CB02411, weather_i, by = c("DateUTC1", "hourUTC1"), all.x = TRUE)
  df <- df %>%
    filter(Count > 0 | is.na(Count))
   df_daily <- df %>%
    mutate(
      weekday_num = lubridate::wday(DateUTC1, week_start = 1),
        weekday_name = factor(case_when(
          weekday_num == 1 ~ "Monday", weekday_num == 2 ~ "Tuesday",
          weekday_num == 3 ~ "Wednesday", weekday_num == 4 ~ "Thursday",
          weekday_num == 5 ~ "Friday", weekday_num == 6 ~ "Saturday", TRUE ~ "Sunday"
        ), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
      is_weekend = weekday_num %in% c(6, 7),
      is_weekday = weekday_num %in% 1:5,
      day_night = ifelse(hourUTC1 >= 7 & hourUTC1 < 19, "day", "night"),
      month = lubridate::month(DateUTC1),
      year = format(DateUTC1, "%Y")
    ) %>%
    group_by(DateUTC1) %>%
    summarise(
      total_count = sum(Count, na.rm = TRUE),
      Ta_ucc = mean(Ta_ucc, na.rm = TRUE),
      rain_ucc = mean(rain_ucc, na.rm = TRUE),
      solar_bxl = mean(solar_bxl, na.rm = TRUE),
      wind_speed_ucc = mean(wind_speed_ucc, na.rm = TRUE),
      press_ucc = mean(press_ucc, na.rm = TRUE),
      cloud_ucc = mean(cloud_ucc, na.rm = TRUE),
      UTCI= mean(UTCI,na.rm=TRUE),
      Tmrt= mean(Tmrt,na.rm=TRUE),
      weekday_name = first(weekday_name),
      is_weekend = any(is_weekend),
      is_weekday = any(is_weekday),
      season = first(season),
      month = first(month),
      year = first(year),
      .groups = "drop"
    )
  
  # adding student holidays
  df_daily <- df_daily %>%
    left_join(student_holidays[, c("Date", "student_holiday", "Students_Holidays_NL", "Students_Holidays_FR")],
              by = c("DateUTC1" = "Date")) %>%
    mutate(
      student_holiday = ifelse(is.na(student_holiday), 0, student_holiday),
      Students_Holidays_NL = ifelse(is.na(Students_Holidays_NL), 0, Students_Holidays_NL),
      Students_Holidays_FR = ifelse(is.na(Students_Holidays_FR), 0, Students_Holidays_FR)
    )
  
  # holidays
  holidays_data <- data.frame(DateUTC1 = public_holidays, public_holiday = 1)
  
  df_daily <- df_daily %>%
    left_join(holidays_data, by = "DateUTC1") %>%
    mutate(
      public_holiday = ifelse(is.na(public_holiday), 0, public_holiday),
      is_holiday = public_holiday == 1
    )
  
 
  daily_datasets[[i]] <- df_daily
}

############################ Function for multiple regression  ###################

# Function to adjust several datasets
fit_multiple_models <- function(formula, datasets, model_name = "Model") {
  cat("=== Fitting", model_name, "===\n")
  models <- list()
  for(i in seq_along(datasets)) {
    cat("Dataset", i, "of", length(datasets), "\n")
    models[[i]] <- lm(formula, data = datasets[[i]])
  }
  return(models)
}

# Function for statistic's models
get_model_stats <- function(models) {
  r2 <- sapply(models, function(x) summary(x)$r.squared)
  adj_r2 <- sapply(models, function(x) summary(x)$adj.r.squared)
  
  list(
    r2_mean = round(mean(r2), 4),
    adj_r2_mean = round(mean(adj_r2), 4),
    r2_range = paste(round(min(r2), 4), "-", round(max(r2), 4)),
    variance_explained = round(mean(r2) * 100, 1)
  )
}

# Function for persistents outliers
detect_consistent_outliers <- function(models, datasets, threshold_datasets = 3) {
  outliers_list <- list()
  
for(i in seq_along(models)) {
    cooksd <- cooks.distance(models[[i]])
    threshold <- 4 / nrow(datasets[[i]])
    outliers_list[[i]] <- which(cooksd > threshold)
    
    cat("Dataset", i, ": ", length(outliers_list[[i]]), "outliers detected\n")
}
  
# threshold_datasets' 
all_outliers <- unique(unlist(outliers_list))
outlier_frequency <- sapply(all_outliers, function(idx) {
    sum(sapply(outliers_list, function(x) idx %in% x))
  })
  
consistent_outliers <- all_outliers[outlier_frequency >= threshold_datasets]
cat("Consistent outliers:", length(consistent_outliers), "\n")
  
  return(consistent_outliers)
}

# Significance table
create_significance_table <- function(pooled_summary) {
  sig_table <- pooled_summary
  sig_table$significance <- ifelse(sig_table$p.value < 0.001, "***",
                                   ifelse(sig_table$p.value < 0.01, "**",
                                          ifelse(sig_table$p.value < 0.05, "*", "")))
  
  result <- sig_table[, c("term", "estimate", "std.error", "p.value", "significance")]
  
    n_significant <- sum(sig_table$p.value < 0.05)
  cat("Significant variables:", n_significant, "/", nrow(sig_table), "\n")
  
  non_sig <- sig_table[sig_table$p.value >= 0.05, "term"]
  if(length(non_sig) > 0) {
    cat("NON-significant variables:", paste(non_sig, collapse = ", "), "\n")
  }
  
  return(result)
}

############################ Multiple regression ######################

# Models
model_1 <- total_count ~ weekday_name + is_holiday + student_holiday + 
           solar_bxl+rain_ucc + Ta_ucc + wind_speed_ucc + year + season + cloud_ucc + press_ucc + UTCI + Tmrt

model_2 <- total_count ~ weekday_name + is_holiday + student_holiday + Tmrt*season+
           rain_ucc + Ta_ucc + wind_speed_ucc + year  + cloud_ucc 

model_3 <- total_count ~ weekday_name + is_holiday + student_holiday + Tmrt+
           rain_ucc + wind_speed_ucc + year + season + cloud_ucc 

# Initial model fitting
cat("\n", paste0(rep("=", 60), collapse = ""), "\n")
cat("STEP 1: INITIAL MODEL FITTING\n")
cat(paste0(rep("=", 60), collapse = ""), "\n")

models_1 <- fit_multiple_models(model_1, daily_datasets, "Model 1")
models_2 <- fit_multiple_models(model_2, daily_datasets, "Model 2")
models_3 <- fit_multiple_models(model_3, daily_datasets, "Modèle 3")

# Pooling 
pooled_1 <- pool(models_1)
pooled_2 <- pool(models_2)
pooled_3 <- pool(models_3)

# Statistics
stats_1 <- get_model_stats(models_1)
stats_2 <- get_model_stats(models_2)
stats_3 <- get_model_stats(models_3)

cat("\nModel 1 - Statistics:\n")
cat("Mean R²:", stats_1$r2_mean, "| Adjusted R²:", stats_1$adj_r2_mean, "\n")
cat("Variance explained:", stats_1$variance_explained, "%\n")

cat("\nModel 2 - Statistics:\n")
cat("Mean R²:", stats_2$r2_mean, "| Adjusted R²:", stats_2$adj_r2_mean, "\n")
cat("Variance explained:", stats_2$variance_explained, "%\n")

cat("\nModel 3 - Statistics:\n")
cat("Mean R²:", stats_3$r2_mean, "| Adjusted R²:", stats_3$adj_r2_mean, "\n")
cat("Variance explained:", stats_3$variance_explained, "%\n")

#VIF

vif(models_1[[1]])
vif(models_2[[1]])
vif(models_3[[1]])


# OUTLIER DETECTION AND TREATMENT
cat("\n", paste0(rep("=", 60), collapse = ""), "\n")
cat("STEP 2: OUTLIER DETECTION\n")
cat(paste0(rep("=", 60), collapse = ""), "\n")

outliers_1 <- detect_consistent_outliers(models_1, daily_datasets)
outliers_2 <- detect_consistent_outliers(models_2, daily_datasets)
outliers_3 <- detect_consistent_outliers(models_3, daily_datasets)

# Print outliers

for (i in 1:3) {
  outliers <- get(paste0("outliers_", i))
  if(length(outliers) > 0) {
    cat(paste0("\nModel ", i, " Outliers:\n"))
    print(daily_datasets[[1]][outliers, c("DateUTC1", "total_count", "weekday_name")])
  }
}

# Adjusting models without outliers
cat("\n", paste0(rep("=", 60), collapse = ""), "\n")
cat("ÉTAPE 3: MODÈLES SANS OUTLIERS\n")
cat(paste0(rep("=", 60), collapse = ""), "\n")

# Function for cleaned datasets
create_clean_datasets <- function(datasets, outliers_to_remove) {
  if(length(outliers_to_remove) == 0) return(datasets)
  
  clean_datasets <- list()
  for(i in seq_along(datasets)) {
    clean_datasets[[i]] <- datasets[[i]][-outliers_to_remove, ]
  }
  return(clean_datasets)
}

# Cleaned datasets
clean_datasets_1 <- create_clean_datasets(daily_datasets, outliers_1)
clean_datasets_2 <- create_clean_datasets(daily_datasets, outliers_2)
clean_datasets_3 <- create_clean_datasets(daily_datasets, outliers_3)

# Adjusted models without outliers
models_1_clean <- fit_multiple_models(model_1, clean_datasets_1, "Modèle 1 (sans outliers)")
models_2_clean <- fit_multiple_models(model_2, clean_datasets_2, "Modèle 2 (sans outliers)")
models_3_clean <- fit_multiple_models(model_3, clean_datasets_3, "Modèle 3 (sans outliers)")

# Pooling of cleaned results
pooled_1_clean <- pool(models_1_clean)
pooled_2_clean <- pool(models_2_clean)
pooled_3_clean <- pool(models_3_clean)

# Comparison
cat("\n", paste0(rep("=", 60), collapse = ""), "\n")
cat("ÉTAPE 4: COMPARAISON DES RÉSULTATS\n")
cat(paste0(rep("=", 60), collapse = ""), "\n")

stats_1_clean <- get_model_stats(models_1_clean)
stats_2_clean <- get_model_stats(models_2_clean)
stats_3_clean <- get_model_stats(models_3_clean)

# Table of comparison
comparison_table <- data.frame(
  Modèle = c("Modèle 1 (avec outliers)", "Modèle 1 (sans outliers)", 
             "Modèle 2 (avec outliers)", "Modèle 2 (sans outliers)",
             "Modèle 3 (avec outliers)", "Modèle 3 (sans outliers)"),
  R2_moyen = c(stats_1$r2_mean, stats_1_clean$r2_mean, 
               stats_2$r2_mean, stats_2_clean$r2_mean,
               stats_3$r2_mean, stats_3_clean$r2_mean),
  R2_ajusté = c(stats_1$adj_r2_mean, stats_1_clean$adj_r2_mean,
                stats_2$adj_r2_mean, stats_2_clean$adj_r2_mean,
                stats_3$adj_r2_mean, stats_3_clean$adj_r2_mean))

print(comparison_table)

# Significance table
cat("\n", paste0(rep("=", 60), collapse = ""), "\n")
cat("ÉTAPE 5: SIGNIFICATIVITÉ DES VARIABLES\n")
cat(paste0(rep("=", 60), collapse = ""), "\n")

cat("\nModèle 1 (sans outliers):\n")
sig_table_1 <- create_significance_table(summary(pooled_1_clean))
print(sig_table_1)

cat("\nModèle 2 (sans outliers):\n")
sig_table_2 <- create_significance_table(summary(pooled_2_clean))
print(sig_table_2)

cat("\nModèle 3 (sans outliers):\n")
sig_table_3 <- create_significance_table(summary(pooled_3_clean))
print(sig_table_3)

# Recommendation
cat("\n", paste0(rep("=", 60), collapse = ""), "\n")
cat("ÉTAPE 6: RECOMMANDATIONS\n")
cat(paste0(rep("=", 60), collapse = ""), "\n")

# Improvement
improvement_1 <- stats_1_clean$r2_mean - stats_1$r2_mean
improvement_2 <- stats_2_clean$r2_mean - stats_2$r2_mean
improvement_3 <- stats_3_clean$r2_mean - stats_3$r2_mean


cat("Amélioration Modèle 1 (sans outliers):", round(improvement_1, 4), "\n")
cat("Amélioration Modèle 2 (sans outliers):", round(improvement_2, 4), "\n")
cat("Amélioration Modèle 3 (sans outliers):", round(improvement_3, 4), "\n")


################### Final  validation : Model 3 ############################

cat("\n", paste0(rep("=", 60), collapse = ""), "\n")
cat("ÉTAPE 7: VALIDATION - Modèle 3\n")
cat(paste0(rep("=", 60), collapse = ""), "\n")

# Prediction
mod3_with_outliers <- models_3[[1]]
mod3_without_outliers <- models_3_clean[[1]]
data_with_outliers <- daily_datasets[[1]]
data_without_outliers <- clean_datasets_3[[1]]

pred_with_outliers <- predict(mod3_with_outliers, newdata = data_with_outliers)
pred_without_outliers <- predict(mod3_without_outliers, newdata = data_without_outliers)

# Clipping to 0
pred_with_outliers <- pmax(pred_with_outliers, 0)
pred_without_outliers <- pmax(pred_without_outliers, 0)

plot_data <- data.frame(
  Date = data_with_outliers$DateUTC1,
  Observed = data_with_outliers$total_count,
  Pred_with_outliers = pred_with_outliers
)

plot_data_clean <- data.frame(
  Date = data_without_outliers$DateUTC1,
  Observed = data_without_outliers$total_count,
  Pred_without_outliers = pred_without_outliers
)

############## Key Performance Indicators (KPIs) ############

# Function

MAE <- function(obs, pred) {
  mean(abs(obs - pred), na.rm = TRUE)
}

sMAPE <- function(obs, pred) {
  100 * mean(2 * abs(pred - obs) / (abs(obs) + abs(pred) + 1e-6), na.rm = TRUE)
}
  
# RSE #
residuals <- mod3_without_outliers$residuals
df_resid <- mod3_without_outliers$df.residual
rse <- sqrt(sum(residuals^2) / df_resid)

mean_observed <- mean(plot_data_clean$Observed, na.rm = TRUE)
rse_percent <- rse / mean_observed * 100

Mae <- MAE(plot_data_clean$Observed, plot_data_clean$Pred_without_outliers)
sMape <- sMAPE(plot_data_clean$Observed, plot_data_clean$Pred_without_outliers)

cat("\n=== Indicateurs de performance (Modèle 3 sans outliers) ===\n")
cat("MAE   :", round(Mae, 2), "passages/jour\n")
cat("sMAPE :", round(sMape, 2), "%\n")
cat("RSE   :", round(rse, 2), "passages/jour\n")
cat("RSE % :", round(rse_percent, 2), "% du total moyen\n")


cat("\nGraphique comparatif : avec et sans outliers (Modèle 3)\n")

ggplot() +
  geom_line(data = plot_data, aes(x = Date, y = Observed, color = "Observé"), alpha = 0.3) +
  geom_line(data = plot_data, aes(x = Date, y = Pred_with_outliers, color = "Prédit"), alpha = 0.3) +
  geom_smooth(data = plot_data, aes(x = Date, y = Observed, color = "Observé"), se = FALSE, method = "loess", span = 0.1) +
  geom_smooth(data = plot_data, aes(x = Date, y = Pred_with_outliers, color = "Prédit"), se = FALSE, method = "loess", span = 0.1) +
  scale_color_manual(values = c("Observé" = "black", "Prédit" = "red")) +
  labs(title = "Modèle 3 avec outliers", y = "Comptage vélo", x = "Date", color = "Légende") +
  theme_minimal()

readline(prompt = "Appuyez sur [Entrée] pour afficher le modèle sans outliers...")

ggplot() +
  geom_line(data = plot_data_clean, aes(x = Date, y = Observed, color = "Observé"), alpha = 0.3) +
  geom_line(data = plot_data_clean, aes(x = Date, y = Pred_without_outliers, color = "Prédit"), alpha = 0.3) +
  geom_smooth(data = plot_data_clean, aes(x = Date, y = Observed, color = "Observé"), se = FALSE, method = "loess", span = 0.1) +
  geom_smooth(data = plot_data_clean, aes(x = Date, y = Pred_without_outliers, color = "Prédit"), se = FALSE, method = "loess", span = 0.1) +
  scale_color_manual(values = c("Observé" = "black", "Prédit" = "blue")) +
  labs(title = "Modèle 3 sans outliers", y = "Comptage vélo", x = "Date", color = "Légende") +
  theme_minimal()

# Comparison hypothesis of the model 3 with & without outliers

cat("\nDiagnostics visuels - Modèle 3\n")
par(mfrow = c(2, 2))
plot(mod3_with_outliers, which = 1, main = "Résidus vs Fitted")
plot(mod3_with_outliers, which = 2, main = "Q-Q Plot")
plot(mod3_with_outliers, which = 3, main = "Échelle vs Localisation")
plot(mod3_with_outliers, which = 5, main = "Résidus vs Leverage")
par(mfrow = c(1, 1))

cat("\nDiagnostics visuels - Modèle 3\n")
par(mfrow = c(2, 2))
plot(mod3_without_outliers, which = 1, main = "Résidus vs Fitted")
plot(mod3_without_outliers, which = 2, main = "Q-Q Plot")
plot(mod3_without_outliers, which = 3, main = "Échelle vs Localisation")
plot(mod3_without_outliers, which = 5, main = "Résidus vs Leverage")
par(mfrow = c(1, 1))

mod3_with_outliers

# Vif
cat("\nVérification multicolinéarité - VIF Modèle 3\n")
vif_values <- vif(mod3_without_outliers)
print(vif_values)


######################## EXPORT FOR POWER BI ###############

###### Raw datas ##########
CB02411$DateUTC1 <- as.Date(CB02411$DateUTC1)

get_season <- function(date) {
  md <- format(date, "%m-%d")
  case_when(
    md >= "12-21" | md < "03-20" ~ "winter",
    md >= "03-20" & md < "06-21" ~ "spring",
    md >= "06-21" & md < "09-22" ~ "summer",
    TRUE ~ "autumn"
  )
}

# Creation of the datasets
df_daily_raw <- CB02411 %>%
  mutate(
    weekday_num = lubridate::wday(DateUTC1, week_start = 1),
    weekday_name = factor(case_when(
      weekday_num == 1 ~ "Monday",
      weekday_num == 2 ~ "Tuesday",
      weekday_num == 3 ~ "Wednesday",
      weekday_num == 4 ~ "Thursday",
      weekday_num == 5 ~ "Friday",
      weekday_num == 6 ~ "Saturday",
      TRUE ~ "Sunday"
    ), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
    is_weekend = weekday_num %in% c(6, 7),
    is_weekday = weekday_num %in% 1:5,
    month = month(DateUTC1, label = TRUE, abbr = FALSE),
    year = year(DateUTC1),
    season = get_season(DateUTC1)
  ) %>%
  group_by(DateUTC1) %>%
  summarise(
    total_count = sum(Count, na.rm = TRUE),
    weekday_name = first(weekday_name),
    is_weekend = first(is_weekend),
    is_weekday = first(is_weekday),
    month = first(month),
    year = first(year),
    season = first(season),
    .groups = "drop"
  )

write.csv(df_daily_raw, "CB02411_daily_full.csv", 
          row.names = FALSE, 
          quote = FALSE, 
          fileEncoding = "UTF-8")


############ Clean data #####################

data_without_outliers$predicted_count <- pred_without_outliers

df_export <- data_without_outliers %>%
  mutate(across(where(is.numeric) & !all_of("predicted_count"), ~ round(., 2)))

write.csv2(df_export, "CB02411_daily_clean.csv", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
write.csv2(df_export, "CB02411_daily_clean_fixed.csv", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")


# Close database connection
dbDisconnect(con)

