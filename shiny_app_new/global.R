
# ── عملگر null-coalescing (باید قبل از همه چیز تعریف شود) ───────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── مسیر مطلق پوشه داده — مستقل از working directory ──────────────────────────
APP_DIR <- tryCatch({
  if (!is.null(getwd())) normalizePath(getwd(), mustWork = FALSE) else "."
}, error = function(e) ".")

DATA_DIR <- file.path(APP_DIR, "data")

if (!dir.exists(DATA_DIR)) {
  dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
  message("پوشه data ساخته شد در: ", DATA_DIR)
}

# ── کتابخانه‌های پایه و پردازش داده ────────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(tibble)
library(purrr)
library(zoo)
library(httr)
library(jsonlite)

# ── کتابخانه‌های مدل‌سازی ────────────────────────────────────────────────────────
library(forecast)
library(prophet)
library(ranger)    # جایگزین سریع‌تر randomForest
library(xgboost)
library(lightgbm)
library(e1071)
library(lightgbm)

# ── کتابخانه‌های خروجی و گزارش ────────────────────────────────────────────────
library(openxlsx)
library(rmarkdown)
library(knitr)
library(ggplot2)
library(scales)

# ── بارگذاری ماژول‌ها و توابع پروژه ─────────────────────────────────────────────
source("R/data_utils.R", local = TRUE)
source("R/modeling_utils.R", local = TRUE)
source("R/metrics_utils.R", local = TRUE)

source("modules/home_module.R", local = TRUE)
source("modules/forecast_module.R", local = TRUE)
source("modules/anomaly_module.R", local = TRUE)
source("modules/leaderboard_module.R", local = TRUE)
source("modules/report_module.R", local = TRUE)

# ── تعریف ایستگاه‌های هواشناسی ──────────────────────────────────────────────────
STATIONS <- list(
  tehran  = list(name = "تهران",  lat = 35.6892, lon = 51.3890),
  isfahan = list(name = "اصفهان", lat = 32.6546, lon = 51.6680),
  mashhad = list(name = "مشهد",   lat = 36.2605, lon = 59.6168),
  shiraz  = list(name = "شیراز",  lat = 29.5918, lon = 52.5837),
  tabriz  = list(name = "تبریز",  lat = 38.0962, lon = 46.2738)
)

# ── Helper: پیدا کردن فایل‌های CSV شهرها (بدون استفاده از perl=TRUE) ─────────
list_weather_csvs <- function(data_dir = DATA_DIR) {
  if (!dir.exists(data_dir)) return(character(0))
  all_csv <- list.files(data_dir, pattern = "^weather_.*\\.csv$", full.names = TRUE)
  basenames <- basename(all_csv)
  keep <- !grepl("^weather_all_cities_", basenames)
  all_csv[keep]
}

# ── Helper: پارس کردن timestamp با چند فرمت رایج ──────────────────────────────
.parse_timestamp <- function(ts_char) {
  if (!is.character(ts_char)) return(ts_char)
  parsed <- as.POSIXct(ts_char, tz = "Asia/Tehran", format = "%Y-%m-%d %H:%M:%S")
  if (all(is.na(parsed))) {
    parsed <- as.POSIXct(ts_char, tz = "Asia/Tehran", format = "%Y-%m-%dT%H:%M")
  }
  if (all(is.na(parsed))) {
    parsed <- lubridate::parse_date_time(ts_char, orders = c("ymd HMS", "ymd HM", "ymd"), tz = "Asia/Tehran")
  }
  parsed
}

# ── یکپارچه‌سازی ستون‌ها (Daily) ────────────────────────────────────────────────
# اصلاح مهم: حفظ ستون station_id هنگام تجمیع روزانه
harmonize_columns <- function(df) {
  if ("timestamp" %in% names(df) && !"date" %in% names(df)) {
    if (is.character(df$timestamp)) {
      df$timestamp <- .parse_timestamp(df$timestamp)
    }
    df$date <- as.Date(df$timestamp)
  }
  if ("date" %in% names(df)) {
    df$date <- as.Date(df$date)
  }
  
  required_cols <- c("date", "temperature", "humidity", "wind_speed", "precipitation")
  for (col in required_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_real_
  }
  
  group_cols <- "date"
  if ("station_id" %in% names(df)) group_cols <- c("station_id", group_cols)
  
  if ("timestamp" %in% names(df)) {
    df <- df %>%
      dplyr::filter(!is.na(date)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::summarise(
        temperature   = mean(temperature, na.rm = TRUE),
        humidity      = mean(humidity, na.rm = TRUE),
        wind_speed    = mean(wind_speed, na.rm = TRUE),
        precipitation = sum(precipitation, na.rm = TRUE),
        .groups       = "drop"
      )
  }
  df <- df %>% dplyr::arrange(date)
  return(df)
}

# ── بارگذاری داده‌های تاریخی از فایل‌های CSV (Daily) ───────────────────────────
load_weather_data <- function(data_dir = DATA_DIR) {
  csv_files <- list_weather_csvs(data_dir)
  if (length(csv_files) == 0) {
    message("هیچ فایل CSV یافت نشد. داده نمونه ساخته می‌شود...")
    return(generate_sample_data())
  }
  
  message("یافت شد: ", length(csv_files), " فایل CSV — در حال بارگذاری...")
  
  all_rows <- purrr::map_dfr(csv_files, function(f) {
    tryCatch({
      df <- utils::read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8")
      if (!"station_id" %in% names(df)) {
        message("هشدار: فایل '", basename(f), "' ستون station_id ندارد — نادیده گرفته شد.")
        return(NULL)
      }
      df
    }, error = function(e) NULL)
  })
  
  if (nrow(all_rows) == 0) return(generate_sample_data())
  
  data_list <- split(all_rows, all_rows$station_id)
  data_list <- purrr::map(data_list, harmonize_columns)
  
  unknown_stations <- setdiff(names(data_list), names(STATIONS))
  if (length(unknown_stations) > 0) {
    message("هشدار: ایستگاه ناشناس در CSV: ", paste(unknown_stations, collapse = ", "))
  }
  
  message("بارگذاری کامل شد: ", length(data_list), " ایستگاه.")
  return(data_list)
}

# ── ساخت داده نمونه روزانه ──────────────────────────────────────────────────────
generate_sample_data <- function() {
  set.seed(42)
  dates <- seq.Date(as.Date("2021-01-01"), Sys.Date() - 1, by = "day")
  n     <- length(dates)
  
  purrr::map(names(STATIONS), function(sid) {
    base_temp <- switch(sid, tehran=16, isfahan=15, mashhad=13, shiraz=18, tabriz=11, 15)
    temp <- base_temp + 10 * sin(2 * pi * seq_len(n) / 365) + rnorm(n, 0, 2)
    
    tibble::tibble(
      date          = dates,
      station_id    = sid,
      temperature   = round(temp, 1),
      humidity      = round(pmax(10, pmin(100, 50 + 20 * cos(2 * pi * seq_len(n) / 365) + rnorm(n, 0, 5))), 1),
      wind_speed    = round(pmax(0, 10 + rnorm(n, 0, 4)), 1),
      precipitation = round(pmax(0, rgamma(n, 0.3, 0.3)), 1)
    )
  }) |> purrr::set_names(names(STATIONS))
}

# ── بارگذاری سراسری داده روزانه ────────────────────────────────────────────────
WEATHER_DATA <- tryCatch(
  load_weather_data(),
  error = function(e) {
    message("خطا در بارگذاری داده: ", e$message, " — داده نمونه استفاده می‌شود")
    generate_sample_data()
  }
)

# ── بارگذاری داده ساعتی (Hourly) ──────────────────────────────────────────────
load_hourly_weather_data <- function() {
  csv_files <- list_weather_csvs(DATA_DIR)
  if (length(csv_files) == 0) return(generate_sample_hourly_data())
  
  all_rows <- purrr::map_dfr(csv_files, function(f) {
    tryCatch({
      df <- utils::read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8")
      if (!"station_id" %in% names(df)) return(NULL)
      df
    }, error = function(e) NULL)
  })
  
  if (nrow(all_rows) == 0) return(generate_sample_hourly_data())
  
  data_list <- split(all_rows, all_rows$station_id)
  
  purrr::map(data_list, function(df) {
    if ("timestamp" %in% names(df)) {
      if (is.character(df$timestamp)) df$timestamp <- .parse_timestamp(df$timestamp)
    } else if ("date" %in% names(df)) {
      return(NULL) # روزانه است، ساعتی نیست
    }
    
    for (col in c("temperature","humidity","wind_speed","precipitation")) {
      if (!col %in% names(df)) df[[col]] <- NA_real_
    }
    
    df %>%
      dplyr::filter(!is.na(timestamp)) %>%
      dplyr::arrange(timestamp) %>%
      tibble::as_tibble()
  })
}

# ── داده ساعتی نمونه ──────────────────────────────────────────────────────────
generate_sample_hourly_data <- function() {
  set.seed(42)
  start_ts <- as.POSIXct("2025-04-01 00:00:00", tz = "Asia/Tehran")
  timestamps <- seq(start_ts, by = "hour", length.out = 60 * 24)
  
  purrr::map(names(STATIONS), function(sid) {
    base_temp <- switch(sid, tehran=16, isfahan=15, mashhad=13, shiraz=18, tabriz=11, 15)
    hours <- as.integer(format(timestamps, "%H"))
    doys  <- as.integer(format(timestamps, "%j"))
    
    daily_pattern <- -cos(2 * pi * hours / 24) * 6
    seasonal <- 8 * sin(2 * pi * doys / 365)
    temp <- base_temp + daily_pattern + seasonal + rnorm(length(timestamps), 0, 1.2)
    
    tibble::tibble(
      timestamp     = timestamps,
      station_id    = sid,
      temperature   = round(temp, 1),
      humidity      = round(pmax(10, pmin(100, 60 - 15 * sin(2*pi*hours/24) + rnorm(length(timestamps),0,4))), 1),
      wind_speed    = round(pmax(0, 10 + 5*sin(2*pi*hours/24) + rnorm(length(timestamps),0,3)), 1),
      precipitation = round(pmax(0, rgamma(length(timestamps), 0.1, 0.5)), 2)
    )
  }) |> purrr::set_names(names(STATIONS))
}

WEATHER_DATA_HOURLY <- tryCatch(
  {
    res <- load_hourly_weather_data()
    if (is.null(res) || all(purrr::map_lgl(res, is.null))) generate_sample_hourly_data() else res
  },
  error = function(e) {
    message("خطا در بارگذاری داده ساعتی: ", e$message, " — نمونه استفاده می‌شود")
    generate_sample_hourly_data()
  }
)

message("══════════════════════════════════════════════════")
message("📁 پوشه داده (DATA_DIR): ", DATA_DIR)
message("   تعداد فایل CSV شهرها: ", length(list_weather_csvs(DATA_DIR)))
message("══════════════════════════════════════════════════")

# ── انتخاب‌های مدل ──────────────────────────────────────────────────────────────
MODEL_CHOICES <- c(
  "ARIMA"         = "arima",
  "SARIMA"        = "sarima",
  "ETS"           = "ets",
  "TBATS"         = "tbats",
  "Prophet"       = "prophet",
  "Random Forest" = "rf",
  "XGBoost"       = "xgboost",
  "LightGBM"      = "lightgbm",
  "CatBoost"      = "catboost",
  "SVM"           = "svm",
  "Naïve"         = "naive",
  "AutoML"        = "automl"
)

# ── رنگ‌بندی ─────────────────────────────────────────────────────────────────────
COLORS <- list(
  primary   = "#2E86AB",
  secondary = "#A23B72",
  accent    = "#F18F01",
  success   = "#C73E1D",
  neutral   = "#3B1F2B",
  light     = "#F5F5F5"
)