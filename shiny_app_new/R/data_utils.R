# File: R/data_utils.R
# ابزارهای مدیریت، دریافت داده هواشناسی و مهندسی ویژگی

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(zoo)

# --------------------------------------------------
# دریافت داده تاریخی و اخیر از Open-Meteo
# --------------------------------------------------
download_historical_data <- function(station_id, start_date, end_date) {
  if (!exists("STATIONS")) stop("متغیر سراسری STATIONS تعریف نشده است.")
  if (!station_id %in% names(STATIONS)) stop("ایستگاه یافت نشد.")
  
  station <- STATIONS[[station_id]]
  lat <- station$lat
  lon <- station$lon
  
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  today <- Sys.Date()
  
  dfs <- list()
  
  # ۱. دریافت داده‌های آرشیو (تا ۶ روز پیش)
  archive_start <- start_date
  archive_end   <- min(end_date, today - 6)
  
  if (archive_start <= archive_end) {
    url_archive <- paste0(
      "https://archive-api.open-meteo.com/v1/archive",
      "?latitude=", lat, "&longitude=", lon,
      "&start_date=", format(archive_start, "%Y-%m-%d"),
      "&end_date=", format(archive_end, "%Y-%m-%d"),
      "&hourly=temperature_2m,relative_humidity_2m,wind_speed_10m,surface_pressure,precipitation,weather_code",
      "&timezone=Asia%2FTehran"
    )
    resp <- httr::GET(url_archive, httr::timeout(60))
    if (!httr::http_error(resp)) {
      raw <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
      if (!is.null(raw$hourly) && length(raw$hourly$time) > 0) {
        h <- raw$hourly
        dfs[[length(dfs) + 1]] <- data.frame(
          timestamp = as.POSIXct(h$time, format = "%Y-%m-%dT%H:%M", tz = "Asia/Tehran"),
          temperature = as.numeric(h$temperature_2m), humidity = as.numeric(h$relative_humidity_2m),
          wind_speed = as.numeric(h$wind_speed_10m), pressure = as.numeric(h$surface_pressure),
          precipitation = as.numeric(h$precipitation), w_code = as.integer(h$weather_code),
          station_id = station_id, stringsAsFactors = FALSE
        )
      }
    }
  }
  
  # ۲. دریافت داده‌های اخیر (۱۶ روز گذشته تا امروز) از Forecast API
  recent_start <- max(start_date, today - 16)
  if (end_date >= recent_start) {
    past_days <- as.integer(today - recent_start) + 1
    past_days <- min(past_days, 92)
    
    url_recent <- paste0(
      "https://api.open-meteo.com/v1/forecast",
      "?latitude=", lat, "&longitude=", lon, "&past_days=", past_days,
      "&hourly=temperature_2m,relative_humidity_2m,wind_speed_10m,surface_pressure,precipitation,weather_code",
      "&timezone=Asia%2FTehran"
    )
    resp2 <- httr::GET(url_recent, httr::timeout(60))
    if (!httr::http_error(resp2)) {
      raw2 <- jsonlite::fromJSON(httr::content(resp2, "text", encoding = "UTF-8"))
      if (!is.null(raw2$hourly) && length(raw2$hourly$time) > 0) {
        h2 <- raw2$hourly
        df_r <- data.frame(
          timestamp = as.POSIXct(h2$time, format = "%Y-%m-%dT%H:%M", tz = "Asia/Tehran"),
          temperature = as.numeric(h2$temperature_2m), humidity = as.numeric(h2$relative_humidity_2m),
          wind_speed = as.numeric(h2$wind_speed_10m), pressure = as.numeric(h2$surface_pressure),
          precipitation = as.numeric(h2$precipitation), w_code = as.integer(h2$weather_code),
          station_id = station_id, stringsAsFactors = FALSE
        )
        
        # فیلتر دقیق برای جلوگیری از ذخیره داده‌های آینده
        now_time <- Sys.time()
        attr(now_time, "tzone") <- "Asia/Tehran"
        df_r <- df_r[df_r$timestamp >= as.POSIXct(start_date, tz = "Asia/Tehran") & df_r$timestamp <= now_time, ]
        dfs[[length(dfs) + 1]] <- df_r
      }
    }
  }
  
  if (length(dfs) == 0) stop("داده‌ای برای ایستگاه دریافت نشد.")
  
  df_all <- do.call(rbind, dfs)
  df_all <- df_all[order(df_all$timestamp), ]
  df_all <- df_all[!duplicated(df_all$timestamp), ]
  df_all <- df_all[!is.na(df_all$temperature), ]
  
  return(df_all)
}

# --------------------------------------------------
# تابع مهندسی ویژگی‌های فیزیکی و لگ‌ها (برای داده‌های ساعتی)
# --------------------------------------------------
add_engineered_features <- function(df) {
  if (nrow(df) == 0) return(df)
  
  df$timestamp <- as.POSIXct(df$timestamp, tz = "Asia/Tehran")
  df <- df %>% dplyr::arrange(timestamp)
  
  df$hour <- lubridate::hour(df$timestamp)
  df$day_of_year <- lubridate::yday(df$timestamp)
  
  # شاخص فیزیکی: تابش خورشیدی
  solar_angle <- sin(2 * pi * (df$hour - 6) / 24)
  solar_angle[solar_angle < 0] <- 0
  df$solar_rad <- solar_angle * (1 + cos(2 * pi * (df$day_of_year - 172) / 365))
  
  # شاخص فیزیکی: قانون بارش
  if ("humidity" %in% names(df) && "pressure" %in% names(df)) {
    df$rain_prob_rule <- as.numeric(df$humidity > 80 & df$pressure < 1010)
  } else {
    df$rain_prob_rule <- 0
  }
  
  # نرم‌سازی دما
  df$smooth_temperature <- zoo::rollmean(df$temperature, k = 3, fill = NA, align = "right")
  
  # لگ‌های ساعتی
  df$lag_1h  <- dplyr::lag(df$temperature, 1)
  df$lag_2h  <- dplyr::lag(df$temperature, 2)
  df$lag_3h  <- dplyr::lag(df$temperature, 3)
  df$lag_24h <- dplyr::lag(df$temperature, 24)
  df$lag_48h <- dplyr::lag(df$temperature, 48)
  df$lag_72h <- dplyr::lag(df$temperature, 72)
  
  # میانگین‌های متحرک ساعتی
  df$rolling_6h  <- zoo::rollmean(df$temperature, k = 6,  fill = NA, align = "right")
  df$rolling_24h <- zoo::rollmean(df$temperature, k = 24, fill = NA, align = "right")
  
  return(df)
}

# --------------------------------------------------
# دانلود از ابتدا و ذخیره ۵ شهر با ویژگی‌های جدید
# --------------------------------------------------
download_all_weather_data <- function(full_start_date = "2021-01-01", out_dir = "data") {
  if (!exists("STATIONS")) stop("STATIONS تعریف نشده است.")
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  cities <- names(STATIONS)
  end_date <- Sys.Date()
  all_data <- list()
  
  for (city in cities) {
    cat("\n==============================\nDownloading city:", city, "\n")
    
    new_df <- tryCatch(
      download_historical_data(city, full_start_date, end_date),
      error = function(e) { message(e$message); NULL }
    )
    
    if (!is.null(new_df) && nrow(new_df) > 0) {
      new_df <- add_engineered_features(new_df)
      file_name <- file.path(out_dir, paste0("weather_", city, ".csv"))
      write.csv(new_df, file_name, row.names = FALSE)
      cat("Saved:", file_name, "\n")
      all_data[[city]] <- new_df
    }
  }
  
  if (length(all_data) > 0) {
    df_combined <- do.call(rbind, all_data)
    write.csv(df_combined, file.path(out_dir, "weather_all_cities_latest.csv"), row.names = FALSE)
  }
  
  return(invisible(all_data))
}

# --------------------------------------------------
# پاکسازی و آماده‌سازی داده‌ها
# --------------------------------------------------
prepare_ts_data <- function(df, variable = "temperature", freq = 365) {
  df <- df %>% dplyr::arrange(date)
  values <- df[[variable]]
  if (any(is.na(values))) {
    values <- zoo::na.approx(values, na.rm = FALSE)
    values[is.na(values)] <- mean(values, na.rm = TRUE)
  }
  ts(values, frequency = freq)
}

add_calendar_features <- function(df) {
  df %>% dplyr::mutate(
    day_of_year = lubridate::yday(date),
    month       = lubridate::month(date),
    week        = lubridate::week(date),
    day_of_week = lubridate::wday(date),
    year        = lubridate::year(date),
    sin_annual  = sin(2 * pi * day_of_year / 365),
    cos_annual  = cos(2 * pi * day_of_year / 365),
    sin_weekly  = sin(2 * pi * day_of_week / 7),
    cos_weekly  = cos(2 * pi * day_of_week / 7)
  )
}
# ── ساخت ماتریس ویژگی برای مدل‌های ML (روزانه - ارتقا یافته نهایی) ──────────
build_feature_matrix <- function(df, target = "temperature", lag_days = c(1, 2, 3, 7, 14)) {
  df <- df %>% dplyr::arrange(date) %>% add_calendar_features()
  
  # نرم‌سازی دمای روزانه
  df[[paste0("smooth_", target)]] <- zoo::rollmean(df[[target]], k = 3, fill = NA, align = "right")
  
  # لگ‌های روزانه
  for (lag in lag_days) {
    df[[paste0("lag_", lag)]] <- dplyr::lag(df[[target]], lag)
  }
  
  # میانگین‌های متحرک روزانه (هماهنگ با modeling_utils.R)
  df$rolling_mean_7  <- zoo::rollmean(df[[target]], k = 7,  fill = NA, align = "right")
  df$rolling_mean_14 <- zoo::rollmean(df[[target]], k = 14, fill = NA, align = "right")
  df$rolling_mean_30 <- zoo::rollmean(df[[target]], k = 30, fill = NA, align = "right")
  df$rolling_sd_7    <- zoo::rollapply(df[[target]], width = 7, FUN = sd, fill = NA, align = "right")
  
  # ویژگی فیزیکی: تغییرات فشار هوا
  if ("pressure" %in% names(df)) {
    df$pressure_delta <- c(NA, diff(df$pressure))
    df$pressure_delta[is.na(df$pressure_delta)] <- 0
  }
  
  df %>% tidyr::drop_na()
}

# ── ساخت ماتریس ویژگی برای مدل‌های ML (ساعتی) ──────────────────────────────
build_hourly_feature_matrix <- function(df, target = "temperature", lag_hours = c(1, 2, 3, 24, 48, 72)) {
  df <- df %>% dplyr::arrange(timestamp) %>% dplyr::mutate(
    hour = lubridate::hour(timestamp), hour_sin = sin(2 * pi * hour / 24), hour_cos = cos(2 * pi * hour / 24),
    day_of_week = lubridate::wday(timestamp),year = lubridate::year(timestamp),day_of_year = lubridate::yday(timestamp), month = lubridate::month(timestamp),
    sin_annual = sin(2 * pi * day_of_year / 365), cos_annual = cos(2 * pi * day_of_year / 365),
    sin_weekly = sin(2 * pi * day_of_week / 7), cos_weekly = cos(2 * pi * day_of_week / 7)
  )
  
  solar_angle <- sin(2 * pi * (df$hour - 6) / 24)
  solar_angle[solar_angle < 0] <- 0
  df$solar_rad <- solar_angle * (1 + cos(2 * pi * (df$day_of_year - 172) / 365))
  
  if ("humidity" %in% names(df) && "pressure" %in% names(df)) {
    df$rain_prob_rule <- as.numeric(df$humidity > 80 & df$pressure < 1010)
  } else {
    df$rain_prob_rule <- 0
  }
  
  df[[paste0("smooth_", target)]] <- zoo::rollmean(df[[target]], k = 3, fill = NA, align = "right")
  
  for (lg in lag_hours) {
    df[[paste0("lag_", lg, "h")]] <- dplyr::lag(df[[target]], lg)
  }
  
  df$rolling_mean_6h  <- zoo::rollmean(df[[target]], k = 6,  fill = NA, align = "right")
  df$rolling_mean_24h <- zoo::rollmean(df[[target]], k = 24, fill = NA, align = "right")
  
  if ("pressure" %in% names(df)) {
    df$pressure_delta_3h <- c(NA, NA, NA, diff(df$pressure, lag = 3))
    df$pressure_delta_3h[is.na(df$pressure_delta_3h)] <- 0
  }
  
  df %>% tidyr::drop_na()
}

HOURLY_FEAT_COLS <- c(
  "hour", "hour_sin", "hour_cos", "day_of_week", "month", "sin_annual", "cos_annual", "sin_weekly", "cos_weekly",
  "solar_rad", "rain_prob_rule", 
  "smooth_temperature", "smooth_humidity", "smooth_wind_speed", "smooth_precipitation",
  "lag_1h", "lag_2h", "lag_3h", "lag_24h", "lag_48h", "lag_72h", 
  "rolling_mean_6h", "rolling_mean_24h", "pressure_delta_3h",
  "temperature", "humidity", "wind_speed", "precipitation"
)

prepare_prophet_data <- function(df, target = "temperature") {
  df %>% dplyr::arrange(date) %>% dplyr::select(ds = date, y = dplyr::all_of(target)) %>% dplyr::mutate(ds = as.POSIXct(ds))
}

build_future_features <- function(last_date, horizon, df_train, target = "temperature") {
  future_dates <- seq.Date(as.Date(last_date) + 1, by = "day", length.out = horizon)
  future_df <- tibble::tibble(date = future_dates) %>% add_calendar_features()
  
  last_vals <- tail(df_train[[target]], 30)
  fallback_val <- if (length(last_vals) > 0) mean(last_vals, na.rm = TRUE) else 0
  
  for (lag in c(1, 2, 3, 7, 14)) {
    lag_val <- if (lag <= length(last_vals)) rev(last_vals)[lag] else fallback_val
    if (is.na(lag_val)) lag_val <- fallback_val
    future_df[[paste0("lag_", lag)]] <- rep(lag_val, horizon)
  }
  
  rm7  <- mean(tail(df_train[[target]], 7),  na.rm = TRUE)
  rm30 <- mean(tail(df_train[[target]], 30), na.rm = TRUE)
  if (is.na(rm7))  rm7  <- fallback_val
  if (is.na(rm30)) rm30 <- fallback_val
  
  future_df$rolling_mean_7  <- rep(rm7,  horizon)
  future_df$rolling_mean_30 <- rep(rm30, horizon)
  future_df[[paste0("smooth_", target)]] <- rep(rm7, horizon)
  future_df$rolling_sd_7 <- rep(sd(tail(df_train[[target]], 7), na.rm = TRUE), horizon)
  if ("pressure" %in% names(df_train)) future_df$pressure_delta <- rep(0, horizon)
  
  future_df
}

station_summary <- function(df) {
  list(
    n_days = nrow(df), date_range = range(df$date, na.rm = TRUE),
    temp_mean = round(mean(df$temperature, na.rm = TRUE), 1), temp_min = round(min(df$temperature, na.rm = TRUE), 1), temp_max = round(max(df$temperature, na.rm = TRUE), 1),
    humidity_avg = round(mean(df$humidity, na.rm = TRUE), 1), precip_total = round(sum(df$precipitation, na.rm = TRUE), 1), wind_avg = round(mean(df$wind_speed, na.rm = TRUE), 1)
  )
}

advanced_clean_data <- function(df) {
  full_dates <- seq.Date(min(df$date), max(df$date), by = "day")
  df <- df %>% dplyr::right_join(data.frame(date = full_dates), by = "date") %>% dplyr::arrange(date)
  df$month <- lubridate::month(df$date)
  
  remove_iqr_outliers <- function(x, month, k = 1.5) {
    for (m in unique(month[!is.na(month)])) {
      idx <- which(month == m)
      val_month <- x[idx]
      Q1 <- stats::quantile(val_month, 0.25, na.rm = TRUE)
      Q3 <- stats::quantile(val_month, 0.75, na.rm = TRUE)
      IQR_val <- Q3 - Q1
      lower_bound <- Q1 - (k * IQR_val)
      upper_bound <- Q3 + (k * IQR_val)
      x[idx][val_month < lower_bound | val_month > upper_bound] <- NA
    }
    return(x)
  }
  
  df$temperature   <- remove_iqr_outliers(df$temperature, df$month)
  df$humidity      <- remove_iqr_outliers(df$humidity, df$month)
  df$wind_speed    <- remove_iqr_outliers(df$wind_speed, df$month)
  df$precipitation <- remove_iqr_outliers(df$precipitation, df$month)
  
  df$temperature   <- zoo::na.approx(df$temperature, na.rm = FALSE)
  df$humidity      <- zoo::na.approx(df$humidity, na.rm = FALSE)
  df$precipitation[is.na(df$precipitation)] <- 0
  df$wind_speed <- zoo::na.fill(df$wind_speed, "extend")
  
  df$month <- NULL
  return(df)
}