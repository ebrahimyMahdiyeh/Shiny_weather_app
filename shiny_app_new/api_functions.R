# ==============================
# Open-Meteo Weather Downloader
# ==============================

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(zoo)


# --------------------------------------------------
# Find data directory
# --------------------------------------------------
find_data_dir <- function() {
  if (exists("DATA_DIR", envir = globalenv())) {
    d <- get("DATA_DIR", envir = globalenv())
    if (!is.null(d) && dir.exists(d)) return(normalizePath(d, mustWork = FALSE))
  }
  
  wd <- tryCatch(getwd(), error = function(e) ".")
  candidates <- c(
    file.path(wd, "data"),
    file.path(wd, "shiny_app_new", "data"),
    file.path(dirname(wd), "shiny_app_new", "data"),
    file.path(dirname(wd), "data")
  )
  
  for (cand in candidates) {
    if (dir.exists(cand)) return(normalizePath(cand, mustWork = FALSE))
  }
  
  target <- candidates[1]
  dir.create(target, showWarnings = FALSE, recursive = TRUE)
  return(normalizePath(target, mustWork = FALSE))
}

# --------------------------------------------------
# Download historical + recent weather from Open-Meteo
# --------------------------------------------------
download_historical_data <- function(station_id, start_date, end_date) {
  coords <- list(
    tehran  = c(lat = 35.6892, lon = 51.3890),
    isfahan = c(lat = 32.6546, lon = 51.6680),
    mashhad = c(lat = 36.2605, lon = 59.6168),
    shiraz  = c(lat = 29.5918, lon = 52.5837),
    tabriz  = c(lat = 38.0962, lon = 46.2738)
  )
  
  if (!station_id %in% names(coords)) stop(paste("Unknown station_id:", station_id))
  
  lat <- coords[[station_id]]["lat"]
  lon <- coords[[station_id]]["lon"]
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  today <- Sys.Date()
  dfs <- list()
  
  # 1) Archive API
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
  
  # 2) Recent data from Forecast API
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
        dfs[[length(dfs) + 1]] <- df_r[as.Date(df_r$timestamp) >= start_date & as.Date(df_r$timestamp) <= end_date, ]
      }
    }
  }
  
  if (length(dfs) == 0) stop(paste("No data retrieved for", station_id))
  
  df_all <- do.call(rbind, dfs)
  df_all <- df_all[order(df_all$timestamp, decreasing = TRUE), ]
  df_all <- df_all[!duplicated(df_all$timestamp), ]
  df_all <- df_all[order(df_all$timestamp), ]
  df_all <- df_all[!is.na(df_all$temperature), ]
  
  return(df_all)
}

# --------------------------------------------------
# Download from scratch and save all 5 cities
# --------------------------------------------------
download_all_weather_data <- function(full_start_date = "2021-01-01", out_dir = NULL) {
  
  if (is.null(out_dir)) out_dir <- find_data_dir()
  message("[download_all_weather_data] Output directory: ", out_dir)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  cities <- c("tehran", "isfahan", "mashhad", "shiraz", "tabriz")
  end_date <- Sys.Date()
  all_data <- list()
  
  for (city in cities) {
    cat("\n==============================\nDownloading city:", city, "\n")
    
    new_df <- tryCatch(
      download_historical_data(city, full_start_date, end_date),
      error = function(e) {
        message(sprintf("[ERROR] %s -> %s", city, e$message))
        return(NULL)
      }
    )
    
    if (!is.null(new_df) && nrow(new_df) > 0) {
      new_df <- add_engineered_features(new_df)
      new_df <- add_daily_min_max(new_df) # Added daily min/max
      
      # FIX MIDNIGHT BUG: Format as string to prevent dropping 00:00:00 in CSV
      new_df$timestamp <- format(new_df$timestamp, "%Y-%m-%d %H:%M:%S")
      
      file_name <- file.path(out_dir, paste0("weather_", city, ".csv"))
      write.csv(new_df, file_name, row.names = FALSE)
      cat("Generated features and saved new file:", file_name, "\n")
      all_data[[city]] <- new_df
    } else {
      cat("No data downloaded for", city, "\n")
    }
  }
  
  if (length(all_data) > 0) {
    df_combined <- do.call(rbind, all_data)
    combined_file <- file.path(out_dir, "weather_all_cities_latest.csv")
    write.csv(df_combined, combined_file, row.names = FALSE)
    message("\nCombined file saved: ", combined_file)
  }
  
  return(invisible(all_data))
}
# --------------------------------------------------
# Feature Engineering (Physical indices + Time-based Lags)
# --------------------------------------------------
add_engineered_features <- function(df) {
  if (nrow(df) == 0) return(df)
  
  df$timestamp <- as.POSIXct(df$timestamp, tz = "Asia/Tehran")
  df$temperature <- as.numeric(df$temperature)
  
  # 1. Create a complete hourly grid to ensure lags are time-based, not row-based
  min_t <- min(df$timestamp, na.rm = TRUE)
  max_t <- max(df$timestamp, na.rm = TRUE)
  full_time <- seq(min_t, max_t, by = "hour")
  full_df <- data.frame(timestamp = full_time)
  
  # Merge existing data into the complete grid
  df <- merge(full_df, df, by = "timestamp", all.x = TRUE)
  df <- df[order(df$timestamp), ]
  
  # 2. Interpolate missing values
  if (any(is.na(df$temperature))) {
    df$temperature <- zoo::na.approx(df$temperature, na.rm = FALSE)
    df$temperature <- zoo::na.locf(df$temperature, na.rm = FALSE)
    df$temperature <- zoo::na.locf(df$temperature, fromLast = TRUE, na.rm = FALSE)
  }
  
  df$hour <- lubridate::hour(df$timestamp)
  df$day_of_year <- lubridate::yday(df$timestamp)
  
  # Physical feature: Solar radiation approximation
  solar_angle <- sin(2 * pi * (df$hour - 6) / 24)
  solar_angle[solar_angle < 0] <- 0
  df$solar_rad <- solar_angle * (1 + cos(2 * pi * (df$day_of_year - 172) / 365))
  
  # Physical feature: Rain rule
  if ("humidity" %in% names(df) && "pressure" %in% names(df)) {
    df$rain_prob_rule <- as.numeric(!is.na(df$humidity) & !is.na(df$pressure) & df$humidity > 80 & df$pressure < 1010)
  } else {
    df$rain_prob_rule <- 0
  }
  
  # Smooth temperature
  df$smooth_temperature <- zoo::rollmean(df$temperature, k = 3, fill = NA, align = "right")
  
  # Time-accurate hourly lags
  df$lag_1h  <- dplyr::lag(df$temperature, 1)
  df$lag_2h  <- dplyr::lag(df$temperature, 2)
  df$lag_3h  <- dplyr::lag(df$temperature, 3)
  df$lag_24h <- dplyr::lag(df$temperature, 24)
  df$lag_48h <- dplyr::lag(df$temperature, 48)
  df$lag_72h <- dplyr::lag(df$temperature, 72)
  
  # Rolling means
  df$rolling_6h  <- zoo::rollmean(df$temperature, k = 6,  fill = NA, align = "right")
  df$rolling_24h <- zoo::rollmean(df$temperature, k = 24, fill = NA, align = "right")
  
  # Fill station_id if missing due to grid expansion
  if ("station_id" %in% names(df)) {
    df$station_id <- df$station_id[1]
  }
  
  return(df)
}

# --------------------------------------------------
# Add daily temp_max / temp_min (AFTER features)
# --------------------------------------------------
add_daily_min_max <- function(df) {
  if (nrow(df) == 0) return(df)
  
  df$timestamp <- as.POSIXct(df$timestamp, tz = "Asia/Tehran")
  df$date_only <- as.Date(df$timestamp, tz = "Asia/Tehran")
  
  daily_summary <- df %>%
    dplyr::group_by(date_only) %>%
    dplyr::summarise(
      temp_max = if (all(is.na(temperature))) NA_real_ else max(temperature, na.rm = TRUE),
      temp_min = if (all(is.na(temperature))) NA_real_ else min(temperature, na.rm = TRUE),
      .groups = "drop"
    )
  
  df$temp_max <- NULL
  df$temp_min <- NULL
  
  df <- dplyr::left_join(df, daily_summary, by = "date_only")
  df$date_only <- NULL
  
  return(df)
}

# --------------------------------------------------
# RUN
# --------------------------------------------------
if (interactive()) {
  result <- download_all_weather_data(full_start_date = "2021-01-01")
}