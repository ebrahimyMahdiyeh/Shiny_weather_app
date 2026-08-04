# ==============================
# Open-Meteo weather downloader
# Save all 5 cities to CSV
# اصلاح‌شده: افزودن تبریز + بروزرسانی افزایشی (Incremental Update)
# ==============================

library(httr)
library(jsonlite)

# --------------------------------------------------
# پیدا کردن پوشه data به‌صورت خودکار
# این تابع سعی می‌کند پوشه shiny_app_new/data را پیدا کند
# حتی اگر api_functions.R در پوشه parent اجرا شود.
# --------------------------------------------------
find_data_dir <- function() {
  
  # 1) اگر global.R قبلاً source شده و DATA_DIR تعریف شده، از همان استفاده کن
  if (exists("DATA_DIR", envir = globalenv())) {
    d <- get("DATA_DIR", envir = globalenv())
    if (!is.null(d) && dir.exists(d)) return(normalizePath(d, mustWork = FALSE))
  }
  
  # 2) تلاش برای پیدا کردن پوشه اپ
  wd <- tryCatch(getwd(), error = function(e) ".")
  
  candidates <- c(
    file.path(wd, "data"),                              # کنار خودمون
    file.path(wd, "shiny_app_new", "data"),             # اگر api_functions.R در parent اجرا میشه
    file.path(dirname(wd), "shiny_app_new", "data"),    # یکی بالاتر
    file.path(dirname(wd), "data")                      # fallback
  )
  
  # 3) اولین کاندید موجود را برگردان
  for (cand in candidates) {
    if (dir.exists(cand)) {
      return(normalizePath(cand, mustWork = FALSE))
    }
  }
  
  # 4) اگر هیچ کدوم نبود، اولین کاندید را بساز
  target <- candidates[1]
  dir.create(target, showWarnings = FALSE, recursive = TRUE)
  message("[find_data_dir] پوشه data ساخته شد در: ", normalizePath(target, mustWork = FALSE))
  return(normalizePath(target, mustWork = FALSE))
}

# --------------------------------------------------
# Download historical + recent weather from Open-Meteo
# --------------------------------------------------
download_historical_data <- function(station_id, start_date, end_date) {
  
  # ── مختصات ایستگاه‌ها ─────────────────────────────────────────────────────
  coords <- list(
    tehran  = c(lat = 35.6892, lon = 51.3890),
    isfahan = c(lat = 32.6546, lon = 51.6680),
    mashhad = c(lat = 36.2605, lon = 59.6168),
    shiraz  = c(lat = 29.5918, lon = 52.5837),
    tabriz  = c(lat = 38.0962, lon = 46.2738)
  )
  
  if (!station_id %in% names(coords)) {
    stop(paste("Unknown station_id:", station_id))
  }
  
  lat <- coords[[station_id]]["lat"]
  lon <- coords[[station_id]]["lon"]
  
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  if (is.na(start_date) || is.na(end_date)) {
    stop("Invalid start_date or end_date")
  }
  
  if (start_date > end_date) {
    stop("start_date cannot be greater than end_date")
  }
  
  today <- Sys.Date()
  dfs <- list()
  
  # -------------------------------
  # 1) Archive API
  # -------------------------------
  archive_start <- start_date
  archive_end   <- min(end_date, today - 6)
  
  if (archive_start <= archive_end) {
    
    url_archive <- paste0(
      "https://archive-api.open-meteo.com/v1/archive",
      "?latitude=", lat,
      "&longitude=", lon,
      "&start_date=", format(archive_start, "%Y-%m-%d"),
      "&end_date=", format(archive_end, "%Y-%m-%d"),
      "&hourly=temperature_2m,relative_humidity_2m,",
      "wind_speed_10m,surface_pressure,precipitation,weather_code",
      "&timezone=Asia%2FTehran"
    )
    
    resp <- httr::GET(url_archive, httr::timeout(60))
    
    if (httr::http_error(resp)) {
      stop(paste(
        "Archive API error for", station_id, ":",
        httr::status_code(resp),
        httr::content(resp, "text", encoding = "UTF-8")
      ))
    }
    
    raw <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    
    if (!is.null(raw$hourly) && length(raw$hourly$time) > 0) {
      h <- raw$hourly
      
      df_a <- data.frame(
        timestamp     = as.POSIXct(h$time, format = "%Y-%m-%dT%H:%M", tz = "Asia/Tehran"),
        temperature   = as.numeric(h$temperature_2m),
        humidity      = as.numeric(h$relative_humidity_2m),
        wind_speed    = as.numeric(h$wind_speed_10m),
        pressure      = as.numeric(h$surface_pressure),
        precipitation = as.numeric(h$precipitation),
        w_code        = as.integer(h$weather_code),
        station_id    = station_id,
        stringsAsFactors = FALSE
      )
      
      dfs[[length(dfs) + 1]] <- df_a
    }
  }
  
  # -------------------------------
  # 2) Recent data from Forecast API
  # -------------------------------
  recent_start <- max(start_date, today - 16)
  
  if (end_date >= recent_start) {
    
    past_days <- as.integer(today - recent_start) + 1
    past_days <- min(past_days, 92)
    
    url_recent <- paste0(
      "https://api.open-meteo.com/v1/forecast",
      "?latitude=", lat,
      "&longitude=", lon,
      "&past_days=", past_days,
      "&hourly=temperature_2m,relative_humidity_2m,",
      "wind_speed_10m,surface_pressure,precipitation,weather_code",
      "&timezone=Asia%2FTehran"
    )
    
    resp2 <- httr::GET(url_recent, httr::timeout(60))
    
    if (!httr::http_error(resp2)) {
      raw2 <- jsonlite::fromJSON(httr::content(resp2, "text", encoding = "UTF-8"))
      
      if (!is.null(raw2$hourly) && length(raw2$hourly$time) > 0) {
        h2 <- raw2$hourly
        
        df_r <- data.frame(
          timestamp     = as.POSIXct(h2$time, format = "%Y-%m-%dT%H:%M", tz = "Asia/Tehran"),
          temperature   = as.numeric(h2$temperature_2m),
          humidity      = as.numeric(h2$relative_humidity_2m),
          wind_speed    = as.numeric(h2$wind_speed_10m),
          pressure      = as.numeric(h2$surface_pressure),
          precipitation = as.numeric(h2$precipitation),
          w_code        = as.integer(h2$weather_code),
          station_id    = station_id,
          stringsAsFactors = FALSE
        )
        
        df_r <- df_r[
          as.Date(df_r$timestamp) >= start_date &
            as.Date(df_r$timestamp) <= end_date,
        ]
        
        dfs[[length(dfs) + 1]] <- df_r
      }
    }
  }
  
  if (length(dfs) == 0) {
    stop(paste("No data retrieved for", station_id))
  }
  
  df_all <- do.call(rbind, dfs)
  df_all <- df_all[order(df_all$timestamp), ]
  df_all <- df_all[!duplicated(df_all$timestamp), ]
  df_all <- df_all[!is.na(df_all$temperature), ]
  
  if (nrow(df_all) == 0) {
    stop(paste("No valid rows after cleaning for", station_id))
  }
  
  temp_range <- range(df_all$temperature, na.rm = TRUE)
  
  message(sprintf(
    "[API] %s: %d rows from %s to %s | temp %.1f to %.1f \u00b0C",
    station_id,
    nrow(df_all),
    format(min(df_all$timestamp), "%Y-%m-%d"),
    format(max(df_all$timestamp), "%Y-%m-%d"),
    temp_range[1],
    temp_range[2]
  ))
  
  return(df_all)
}

# --------------------------------------------------
# Update and save all 5 cities (Incremental Hourly Update)
# این تابع فایل قبلی را بررسی میکند و فقط ساعات جدید را دریافت و اضافه میکند
# --------------------------------------------------
update_weather_data <- function(full_start_date = "2021-01-01", out_dir = NULL) {
  
  if (is.null(out_dir)) {
    out_dir <- find_data_dir()
  }
  
  message("[update_weather_data] پوشه خروجی: ", out_dir)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  cities <- c("tehran", "isfahan", "mashhad", "shiraz", "tabriz")
  end_date <- Sys.Date()
  all_updated_data <- list()
  
  for (city in cities) {
    cat("\n==============================\n")
    cat("Checking city:", city, "\n")
    
    # نام فایل ثابت برای هر شهر
    file_name <- file.path(out_dir, paste0("weather_", city, ".csv"))
    
    # ── ۱. اگر فایل از قبل وجود داشت (بروزرسانی) ──
    if (file.exists(file_name)) {
      cat("Existing file found. Reading last timestamp...\n")
      
      existing_df <- read.csv(file_name, stringsAsFactors = FALSE)
      existing_df$timestamp <- as.POSIXct(existing_df$timestamp, tz = "Asia/Tehran")
      
      # پیدا کردن آخرین زمانی (ساعت) که داده داریم
      last_ts <- max(existing_df$timestamp, na.rm = TRUE)
      last_date <- as.Date(last_ts)
      
      # اگر آخرین رکورد کمتر از ۱ ساعت پیش است، یعنی آپدیت است
      if (last_ts >= (Sys.time() - 3600)) {
        cat("Data is already up to date for", city, "\n")
        all_updated_data[[city]] <- existing_df
        next
      }
      
      # از همان روز آخرین رکورد شروع میکنیم تا ساعات جدید آن روز را هم بگیریم
      start_d <- last_date
      cat("Updating from", as.character(start_d), "to", as.character(end_date), "\n")
      
      new_df <- tryCatch(
        download_historical_data(city, start_d, end_date),
        error = function(e) {
          message(sprintf("[ERROR] %s -> %s", city, e$message))
          return(NULL)
        }
      )
      
      if (!is.null(new_df) && nrow(new_df) > 0) {
        # ترکیب داده‌های قدیمی و جدید
        combined_df <- rbind(existing_df, new_df)
        combined_df <- combined_df[order(combined_df$timestamp), ]
        # حذف رکوردهای تکراری بر اساس زمان (نگه داشتن جدیدترین رکورد برای هر ساعت)
        combined_df <- combined_df[!duplicated(combined_df$timestamp, fromLast = TRUE), ]
        
        write.csv(combined_df, file_name, row.names = FALSE)
        cat("Appended new hours and saved to:", file_name, "\n")
        all_updated_data[[city]] <- combined_df
      } else {
        cat("No new data found to append for", city, "\n")
        all_updated_data[[city]] <- existing_df
      }
      
    } else {
      # ── ۲. اگر فایل وجود نداشت (اولین بار) ──
      cat("No existing file. Doing full download from", full_start_date, "\n")
      
      new_df <- tryCatch(
        download_historical_data(city, full_start_date, end_date),
        error = function(e) {
          message(sprintf("[ERROR] %s -> %s", city, e$message))
          return(NULL)
        }
      )
      
      if (!is.null(new_df) && nrow(new_df) > 0) {
        write.csv(new_df, file_name, row.names = FALSE)
        cat("Saved new file:", file_name, "\n")
        all_updated_data[[city]] <- new_df
      }
    }
  }
  
  # ذخیره فایل ترکیبی همه شهرها
  if (length(all_updated_data) > 0) {
    df_combined <- do.call(rbind, all_updated_data)
    combined_file <- file.path(out_dir, "weather_all_cities_latest.csv")
    write.csv(df_combined, combined_file, row.names = FALSE)
    message("\nCombined file updated: ", combined_file)
  }
  
  return(invisible(all_updated_data))
}

# --------------------------------------------------
# RUN
# --------------------------------------------------
# هر بار که این فایل را Run کنید، فقط داده‌های ساعات جدید گرفته شده و به فایل اضافه میشود
if (interactive()) {
  result <- update_weather_data(
    full_start_date = "2021-01-01" # این تاریخ فقط برای اولین بار استفاده می‌شود
  )
}