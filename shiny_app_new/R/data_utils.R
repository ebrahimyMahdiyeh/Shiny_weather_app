# File: R/data_utils.R
# ابزارهای مدیریت و دریافت داده هواشناسی

# ── دریافت داده تاریخی از Open-Meteo ────────────────────────────────────────────
# اصلاح‌شده: رفع ناسازگاری نام متغیرها (start_d/start_date)،
# حذف تعریف تکراری تابع، و افزودن بررسی وجود STATIONS
download_historical_data <- function(station_id, start_d, end_d) {

  # بررسی وجود شیء STATIONS در محیط
  if (!exists("STATIONS")) {
    stop("متغیر سراسری STATIONS تعریف نشده است. ابتدا global.R را اجرا کنید.")
  }

  # بررسی وجود ایستگاه در STATIONS
  if (!station_id %in% names(STATIONS)) {
    stop("ایستگاه '", station_id, "' در STATIONS یافت نشد.")
  }

  station <- STATIONS[[station_id]]
  lat     <- station$lat
  lon     <- station$lon

  # ساخت URL با نام صحیح پارامترها (start_d و end_d — نه start_date/end_date)
  base_url <- "https://archive-api.open-meteo.com/v1/archive"

  url <- paste0(
    base_url,
    "?latitude=",  lat,
    "&longitude=", lon,
    "&start_date=", start_d,   # اصلاح: از start_date به start_d
    "&end_date=",   end_d,     # اصلاح: از end_date به end_d
    "&hourly=temperature_2m,relative_humidity_2m,wind_speed_10m,",
    "surface_pressure,weather_code,precipitation",
    "&timezone=UTC"
  )

  message("در حال دریافت داده برای ایستگاه: ", station$name, " ...")

  # ارسال درخواست HTTP
  response <- httr::GET(url, httr::timeout(60))

  if (httr::status_code(response) != 200) {
    stop(
      "خطای API برای ایستگاه ", station$name,
      ": کد وضعیت ", httr::status_code(response)
    )
  }

  # تجزیه محتوای JSON
  content_json <- httr::content(response, "text", encoding = "UTF-8")
  data_list    <- jsonlite::fromJSON(content_json, simplifyDataFrame = FALSE)

  # استخراج داده‌های ساعتی
  hourly_data <- data_list$hourly

  # لاگ بررسی داده خام
  message("--- بررسی داده خام API ---")
  message("تعداد ردیف‌های دریافتی: ", length(hourly_data$time))
  message("۵ نمونه اول زمان: ", paste(utils::head(hourly_data$time, 5), collapse = ", "))
  message("---------------------------")

  # تبدیل زمان به POSIXct
  raw_time      <- hourly_data$time
  timestamp_vec <- as.POSIXct(raw_time, tz = "UTC", format = "%Y-%m-%dT%H:%M")

  # تبدیل به تایم‌زون تهران
  timestamp_vec <- lubridate::with_tz(timestamp_vec, tzone = "Asia/Tehran")

  # ساخت tibble خروجی
  df <- tibble::tibble(
    timestamp     = timestamp_vec,
    temperature   = as.numeric(hourly_data$temperature_2m),
    humidity      = as.numeric(hourly_data$relative_humidity_2m),
    wind_speed    = as.numeric(hourly_data$wind_speed_10m),
    pressure      = as.numeric(hourly_data$surface_pressure),
    precipitation = as.numeric(hourly_data$precipitation),
    w_code        = as.integer(hourly_data$weather_code)
  )

  # حذف ردیف‌های با زمان NA
  df <- df %>% dplyr::filter(!is.na(timestamp))

  message("دریافت موفق: ", nrow(df), " ردیف برای ایستگاه ", station$name)
  return(df)
}

# ── اجرای دانلود دسته‌جمعی ──────────────────────────────────────────────────────
# این تابع را برای به‌روزرسانی فایل historical_weather.rds فراخوانی کنید
run_bulk_download <- function(start_d = "2021-01-01",
                               end_d   = format(Sys.Date() - 1, "%Y-%m-%d"),
                               output_path = "data/historical_weather.rds") {

  if (!exists("STATIONS")) {
    stop("متغیر STATIONS تعریف نشده است.")
  }

  # ساخت پوشه data در صورت نبود
  if (!dir.exists(dirname(output_path))) {
    dir.create(dirname(output_path), recursive = TRUE)
  }

  message("شروع دانلود برای ", length(STATIONS), " ایستگاه...")

  historical_data_list <- purrr::map(names(STATIONS), function(sid) {
    tryCatch(
      download_historical_data(sid, start_d, end_d),
      error = function(e) {
        message("خطا در دانلود ایستگاه ", sid, ": ", e$message)
        NULL
      }
    )
  }) |> purrr::set_names(names(STATIONS))

  # حذف ایستگاه‌های ناموفق
  historical_data_list <- purrr::compact(historical_data_list)

  if (length(historical_data_list) == 0) {
    stop("هیچ داده‌ای دانلود نشد.")
  }

  saveRDS(historical_data_list, output_path)
  message("داده‌ها در '", output_path, "' ذخیره شدند.")
  return(invisible(historical_data_list))
}

# ── آماده‌سازی سری زمانی برای مدل‌سازی ─────────────────────────────────────────
prepare_ts_data <- function(df, variable = "temperature", freq = 365) {
  # اطمینان از مرتب‌سازی بر اساس تاریخ
  df <- df %>% dplyr::arrange(date)

  # استخراج مقادیر هدف
  values <- df[[variable]]

  # جایگذاری NA با میانگین متحرک
  if (any(is.na(values))) {
    values <- zoo::na.approx(values, na.rm = FALSE)
    values[is.na(values)] <- mean(values, na.rm = TRUE)
  }

  # ساخت شیء ts
  ts_obj <- ts(values, frequency = freq)
  return(ts_obj)
}

# ── افزودن ویژگی‌های تقویمی برای ML ───────────────────────────────────────────
add_calendar_features <- function(df) {
  df %>%
    dplyr::mutate(
      day_of_year = lubridate::yday(date),
      month       = lubridate::month(date),
      week        = lubridate::week(date),
      day_of_week = lubridate::wday(date),
      year        = lubridate::year(date),
      # ویژگی‌های چرخه‌ای (Fourier)
      sin_annual  = sin(2 * pi * day_of_year / 365),
      cos_annual  = cos(2 * pi * day_of_year / 365),
      sin_weekly  = sin(2 * pi * day_of_week / 7),
      cos_weekly  = cos(2 * pi * day_of_week / 7)
    )
}

# ── ساخت ماتریس ویژگی برای مدل‌های ML ─────────────────────────────────────────
# ── ساخت ماتریس ویژگی برای مدل‌های ML (روزانه - ارتقا یافته) ──────────────────
build_feature_matrix <- function(df, target = "temperature",
                                 lag_days = c(1, 2, 3, 7, 14)) {
  df <- df %>%
    dplyr::arrange(date) %>%
    add_calendar_features()
  
  # ── [جدید] نرم‌سازی نویز (Noise Reduction) طبق مقاله ──
  # ایجاد یک نسخه نرم‌شده از متغیر هدف با میانگین متحرک ۳ روزه
  smooth_col <- paste0("smooth_", target)
  df[[smooth_col]] <- zoo::rollmean(df[[target]], k = 3, fill = NA, align = "right")
  
  # افزودن lag features
  for (lag in lag_days) {
    col_name <- paste0("lag_", lag)
    df[[col_name]] <- dplyr::lag(df[[target]], lag)
  }
  
  # میانگین متحرک اصلی
  df$rolling_mean_7  <- zoo::rollmean(df[[target]], k = 7,  fill = NA, align = "right")
  df$rolling_mean_30 <- zoo::rollmean(df[[target]], k = 30, fill = NA, align = "right")
  
  # ── [جدید] انحراف معیار متحرک (نشانگر نوسانات جوی) ──
  df$rolling_sd_7 <- zoo::rollapply(df[[target]], width = 7, FUN = sd, fill = NA, align = "right")
  
  # ── [جدید] ویژگی فیزیکی: تغییرات فشار هوا ──
  if ("pressure" %in% names(df)) {
    df$pressure_delta <- c(NA, diff(df$pressure))
    df$pressure_delta[is.na(df$pressure_delta)] <- 0
  }
  
  # حذف ردیف‌های با NA
  df <- df %>% tidyr::drop_na()
  return(df)
}
# ── ساخت ماتریس ویژگی برای مدل‌های ML (ساعتی - جدید) ──────────────────────────
# ── ساخت ماتریس ویژگی برای مدل‌های ML (ساعتی + ویژگی‌های فیزیکی مقاله) ───────
build_hourly_feature_matrix <- function(df, target = "temperature",
                                        lag_hours = c(1, 2, 3, 24, 48, 72)) {
  
  df <- df %>%
    dplyr::arrange(timestamp) %>%
    dplyr::mutate(
      hour        = lubridate::hour(timestamp),
      hour_sin    = sin(2 * pi * hour / 24),
      hour_cos    = cos(2 * pi * hour / 24),
      day_of_week = lubridate::wday(timestamp),
      day_of_year = lubridate::yday(timestamp),
      month       = lubridate::month(timestamp),
      sin_annual  = sin(2 * pi * day_of_year / 365),
      cos_annual  = cos(2 * pi * day_of_year / 365),
      sin_weekly  = sin(2 * pi * day_of_week / 7),
      cos_weekly  = cos(2 * pi * day_of_week / 7)
    )
  
  # ── [جدید] ویژگی فیزیکی ۱: تخمین تابش خورشیدی (Solar Radiation Proxy) ──
  # طبق مقاله: تابش خورشید عامل اصلی گرمایش روزانه است
  # در شب صفر، در ظهر حداکثر، و با توجه به روزهای سال تغییر می‌کند
  solar_angle <- sin(2 * pi * (hour - 6) / 24) # شروع تابش از ساعت 6 صبح
  solar_angle[solar_angle < 0] <- 0 # در شب تابش وجود ندارد
  df$solar_rad <- solar_angle * (1 + cos(2 * pi * (day_of_year - 172) / 365)) # تابش بیشتر در تابستان
  
  # ── [جدید] ویژگی فیزیکی ۲: شاخص قانون‌مند بارش (Rule-Based Rain Index) ──
  # طبق مقاله: رطوبت > 80٪ و فشار < 1010 نشان‌دهنده احتمال بالای بارش است
  if ("humidity" %in% names(df) && "pressure" %in% names(df)) {
    df$rain_prob_rule <- as.numeric(df$humidity > 80 & df$pressure < 1010)
  } else {
    df$rain_prob_rule <- 0
  }
  
  # نرم‌سازی نویز
  df[[paste0("smooth_", target)]] <- zoo::rollmean(df[[target]], k = 3, fill = NA, align = "right")
  
  # افزودن Lag های ساعتی
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

# آپدیت لیست ستون‌ها
HOURLY_FEAT_COLS <- c(
  "hour", "hour_sin", "hour_cos",
  "day_of_week", "month", "sin_annual", "cos_annual", "sin_weekly", "cos_weekly",
  "solar_rad", "rain_prob_rule", # ویژگی‌های فیزیکی جدید اضافه شد
  "smooth_temperature", "smooth_humidity", "smooth_wind_speed", "smooth_precipitation",
  "lag_1h", "lag_2h", "lag_3h", "lag_24h", "lag_48h", "lag_72h",
  "rolling_mean_6h", "rolling_mean_24h", "pressure_delta_3h",
  "temperature", "humidity", "wind_speed", "precipitation"
)

# لیست ستون‌های ساعتی برای استفاده در modeling_utils.R
HOURLY_FEAT_COLS <- c(
  "hour", "hour_sin", "hour_cos",
  "day_of_week", "month", "sin_annual", "cos_annual", "sin_weekly", "cos_weekly",
  "smooth_temperature", "smooth_humidity", "smooth_wind_speed", "smooth_precipitation",
  "lag_1h", "lag_2h", "lag_3h", "lag_24h", "lag_48h", "lag_72h",
  "rolling_mean_6h", "rolling_mean_24h", "pressure_delta_3h",
  "temperature", "humidity", "wind_speed", "precipitation"
)
# ── آماده‌سازی داده Prophet ────────────────────────────────────────────────────
prepare_prophet_data <- function(df, target = "temperature") {
  df %>%
    dplyr::arrange(date) %>%
    dplyr::select(ds = date, y = dplyr::all_of(target)) %>%
    dplyr::mutate(ds = as.POSIXct(ds))
}

# ── آماده‌سازی داده آینده برای پیش‌بینی ─────────────────────────────────────────
# نکته مهم: تمام ردیف‌های افق آینده باید مقدار غیر-NA داشته باشند،
# در غیر این صورت predict() برای Random Forest / SVM / XGBoost خطا می‌دهد.
# چون مقادیر واقعی آینده را نمی‌دانیم، آخرین مقادیر شناخته‌شده را برای
# تمام ردیف‌های افق تکرار می‌کنیم (پیش‌بینی مستقیم/direct، نه recursive).
# ── آماده‌سازی داده آینده برای پیش‌بینی (روزانه - ارتقا یافته) ──────────────────
build_future_features <- function(last_date, horizon, df_train, target = "temperature") {
  future_dates <- seq.Date(as.Date(last_date) + 1,
                           by = "day",
                           length.out = horizon)
  
  future_df <- tibble::tibble(date = future_dates) %>%
    add_calendar_features()
  
  last_vals <- tail(df_train[[target]], max(14, 30))
  last_vals <- last_vals[!is.na(last_vals)]
  
  fallback_val <- if (length(last_vals) > 0) {
    mean(last_vals, na.rm = TRUE)
  } else {
    mean(df_train[[target]], na.rm = TRUE)
  }
  if (is.na(fallback_val)) fallback_val <- 0
  
  for (lag in c(1, 2, 3, 7, 14)) {
    col_name <- paste0("lag_", lag)
    lag_val <- if (lag <= length(last_vals)) {
      rev(last_vals)[lag]
    } else {
      fallback_val
    }
    if (is.na(lag_val)) lag_val <- fallback_val
    future_df[[col_name]] <- rep(lag_val, horizon)
  }
  
  rm7  <- mean(tail(df_train[[target]], 7),  na.rm = TRUE)
  rm30 <- mean(tail(df_train[[target]], 30), na.rm = TRUE)
  if (is.na(rm7))  rm7  <- fallback_val
  if (is.na(rm30)) rm30 <- fallback_val
  
  future_df$rolling_mean_7  <- rep(rm7,  horizon)
  future_df$rolling_mean_30 <- rep(rm30, horizon)
  
  # مقادیر پیش‌فرض برای ستون‌های جدید
  future_df[[paste0("smooth_", target)]] <- rep(rm7, horizon)
  future_df$rolling_sd_7 <- rep(sd(tail(df_train[[target]], 7), na.rm = TRUE), horizon)
  if ("pressure" %in% names(df_train)) {
    future_df$pressure_delta <- rep(0, horizon)
  }
  
  future_df
}

# ── خلاصه آماری ایستگاه ─────────────────────────────────────────────────────────
station_summary <- function(df) {
  list(
    n_days       = nrow(df),
    date_range   = range(df$date, na.rm = TRUE),
    temp_mean    = round(mean(df$temperature,   na.rm = TRUE), 1),
    temp_min     = round(min(df$temperature,    na.rm = TRUE), 1),
    temp_max     = round(max(df$temperature,    na.rm = TRUE), 1),
    humidity_avg = round(mean(df$humidity,      na.rm = TRUE), 1),
    precip_total = round(sum(df$precipitation,  na.rm = TRUE), 1),
    wind_avg     = round(mean(df$wind_speed,    na.rm = TRUE), 1)
  )
}

# ── پاکسازی عمیق و پیشرفته داده‌های سری زمانی ──────────────────────────────────
# ── پاکسازی عمیق و پیشرفته داده‌های سری زمانی (نسخه ارتقا یافته با IQR) ─────────
advanced_clean_data <- function(df) {
  full_dates <- seq.Date(min(df$date), max(df$date), by = "day")
  df <- df %>% 
    dplyr::right_join(data.frame(date = full_dates), by = "date") %>% 
    dplyr::arrange(date)
  
  df$month <- lubridate::month(df$date)
  
  # ── [ارتقا] تشخیص داده پرت با روش IQR طبق مقاله ──
  remove_iqr_outliers <- function(x, month, k = 1.5) {
    for (m in unique(month[!is.na(month)])) {
      idx <- which(month == m)
      val_month <- x[idx]
      
      Q1 <- stats::quantile(val_month, 0.25, na.rm = TRUE)
      Q3 <- stats::quantile(val_month, 0.75, na.rm = TRUE)
      IQR_val <- Q3 - Q1
      
      lower_bound <- Q1 - (k * IQR_val)
      upper_bound <- Q3 + (k * IQR_val)
      
      # داده‌های خارج از بازه را به NA تبدیل کن
      x[idx][val_month < lower_bound | val_month > upper_bound] <- NA
    }
    return(x)
  }
  
  df$temperature   <- remove_iqr_outliers(df$temperature, df$month)
  df$humidity      <- remove_iqr_outliers(df$humidity, df$month)
  df$wind_speed    <- remove_iqr_outliers(df$wind_speed, df$month)
  df$precipitation <- remove_iqr_outliers(df$precipitation, df$month)
  
  # جبران‌سازی هوشمند (Imputation)
  df$temperature   <- zoo::na.approx(df$temperature, na.rm = FALSE)
  df$humidity      <- zoo::na.approx(df$humidity, na.rm = FALSE)
  df$precipitation[is.na(df$precipitation)] <- 0
  df$wind_speed <- zoo::na.fill(df$wind_speed, "extend")
  
  df$month <- NULL
  return(df)
}
