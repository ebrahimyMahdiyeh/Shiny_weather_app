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
build_feature_matrix <- function(df, target = "temperature",
                                  lag_days = c(1, 2, 3, 7)) {
  df <- df %>%
    dplyr::arrange(date) %>%
    add_calendar_features()

  # افزودن lag features
  for (lag in lag_days) {
    col_name <- paste0("lag_", lag)
    df[[col_name]] <- dplyr::lag(df[[target]], lag)
  }

  # میانگین متحرک
  df$rolling_mean_7  <- zoo::rollmean(df[[target]], k = 7,  fill = NA, align = "right")
  df$rolling_mean_30 <- zoo::rollmean(df[[target]], k = 30, fill = NA, align = "right")

  # حذف ردیف‌های با NA
  df <- df %>% tidyr::drop_na()
  return(df)
}

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
build_future_features <- function(last_date, horizon, df_train, target = "temperature") {
  future_dates <- seq.Date(as.Date(last_date) + 1,
                           by = "day",
                           length.out = horizon)

  future_df <- tibble::tibble(date = future_dates) %>%
    add_calendar_features()

  # استفاده از آخرین مقادیر برای lag features
  last_vals <- tail(df_train[[target]], max(7, 30))
  last_vals <- last_vals[!is.na(last_vals)]

  fallback_val <- if (length(last_vals) > 0) {
    mean(last_vals, na.rm = TRUE)
  } else {
    mean(df_train[[target]], na.rm = TRUE)
  }
  # اگر فال‌بک هم NA بود (همه داده NA)، صفر بگذار تا حداقل خطا ندهد
  if (is.na(fallback_val)) fallback_val <- 0

  for (lag in c(1, 2, 3, 7)) {
    col_name <- paste0("lag_", lag)
    lag_val <- if (lag <= length(last_vals)) {
      rev(last_vals)[lag]
    } else {
      fallback_val
    }
    if (is.na(lag_val)) lag_val <- fallback_val
    # مقدار ثابت برای تمام ردیف‌های افق (نه فقط ردیف اول)
    future_df[[col_name]] <- rep(lag_val, horizon)
  }

  # میانگین متحرک از داده‌های تاریخی — تکرار برای همه ردیف‌ها
  rm7  <- mean(tail(df_train[[target]], 7),  na.rm = TRUE)
  rm30 <- mean(tail(df_train[[target]], 30), na.rm = TRUE)
  if (is.na(rm7))  rm7  <- fallback_val
  if (is.na(rm30)) rm30 <- fallback_val

  future_df$rolling_mean_7  <- rep(rm7,  horizon)
  future_df$rolling_mean_30 <- rep(rm30, horizon)

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
advanced_clean_data <- function(df) {
  # ۱. یکپارچگی زمانی: پیدا کردن روزهای جاافتاده
  full_dates <- seq.Date(min(df$date), max(df$date), by = "day")
  df <- df %>% 
    dplyr::right_join(data.frame(date = full_dates), by = "date") %>% 
    dplyr::arrange(date)
  
  # افزودن ماه برای بررسی فصلی
  df$month <- lubridate::month(df$date)
  
  # ۲. تشخیص داده پرت زمینه‌ای (فصلی) با روش Z-Score
  remove_contextual_outliers <- function(x, month, threshold = 3.5) {
    # محاسبه میانگین و انحراف معیار برای هر ماه (نادیده گرفتن خود داده‌های پرت در محاسبه)
    monthly_stats <- split(x, month)
    means <- sapply(monthly_stats, mean, na.rm = TRUE)
    sds <- sapply(monthly_stats, sd, na.rm = TRUE)
    
    # جایگزینی داده‌های پرت با NA
    for (m in unique(month[!is.na(month)])) {
      idx <- which(month == m)
      z_score <- abs((x[idx] - means[as.character(m)]) / sds[as.character(m)])
      z_score[is.na(z_score)] <- 0
      x[idx][z_score > threshold] <- NA
    }
    return(x)
  }
  
  df$temperature   <- remove_contextual_outliers(df$temperature, df$month)
  df$humidity      <- remove_contextual_outliers(df$humidity, df$month)
  df$wind_speed    <- remove_contextual_outliers(df$wind_speed, df$month)
  df$precipitation <- remove_contextual_outliers(df$precipitation, df$month)
  
  # ۳. جبران‌سازی هوشمند (Imputation)
  # برای دما و رطوبت: درون‌یابی خطی (Linear Interpolation)
  df$temperature   <- zoo::na.approx(df$temperature, na.rm = FALSE)
  df$humidity      <- zoo::na.approx(df$humidity, na.rm = FALSE)
  
  # برای بارش: روزهای خالی یعنی بارش صفر
  df$precipitation[is.na(df$precipitation)] <- 0
  
  # برای سرعت باد: پر کردن با میانگین متحرک (Rolling Mean)
  df$wind_speed <- zoo::na.fill(df$wind_speed, "extend")
  
  # حذف ستون کمکی ماه
  df$month <- NULL
  
  return(df)
}
