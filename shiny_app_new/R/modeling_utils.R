# File: R/modeling_utils.R
# پیاده‌سازی تمام مدل‌های پیش‌بینی سری زمانی
# ─────────────────────────────────────────────────────────────────────────────

# ════════════════════════════════════════════════════════════════════════════
# بخش ۱: مدل‌های کلاسیک سری زمانی (روزانه)
# ════════════════════════════════════════════════════════════════════════════
# ── ساخت ماتریس ویژگی برای مدل‌های ML (ساعتی - محلی و ایمن) ──────────────────
build_hourly_feature_matrix <- function(df, target = "temperature", lag_hours = c(1, 2, 3, 24)) {
  df <- df %>%
    dplyr::arrange(timestamp) %>%
    dplyr::mutate(
      hour = lubridate::hour(timestamp), hour_sin = sin(2 * pi * hour / 24), hour_cos = cos(2 * pi * hour / 24),
      day_of_week = lubridate::wday(timestamp), day_of_year = lubridate::yday(timestamp), month = lubridate::month(timestamp),
      sin_annual = sin(2 * pi * day_of_year / 365), cos_annual = cos(2 * pi * day_of_year / 365),
      sin_weekly = sin(2 * pi * day_of_week / 7), cos_weekly = cos(2 * pi * day_of_week / 7)
    )
  for (lg in lag_hours) {
    df[[paste0("lag_", lg, "h")]] <- dplyr::lag(df[[target]], lg)
  }
  df$rolling_6h <- zoo::rollmean(df[[target]], k = 6, fill = NA, align = "right")
  df$rolling_24h <- zoo::rollmean(df[[target]], k = 24, fill = NA, align = "right")
  df %>% tidyr::drop_na()
}

HOURLY_FEAT_COLS <- c(
  "hour", "hour_sin", "hour_cos", "day_of_week", "month", "sin_annual", "cos_annual", "sin_weekly", "cos_weekly",
  "lag_1h", "lag_2h", "lag_3h", "lag_24h", "rolling_6h", "rolling_24h",
  "temperature", "humidity", "wind_speed", "precipitation"
)

forecast_arima <- function(train_df, horizon, target = "temperature") {
  n <- min(nrow(train_df), 1095)
  recent_df <- tail(train_df, n)
  ts_data <- ts(recent_df[[target]], frequency = 365)
  K <- 5
  xreg <- forecast::fourier(ts_data, K = K)
  fit <- tryCatch(
    forecast::auto.arima(ts_data, xreg = xreg, seasonal = FALSE, stepwise = TRUE, approximation = TRUE),
    error = function(e) forecast::Arima(ts_data, order = c(1,1,1), xreg = xreg)
  )
  xreg_future <- forecast::fourier(ts_data, K = K, h = horizon)
  fc <- forecast::forecast(fit, xreg = xreg_future)
  list(model = fit, predictions = as.numeric(fc$mean), lower = as.numeric(if(is.matrix(fc$lower)) fc$lower[,2] else fc$lower), upper = as.numeric(if(is.matrix(fc$upper)) fc$upper[,2] else fc$upper), method = "ARIMA (Fourier)")
}

forecast_sarima <- function(train_df, horizon, target = "temperature") {
  n <- min(nrow(train_df), 1095)
  recent_df <- tail(train_df, n)
  
  # سری زمانی با فرکانس هفتگی برای捕捉 الگوهای SARIMA
  ts_data <- ts(recent_df[[target]], frequency = 7)
  
  # ساخت ماتریس فوریه برای فصلی بودن سالانه (جلوگیری از تداخل ts)
  K <- 3
  t_train <- seq_len(n)
  t_future <- n + seq_len(horizon)
  
  xreg <- matrix(NA, nrow = n, ncol = 2 * K)
  xreg_future <- matrix(NA, nrow = horizon, ncol = 2 * K)
  
  for (k in 1:K) {
    xreg[, 2*k - 1] <- sin(2 * pi * k * t_train / 365.25)
    xreg[, 2*k]     <- cos(2 * pi * k * t_train / 365.25)
    xreg_future[, 2*k - 1] <- sin(2 * pi * k * t_future / 365.25)
    xreg_future[, 2*k]     <- cos(2 * pi * k * t_future / 365.25)
  }
  
  # استفاده از auto.arima برای پیدا کردن خودکار d (تفاضل‌گیری روند) و D (تفاضل‌گیری فصلی)
  fit <- tryCatch(
    forecast::auto.arima(
      ts_data, 
      xreg = xreg, 
      seasonal = TRUE, 
      stepwise = TRUE, 
      approximation = FALSE, 
      D = 1  # تفاضل‌گیری فصلی هفتگی فعال شود تا خط صاف نشود
    ),
    error = function(e) forecast::Arima(ts_data, order = c(1,1,1), seasonal = list(order = c(1,0,1), period = 7), xreg = xreg)
  )
  
  fc <- forecast::forecast(fit, xreg = xreg_future)
  
  list(
    model = fit, 
    predictions = as.numeric(fc$mean), 
    lower = as.numeric(if(is.matrix(fc$lower)) fc$lower[,2] else fc$lower), 
    upper = as.numeric(if(is.matrix(fc$upper)) fc$upper[,2] else fc$upper), 
    method = "SARIMA (Fourier)"
  )
}

# ── اضافه شدن تابع ETS روزانه که فراموش شده بود ──
forecast_ets <- function(train_df, horizon, target = "temperature") {
  ts_data <- prepare_ts_data(train_df, variable = target, freq = 365)
  fit <- tryCatch(forecast::ets(ts_data), error = function(e) NULL)
  if (is.null(fit)) stop("ETS شکست خورد")
  fc <- forecast::forecast(fit, h = horizon)
  list(model = fit, predictions = as.numeric(fc$mean), lower = as.numeric(if(is.matrix(fc$lower)) fc$lower[,2] else fc$lower), upper = as.numeric(if(is.matrix(fc$upper)) fc$upper[,2] else fc$upper), method = "ETS")
}

forecast_tbats <- function(train_df, horizon, target = "temperature") {
  msts_data <- forecast::msts(train_df[[target]], seasonal.periods = c(7, 365.25))
  if (length(msts_data) > 1095) {
    msts_data <- tail(msts_data, 1095)
  }
  fit <- tryCatch(
    forecast::tbats(msts_data, use.parallel = FALSE),
    error = function(e) tryCatch(forecast::bats(msts_data, use.parallel = FALSE), error = function(e2) NULL)
  )
  if (is.null(fit)) stop("TBATS شکست خورد")
  fc <- forecast::forecast(fit, h = horizon)
  list(model = fit, predictions = as.numeric(fc$mean), lower = as.numeric(if(is.matrix(fc$lower)) fc$lower[,2] else fc$lower), upper = as.numeric(if(is.matrix(fc$upper)) fc$upper[,2] else fc$upper), method = "TBATS")
}

# ── اضافه شدن تابع Prophet روزانه که فراموش شده بود ──
forecast_prophet <- function(train_df, horizon, target = "temperature") {
  df <- data.frame(ds = as.POSIXct(train_df$date), y = train_df[[target]])
  df <- df[!is.na(df$y), ]
  if (nrow(df) < 30) stop("داده کافی برای Prophet نیست")
  
  m <- tryCatch({
    prophet_model <- prophet::prophet(yearly.seasonality = TRUE, weekly.seasonality = TRUE, daily.seasonality = FALSE, interval.width = 0.95)
    prophet::fit.prophet(prophet_model, df)
  }, error = function(e) NULL)
  
  if (is.null(m)) stop("Prophet شکست خورد")
  future <- data.frame(ds = seq(max(df$ds) + 86400, by = "day", length.out = horizon))
  forecast_df <- stats::predict(m, future)
  list(model = m, predictions = as.numeric(forecast_df$yhat), lower = as.numeric(forecast_df$yhat_lower), upper = as.numeric(forecast_df$yhat_upper), method = "Prophet")
}

# ── پیش‌بینی ساعتی Prophet با متغیرهای کمکی (Multivariate) ───────────────────
forecast_hourly_prophet <- function(hourly_df, horizon_h = 24, target = "temperature") {
  df <- data.frame(ds = hourly_df$timestamp, y = hourly_df[[target]])
  df <- df[!is.na(df$y), ]
  if (nrow(df) < 48) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — Prophet"))
  
  if (nrow(df) > 336) df <- tail(df, 336)
  
  use_humidity <- "humidity" %in% names(hourly_df)
  if (use_humidity) {
    df$humidity <- hourly_df$humidity[match(df$ds, hourly_df$timestamp)]
    df$humidity[is.na(df$humidity)] <- mean(df$humidity, na.rm = TRUE)
  }
  
  m <- tryCatch({
    prophet_model <- prophet::prophet(yearly.seasonality = FALSE, weekly.seasonality = TRUE, daily.seasonality = TRUE, seasonality.mode = "additive", interval.width = 0.95)
    if (use_humidity) prophet_model <- prophet::add_regressor(prophet_model, "humidity")
    prophet::fit.prophet(prophet_model, df)
  }, error = function(e) NULL)
  
  if (is.null(m)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — Prophet"))
  
  future <- data.frame(ds = seq(max(df$ds) + 3600, by = 3600, length.out = horizon_h))
  if (use_humidity) {
    last_day_humidity <- tail(df$humidity, 24)
    future$humidity <- rep(last_day_humidity, length.out = horizon_h)
  }
  
  forecast_df <- stats::predict(m, future)
  preds <- as.numeric(forecast_df$yhat)
  
  recent_vals <- tail(df$y, 48)
  hard_min <- min(recent_vals, na.rm = TRUE) - 2
  hard_max <- max(recent_vals, na.rm = TRUE) + 2
  preds[preds < hard_min] <- hard_min
  preds[preds > hard_max] <- hard_max
  
  list(
    predictions = preds,
    lower       = as.numeric(forecast_df$yhat_lower),
    upper       = as.numeric(forecast_df$yhat_upper),
    method      = "Prophet (Multivariate)"
  )
}

forecast_hourly_ets <- function(hourly_df, horizon_h = 24, target = "temperature") {
  vals <- hourly_df[[target]]
  vals[is.na(vals)] <- zoo::na.approx(vals, na.rm = FALSE)[is.na(vals)]
  vals[is.na(vals)] <- mean(vals, na.rm = TRUE)
  if (length(vals) > 720) vals <- tail(vals, 720)
  
  if (length(vals) >= 3) {
    vals_smoothed <- zoo::rollmean(vals, k = 3, fill = NA, align = "right")
    vals_smoothed[is.na(vals_smoothed)] <- vals[is.na(vals_smoothed)]
  } else {
    vals_smoothed <- vals
  }
  ts_obj <- ts(vals_smoothed, frequency = 24)
  
  # استفاده مستقیم از ets با اجازه انتخاب مدل بهینه (ZZZ) به جای stlf
  fc <- tryCatch(
    forecast::forecast(forecast::ets(ts_obj), h = horizon_h),
    error = function(e) tryCatch(
      forecast::stlf(ts_obj, h = horizon_h, method = "ets", s.window = 7, robust = TRUE), 
      error = function(e2) NULL
    )
  )
  
  if (is.null(fc)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — ETS"))
  
  lower_vals <- tryCatch({ if (is.matrix(fc$lower)) as.numeric(fc$lower[, 2]) else as.numeric(fc$lower) }, error = function(e) rep(NA_real_, horizon_h))
  upper_vals <- tryCatch({ if (is.matrix(fc$upper)) as.numeric(fc$upper[, 2]) else as.numeric(fc$upper) }, error = function(e) rep(NA_real_, horizon_h))
  preds <- as.numeric(fc$mean)
  
  if (length(preds) != horizon_h || any(!is.finite(preds))) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — ETS"))
  if (any(!is.finite(lower_vals)) || length(lower_vals) != horizon_h) {
    sd_val <- ifelse(sd(vals, na.rm = TRUE) > 0, sd(vals, na.rm = TRUE), 1)
    lower_vals <- preds - 1.96 * sd_val
    upper_vals <- preds + 1.96 * sd_val
  }
  
  list(predictions = preds, lower = lower_vals, upper = upper_vals, method = "ETS (Hourly)")
}

# ════════════════════════════════════════════════════════════════════════════
# بخش ۲: Helperهای مشترک برای مدل‌های ML روزانه
# ════════════════════════════════════════════════════════════════════════════
recursive_feature_forecast <- function(predict_fn, train_df, horizon, target, feat_cols, lag_days = c(1, 2, 3, 7, 14, 21)) {
  last_date <- max(train_df$date)
  max_lag <- max(c(lag_days, 30))
  history_vals <- tail(train_df[[target]], max_lag)
  n_hist <- length(history_vals)
  buffer_vals <- c(history_vals, rep(NA_real_, horizon))
  last_row <- dplyr::slice_tail(train_df, n = 1)
  
  safe_window <- function(buf, from, to) {
    from <- max(1, from)
    if (to < from) return(buf[max(1, to)])
    buf[from:to]
  }
  
  predictions <- numeric(horizon)
  for (h in seq_len(horizon)) {
    current_date <- last_date + h
    pos <- n_hist + h
    cal <- tibble::tibble(date = current_date) %>% add_calendar_features()
    for (lag in lag_days) {
      idx <- pos - lag
      cal[[paste0("lag_", lag)]] <- if (idx >= 1) buffer_vals[idx] else buffer_vals[1]
    }
    cal$rolling_mean_7 <- mean(safe_window(buffer_vals, pos - 7, pos - 1), na.rm = TRUE)
    cal$rolling_mean_14 <- mean(safe_window(buffer_vals, pos - 14, pos - 1), na.rm = TRUE)
    cal$rolling_mean_30 <- mean(safe_window(buffer_vals, pos - 30, pos - 1), na.rm = TRUE)
    cal$rolling_sd_7 <- stats::sd(safe_window(buffer_vals, pos - 7, pos - 1), na.rm = TRUE)
    if (!is.finite(cal$rolling_sd_7)) cal$rolling_sd_7 <- 0
    for (fc in feat_cols) {
      if (!fc %in% names(cal)) cal[[fc]] <- if (fc %in% names(last_row)) last_row[[fc]][1] else 0
    }
    X_row <- as.matrix(cal[, feat_cols, drop = FALSE])
    pred_h <- as.numeric(predict_fn(X_row))[1]
    predictions[h] <- pred_h
    buffer_vals[pos] <- pred_h
  }
  predictions
}

.prepare_ml_training_data <- function(train_df, target = "temperature") {
  feat_df <- build_feature_matrix(train_df, target = target)
  feat_cols <- setdiff(names(feat_df), c("date", target, "station_id", "timestamp", "pressure", "w_code"))
  feat_cols <- intersect(feat_cols, names(feat_df))
  list(feat_df = feat_df, feat_cols = feat_cols, X_train = as.matrix(feat_df[, feat_cols, drop = FALSE]), y_train = feat_df[[target]])
}

.train_valid_split_idx <- function(n, valid_ratio = 0.15, min_valid = 10) {
  n_valid <- max(min_valid, floor(n * valid_ratio))
  n_valid <- min(n_valid, n - min_valid)
  if (n_valid < 5 || n - n_valid < 20) return(NULL)
  list(train_idx = seq_len(n - n_valid), valid_idx = (n - n_valid + 1):n)
}

.compute_ci <- function(preds, resid_sd, horizon) {
  growth <- sqrt(seq_len(horizon))
  list(lower = preds - 1.96 * resid_sd * growth, upper = preds + 1.96 * resid_sd * growth)
}

# ════════════════════════════════════════════════════════════════════════════
# بخش ۳: مدل‌های ML روزانه (AutoML) - با خروجی feat_imp
# ════════════════════════════════════════════════════════════════════════════
forecast_rf <- function(train_df, horizon, target = "temperature") {
  prep <- .prepare_ml_training_data(train_df, target)
  fit <- ranger::ranger(x = prep$X_train, y = prep$y_train, num.trees = 500, mtry = max(1, floor(ncol(prep$X_train) / 3)), min.node.size = 5, sample.fraction = 0.8, importance = "impurity", keep.inbag = FALSE)
  preds <- recursive_feature_forecast(predict_fn = function(X_row) { newdata <- as.data.frame(X_row); names(newdata) <- prep$feat_cols; stats::predict(fit, data = newdata)$predictions }, train_df = train_df, horizon = horizon, target = target, feat_cols = prep$feat_cols)
  ci <- .compute_ci(preds, sqrt(fit$prediction.error), horizon)
  imp <- tryCatch(data.frame(Feature = names(fit$variable.importance), Gain = unname(fit$variable.importance)), error = function(e) NULL)
  list(model = fit, predictions = preds, lower = ci$lower, upper = ci$upper, method = "Random Forest (ranger)", feat_imp = imp)
}

forecast_xgboost <- function(train_df, horizon, target = "temperature") {
  prep <- .prepare_ml_training_data(train_df, target)
  params <- list(objective = "reg:squarederror", max_depth = 8, eta = 0.05, subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 5, gamma = 0.1)
  split <- .train_valid_split_idx(nrow(prep$X_train))
  if (!is.null(split)) {
    dtr <- xgboost::xgb.DMatrix(prep$X_train[split$train_idx, , drop = FALSE], label = prep$y_train[split$train_idx])
    dva <- xgboost::xgb.DMatrix(prep$X_train[split$valid_idx, , drop = FALSE], label = prep$y_train[split$valid_idx])
    fit <- xgboost::xgb.train(params = params, data = dtr, nrounds = 1000, watchlist = list(train = dtr, valid = dva), early_stopping_rounds = 50, verbose = 0)
  } else {
    dtr <- xgboost::xgb.DMatrix(prep$X_train, label = prep$y_train)
    fit <- xgboost::xgb.train(params = params, data = dtr, nrounds = 200, verbose = 0)
  }
  preds <- recursive_feature_forecast(predict_fn = function(X_row) stats::predict(fit, xgboost::xgb.DMatrix(X_row)), train_df = train_df, horizon = horizon, target = target, feat_cols = prep$feat_cols)
  ci <- .compute_ci(preds, sd(prep$y_train - stats::predict(fit, xgboost::xgb.DMatrix(prep$X_train))), horizon)
  imp <- tryCatch(xgboost::xgb.importance(model = fit), error = function(e) NULL)
  feat_imp <- if (!is.null(imp)) imp[, c("Feature", "Gain")] else NULL
  list(model = fit, predictions = preds, lower = ci$lower, upper = ci$upper, method = "XGBoost", feat_imp = feat_imp)
}

forecast_lightgbm <- function(train_df, horizon, target = "temperature") {
  if (!requireNamespace("lightgbm", quietly = TRUE)) stop("پکیج lightgbm نصب نیست.")
  prep <- .prepare_ml_training_data(train_df, target)
  params <- list(objective = "regression", metric = "rmse", num_leaves = 31, max_depth = 8, learning_rate = 0.05, feature_fraction = 0.8, bagging_fraction = 0.8, bagging_freq = 1, min_data_in_leaf = 10)
  split <- .train_valid_split_idx(nrow(prep$X_train))
  if (!is.null(split)) {
    dtr <- lightgbm::lgb.Dataset(prep$X_train[split$train_idx, , drop = FALSE], label = prep$y_train[split$train_idx])
    dva <- lightgbm::lgb.Dataset.create.valid(dtr, prep$X_train[split$valid_idx, , drop = FALSE], label = prep$y_train[split$valid_idx])
    fit <- lightgbm::lgb.train(params = params, data = dtr, nrounds = 500, valids = list(valid = dva), early_stopping_rounds = 20, verbose = -1)
  } else {
    dtr <- lightgbm::lgb.Dataset(prep$X_train, label = prep$y_train)
    fit <- lightgbm::lgb.train(params = params, data = dtr, nrounds = 100, verbose = -1)
  }
  preds <- recursive_feature_forecast(predict_fn = function(X_row) stats::predict(fit, X_row), train_df = train_df, horizon = horizon, target = target, feat_cols = prep$feat_cols)
  ci <- .compute_ci(preds, sd(prep$y_train - stats::predict(fit, prep$X_train)), horizon)
  imp <- tryCatch(lightgbm::lgb.importance(fit, percentage = TRUE), error = function(e) NULL)
  feat_imp <- if (!is.null(imp)) imp[, c("Feature", "Gain")] else NULL
  list(model = fit, predictions = preds, lower = ci$lower, upper = ci$upper, method = "LightGBM", feat_imp = feat_imp)
}

forecast_catboost <- function(train_df, horizon, target = "temperature") {
  if (!requireNamespace("catboost", quietly = TRUE)) stop("پکیج catboost نصب نیست.")
  prep <- .prepare_ml_training_data(train_df, target)
  split <- .train_valid_split_idx(nrow(prep$X_train))
  train_idx <- if (!is.null(split)) split$train_idx else seq_len(nrow(prep$X_train))
  train_pool <- catboost::catboost.load_pool(data = as.data.frame(prep$X_train[train_idx, , drop = FALSE]), label = prep$y_train[train_idx])
  params <- list(loss_function = "RMSE", iterations = 500, depth = 8, learning_rate = 0.05, l2_leaf_reg = 3, logging_level = "Silent")
  if (!is.null(split)) {
    valid_pool <- catboost::catboost.load_pool(data = as.data.frame(prep$X_train[split$valid_idx, , drop = FALSE]), label = prep$y_train[split$valid_idx])
    params$early_stopping_rounds <- 20
    fit <- catboost::catboost.train(train_pool, valid_pool, params = params)
  } else {
    params$iterations <- 150
    fit <- catboost::catboost.train(train_pool, params = params)
  }
  preds <- recursive_feature_forecast(predict_fn = function(X_row) { pool <- catboost::catboost.load_pool(data = as.data.frame(X_row)); catboost::catboost.predict(fit, pool) }, train_df = train_df, horizon = horizon, target = target, feat_cols = prep$feat_cols)
  ci <- .compute_ci(preds, sd(prep$y_train - catboost::catboost.predict(fit, catboost::catboost.load_pool(data = as.data.frame(prep$X_train)))), horizon)
  imp <- tryCatch(catboost::catboost.get_feature_importance(fit), error = function(e) NULL)
  feat_imp <- if (!is.null(imp)) data.frame(Feature = colnames(prep$X_train), Gain = imp) else NULL
  list(model = fit, predictions = preds, lower = ci$lower, upper = ci$upper, method = "CatBoost", feat_imp = feat_imp)
}

forecast_svm <- function(train_df, horizon, target = "temperature") {
  prep <- .prepare_ml_training_data(train_df, target)
  fit <- e1071::svm(x = prep$X_train, y = prep$y_train, kernel = "radial", cost = 10, epsilon = 0.1)
  preds <- recursive_feature_forecast(predict_fn = function(X_row) stats::predict(fit, newdata = X_row), train_df = train_df, horizon = horizon, target = target, feat_cols = prep$feat_cols)
  ci <- .compute_ci(preds, sd(prep$y_train - as.numeric(stats::predict(fit, newdata = prep$X_train))), horizon)
  list(model = fit, predictions = preds, lower = ci$lower, upper = ci$upper, method = "SVM", feat_imp = NULL)
}

forecast_naive <- function(train_df, horizon, target = "temperature") {
  ts_data <- prepare_ts_data(train_df, variable = target, freq = 7)
  fit <- forecast::naive(ts_data, h = horizon)
  list(model = fit, predictions = as.numeric(fit$mean), lower = as.numeric(if(is.matrix(fit$lower)) fit$lower[,2] else fit$lower), upper = as.numeric(if(is.matrix(fit$upper)) fit$upper[,2] else fit$upper), method = "Naïve")
}

# ════════════════════════════════════════════════════════════════════════════
# بخش ۴: Dispatcher و فهرست مدل‌ها
# ════════════════════════════════════════════════════════════════════════════
run_model_by_name <- function(model_name, train_df, horizon, target = "temperature") {
  model_name <- tolower(trimws(as.character(model_name)[1]))
  fn <- switch(model_name, arima = forecast_arima, sarima = forecast_sarima, ets = forecast_ets, tbats = forecast_tbats, prophet = forecast_prophet, rf = forecast_rf, xgboost = forecast_xgboost, lightgbm = forecast_lightgbm, catboost = forecast_catboost, svm = forecast_svm, naive = forecast_naive, stop("مدل ناشناخته: ", model_name))
  fn(train_df, horizon, target)
}

AUTOML_MODEL_NAMES <- c("sarima", "rf", "xgboost", "lightgbm", "catboost", "svm", "naive")

# ════════════════════════════════════════════════════════════════════════════
# بخش ۵: AutoML و Ensemble
# ════════════════════════════════════════════════════════════════════════════
run_automl <- function(data, horizon, target = "temperature", ensemble_tol = 0.05) {
  splits <- train_test_split(data, test_ratio = 0.15)
  train_df <- splits$train
  test_df <- splits$test
  test_h <- nrow(test_df)
  results <- list()
  for (mn in AUTOML_MODEL_NAMES) {
    tryCatch({
      fc <- run_model_by_name(mn, train_df, test_h, target)
      preds <- fc$predictions[seq_len(min(test_h, length(fc$predictions)))]
      actual <- test_df[[target]][seq_len(length(preds))]
      results[[mn]] <- list(forecast = fc, metrics = compute_all_metrics(actual, preds, model_name = mn))
    }, error = function(e) message("خطا در مدل ", mn, ": ", conditionMessage(e)))
  }
  if (length(results) == 0) stop("هیچ مدلی آموزش داده نشد")
  all_metrics <- compute_composite_score(dplyr::bind_rows(purrr::map(results, "metrics")))
  best_model_name <- all_metrics$model[1]
  top3 <- all_metrics[1:min(3, nrow(all_metrics)), ]
  final_result <- list(all_metrics = all_metrics, best_model_name = best_model_name, all_results = results)
  if (diff(range(top3$composite_score, na.rm = TRUE)) <= ensemble_tol && nrow(top3) >= 2) {
    final_result$best_model_name <- "Ensemble"
    final_result$ensemble_forecast <- build_weighted_ensemble(top3$model, top3$composite_score, data, horizon, target)
    final_result$ensemble_models <- top3$model
  } else {
    final_result$final_forecast <- tryCatch(run_model_by_name(best_model_name, data, horizon, target), error = function(e) NULL)
  }
  final_result
}

build_weighted_ensemble <- function(model_names, scores, train_df, horizon, target = "temperature") {
  weights <- (1 / (scores + 1e-6))
  weights <- weights / sum(weights)
  forecasts <- purrr::map(model_names, function(mn) tryCatch(run_model_by_name(mn, train_df, horizon, target), error = function(e) NULL))
  valid_idx <- !purrr::map_lgl(forecasts, is.null)
  valid_fcs <- forecasts[valid_idx]
  valid_wts <- weights[valid_idx] / sum(weights[valid_idx])
  pad_vec <- function(v, n) { length(v) <- n; v }
  pred_matrix <- do.call(cbind, purrr::map(valid_fcs, ~ pad_vec(.x$predictions, horizon)))
  lower_matrix <- do.call(cbind, purrr::map(valid_fcs, ~ pad_vec(.x$lower, horizon)))
  upper_matrix <- do.call(cbind, purrr::map(valid_fcs, ~ pad_vec(.x$upper, horizon)))
  list(predictions = as.numeric(pred_matrix %*% valid_wts), lower = as.numeric(lower_matrix %*% valid_wts), upper = as.numeric(upper_matrix %*% valid_wts), weights = valid_wts, models = model_names[valid_idx], method = "Ensemble")
}

extract_automl_forecast <- function(automl_result) {
  if (!is.null(automl_result$ensemble_forecast)) return(automl_result$ensemble_forecast)
  if (!is.null(automl_result$final_forecast)) return(automl_result$final_forecast)
  if (!is.null(automl_result$all_results[[automl_result$best_model_name]])) return(automl_result$all_results[[automl_result$best_model_name]]$forecast)
  NULL
}

# ════════════════════════════════════════════════════════════════════════════
# بخش ۶: پیش‌بینی ساعتی — آموزش روی داده ساعتی با featureهای زمانی
# ════════════════════════════════════════════════════════════════════════════

.prepare_hourly_training_data <- function(hourly_df, target = "temperature", use_multivariate = FALSE) {
  if (nrow(hourly_df) > 1440) hourly_df <- tail(hourly_df, 1440)
  feat_df <- build_hourly_feature_matrix(hourly_df, target)
  if (nrow(feat_df) < 10) stop("داده کافی بعد از حذف NA باقی نمانده")
  exog_vars <- setdiff(c("temperature", "humidity", "wind_speed", "precipitation"), target)
  if (isTRUE(use_multivariate)) {
    feat_use <- setdiff(intersect(HOURLY_FEAT_COLS, names(feat_df)), target)
  } else {
    feat_use <- setdiff(intersect(HOURLY_FEAT_COLS, names(feat_df)), c(target, exog_vars))
  }
  list(feat_df = feat_df, feat_use = feat_use, X_train = as.matrix(feat_df[, feat_use, drop = FALSE]), y_train = feat_df[[target]], hourly_df = hourly_df)
}

hourly_recursive_forecast <- function(predict_fn, model, train_df, horizon_h, target, feat_use) {
  last_ts <- as.POSIXct(max(train_df$timestamp), tz = "Asia/Tehran")
  history <- tail(train_df[[target]], 48)
  n_hist <- length(history)
  buf <- c(history, rep(NA_real_, horizon_h))
  
  exog_vars <- setdiff(c("temperature", "humidity", "wind_speed", "precipitation"), target)
  active_exog_vars <- intersect(exog_vars, feat_use)
  last_exog <- if (length(active_exog_vars) > 0) tail(train_df[, active_exog_vars, drop = FALSE], 1) else NULL
  
  preds <- numeric(horizon_h)
  
  for (h in seq_len(horizon_h)) {
    ts_h <- last_ts + h * 3600
    pos <- n_hist + h
    
    get_lag <- function(k) {
      idx <- pos - k
      if (idx < 1) return(mean(history, na.rm = TRUE))
      v <- buf[idx]
      if (is.na(v)) mean(history, na.rm = TRUE) else v
    }
    
    avail <- buf[max(1, pos - 24):(pos - 1)]
    avail <- avail[!is.na(avail)]
    roll_6h <- if (length(avail) >= 6) mean(tail(avail, 6)) else mean(history, na.rm = TRUE)
    roll_24h <- if (length(avail) >= 24) mean(tail(avail, 24)) else mean(history, na.rm = TRUE)
    
    hour_val <- as.integer(format(ts_h, "%H"))
    dow_val <- as.integer(format(ts_h, "%u"))
    doy_val <- as.integer(format(ts_h, "%j"))
    month_val <- as.integer(format(ts_h, "%m"))
    
    newrow <- data.frame(
      hour = hour_val, hour_sin = sin(2 * pi * hour_val / 24), hour_cos = cos(2 * pi * hour_val / 24),
      day_of_week = dow_val, month = month_val, sin_annual = sin(2 * pi * doy_val / 365), cos_annual = cos(2 * pi * doy_val / 365),
      sin_weekly = sin(2 * pi * dow_val / 7), cos_weekly = cos(2 * pi * dow_val / 7),
      lag_1h = get_lag(1), lag_2h = get_lag(2), lag_3h = get_lag(3), lag_24h = get_lag(24),
      rolling_6h = roll_6h, rolling_24h = roll_24h
    )
    
    # اصلاح باگ ۴: تضمین وجود تمام ستون‌های مورد نیاز مدل ML
    for (fc in feat_use) {
      if (!fc %in% names(newrow)) {
        newrow[[fc]] <- if (fc %in% names(train_df)) tail(train_df[[fc]], 1) else 0
      }
    }
    
    if (!is.null(last_exog)) {
      for (ev in active_exog_vars) {
        if (ev %in% names(newrow)) newrow[[ev]] <- as.numeric(last_exog[[ev]][1])
      }
    }
    
    p <- tryCatch(predict_fn(model, newrow), error = function(e) NA_real_)
    if (is.na(p) || is.null(p)) p <- get_lag(1)
    preds[h] <- p
    buf[pos] <- p
  }
  preds
}

hourly_fallback_forecast <- function(hourly_df, horizon_h, target, method_suffix = "") {
  vals <- hourly_df[[target]]
  vals[is.na(vals)] <- mean(vals, na.rm = TRUE)
  n <- length(vals)
  hours_idx <- (lubridate::hour(hourly_df$timestamp) %% 24) + 1
  hourly_avg <- sapply(0:23, function(h) {
    idx <- which(hours_idx == (h + 1))
    if (length(idx) > 0) mean(vals[idx], na.rm = TRUE) else mean(vals, na.rm = TRUE)
  })
  last_hour <- lubridate::hour(max(hourly_df$timestamp))
  drift <- mean(tail(vals, 24), na.rm = TRUE) - mean(vals, na.rm = TRUE)
  preds <- numeric(horizon_h)
  for (h in seq_len(horizon_h)) preds[h] <- hourly_avg[((last_hour + h) %% 24) + 1] + drift
  sd_val <- ifelse(sd(vals, na.rm = TRUE) > 0, sd(vals, na.rm = TRUE), 1)
  list(predictions = preds, lower = preds - 1.96 * sd_val, upper = preds + 1.96 * sd_val, method = paste0("الگوی میانگین ساعتی", method_suffix, " (fallback)"))
}

.valid_hourly_result <- function(res, horizon_h) {
  !is.null(res) && !is.null(res$predictions) && length(res$predictions) == horizon_h && all(is.finite(res$predictions))
}

# ── مدل‌های ML ساعتی ──

forecast_hourly_xgboost <- function(hourly_df, horizon_h = 24, target = "temperature", use_multivariate = FALSE) {
  result <- tryCatch({
    prep <- .prepare_hourly_training_data(hourly_df, target, use_multivariate)
    params <- list(objective = "reg:squarederror", max_depth = 8, eta = 0.05, subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 5, gamma = 0.1)
    split <- .train_valid_split_idx(nrow(prep$X_train))
    if (!is.null(split)) {
      dtr <- xgboost::xgb.DMatrix(prep$X_train[split$train_idx, , drop = FALSE], label = prep$y_train[split$train_idx])
      dva <- xgboost::xgb.DMatrix(prep$X_train[split$valid_idx, , drop = FALSE], label = prep$y_train[split$valid_idx])
      model <- xgboost::xgb.train(params = params, data = dtr, nrounds = 1000, watchlist = list(train = dtr, valid = dva), early_stopping_rounds = 50, verbose = 0)
    } else {
      dtr <- xgboost::xgb.DMatrix(prep$X_train, label = prep$y_train)
      model <- xgboost::xgb.train(params = params, data = dtr, nrounds = 200, verbose = 0)
    }
    preds <- hourly_recursive_forecast(predict_fn = function(m, nd) stats::predict(m, xgboost::xgb.DMatrix(data.matrix(nd[, prep$feat_use, drop = FALSE]))), model = model, train_df = prep$hourly_df, horizon_h = horizon_h, target = target, feat_use = prep$feat_use)
    imp <- tryCatch(xgboost::xgb.importance(model = model), error = function(e) NULL)
    list(predictions = preds, method = paste0("XGBoost (ساعتی", ifelse(isTRUE(use_multivariate), "، چندمتغیره)", ")")), feat_imp = if (!is.null(imp)) imp[, c("Feature", "Gain")] else NULL)
  }, error = function(e) { message("XGBoost ساعتی شکست خورد: ", conditionMessage(e)); NULL })
  if (!.valid_hourly_result(result, horizon_h)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — XGBoost"))
  result
}

forecast_hourly_rf <- function(hourly_df, horizon_h = 24, target = "temperature", use_multivariate = FALSE) {
  result <- tryCatch({
    prep <- .prepare_hourly_training_data(hourly_df, target, use_multivariate)
    model <- ranger::ranger(x = prep$X_train, y = prep$y_train, num.trees = 500, mtry = max(1, floor(ncol(prep$X_train) / 3)), min.node.size = 5, sample.fraction = 0.8, importance = "impurity", keep.inbag = FALSE)
    preds <- hourly_recursive_forecast(predict_fn = function(m, nd) { newdata <- as.data.frame(data.matrix(nd[, prep$feat_use, drop = FALSE])); names(newdata) <- prep$feat_use; stats::predict(m, data = newdata)$predictions }, model = model, train_df = prep$hourly_df, horizon_h = horizon_h, target = target, feat_use = prep$feat_use)
    imp <- tryCatch(data.frame(Feature = names(model$variable.importance), Gain = unname(model$variable.importance)), error = function(e) NULL)
    list(predictions = preds, method = paste0("Random Forest (ساعتی", ifelse(isTRUE(use_multivariate), "، چندمتغیره)", ")")), feat_imp = imp)
  }, error = function(e) { message("RF ساعتی شکست خورد: ", conditionMessage(e)); NULL })
  if (!.valid_hourly_result(result, horizon_h)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — RF"))
  result
}

forecast_hourly_lightgbm <- function(hourly_df, horizon_h = 24, target = "temperature", use_multivariate = FALSE) {
  if (!requireNamespace("lightgbm", quietly = TRUE)) stop("پکیج lightgbm نصب نیست.")
  result <- tryCatch({
    prep <- .prepare_hourly_training_data(hourly_df, target, use_multivariate)
    params <- list(objective = "regression", metric = "rmse", num_leaves = 31, max_depth = 8, learning_rate = 0.05, feature_fraction = 0.8, bagging_fraction = 0.8, bagging_freq = 1, min_data_in_leaf = 10)
    split <- .train_valid_split_idx(nrow(prep$X_train))
    if (!is.null(split)) {
      dtr <- lightgbm::lgb.Dataset(prep$X_train[split$train_idx, , drop = FALSE], label = prep$y_train[split$train_idx])
      dva <- lightgbm::lgb.Dataset.create.valid(dtr, prep$X_train[split$valid_idx, , drop = FALSE], label = prep$y_train[split$valid_idx])
      model <- lightgbm::lgb.train(params = params, data = dtr, nrounds = 500, valids = list(valid = dva), early_stopping_rounds = 20, verbose = -1)
    } else {
      dtr <- lightgbm::lgb.Dataset(prep$X_train, label = prep$y_train)
      model <- lightgbm::lgb.train(params = params, data = dtr, nrounds = 100, verbose = -1)
    }
    preds <- hourly_recursive_forecast(predict_fn = function(m, nd) stats::predict(m, data.matrix(nd[, prep$feat_use, drop = FALSE])), model = model, train_df = prep$hourly_df, horizon_h = horizon_h, target = target, feat_use = prep$feat_use)
    imp <- tryCatch(lightgbm::lgb.importance(model, percentage = TRUE), error = function(e) NULL)
    list(predictions = preds, method = paste0("LightGBM (ساعتی", ifelse(isTRUE(use_multivariate), "، چندمتغیره)", ")")), feat_imp = if (!is.null(imp)) imp[, c("Feature", "Gain")] else NULL)
  }, error = function(e) { message("LightGBM ساعتی شکست خورد: ", conditionMessage(e)); NULL })
  if (!.valid_hourly_result(result, horizon_h)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — LightGBM"))
  result
}

forecast_hourly_catboost <- function(hourly_df, horizon_h = 24, target = "temperature", use_multivariate = FALSE) {
  if (!requireNamespace("catboost", quietly = TRUE)) stop("پکیج catboost نصب نیست.")
  result <- tryCatch({
    prep <- .prepare_hourly_training_data(hourly_df, target, use_multivariate)
    split <- .train_valid_split_idx(nrow(prep$X_train))
    train_idx <- if (!is.null(split)) split$train_idx else seq_len(nrow(prep$X_train))
    train_pool <- catboost::catboost.load_pool(data = as.data.frame(prep$X_train[train_idx, , drop = FALSE]), label = prep$y_train[train_idx])
    
    # اصلاح پارامترها: کاهش depth به 6 برای جلوگیری از Overfitting روی داده ساعتی
    params <- list(
      loss_function = "RMSE", 
      iterations = 1000, 
      depth = 6, 
      learning_rate = 0.05, 
      l2_leaf_reg = 3, 
      logging_level = "Silent",
      od_type = "Iter",
      od_wait = 30
    )
    
    if (!is.null(split)) {
      valid_pool <- catboost::catboost.load_pool(data = as.data.frame(prep$X_train[split$valid_idx, , drop = FALSE]), label = prep$y_train[split$valid_idx])
      fit <- catboost::catboost.train(train_pool, valid_pool, params = params)
    } else {
      params$iterations <- 200
      fit <- catboost::catboost.train(train_pool, params = params)
    }
    
    preds <- hourly_recursive_forecast(
      predict_fn = function(m, nd) { 
        pool <- catboost::catboost.load_pool(data = as.data.frame(data.matrix(nd[, prep$feat_use, drop = FALSE]))); 
        catboost::catboost.predict(m, pool) 
      }, 
      model = fit, 
      train_df = prep$hourly_df, 
      horizon_h = horizon_h, 
      target = target, 
      feat_use = prep$feat_use
    )
    
    imp <- tryCatch(catboost::catboost.get_feature_importance(fit), error = function(e) NULL)
    list(
      predictions = preds, 
      method = paste0("CatBoost (ساعتی", ifelse(isTRUE(use_multivariate), "، چندمتغیره)", ")")), 
      feat_imp = if (!is.null(imp)) data.frame(Feature = colnames(prep$X_train), Gain = imp) else NULL
    )
  }, error = function(e) { 
    message("CatBoost ساعتی شکست خورد: ", conditionMessage(e)); NULL 
  })
  
  if (!.valid_hourly_result(result, horizon_h)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — CatBoost"))
  result
}

forecast_hourly_svm <- function(hourly_df, horizon_h = 24, target = "temperature", use_multivariate = FALSE) {
  result <- tryCatch({
    prep <- .prepare_hourly_training_data(hourly_df, target, use_multivariate)
    model <- e1071::svm(x = prep$X_train, y = prep$y_train, kernel = "radial", cost = 10, epsilon = 0.1)
    preds <- hourly_recursive_forecast(predict_fn = function(m, nd) stats::predict(m, newdata = data.matrix(nd[, prep$feat_use, drop = FALSE])), model = model, train_df = prep$hourly_df, horizon_h = horizon_h, target = target, feat_use = prep$feat_use)
    list(predictions = preds, method = paste0("SVM (ساعتی", ifelse(isTRUE(use_multivariate), "، چندمتغیره)", ")")))
  }, error = function(e) { message("SVM ساعتی شکست خورد: ", conditionMessage(e)); NULL })
  if (!.valid_hourly_result(result, horizon_h)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — SVM"))
  result
}

# ── مدل‌های آماری ساعتی ──

forecast_hourly_arima <- function(hourly_df, horizon_h = 24, target = "temperature") {
  vals <- hourly_df[[target]]
  vals[is.na(vals)] <- zoo::na.approx(vals, na.rm = FALSE)[is.na(vals)]
  vals[is.na(vals)] <- mean(vals, na.rm = TRUE)
  if (length(vals) > 720) vals <- tail(vals, 720)
  ts_obj <- ts(vals, frequency = 24)
  fit <- tryCatch(forecast::Arima(ts_obj, order = c(2, 0, 1), seasonal = list(order = c(1, 0, 0), period = 24)), error = function(e) tryCatch(forecast::Arima(ts_obj, order = c(1, 1, 1)), error = function(e2) tryCatch(forecast::auto.arima(tail(ts_obj, 168), seasonal = FALSE, stepwise = TRUE, approximation = TRUE), error = function(e3) NULL)))
  if (is.null(fit)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — ARIMA"))
  fc <- tryCatch(forecast::forecast(fit, h = horizon_h), error = function(e) NULL)
  if (is.null(fc)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — ARIMA"))
  list(predictions = as.numeric(fc$mean), lower = as.numeric(if(is.matrix(fc$lower)) fc$lower[,2] else fc$lower), upper = as.numeric(if(is.matrix(fc$upper)) fc$upper[,2] else fc$upper), method = "ARIMA (ساعتی)")
}

forecast_hourly_sarima <- function(hourly_df, horizon_h = 24, target = "temperature") {
  vals <- hourly_df[[target]]
  vals[is.na(vals)] <- zoo::na.approx(vals, na.rm = FALSE)[is.na(vals)]
  vals[is.na(vals)] <- mean(vals, na.rm = TRUE)
  if (length(vals) > 720) vals <- tail(vals, 720)
  
  ts_obj <- ts(vals, frequency = 24)
  
  # استفاده از auto.arima برای پیدا کردن پارامترهای بهینه به جای اجبار دستی
  fit <- tryCatch(
    forecast::auto.arima(ts_obj, seasonal = TRUE, D = 1, stepwise = TRUE, approximation = TRUE),
    error = function(e) tryCatch(
      forecast::Arima(ts_obj, order = c(1,1,1), seasonal = list(order = c(0,1,1), period = 24)), 
      error = function(e2) NULL
    )
  )
  
  if (is.null(fit)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — SARIMA"))
  fc <- tryCatch(forecast::forecast(fit, h = horizon_h), error = function(e) NULL)
  if (is.null(fc)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — SARIMA"))
  
  list(
    predictions = as.numeric(fc$mean), 
    lower = as.numeric(if(is.matrix(fc$lower)) fc$lower[,2] else fc$lower), 
    upper = as.numeric(if(is.matrix(fc$upper)) fc$upper[,2] else fc$upper), 
    method = "SARIMA (ساعتی)"
  )
}

forecast_hourly_tbats <- function(hourly_df, horizon_h = 24, target = "temperature") {
  vals <- hourly_df[[target]]
  vals[is.na(vals)] <- zoo::na.approx(vals, na.rm = FALSE)[is.na(vals)]
  vals[is.na(vals)] <- mean(vals, na.rm = TRUE)
  if (length(vals) > 720) vals <- tail(vals, 720)
  ts_obj <- ts(vals, frequency = 24)
  fit <- tryCatch(forecast::tbats(ts_obj, use.parallel = FALSE, use.box.cox = FALSE), error = function(e) tryCatch(forecast::bats(ts_obj, use.parallel = FALSE, use.box.cox = FALSE), error = function(e2) NULL))
  if (is.null(fit)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — TBATS"))
  fc <- tryCatch(forecast::forecast(fit, h = horizon_h), error = function(e) NULL)
  if (is.null(fc)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — TBATS"))
  list(predictions = as.numeric(fc$mean), lower = as.numeric(if(is.matrix(fc$lower)) fc$lower[,2] else fc$lower), upper = as.numeric(if(is.matrix(fc$upper)) fc$upper[,2] else fc$upper), method = if (inherits(fit, "tbats")) "TBATS (ساعتی)" else "BATS (ساعتی)")
}

# ════════════════════════════════════════════════════════════════════════════
# بخش ۷: Dispatcher ساعتی
# ════════════════════════════════════════════════════════════════════════════
run_hourly_model <- function(model_name, hourly_df, horizon_h = 24, target = "temperature", use_multivariate = FALSE) {
  mn <- tolower(trimws(model_name))
  
  hourly_df$timestamp <- as.POSIXct(hourly_df$timestamp, tz = "Asia/Tehran")
  
  if (nrow(hourly_df) < 48) stop("حداقل ۴۸ ردیف ساعتی برای آموزش نیاز است.")
  hourly_df <- dplyr::arrange(hourly_df, timestamp)
  vals_clean <- hourly_df[[target]]
  vals_clean[is.na(vals_clean)] <- zoo::na.approx(vals_clean, na.rm = FALSE)[is.na(vals_clean)]
  vals_clean[is.na(vals_clean)] <- mean(vals_clean, na.rm = TRUE)
  hourly_df[[target]] <- vals_clean
  
  result <- NULL
  if (mn == "xgboost") result <- forecast_hourly_xgboost(hourly_df, horizon_h, target, use_multivariate)
  else if (mn == "lightgbm") result <- forecast_hourly_lightgbm(hourly_df, horizon_h, target, use_multivariate)
  else if (mn == "catboost") result <- forecast_hourly_catboost(hourly_df, horizon_h, target, use_multivariate)
  else if (mn == "rf") result <- forecast_hourly_rf(hourly_df, horizon_h, target, use_multivariate)
  else if (mn == "arima") result <- forecast_hourly_arima(hourly_df, horizon_h, target)
  else if (mn == "sarima") result <- forecast_hourly_sarima(hourly_df, horizon_h, target)
  else if (mn == "ets") result <- forecast_hourly_ets(hourly_df, horizon_h, target)
  else if (mn == "tbats") result <- forecast_hourly_tbats(hourly_df, horizon_h, target)
  else if (mn == "prophet") result <- forecast_hourly_prophet(hourly_df, horizon_h, target)
  else if (mn == "svm") result <- forecast_hourly_svm(hourly_df, horizon_h, target, use_multivariate)
  else if (mn == "naive") {
    preds <- rep(tail(vals_clean, 24), ceiling(horizon_h / 24))[seq_len(horizon_h)]
    result <- list(predictions = preds, method = "Naïve (ساعتی)")
  } else stop("مدل ساعتی ناشناخته: ", mn)
  
  if (!.valid_hourly_result(result, horizon_h)) {
    result <- hourly_fallback_forecast(hourly_df, horizon_h, target, paste0(" — ", mn))
  }
  result
}