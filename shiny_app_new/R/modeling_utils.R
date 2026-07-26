# File: R/modeling_utils.R
# پیاده‌سازی تمام مدل‌های پیش‌بینی سری زمانی
#
# ── Changelog (Refactor کامل) ────────────────────────────────────────────────
#   1. ARIMA، ETS، TBATS و Prophet از AutoML حذف شدند (فقط در انتخاب تکی در دسترسند)
#   2. LightGBM اضافه شد (روزانه + ساعتی)
#   3. CatBoost اضافه شد (روزانه + ساعتی)
#   4. randomForest جایگزین شد با ranger (روزانه + ساعتی)
#   5. Early Stopping در XGBoost، LightGBM و CatBoost (روزانه + ساعتی)
#   6. Hyperparameterهای بهتر برای داده‌ی هواشناسی
#   7. Refactor کامل: کاهش تکرار کد با helperهای مشترک
#   8. سازگاری کامل با پروژه‌ی Shiny حفظ شده
#   9. پشتیبانی از پیش‌بینی چندمتغیره (Multivariate) در مدل‌های ساعتی
# ─────────────────────────────────────────────────────────────────────────────

# ════════════════════════════════════════════════════════════════════════════
# بخش ۱: مدل‌های کلاسیک سری زمانی (فقط برای انتخاب تکی — در AutoML نیستند)
# ════════════════════════════════════════════════════════════════════════════

# ── پیش‌بینی ARIMA ────────────────────────────────────────────────────────────
forecast_arima <- function(train_df, horizon, target = "temperature") {
  ts_data  <- prepare_ts_data(train_df, variable = target, freq = 7)
  
  if (length(ts_data) > 1095) {
    ts_data <- tail(ts_data, 1095)
    message("[ARIMA] محدود شد به ۱۰۹۵ روز آخر")
  }
  
  fit <- tryCatch(
    forecast::Arima(ts_data, order = c(1, 0, 1),
                    seasonal = list(order = c(1, 0, 0), period = 7)),
    error = function(e) forecast::auto.arima(
      ts_data, seasonal = TRUE,
      stepwise = TRUE, approximation = TRUE,
      max.p = 5, max.q = 5, max.P = 1, max.Q = 1
    )
  )
  fc <- forecast::forecast(fit, h = horizon)
  list(
    model       = fit,
    predictions = as.numeric(fc$mean),
    lower       = as.numeric(fc$lower[, 2]),
    upper       = as.numeric(fc$upper[, 2]),
    method      = "ARIMA"
  )
}

# ── پیش‌بینی SARIMA ───────────────────────────────────────────────────────────
forecast_sarima <- function(train_df, horizon, target = "temperature") {
  ts_data <- prepare_ts_data(train_df, variable = target, freq = 7)
  
  if (length(ts_data) > 1095) {
    ts_data <- tail(ts_data, 1095)
    message("[SARIMA] محدود شد به ۱۰۹۵ روز آخر")
  }
  
  fit <- tryCatch(
    forecast::Arima(ts_data, order = c(1, 0, 1),
                    seasonal = list(order = c(1, 0, 1), period = 7)),
    error = function(e) forecast::auto.arima(
      ts_data, seasonal = TRUE,
      stepwise = TRUE, approximation = TRUE,
      max.p = 3, max.q = 3, max.P = 1, max.Q = 1
    )
  )
  fc <- forecast::forecast(fit, h = horizon)
  list(
    model       = fit,
    predictions = as.numeric(fc$mean),
    lower       = as.numeric(fc$lower[, 2]),
    upper       = as.numeric(fc$upper[, 2]),
    method      = "SARIMA"
  )
}

# ── پیش‌بینی ETS ──────────────────────────────────────────────────────────────
forecast_ets <- function(train_df, horizon, target = "temperature") {
  ts_data <- prepare_ts_data(train_df, variable = target, freq = 7)
  fit <- tryCatch(
    forecast::ets(ts_data, model = "AAA"),
    error = function(e) tryCatch(
      forecast::ets(ts_data, model = "AAN"),
      error = function(e2) forecast::ets(ts_data)
    )
  )
  fc <- forecast::forecast(fit, h = horizon)
  list(
    model       = fit,
    predictions = as.numeric(fc$mean),
    lower       = as.numeric(fc$lower[, 2]),
    upper       = as.numeric(fc$upper[, 2]),
    method      = "ETS"
  )
}

# ── پیش‌بینی TBATS ────────────────────────────────────────────────────────────
forecast_tbats <- function(train_df, horizon, target = "temperature") {
  ts_data <- prepare_ts_data(train_df, variable = target, freq = 7)
  
  if (length(ts_data) > 730) {
    ts_data <- tail(ts_data, 730)
    message("[TBATS] محدود شد به ۷۳۰ روز آخر")
  }
  
  fit <- tryCatch(
    forecast::tbats(
      ts_data,
      use.parallel     = FALSE,
      use.box.cox      = FALSE,
      use.trend        = TRUE,
      use.damped.trend = NULL,
      num.arma.errors  = 1
    ),
    error = function(e) {
      message("[TBATS] خطا، استفاده از BATS به‌عنوان fallback")
      tryCatch(
        forecast::bats(
          ts_data,
          use.parallel     = FALSE,
          use.box.cox      = FALSE,
          use.trend        = TRUE,
          use.damped.trend = FALSE
        ),
        error = function(e2) NULL
      )
    }
  )
  
  if (is.null(fit)) stop("TBATS و fallback آن هر دو شکست خوردند")
  
  fc <- forecast::forecast(fit, h = horizon)
  list(
    model       = fit,
    predictions = as.numeric(fc$mean),
    lower       = as.numeric(fc$lower[, 2]),
    upper       = as.numeric(fc$upper[, 2]),
    method      = if (inherits(fit, "tbats")) "TBATS" else "BATS (TBATS fallback)"
  )
}

# ── پیش‌بینی Prophet ──────────────────────────────────────────────────────────
forecast_prophet <- function(train_df, horizon, target = "temperature") {
  prophet_df <- prepare_prophet_data(train_df, target = target)
  
  if (nrow(prophet_df) > 730) {
    prophet_df <- tail(prophet_df, 730)
    message("[Prophet] محدود شد به ۷۳۰ روز آخر")
  }
  
  m <- prophet::prophet(
    yearly.seasonality = TRUE,
    weekly.seasonality = TRUE,
    daily.seasonality  = FALSE,
    seasonality.mode   = "additive"
  )
  m <- prophet::fit.prophet(m, prophet_df)
  
  future     <- prophet::make_future_dataframe(m, periods = horizon)
  forecast_df <- stats::predict(m, future)
  fc_rows    <- tail(forecast_df, horizon)
  
  list(
    model       = m,
    predictions = as.numeric(fc_rows$yhat),
    lower       = as.numeric(fc_rows$yhat_lower),
    upper       = as.numeric(fc_rows$yhat_upper),
    method      = "Prophet"
  )
}

# ════════════════════════════════════════════════════════════════════════════
# بخش ۲: Helperهای مشترک برای مدل‌های ML (کاهش تکرار کد)
# ════════════════════════════════════════════════════════════════════════════

# ── پیش‌بینی بازگشتی چندمرحله‌ای برای مدل‌های مبتنی بر ویژگی ──────────────────
recursive_feature_forecast <- function(predict_fn, train_df, horizon,
                                       target, feat_cols,
                                       lag_days = c(1, 2, 3, 7, 14, 21)) {
  
  last_date <- max(train_df$date)
  max_lag   <- max(c(lag_days, 30))
  
  history_vals <- tail(train_df[[target]], max_lag)
  n_hist       <- length(history_vals)
  buffer_vals  <- c(history_vals, rep(NA_real_, horizon))
  
  last_row <- dplyr::slice_tail(train_df, n = 1)
  
  safe_window <- function(buf, from, to) {
    from <- max(1, from)
    if (to < from) return(buf[max(1, to)])
    buf[from:to]
  }
  
  predictions <- numeric(horizon)
  
  for (h in seq_len(horizon)) {
    current_date <- last_date + h
    pos          <- n_hist + h
    
    cal <- tibble::tibble(date = current_date) %>% add_calendar_features()
    
    for (lag in lag_days) {
      idx <- pos - lag
      cal[[paste0("lag_", lag)]] <- if (idx >= 1) buffer_vals[idx] else buffer_vals[1]
    }
    
    cal$rolling_mean_7  <- mean(safe_window(buffer_vals, pos - 7,  pos - 1), na.rm = TRUE)
    cal$rolling_mean_14 <- mean(safe_window(buffer_vals, pos - 14, pos - 1), na.rm = TRUE)
    cal$rolling_mean_30 <- mean(safe_window(buffer_vals, pos - 30, pos - 1), na.rm = TRUE)
    cal$rolling_sd_7    <- stats::sd(safe_window(buffer_vals, pos - 7, pos - 1), na.rm = TRUE)
    if (!is.finite(cal$rolling_sd_7)) cal$rolling_sd_7 <- 0
    
    for (fc in feat_cols) {
      if (!fc %in% names(cal)) {
        cal[[fc]] <- if (fc %in% names(last_row)) last_row[[fc]][1] else 0
      }
    }
    
    X_row  <- as.matrix(cal[, feat_cols, drop = FALSE])
    pred_h <- as.numeric(predict_fn(X_row))[1]
    
    predictions[h]   <- pred_h
    buffer_vals[pos] <- pred_h
  }
  
  predictions
}

# ── آماده‌سازی مشترک داده‌ی روزانه برای همه مدل‌های ML ─────────────────────────
.prepare_ml_training_data <- function(train_df, target = "temperature") {
  feat_df   <- build_feature_matrix(train_df, target = target)
  feat_cols <- setdiff(names(feat_df), c("date", target, "station_id",
                                         "timestamp", "pressure", "w_code"))
  feat_cols <- intersect(feat_cols, names(feat_df))
  
  list(
    feat_df   = feat_df,
    feat_cols = feat_cols,
    X_train   = as.matrix(feat_df[, feat_cols, drop = FALSE]),
    y_train   = feat_df[[target]]
  )
}

# ── تقسیم آموزش/اعتبارسنجی داخلی برای Early Stopping ─────────────────────────
.train_valid_split_idx <- function(n, valid_ratio = 0.15, min_valid = 10) {
  n_valid <- max(min_valid, floor(n * valid_ratio))
  n_valid <- min(n_valid, n - min_valid)
  if (n_valid < 5 || n - n_valid < 20) return(NULL)
  list(train_idx = seq_len(n - n_valid), valid_idx = (n - n_valid + 1):n)
}

# ── محاسبه‌ی فاصله‌ی اطمینان بر اساس خطای آموزش + رشد با ریشه افق ─────────────
.compute_ci <- function(preds, resid_sd, horizon) {
  growth <- sqrt(seq_len(horizon))
  list(
    lower = preds - 1.96 * resid_sd * growth,
    upper = preds + 1.96 * resid_sd * growth
  )
}

# ════════════════════════════════════════════════════════════════════════════
# بخش ۳: مدل‌های ML روزانه (اعضای AutoML)
# ════════════════════════════════════════════════════════════════════════════

# ── پیش‌بینی Random Forest (ranger) ───────────────────────────────────────────
forecast_rf <- function(train_df, horizon, target = "temperature") {
  prep      <- .prepare_ml_training_data(train_df, target)
  feat_cols <- prep$feat_cols
  X_train   <- prep$X_train
  y_train   <- prep$y_train
  
  fit <- ranger::ranger(
    x               = X_train,
    y               = y_train,
    num.trees       = 500,
    mtry            = max(1, floor(ncol(X_train) / 3)),
    min.node.size   = 5,
    sample.fraction = 0.8,
    importance      = "impurity",
    keep.inbag      = FALSE
  )
  
  preds <- recursive_feature_forecast(
    predict_fn = function(X_row) {
      newdata <- as.data.frame(X_row)
      names(newdata) <- feat_cols
      stats::predict(fit, data = newdata)$predictions
    },
    train_df  = train_df,
    horizon   = horizon,
    target    = target,
    feat_cols = feat_cols
  )
  
  ci <- .compute_ci(preds, sqrt(fit$prediction.error), horizon)
  
  list(
    model       = fit,
    predictions = preds,
    lower       = ci$lower,
    upper       = ci$upper,
    method      = "Random Forest (ranger)"
  )
}

# ── پیش‌بینی XGBoost (با Early Stopping) ──────────────────────────────────────
forecast_xgboost <- function(train_df, horizon, target = "temperature") {
  prep      <- .prepare_ml_training_data(train_df, target)
  feat_cols <- prep$feat_cols
  X_train   <- prep$X_train
  y_train   <- prep$y_train
  
  params <- list(
    objective        = "reg:squarederror",
    max_depth        = 6,
    eta              = 0.05,
    subsample        = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 3,
    gamma            = 0.1
  )
  
  split <- .train_valid_split_idx(nrow(X_train))
  
  if (!is.null(split)) {
    dtr <- xgboost::xgb.DMatrix(X_train[split$train_idx, , drop = FALSE],
                                label = y_train[split$train_idx])
    dva <- xgboost::xgb.DMatrix(X_train[split$valid_idx, , drop = FALSE],
                                label = y_train[split$valid_idx])
    fit <- xgboost::xgb.train(
      params                = params,
      data                  = dtr,
      nrounds               = 500,
      watchlist             = list(train = dtr, valid = dva),
      early_stopping_rounds = 20,
      verbose               = 0
    )
  } else {
    dtr <- xgboost::xgb.DMatrix(X_train, label = y_train)
    fit <- xgboost::xgb.train(params = params, data = dtr, nrounds = 100, verbose = 0)
  }
  
  dtrain_full <- xgboost::xgb.DMatrix(X_train, label = y_train)
  
  preds <- recursive_feature_forecast(
    predict_fn = function(X_row) stats::predict(fit, xgboost::xgb.DMatrix(X_row)),
    train_df   = train_df,
    horizon    = horizon,
    target     = target,
    feat_cols  = feat_cols
  )
  
  resid_sd <- sd(y_train - stats::predict(fit, dtrain_full))
  ci       <- .compute_ci(preds, resid_sd, horizon)
  
  list(
    model       = fit,
    predictions = preds,
    lower       = ci$lower,
    upper       = ci$upper,
    method      = "XGBoost"
  )
}

# ── پیش‌بینی LightGBM (با Early Stopping) ─────────────────────────────────────
forecast_lightgbm <- function(train_df, horizon, target = "temperature") {
  if (!requireNamespace("lightgbm", quietly = TRUE)) {
    stop("پکیج lightgbm نصب نیست. نصب با: install.packages('lightgbm')")
  }
  
  prep      <- .prepare_ml_training_data(train_df, target)
  feat_cols <- prep$feat_cols
  X_train   <- prep$X_train
  y_train   <- prep$y_train
  
  params <- list(
    objective        = "regression",
    metric           = "rmse",
    num_leaves       = 31,
    max_depth        = 6,
    learning_rate    = 0.05,
    feature_fraction = 0.8,
    bagging_fraction = 0.8,
    bagging_freq     = 1,
    min_data_in_leaf = 10
  )
  
  split <- .train_valid_split_idx(nrow(X_train))
  
  if (!is.null(split)) {
    dtr <- lightgbm::lgb.Dataset(X_train[split$train_idx, , drop = FALSE],
                                 label = y_train[split$train_idx])
    dva <- lightgbm::lgb.Dataset.create.valid(dtr,
                                              X_train[split$valid_idx, , drop = FALSE],
                                              label = y_train[split$valid_idx])
    fit <- lightgbm::lgb.train(
      params                = params,
      data                  = dtr,
      nrounds               = 500,
      valids                = list(valid = dva),
      early_stopping_rounds = 20,
      verbose               = -1
    )
  } else {
    dtr <- lightgbm::lgb.Dataset(X_train, label = y_train)
    fit <- lightgbm::lgb.train(params = params, data = dtr, nrounds = 100, verbose = -1)
  }
  
  preds <- recursive_feature_forecast(
    predict_fn = function(X_row) stats::predict(fit, X_row),
    train_df   = train_df,
    horizon    = horizon,
    target     = target,
    feat_cols  = feat_cols
  )
  
  resid_sd <- sd(y_train - stats::predict(fit, X_train))
  ci       <- .compute_ci(preds, resid_sd, horizon)
  
  list(
    model       = fit,
    predictions = preds,
    lower       = ci$lower,
    upper       = ci$upper,
    method      = "LightGBM"
  )
}

# ── پیش‌بینی CatBoost (با Early Stopping) ─────────────────────────────────────
forecast_catboost <- function(train_df, horizon, target = "temperature") {
  if (!requireNamespace("catboost", quietly = TRUE)) {
    stop("پکیج catboost نصب نیست. راهنمای نصب: https://catboost.ai/docs/en/concepts/r-installation")
  }
  
  prep      <- .prepare_ml_training_data(train_df, target)
  feat_cols <- prep$feat_cols
  X_train   <- prep$X_train
  y_train   <- prep$y_train
  
  split <- .train_valid_split_idx(nrow(X_train))
  
  train_idx <- if (!is.null(split)) split$train_idx else seq_len(nrow(X_train))
  
  train_pool <- catboost::catboost.load_pool(
    data  = as.data.frame(X_train[train_idx, , drop = FALSE]),
    label = y_train[train_idx]
  )
  
  params <- list(
    loss_function    = "RMSE",
    iterations       = 500,
    depth            = 6,
    learning_rate    = 0.05,
    l2_leaf_reg      = 3,
    logging_level    = "Silent"
  )
  
  if (!is.null(split)) {
    valid_pool <- catboost::catboost.load_pool(
      data  = as.data.frame(X_train[split$valid_idx, , drop = FALSE]),
      label = y_train[split$valid_idx]
    )
    params$early_stopping_rounds <- 20
    fit <- catboost::catboost.train(train_pool, valid_pool, params = params)
  } else {
    params$iterations <- 150
    fit <- catboost::catboost.train(train_pool, params = params)
  }
  
  preds <- recursive_feature_forecast(
    predict_fn = function(X_row) {
      pool <- catboost::catboost.load_pool(data = as.data.frame(X_row))
      catboost::catboost.predict(fit, pool)
    },
    train_df   = train_df,
    horizon    = horizon,
    target     = target,
    feat_cols  = feat_cols
  )
  
  full_pool <- catboost::catboost.load_pool(data = as.data.frame(X_train))
  resid_sd  <- sd(y_train - catboost::catboost.predict(fit, full_pool))
  ci        <- .compute_ci(preds, resid_sd, horizon)
  
  list(
    model       = fit,
    predictions = preds,
    lower       = ci$lower,
    upper       = ci$upper,
    method      = "CatBoost"
  )
}

# ── پیش‌بینی SVM ──────────────────────────────────────────────────────────────
forecast_svm <- function(train_df, horizon, target = "temperature") {
  prep      <- .prepare_ml_training_data(train_df, target)
  feat_cols <- prep$feat_cols
  X_train   <- prep$X_train
  y_train   <- prep$y_train
  
  fit <- e1071::svm(
    x       = X_train,
    y       = y_train,
    kernel  = "radial",
    cost    = 10,
    epsilon = 0.1
  )
  
  preds <- recursive_feature_forecast(
    predict_fn = function(X_row) stats::predict(fit, newdata = X_row),
    train_df   = train_df,
    horizon    = horizon,
    target     = target,
    feat_cols  = feat_cols
  )
  
  train_preds <- as.numeric(stats::predict(fit, newdata = X_train))
  resid_sd    <- sd(y_train - train_preds)
  ci          <- .compute_ci(preds, resid_sd, horizon)
  
  list(
    model       = fit,
    predictions = preds,
    lower       = ci$lower,
    upper       = ci$upper,
    method      = "SVM"
  )
}

# ── پیش‌بینی Naïve (بیس‌لاین) ───────────────────────────────────────────────
forecast_naive <- function(train_df, horizon, target = "temperature") {
  ts_data <- prepare_ts_data(train_df, variable = target, freq = 7)
  fit     <- forecast::naive(ts_data, h = horizon)
  list(
    model       = fit,
    predictions = as.numeric(fit$mean),
    lower       = as.numeric(fit$lower[, 2]),
    upper       = as.numeric(fit$upper[, 2]),
    method      = "Naïve"
  )
}

# ════════════════════════════════════════════════════════════════════════════
# بخش ۴: Dispatcher و فهرست مدل‌ها
# ════════════════════════════════════════════════════════════════════════════

run_model_by_name <- function(model_name, train_df, horizon,
                              target = "temperature") {
  model_name <- as.character(model_name)[1]
  model_name <- gsub("[\u200B\u200C\u200D\u200E\u200F\u202A-\u202E\uFEFF]", "", model_name)
  model_name <- trimws(model_name)
  
  fn <- switch(model_name,
               arima     = forecast_arima,
               sarima    = forecast_sarima,
               ets       = forecast_ets,
               tbats     = forecast_tbats,
               prophet   = forecast_prophet,
               rf        = forecast_rf,
               xgboost   = forecast_xgboost,
               lightgbm  = forecast_lightgbm,
               catboost  = forecast_catboost,
               svm       = forecast_svm,
               naive     = forecast_naive,
               stop("مدل ناشناخته: ", model_name)
  )
  fn(train_df, horizon, target)
}

AUTOML_MODEL_NAMES <- c("sarima", "rf", "xgboost", "lightgbm",
                        "catboost", "svm", "naive")

# ════════════════════════════════════════════════════════════════════════════
# بخش ۵: AutoML و Ensemble
# ════════════════════════════════════════════════════════════════════════════

run_automl <- function(data, horizon, target = "temperature",
                       ensemble_tol = 0.05) {
  
  message("=== شروع AutoML ===")
  
  splits    <- train_test_split(data, test_ratio = 0.15)
  train_df  <- splits$train
  test_df   <- splits$test
  test_vals <- test_df[[target]]
  test_h    <- nrow(test_df)
  
  model_names <- AUTOML_MODEL_NAMES
  results     <- list()
  
  for (mn in model_names) {
    message("آموزش مدل: ", mn)
    tryCatch({
      fc            <- run_model_by_name(mn, train_df, test_h, target)
      preds         <- fc$predictions[seq_len(min(test_h, length(fc$predictions)))]
      actual        <- test_vals[seq_len(length(preds))]
      m             <- compute_all_metrics(actual, preds, model_name = mn)
      results[[mn]] <- list(forecast = fc, metrics = m)
    }, error = function(e) {
      message("خطا در مدل ", mn, ": ", conditionMessage(e))
    })
  }
  
  if (length(results) == 0) stop("هیچ مدلی آموزش داده نشد")
  
  all_metrics <- dplyr::bind_rows(purrr::map(results, "metrics"))
  all_metrics <- compute_composite_score(all_metrics)
  
  best_model_name <- all_metrics$model[1]
  best_score      <- all_metrics$composite_score[1]
  
  message("بهترین مدل: ", best_model_name, " با نمره: ", round(best_score, 4))
  
  top3       <- all_metrics[1:min(3, nrow(all_metrics)), ]
  score_range <- diff(range(top3$composite_score, na.rm = TRUE))
  
  final_result <- list(
    all_metrics     = all_metrics,
    best_model_name = best_model_name,
    best_score      = best_score,
    all_results     = results
  )
  
  if (score_range <= ensemble_tol && nrow(top3) >= 2) {
    message("نمرات نزدیک هستند — ساخت Ensemble از ", nrow(top3), " مدل برتر")
    
    ensemble_fc <- build_weighted_ensemble(
      model_names = top3$model,
      scores      = top3$composite_score,
      train_df    = data,
      horizon     = horizon,
      target      = target
    )
    
    final_result$best_model_name   <- "Ensemble"
    final_result$ensemble_forecast <- ensemble_fc
    final_result$ensemble_models   <- top3$model
  } else {
    message("آموزش نهایی ", best_model_name, " روی تمام داده...")
    final_fc <- tryCatch(
      run_model_by_name(best_model_name, data, horizon, target),
      error = function(e) {
        message("خطا در آموزش نهایی: ", conditionMessage(e))
        NULL
      }
    )
    final_result$final_forecast <- final_fc
  }
  
  message("=== AutoML کامل شد ===")
  return(final_result)
}

build_weighted_ensemble <- function(model_names, scores, train_df,
                                    horizon, target = "temperature") {
  inv_scores <- 1 / (scores + 1e-6)
  weights    <- inv_scores / sum(inv_scores)
  
  message("وزن‌های Ensemble:")
  for (i in seq_along(model_names)) {
    message("  ", model_names[i], ": ", round(weights[i], 3))
  }
  
  forecasts <- purrr::map(model_names, function(mn) {
    tryCatch(
      run_model_by_name(mn, train_df, horizon, target),
      error = function(e) {
        message("خطا در ensemble مدل ", mn, ": ", conditionMessage(e))
        NULL
      }
    )
  })
  
  valid_idx <- !purrr::map_lgl(forecasts, is.null)
  valid_fcs <- forecasts[valid_idx]
  valid_wts <- weights[valid_idx]
  valid_wts <- valid_wts / sum(valid_wts)
  
  pad_vec <- function(v, n) { length(v) <- n; v }
  
  pred_matrix   <- do.call(cbind, purrr::map(valid_fcs, ~ pad_vec(.x$predictions, horizon)))
  lower_matrix  <- do.call(cbind, purrr::map(valid_fcs, ~ pad_vec(.x$lower, horizon)))
  upper_matrix  <- do.call(cbind, purrr::map(valid_fcs, ~ pad_vec(.x$upper, horizon)))
  
  list(
    predictions = as.numeric(pred_matrix %*% valid_wts),
    lower       = as.numeric(lower_matrix %*% valid_wts),
    upper       = as.numeric(upper_matrix %*% valid_wts),
    weights     = valid_wts,
    models      = model_names[valid_idx],
    method      = "Ensemble"
  )
}

extract_automl_forecast <- function(automl_result) {
  if (!is.null(automl_result$ensemble_forecast)) {
    return(automl_result$ensemble_forecast)
  }
  if (!is.null(automl_result$final_forecast)) {
    return(automl_result$final_forecast)
  }
  best <- automl_result$best_model_name
  if (!is.null(automl_result$all_results[[best]])) {
    return(automl_result$all_results[[best]]$forecast)
  }
  NULL
}

# ════════════════════════════════════════════════════════════════════════════
# بخش ۶: پیش‌بینی ساعتی — آموزش روی داده ساعتی با featureهای زمانی
# ════════════════════════════════════════════════════════════════════════════

# ════════════════════════════════════════════════════════════════════════════
# بخش ۶: پیش‌بینی ساعتی — آموزش روی داده ساعتی با featureهای زمانی
# ════════════════════════════════════════════════════════════════════════════

build_hourly_feature_matrix <- function(df, target = "temperature",
                                        lag_hours = c(1, 2, 3, 24)) {
  df <- df %>%
    dplyr::arrange(timestamp) %>%
    dplyr::mutate(
      hour        = lubridate::hour(timestamp),
      hour_sin    = sin(2 * pi * hour / 24),
      hour_cos    = cos(2 * pi * hour / 24),
      day_of_week = lubridate::wday(timestamp),
      day_of_year = lubridate::yday(timestamp),
      month       = lubridate::month(timestamp),
      year        = lubridate::year(timestamp),
      sin_annual  = sin(2 * pi * day_of_year / 365),
      cos_annual  = cos(2 * pi * day_of_year / 365),
      sin_weekly  = sin(2 * pi * day_of_week / 7),
      cos_weekly  = cos(2 * pi * day_of_week / 7)
    )
  
  for (lg in lag_hours) {
    df[[paste0("lag_", lg, "h")]] <- dplyr::lag(df[[target]], lg)
  }
  
  df$rolling_6h  <- zoo::rollmean(df[[target]], k = 6,  fill = NA, align = "right")
  df$rolling_24h <- zoo::rollmean(df[[target]], k = 24, fill = NA, align = "right")
  
  df %>% tidyr::drop_na()
}

HOURLY_FEAT_COLS <- c(
  "hour", "hour_sin", "hour_cos",
  "day_of_week", "month", "sin_annual", "cos_annual", "sin_weekly", "cos_weekly",
  "lag_1h", "lag_2h", "lag_3h", "lag_24h", "rolling_6h", "rolling_24h",
  "temperature", "humidity", "wind_speed", "precipitation"
)

.prepare_hourly_training_data <- function(hourly_df, target = "temperature", use_multivariate = FALSE) {
  if (nrow(hourly_df) > 1440) {
    hourly_df <- tail(hourly_df, 1440)
    message("[Hourly ML] محدود شد به ۱۴۴۰ ساعت آخر")
  }
  
  feat_df <- build_hourly_feature_matrix(hourly_df, target)
  if (nrow(feat_df) < 10) stop("داده کافی بعد از حذف NA باقی نمانده")
  
  # اگر حالت تک‌متغیره باشد، سایر متغیرهای هواشناسی حذف می‌شوند
  exog_vars <- setdiff(c("temperature", "humidity", "wind_speed", "precipitation"), target)
  if (isTRUE(use_multivariate)) {
    feat_use <- setdiff(intersect(HOURLY_FEAT_COLS, names(feat_df)), target)
  } else {
    feat_use <- setdiff(intersect(HOURLY_FEAT_COLS, names(feat_df)), c(target, exog_vars))
  }
  
  list(
    feat_df   = feat_df,
    feat_use  = feat_use,
    X_train   = as.matrix(feat_df[, feat_use, drop = FALSE]),
    y_train   = feat_df[[target]],
    hourly_df = hourly_df
  )
}

hourly_recursive_forecast <- function(predict_fn, model, train_df, horizon_h, target, feat_use) {
  last_ts    <- max(train_df$timestamp)
  history    <- tail(train_df[[target]], 48)
  n_hist     <- length(history)
  buf        <- c(history, rep(NA_real_, horizon_h))
  
  # متغیرهای کمکی (Exogenous) - فقط اگر در feat_use باشند
  exog_vars <- setdiff(c("temperature", "humidity", "wind_speed", "precipitation"), target)
  active_exog_vars <- intersect(exog_vars, feat_use)
  last_exog <- if (length(active_exog_vars) > 0) tail(train_df[, active_exog_vars, drop = FALSE], 1) else NULL
  
  preds <- numeric(horizon_h)
  
  for (h in seq_len(horizon_h)) {
    ts_h <- last_ts + h * 3600
    pos  <- n_hist + h
    
    get_lag <- function(k) {
      idx <- pos - k
      if (idx < 1) return(mean(history, na.rm = TRUE))
      v <- buf[idx]
      if (is.na(v)) mean(history, na.rm = TRUE) else v
    }
    
    lag_1h  <- get_lag(1)
    lag_2h  <- get_lag(2)
    lag_3h  <- get_lag(3)
    lag_24h <- get_lag(24)
    
    avail    <- buf[max(1, pos - 24):(pos - 1)]
    avail    <- avail[!is.na(avail)]
    roll_6h  <- if (length(avail) >= 6)  mean(tail(avail, 6))  else mean(history, na.rm = TRUE)
    roll_24h <- if (length(avail) >= 24) mean(tail(avail, 24)) else mean(history, na.rm = TRUE)
    
    hour_val  <- as.integer(format(ts_h, "%H"))
    dow_val   <- as.integer(format(ts_h, "%u"))
    doy_val   <- as.integer(format(ts_h, "%j"))
    month_val <- as.integer(format(ts_h, "%m"))
    
    newrow <- data.frame(
      hour        = hour_val,
      hour_sin    = sin(2 * pi * hour_val / 24),
      hour_cos    = cos(2 * pi * hour_val / 24),
      day_of_week = dow_val,
      month       = month_val,
      sin_annual  = sin(2 * pi * doy_val / 365),
      cos_annual  = cos(2 * pi * doy_val / 365),
      sin_weekly  = sin(2 * pi * dow_val / 7),
      cos_weekly  = cos(2 * pi * dow_val / 7),
      lag_1h      = lag_1h,
      lag_2h      = lag_2h,
      lag_3h      = lag_3h,
      lag_24h     = lag_24h,
      rolling_6h  = roll_6h,
      rolling_24h = roll_24h
    )
    
    # استفاده از آخرین مقدار شناخته شده برای متغیرهای کمکی در تمام ۲۴ ساعت آینده
    if (!is.null(last_exog)) {
      for (ev in active_exog_vars) {
        newrow[[ev]] <- last_exog[[ev]][1]
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
  
  hours_idx  <- (lubridate::hour(hourly_df$timestamp) %% 24) + 1
  hourly_avg <- sapply(0:23, function(h) {
    idx <- which(hours_idx == (h + 1))
    if (length(idx) > 0) mean(vals[idx], na.rm = TRUE) else mean(vals, na.rm = TRUE)
  })
  
  last_hour    <- lubridate::hour(max(hourly_df$timestamp))
  recent_mean  <- mean(tail(vals, min(24, n)), na.rm = TRUE)
  overall_mean <- mean(vals, na.rm = TRUE)
  drift        <- recent_mean - overall_mean
  
  preds <- numeric(horizon_h)
  for (h in seq_len(horizon_h)) {
    hh <- (last_hour + h) %% 24
    preds[h] <- hourly_avg[hh + 1] + drift
  }
  
  sd_val <- stats::sd(vals, na.rm = TRUE)
  if (!is.finite(sd_val) || sd_val == 0)
    sd_val <- max(1, abs(mean(vals, na.rm = TRUE)) * 0.1)
  
  list(
    predictions = preds,
    lower       = preds - 1.96 * sd_val,
    upper       = preds + 1.96 * sd_val,
    method      = paste0("الگوی میانگین ساعتی", method_suffix, " (fallback)")
  )
}

.valid_hourly_result <- function(res, horizon_h) {
  !is.null(res) &&
    !is.null(res$predictions) &&
    length(res$predictions) == horizon_h &&
    all(is.finite(res$predictions))
}

forecast_hourly_xgboost <- function(hourly_df, horizon_h = 24, target = "temperature", use_multivariate = FALSE) {
  result <- tryCatch({
    prep     <- .prepare_hourly_training_data(hourly_df, target, use_multivariate)
    feat_use <- prep$feat_use
    X_train  <- prep$X_train
    y_train  <- prep$y_train
    
    params <- list(objective = "reg:squarederror", max_depth = 6, eta = 0.05, subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 3, gamma = 0.1)
    split <- .train_valid_split_idx(nrow(X_train))
    
    if (!is.null(split)) {
      dtr <- xgboost::xgb.DMatrix(X_train[split$train_idx, , drop = FALSE], label = y_train[split$train_idx])
      dva <- xgboost::xgb.DMatrix(X_train[split$valid_idx, , drop = FALSE], label = y_train[split$valid_idx])
      model <- xgboost::xgb.train(params = params, data = dtr, nrounds = 500, watchlist = list(train = dtr, valid = dva), early_stopping_rounds = 20, verbose = 0)
    } else {
      dtr   <- xgboost::xgb.DMatrix(X_train, label = y_train)
      model <- xgboost::xgb.train(params = params, data = dtr, nrounds = 100, verbose = 0)
    }
    
    preds <- hourly_recursive_forecast(
      predict_fn = function(m, nd) stats::predict(m, xgboost::xgb.DMatrix(as.matrix(nd[, feat_use, drop = FALSE]))),
      model      = model, train_df = prep$hourly_df, horizon_h = horizon_h, target = target, feat_use = feat_use
    )
    
    imp <- tryCatch(xgboost::xgb.importance(model = model), error = function(e) NULL)
    feat_imp <- if (!is.null(imp)) imp[, c("Feature", "Gain")] else NULL
    mv_tag <- if (isTRUE(use_multivariate)) "، چندمتغیره" else ""
    list(predictions = preds, method = paste0("XGBoost (ساعتی", mv_tag, ")"), feat_imp = feat_imp)
  }, error = function(e) { message("XGBoost ساعتی شکست خورد: ", conditionMessage(e)); NULL })
  
  if (!.valid_hourly_result(result, horizon_h)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — XGBoost"))
  result
}

forecast_hourly_rf <- function(hourly_df, horizon_h = 24, target = "temperature", use_multivariate = FALSE) {
  result <- tryCatch({
    prep     <- .prepare_hourly_training_data(hourly_df, target, use_multivariate)
    feat_use <- prep$feat_use
    X_train  <- prep$X_train
    y_train  <- prep$y_train
    
    model <- ranger::ranger(x = X_train, y = y_train, num.trees = 500, mtry = max(1, floor(ncol(X_train) / 3)), min.node.size = 5, sample.fraction = 0.8, importance = "impurity", keep.inbag = FALSE)
    
    preds <- hourly_recursive_forecast(
      predict_fn = function(m, nd) { newdata <- as.data.frame(as.matrix(nd[, feat_use, drop = FALSE])); names(newdata) <- feat_use; stats::predict(m, data = newdata)$predictions },
      model      = model, train_df = prep$hourly_df, horizon_h = horizon_h, target = target, feat_use = feat_use
    )
    
    imp <- tryCatch(model$variable.importance, error = function(e) NULL)
    feat_imp <- if (!is.null(imp)) data.frame(Feature = names(imp), Gain = unname(imp)) else NULL
    mv_tag <- if (isTRUE(use_multivariate)) "، چندمتغیره" else ""
    list(predictions = preds, method = paste0("Random Forest (ساعتی", mv_tag, ")"), feat_imp = feat_imp)
  }, error = function(e) { message("Random Forest ساعتی شکست خورد: ", conditionMessage(e)); NULL })
  
  if (!.valid_hourly_result(result, horizon_h)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — RF"))
  result
}

forecast_hourly_lightgbm <- function(hourly_df, horizon_h = 24, target = "temperature", use_multivariate = FALSE) {
  if (!requireNamespace("lightgbm", quietly = TRUE)) stop("پکیج lightgbm نصب نیست.")
  
  result <- tryCatch({
    prep     <- .prepare_hourly_training_data(hourly_df, target, use_multivariate)
    feat_use <- prep$feat_use
    X_train  <- prep$X_train
    y_train  <- prep$y_train
    
    params <- list(objective = "regression", metric = "rmse", num_leaves = 31, max_depth = 6, learning_rate = 0.05, feature_fraction = 0.8, bagging_fraction = 0.8, bagging_freq = 1, min_data_in_leaf = 10)
    split <- .train_valid_split_idx(nrow(X_train))
    
    if (!is.null(split)) {
      dtr <- lightgbm::lgb.Dataset(X_train[split$train_idx, , drop = FALSE], label = y_train[split$train_idx])
      dva <- lightgbm::lgb.Dataset.create.valid(dtr, X_train[split$valid_idx, , drop = FALSE], label = y_train[split$valid_idx])
      model <- lightgbm::lgb.train(params = params, data = dtr, nrounds = 500, valids = list(valid = dva), early_stopping_rounds = 20, verbose = -1)
    } else {
      dtr   <- lightgbm::lgb.Dataset(X_train, label = y_train)
      model <- lightgbm::lgb.train(params = params, data = dtr, nrounds = 100, verbose = -1)
    }
    
    preds <- hourly_recursive_forecast(
      predict_fn = function(m, nd) stats::predict(m, as.matrix(nd[, feat_use, drop = FALSE])),
      model      = model, train_df = prep$hourly_df, horizon_h = horizon_h, target = target, feat_use = feat_use
    )
    
    imp <- tryCatch(lightgbm::lgb.importance(model, percentage = TRUE), error = function(e) NULL)
    feat_imp <- if (!is.null(imp)) imp[, c("Feature", "Gain")] else NULL
    mv_tag <- if (isTRUE(use_multivariate)) "، چندمتغیره" else ""
    list(predictions = preds, method = paste0("LightGBM (ساعتی", mv_tag, ")"), feat_imp = feat_imp)
  }, error = function(e) { message("LightGBM ساعتی شکست خورد: ", conditionMessage(e)); NULL })
  
  if (!.valid_hourly_result(result, horizon_h)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — LightGBM"))
  result
}

forecast_hourly_catboost <- function(hourly_df, horizon_h = 24, target = "temperature", use_multivariate = FALSE) {
  if (!requireNamespace("catboost", quietly = TRUE)) stop("پکیج catboost نصب نیست.")
  
  result <- tryCatch({
    prep     <- .prepare_hourly_training_data(hourly_df, target, use_multivariate)
    feat_use <- prep$feat_use
    X_train  <- prep$X_train
    y_train  <- prep$y_train
    
    split <- .train_valid_split_idx(nrow(X_train))
    train_idx <- if (!is.null(split)) split$train_idx else seq_len(nrow(X_train))
    
    train_pool <- catboost::catboost.load_pool(data = as.data.frame(X_train[train_idx, , drop = FALSE]), label = y_train[train_idx])
    params <- list(loss_function = "RMSE", iterations = 500, depth = 6, learning_rate = 0.05, l2_leaf_reg = 3, logging_level = "Silent")
    
    if (!is.null(split)) {
      valid_pool <- catboost::catboost.load_pool(data = as.data.frame(X_train[split$valid_idx, , drop = FALSE]), label = y_train[split$valid_idx])
      params$early_stopping_rounds <- 20
      model <- catboost::catboost.train(train_pool, valid_pool, params = params)
    } else {
      params$iterations <- 150
      model <- catboost::catboost.train(train_pool, params = params)
    }
    
    preds <- hourly_recursive_forecast(
      predict_fn = function(m, nd) { pool <- catboost::catboost.load_pool(data = as.data.frame(nd[, feat_use, drop = FALSE])); catboost::catboost.predict(m, pool) },
      model      = model, train_df = prep$hourly_df, horizon_h = horizon_h, target = target, feat_use = feat_use
    )
    
    imp <- tryCatch(catboost::catboost.get_feature_importance(model), error = function(e) NULL)
    feat_imp <- if (!is.null(imp)) data.frame(Feature = colnames(X_train), Gain = imp) else NULL
    mv_tag <- if (isTRUE(use_multivariate)) "، چندمتغیره" else ""
    list(predictions = preds, method = paste0("CatBoost (ساعتی", mv_tag, ")"), feat_imp = feat_imp)
  }, error = function(e) { message("CatBoost ساعتی شکست خورد: ", conditionMessage(e)); NULL })
  
  if (!.valid_hourly_result(result, horizon_h)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — CatBoost"))
  result
}

forecast_hourly_svm <- function(hourly_df, horizon_h = 24, target = "temperature", use_multivariate = FALSE) {
  result <- tryCatch({
    prep     <- .prepare_hourly_training_data(hourly_df, target, use_multivariate)
    feat_use <- prep$feat_use
    X_train  <- prep$X_train
    y_train  <- prep$y_train
    
    model <- e1071::svm(x = X_train, y = y_train, kernel = "radial", cost = 10, epsilon = 0.1)
    
    preds <- hourly_recursive_forecast(
      predict_fn = function(m, nd) stats::predict(m, newdata = as.matrix(nd[, feat_use, drop = FALSE])),
      model      = model, train_df = prep$hourly_df, horizon_h = horizon_h, target = target, feat_use = feat_use
    )
    
    mv_tag <- if (isTRUE(use_multivariate)) "، چندمتغیره" else ""
    list(predictions = preds, method = paste0("SVM (ساعتی", mv_tag, ")"))
  }, error = function(e) { message("SVM ساعتی شکست خورد: ", conditionMessage(e)); NULL })
  
  if (!.valid_hourly_result(result, horizon_h)) return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — SVM"))
  result
}

# ── پیش‌بینی ساعتی SARIMA ─────────────────────────────────────────────────────
forecast_hourly_sarima <- function(hourly_df, horizon_h = 24, target = "temperature") {
  vals <- hourly_df[[target]]
  vals[is.na(vals)] <- zoo::na.approx(vals, na.rm = FALSE)[is.na(vals)]
  vals[is.na(vals)] <- mean(vals, na.rm = TRUE)
  hourly_df[[target]] <- vals
  
  n <- length(vals)
  
  if (n < 48)
    return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — SARIMA"))
  
  MAX_HOURLY_FOR_SARIMA <- 720
  if (n > MAX_HOURLY_FOR_SARIMA) {
    vals <- tail(vals, MAX_HOURLY_FOR_SARIMA)
    message("[SARIMA hourly] محدود شدن به ", MAX_HOURLY_FOR_SARIMA,
            " ساعت آخر (از ", n, " ردیف)")
    n <- length(vals)
  }
  
  ts_obj <- ts(vals, frequency = 24)
  
  fit <- tryCatch(
    forecast::Arima(ts_obj, order = c(1, 0, 1),
                    seasonal = list(order = c(1, 0, 1), period = 24)),
    error = function(e) tryCatch(
      forecast::Arima(ts_obj, order = c(1, 1, 1)),
      error = function(e2) tryCatch(
        forecast::auto.arima(tail(ts_obj, 168), seasonal = FALSE,
                             stepwise = TRUE, approximation = TRUE),
        error = function(e3) NULL
      )
    )
  )
  
  if (is.null(fit))
    return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — SARIMA"))
  
  fc <- tryCatch(forecast::forecast(fit, h = horizon_h), error = function(e) NULL)
  if (is.null(fc))
    return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — SARIMA"))
  
  preds <- as.numeric(fc$mean)
  lower <- tryCatch(as.numeric(fc$lower[, 2]), error = function(e) NULL)
  upper <- tryCatch(as.numeric(fc$upper[, 2]), error = function(e) NULL)
  
  valid <- length(preds) == horizon_h &&
    all(is.finite(preds)) &&
    !is.null(lower) && length(lower) == horizon_h && all(is.finite(lower)) &&
    !is.null(upper) && length(upper) == horizon_h && all(is.finite(upper))
  
  if (!valid)
    return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — SARIMA"))
  
  list(
    predictions = preds,
    lower       = lower,
    upper       = upper,
    method      = "SARIMA (ساعتی، freq=24)"
  )
}

# ── پیش‌بینی ساعتی ARIMA ──────────────────────────────────────────────────────
forecast_hourly_arima <- function(hourly_df, horizon_h = 24, target = "temperature") {
  vals <- hourly_df[[target]]
  vals[is.na(vals)] <- zoo::na.approx(vals, na.rm = FALSE)[is.na(vals)]
  vals[is.na(vals)] <- mean(vals, na.rm = TRUE)
  if (length(vals) > 720) vals <- tail(vals, 720)
  
  ts_obj <- ts(vals, frequency = 24)
  
  fit <- tryCatch(
    forecast::Arima(ts_obj, order = c(2, 0, 1),
                    seasonal = list(order = c(1, 0, 0), period = 24)),
    error = function(e) tryCatch(
      forecast::Arima(ts_obj, order = c(1, 1, 1)),
      error = function(e2) tryCatch(
        forecast::auto.arima(tail(ts_obj, 168), seasonal = FALSE,
                             stepwise = TRUE, approximation = TRUE),
        error = function(e3) NULL
      )
    )
  )
  if (is.null(fit))
    return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — ARIMA"))
  
  fc <- tryCatch(forecast::forecast(fit, h = horizon_h), error = function(e) NULL)
  if (is.null(fc))
    return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — ARIMA"))
  
  list(
    predictions = as.numeric(fc$mean),
    lower       = as.numeric(fc$lower[, 2]),
    upper       = as.numeric(fc$upper[, 2]),
    method      = "ARIMA (ساعتی)"
  )
}

# ── پیش‌بینی ساعتی ETS ────────────────────────────────────────────────────────
forecast_hourly_ets <- function(hourly_df, horizon_h = 24, target = "temperature") {
  vals <- hourly_df[[target]]
  vals[is.na(vals)] <- zoo::na.approx(vals, na.rm = FALSE)[is.na(vals)]
  vals[is.na(vals)] <- mean(vals, na.rm = TRUE)
  if (length(vals) > 720) vals <- tail(vals, 720)
  
  ts_obj <- ts(vals, frequency = 24)
  fit <- tryCatch(
    forecast::ets(ts_obj, model = "AAA"),
    error = function(e) tryCatch(
      forecast::ets(ts_obj, model = "AAN"),
      error = function(e2) tryCatch(
        forecast::ets(ts_obj),
        error = function(e3) NULL
      )
    )
  )
  if (is.null(fit))
    return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — ETS"))
  
  fc <- tryCatch(forecast::forecast(fit, h = horizon_h), error = function(e) NULL)
  if (is.null(fc))
    return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — ETS"))
  
  list(
    predictions = as.numeric(fc$mean),
    lower       = as.numeric(fc$lower[, 2]),
    upper       = as.numeric(fc$upper[, 2]),
    method      = "ETS (ساعتی)"
  )
}

# ── پیش‌بینی ساعتی TBATS ──────────────────────────────────────────────────────
forecast_hourly_tbats <- function(hourly_df, horizon_h = 24, target = "temperature") {
  vals <- hourly_df[[target]]
  vals[is.na(vals)] <- zoo::na.approx(vals, na.rm = FALSE)[is.na(vals)]
  vals[is.na(vals)] <- mean(vals, na.rm = TRUE)
  if (length(vals) > 720) vals <- tail(vals, 720)
  ts_obj <- ts(vals, frequency = 24)
  
  fit <- tryCatch(
    forecast::tbats(ts_obj, use.parallel = FALSE, use.box.cox = FALSE),
    error = function(e) tryCatch(
      forecast::bats(ts_obj, use.parallel = FALSE, use.box.cox = FALSE),
      error = function(e2) NULL
    )
  )
  if (is.null(fit))
    return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — TBATS"))
  
  fc <- tryCatch(forecast::forecast(fit, h = horizon_h), error = function(e) NULL)
  if (is.null(fc))
    return(hourly_fallback_forecast(hourly_df, horizon_h, target, " — TBATS"))
  
  list(
    predictions = as.numeric(fc$mean),
    lower       = as.numeric(fc$lower[, 2]),
    upper       = as.numeric(fc$upper[, 2]),
    method      = if (inherits(fit, "tbats")) "TBATS (ساعتی)" else "BATS (ساعتی، TBATS fallback)"
  )
}

# ════════════════════════════════════════════════════════════════════════════
# بخش ۸: Dispatcher ساعتی
# ════════════════════════════════════════════════════════════════════════════

# ── dispatcher: انتخاب مدل ساعتی ──────────────────────────────────────────────
run_hourly_model <- function(model_name, hourly_df, horizon_h = 24, target = "temperature", use_multivariate = FALSE) {
  mn <- tolower(trimws(model_name))
  message("[Dispatcher] درخواست مدل ساعتی: '", mn, "' | Multivariate: ", isTRUE(use_multivariate))
  
  if (nrow(hourly_df) < 48) stop("حداقل ۴۸ ردیف ساعتی برای آموزش نیاز است.")
  
  hourly_df <- dplyr::arrange(hourly_df, timestamp)
  vals_clean <- hourly_df[[target]]
  vals_clean[is.na(vals_clean)] <- zoo::na.approx(vals_clean, na.rm = FALSE)[is.na(vals_clean)]
  vals_clean[is.na(vals_clean)] <- mean(vals_clean, na.rm = TRUE)
  hourly_df[[target]] <- vals_clean
  
  result <- NULL
  
  if (mn == "xgboost") {
    result <- forecast_hourly_xgboost(hourly_df, horizon_h, target, use_multivariate)
  } else if (mn == "lightgbm") {
    result <- forecast_hourly_lightgbm(hourly_df, horizon_h, target, use_multivariate)
  } else if (mn == "catboost") {
    result <- forecast_hourly_catboost(hourly_df, horizon_h, target, use_multivariate)
  } else if (mn == "rf") {
    result <- forecast_hourly_rf(hourly_df, horizon_h, target, use_multivariate)
  } else if (mn == "arima") {
    result <- forecast_hourly_arima(hourly_df, horizon_h, target)
  } else if (mn == "sarima") {
    result <- forecast_hourly_sarima(hourly_df, horizon_h, target)
  } else if (mn == "ets") {
    result <- forecast_hourly_ets(hourly_df, horizon_h, target)
  } else if (mn == "tbats") {
    result <- forecast_hourly_tbats(hourly_df, horizon_h, target)
  } else if (mn == "prophet") {
    result <- forecast_hourly_prophet(hourly_df, horizon_h, target)
  } else if (mn == "svm") {
    result <- forecast_hourly_svm(hourly_df, horizon_h, target, use_multivariate)
  } else if (mn == "naive") {
    last_24 <- tail(vals_clean, 24)
    preds   <- rep(last_24, ceiling(horizon_h / 24))[seq_len(horizon_h)]
    result <- list(predictions = preds, method = "Naïve (ساعتی)")
  } else {
    stop("مدل ساعتی ناشناخته: ", mn)
  }
  
  if (!.valid_hourly_result(result, horizon_h)) {
    message("نتیجه‌ی مدل '", mn, "' نامعتبر بود — fallback استفاده می‌شود")
    result <- hourly_fallback_forecast(hourly_df, horizon_h, target, paste0(" — ", mn))
  }
  
  result
}