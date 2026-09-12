# File: R/metrics_utils.R
# محاسبه معیارهای ارزیابی مدل‌های پیش‌بینی

# ── RMSE: جذر میانگین مربعات خطا ─────────────────────────────────────────────
calc_rmse <- function(actual, predicted) {
  # حذف جفت‌های NA
  idx <- complete.cases(actual, predicted)
  if (sum(idx) == 0) return(NA_real_)
  sqrt(mean((actual[idx] - predicted[idx])^2))
}

# ── MAE: میانگین قدر مطلق خطا ────────────────────────────────────────────────
calc_mae <- function(actual, predicted) {
  idx <- complete.cases(actual, predicted)
  if (sum(idx) == 0) return(NA_real_)
  mean(abs(actual[idx] - predicted[idx]))
}

# ── MAPE: میانگین درصد قدر مطلق خطا ─────────────────────────────────────────
calc_mape <- function(actual, predicted) {
  idx <- complete.cases(actual, predicted) & actual != 0
  if (sum(idx) == 0) return(NA_real_)
  mean(abs((actual[idx] - predicted[idx]) / actual[idx])) * 100
}

# ── R²: ضریب تعیین ────────────────────────────────────────────────────────────
calc_r2 <- function(actual, predicted) {
  idx <- complete.cases(actual, predicted)
  if (sum(idx) < 2) return(NA_real_)
  ss_res <- sum((actual[idx] - predicted[idx])^2)
  ss_tot <- sum((actual[idx] - mean(actual[idx]))^2)
  if (ss_tot == 0) return(NA_real_)
  1 - ss_res / ss_tot
}

# ── SMAPE: میانگین درصد قدر مطلق خطای متقارن ──────────────────────────────────
calc_smape <- function(actual, predicted) {
  idx <- complete.cases(actual, predicted)
  if (sum(idx) == 0) return(NA_real_)
  a <- actual[idx]
  p <- predicted[idx]
  denom <- (abs(a) + abs(p)) / 2
  # جلوگیری از تقسیم بر صفر
  valid <- denom > 0
  if (sum(valid) == 0) return(NA_real_)
  mean(abs(a[valid] - p[valid]) / denom[valid]) * 100
}

# ── محاسبه همه معیارها ───────────────────────────────────────────────────────
compute_all_metrics <- function(actual, predicted, model_name = "model") {
  tibble::tibble(
    model  = model_name,
    RMSE   = calc_rmse(actual, predicted),
    MAE    = calc_mae(actual, predicted),
    MAPE   = calc_mape(actual, predicted),
    R2     = calc_r2(actual, predicted),
    SMAPE  = calc_smape(actual, predicted)
  )
}

# ── نمره ترکیبی نرمال‌شده برای رتبه‌بندی ────────────────────────────────────────
# نمره پایین‌تر بهتر است (مشابه رتبه‌بندی گلف)
compute_composite_score <- function(metrics_df) {
  # 🔴 روش استاندارد Relative Accuracy Score (0 to 100)
  
  # فیلتر کردن مدل‌های کرش کرده برای پیدا کردن بهترین‌ها
  valid_metrics <- metrics_df %>% 
    dplyr::filter(!is.na(RMSE), !is.na(MAE), is.finite(RMSE), is.finite(MAE), is.finite(R2))
  
  if (nrow(valid_metrics) == 0) {
    metrics_df$composite_score <- 0
    return(metrics_df)
  }
  
  # پیدا کردن بهترین مقادیر (کمترین خطا و بیشترین R²)
  min_rmse <- min(valid_metrics$RMSE, na.rm = TRUE)
  min_mae  <- min(valid_metrics$MAE, na.rm = TRUE)
  
  # محاسبه نمره هر مدل (نسبت به بهترین مدل)
  metrics_df <- metrics_df %>%
    dplyr::mutate(
      # نمره RMSE: اگر خطا 2 برابر بهترین باشد، نمره 50 می‌گیرد
      score_rmse = ifelse(is.na(RMSE) | !is.finite(RMSE) | RMSE == 0, 0, 100 * (min_rmse / RMSE)),
      
      # نمره MAE
      score_mae  = ifelse(is.na(MAE) | !is.finite(MAE) | MAE == 0, 0, 100 * (min_mae / MAE)),
      
      # نمره R²: بین 0 تا 100 (اگر منفی شد، صفر می‌گیرد)
      score_r2   = 100 * pmax(0, pmin(1, R2)),
      
      # نمره SMAPE: تبدیل درصد خطا به نمره (خطای صفر = نمره 100)
      score_smape = ifelse(is.na(SMAPE) | !is.finite(SMAPE), 0, 100 * (1 - (SMAPE / 100))),
      score_smape = pmax(0, pmin(100, score_smape))
    ) %>%
    dplyr::mutate(
      # 🔴 وزن‌دهی استاندارد: دقت خطی (RMSE, MAE) 70% و دقت زاویه‌ای/درصدی (R², SMAPE) 30%
      composite_score = round((0.35 * score_rmse) + (0.25 * score_mae) + (0.20 * score_r2) + (0.20 * score_smape), 1)
    ) %>%
    dplyr::arrange(dplyr::desc(composite_score))
  
  return(metrics_df)
}

# ── ارزیابی مدل روی مجموعه آزمون ────────────────────────────────────────────
evaluate_model <- function(model_fn, train_data, test_data,
                            target = "temperature", horizon = NULL) {
  if (is.null(horizon)) horizon <- nrow(test_data)

  tryCatch({
    preds <- model_fn(train_data, horizon, target)
    actual <- test_data[[target]][seq_len(min(horizon, nrow(test_data)))]
    preds  <- preds[seq_len(length(actual))]
    compute_all_metrics(actual, preds)
  }, error = function(e) {
    tibble::tibble(
      model = "unknown",
      RMSE  = NA_real_,
      MAE   = NA_real_,
      MAPE  = NA_real_,
      R2    = NA_real_,
      SMAPE = NA_real_
    )
  })
}

# ── تقسیم train/test ─────────────────────────────────────────────────────────
train_test_split <- function(df, test_ratio = 0.15) {
  n          <- nrow(df)
  test_n     <- max(7, floor(n * test_ratio))
  train_end  <- n - test_n
  list(
    train = df[seq_len(train_end), ],
    test  = df[(train_end + 1):n, ]
  )
}

# ── قالب‌بندی جدول معیارها برای نمایش ────────────────────────────────────────
format_metrics_table <- function(metrics_df) {
  metrics_df %>%
    dplyr::mutate(
      RMSE           = round(RMSE,  3),
      MAE            = round(MAE,   3),
      MAPE           = round(MAPE,  2),
      R2             = round(R2,    3),
      SMAPE          = round(SMAPE, 2),
      composite_score = round(composite_score, 4)
    ) %>%
    dplyr::rename(
      "مدل"           = model,
      "RMSE"          = RMSE,
      "MAE"           = MAE,
      "MAPE (%)"      = MAPE,
      "R²"            = R2,
      "SMAPE (%)"     = SMAPE,
      "نمره ترکیبی"   = composite_score
    )
}
