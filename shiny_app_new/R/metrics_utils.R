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
  # نرمال‌سازی min-max برای هر معیار
  normalize_col <- function(x, lower_is_better = TRUE) {
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) return(rep(0.5, length(x)))
    normed <- (x - rng[1]) / (rng[2] - rng[1])
    if (lower_is_better) normed else 1 - normed
  }

  metrics_df %>%
    dplyr::mutate(
      n_RMSE  = normalize_col(RMSE,  lower_is_better = TRUE),
      n_MAE   = normalize_col(MAE,   lower_is_better = TRUE),
      n_MAPE  = normalize_col(MAPE,  lower_is_better = TRUE),
      n_R2    = normalize_col(R2,    lower_is_better = FALSE),
      n_SMAPE = normalize_col(SMAPE, lower_is_better = TRUE),
      # میانگین وزن‌دار معیارها
      composite_score = round(
        0.25 * n_RMSE +
          0.20 * n_MAE  +
          0.20 * n_MAPE +
          0.20 * n_R2   +
          0.15 * n_SMAPE,
        4
      )
    ) %>%
    dplyr::select(-dplyr::starts_with("n_")) %>%
    dplyr::arrange(composite_score)
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
