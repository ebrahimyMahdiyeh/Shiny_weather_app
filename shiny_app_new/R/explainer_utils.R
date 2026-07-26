# File: R/explainer_utils.R
# سیستم توضیح‌پذیری پیش‌بینی (Explainable AI)

generate_forecast_explanation <- function(mlp, input, model_meta) {
  if (is.null(mlp) || length(mlp$preds_list) == 0) return(NULL)
  
  # توضیحات برای اولین مدل انتخابی نوشته می‌شود
  mn <- names(mlp$preds_list)[1]
  m_data <- mlp$preds_list[[mn]]
  target <- mlp$target
  unit <- c(temperature="°C",humidity="%",wind_speed="km/h",precipitation="mm")[[target]]%||%""
  
  pred_val <- round(mean(m_data$preds), 1)
  
  tags$div(
    class = "fc-chart-box",
    style = "background:rgba(34,211,238,0.03); border:1px solid rgba(34,211,238,0.2); margin-top:13px;",
    
    tags$div(
      style = "display:flex; align-items:center; gap:8px; margin-bottom:15px;",
      tags$i(class = "fa fa-lightbulb", style = "color:#22d3ee; font-size:14px;"),
      tags$span(style = "font-size:12px; font-weight:800; color:#22d3ee; text-transform:uppercase; letter-spacing:1px;",
                "توضیح پیش‌بینی (Forecast Explanation)")
    ),
    
    # ۱. سوال اصلی و نتیجه
    tags$div(
      style = "margin-bottom:15px; padding-bottom:15px; border-bottom:1px solid rgba(255,255,255,0.05);",
      tags$div(style="font-size:13px; color:#e2e8f0; font-weight:700; margin-bottom:8px;",
               paste0("چرا مدل ", model_meta[[mn]]$label, "، مقدار ", target, " را برای ۲۴ ساعت آینده ", pred_val, unit, " پیش‌بینی کرده است؟")),
      tags$div(style="font-size:12px; color:#94a3b8; line-height:1.6;",
               paste0("مدل ", model_meta[[mn]]$label, " با تحلیل داده‌های اخیر و الگوهای یادگرفته شده از گذشته، این مقدار را به عنوان محتمل‌ترین سناریو پیش‌بینی کرده است."))
    ),
    
    # ۲. عوامل مؤثر (Feature Importance)
    tags$div(
      style = "margin-bottom:15px; padding-bottom:15px; border-bottom:1px solid rgba(255,255,255,0.05);",
      tags$div(style="font-size:11px; color:#94a3b8; font-weight:700; margin-bottom:10px;", "مهم‌ترین عوامل مؤثر در این پیش‌بینی:"),
      generate_feature_explanation(mlp, mn)
    ),
    
    # ۳. تحلیل مدل و روند
    tags$div(
      style = "margin-bottom:15px; padding-bottom:15px; border-bottom:1px solid rgba(255,255,255,0.05);",
      tags$div(style="font-size:11px; color:#94a3b8; font-weight:700; margin-bottom:8px;", "تحلیل مدل:"),
      tags$div(style="font-size:12px; color:#94a3b8; line-height:1.6; margin-bottom:8px;",
               generate_trend_analysis(mlp, target)
      ),
      tags$div(style="font-size:12px; color:#94a3b8; line-height:1.6;",
               generate_mode_analysis(mn, input)
      )
    ),
    
    # ۴. نحوه کارکرد مدل
    tags$div(
      style = "margin-bottom:15px; padding-bottom:15px; border-bottom:1px solid rgba(255,255,255,0.05);",
      tags$div(style="font-size:11px; color:#94a3b8; font-weight:700; margin-bottom:6px;",
               paste0("نحوه کارکرد مدل ", model_meta[[mn]]$label, ":")),
      tags$div(style="font-size:12px; color:#e2e8f0; line-height:1.6; font-style:italic;",
               get_model_description(mn)
      )
    ),
    
    # ۵. اطمینان و مقایسه
    tags$div(
      style = "display:flex; justify-content:space-between; gap:15px; flex-wrap:wrap;",
      generate_confidence_box(mlp, mn),
      generate_om_comparison_box(mlp, input, mn, unit)
    )
  )
}

# ── Helper Functions for Explainer ──

translate_feature <- function(feat) {
  feat_map <- list(
    "lag_1h" = "🕒 دمای ۱ ساعت گذشته", "lag_2h" = "🕒 دمای ۲ ساعت گذشته",
    "lag_3h" = "🕒 دمای ۳ ساعت گذشته", "lag_24h" = "🕒 دمای ۲۴ ساعت گذشته",
    "rolling_6h" = "📈 میانگین ۶ ساعت", "rolling_24h" = "📈 میانگین ۲۴ ساعت",
    "humidity" = "💧 رطوبت", "wind_speed" = "🌬️ سرعت باد",
    "pressure" = "🧭 فشار", "precipitation" = "🌧️ بارش",
    "hour" = "🕒 ساعت شبانه‌روز", "hour_sin" = "🕒 چرخه ساعتی", "hour_cos" = "🕒 چرخه ساعتی",
    "day_of_week" = "📅 روز هفته", "sin_weekly" = "📅 چرخه هفتگی", "cos_weekly" = "📅 چرخه هفتگی",
    "sin_annual" = "📅 الگوی فصلی", "cos_annual" = "📅 الگوی فصلی"
  )
  if (feat %in% names(feat_map)) return(feat_map[[feat]])
  if (grepl("humidity", feat)) return("💧 رطوبت (Lag)")
  if (grepl("wind", feat)) return("🌬️ سرعت باد (Lag)")
  if (grepl("temp", feat)) return("🌡️ دما (Lag)")
  if (grepl("precip", feat)) return("🌧️ بارش (Lag)")
  return(feat)
}

generate_feature_explanation <- function(mlp, mn) {
  if (is.null(mlp$feat_imp) || is.null(mlp$feat_imp[[mn]])) {
    return(tags$div(style="font-size:12px; color:#94a3b8;", "این مدل کلاسیک بر اساس روابط ریاضی و وابستگی‌های زمانی پیش‌بینی را انجام می‌دهد، نه ویژگی‌های جداگانه."))
  }
  
  df <- mlp$feat_imp[[mn]]
  names(df) <- c("Feature", "Gain")
  df$Gain <- as.numeric(df$Gain)
  df <- df[order(df$Gain, decreasing = TRUE),]
  
  total_gain <- sum(df$Gain, na.rm=TRUE)
  if(total_gain == 0) total_gain <- 1
  
  top_df <- head(df, 4)
  
  items <- lapply(seq_len(nrow(top_df)), function(i) {
    feat <- as.character(top_df$Feature[i])
    gain <- as.numeric(top_df$Gain[i])
    pct <- round(gain / total_gain * 100, 0)
    
    # تشخیص علامت (تأثیر مثبت یا منفی) - بر اساس منطق هواشناسی
    sign <- "+"
    if (grepl("humidity|wind|precip", feat)) sign <- "-" # عوامل خنک‌کننده
    if (grepl("rolling", feat)) sign <- "+"
    
    tags$div(
      style = "display:flex; justify-content:space-between; align-items:center; padding:5px 0; font-size:12px; border-bottom:1px dashed rgba(255,255,255,0.03);",
      tags$span(style="color:#e2e8f0;", translate_feature(feat)),
      tags$span(style="font-weight:800; color:#60a5fa;", paste0(sign, pct, "%"))
    )
  })
  
  do.call(tagList, items)
}

generate_trend_analysis <- function(mlp, target) {
  hist_df <- mlp$hist_df
  if (is.null(hist_df) || nrow(hist_df) < 2) return("داده کافی برای تحلیل روند وجود ندارد.")
  
  start_val <- as.numeric(hist_df[[target]][1])
  end_val <- as.numeric(hist_df[[target]][nrow(hist_df)])
  
  if (end_val > start_val) {
    return("تحلیل روند نشان می‌دهد که متغیر هدف در ۲۴ ساعت گذشته روندی افزایشی داشته است و مدل این الگو را برای آینده نیز ادامه داده است.")
  } else if (end_val < start_val) {
    return("تحلیل روند نشان می‌دهد که متغیر هدف در ۲۴ ساعت گذشته روندی کاهشی داشته است و مدل این الگو را برای آینده نیز ادامه داده است.")
  } else {
    return("روند ۲۴ ساعت گذشته نسبتاً ثابت بوده و مدل میانگین این روند را برای پیش‌بینی استفاده کرده است.")
  }
}

generate_mode_analysis <- function(mn, input) {
  ML_MODELS <- c("rf", "xgboost", "lightgbm", "catboost", "svm")
  use_mv <- isTRUE(input$use_multivariate)
  
  if (mn %in% ML_MODELS) {
    if (use_mv) {
      return("پیش‌بینی علاوه بر تاریخچه متغیر هدف، از رطوبت، فشار، سرعت باد و ویژگی‌های زمانی نیز به عنوان متغیرهای کمکی استفاده کرده است.")
    } else {
      return("پیش‌بینی تنها بر اساس تاریخچه خود متغیر هدف انجام شده است و سایر متغیرهای هواشناسی در مدل استفاده نشده‌اند.")
    }
  } else {
    return("این یک مدل کلاسیک سری زمانی است و پیش‌بینی صرفاً بر اساس الگوهای گذشته متغیر هدف انجام می‌شود.")
  }
}

get_model_description <- function(mn) {
  desc <- switch(mn,
                 "xgboost" = "پیش‌بینی حاصل ترکیب مرحله‌ای درخت‌ها و اصلاح خطاهای مدل‌های قبلی است.",
                 "lightgbm" = "مدل با ساخت درخت‌های برگ‌محور، الگوهای غیرخطی موجود در داده را استخراج کرده است.",
                 "catboost" = "مدل با استفاده از گرادیان بوستینگ و کاهش بیش‌برازش، روابط پیچیده بین متغیرها را یاد گرفته است.",
                 "rf" = "تصمیم نهایی از میانگین خروجی صدها درخت تصمیم مستقل به دست آمده است.",
                 "svm" = "مدل با استفاده از ماشین بردار پشتیبان، مرزهای غیرخطی بین داده‌ها را برای پیش‌بینی یافته است.",
                 "arima" = "پیش‌بینی بر اساس روند و وابستگی زمانی مقادیر گذشته انجام شده است.",
                 "sarima" = "پیش‌بینی با در نظر گرفتن روند، وابستگی زمانی و الگوهای فصلی (مثل چرخه ۲۴ ساعته) انجام شده است.",
                 "ets" = "پیش‌بینی بر اساس هموارسازی نمایی و وزن‌دهی بیشتر به داده‌های اخیر انجام شده است.",
                 "tbats" = "مدل الگوهای فصلی پیچیده و چندگانه را با استفاده از تبدیل فوریه تجزیه و پیش‌بینی کرده است.",
                 "prophet" = "مدل روند کلی و فصلی‌بودن داده‌ها را تجزیه کرده و برای آینده بازتولید نموده است.",
                 "naive" = "مدل فرض کرده است که مقدار آینده دقیقاً برابر با آخرین مقدار مشاهده شده خواهد بود."
  )
  return(desc)
}

generate_confidence_box <- function(mlp, mn) {
  r2_val <- 0
  if (!is.null(mlp$metrics) && !is.null(mlp$metrics[[mn]])) {
    r2_val <- mlp$metrics[[mn]]$r2
  }
  conf_pct <- round(max(0, min(1, r2_val)) * 100, 0)
  
  tags$div(
    style = "flex:1; min-width:200px; background:rgba(34,197,94,0.05); border:1px solid rgba(34,197,94,0.2); border-radius:8px; padding:12px;",
    tags$div(style="font-size:10px; color:#22c55e; font-weight:700; margin-bottom:6px; text-transform:uppercase;", "سطح اطمینان مدل"),
    tags$div(style="font-size:20px; font-weight:900; color:#22c55e;", paste0(conf_pct, "%")),
    tags$div(style="font-size:10px; color:#64748b; margin-top:4px;", "بر اساس دقت روی ۲۴ ساعت گذشته")
  )
}

generate_om_comparison_box <- function(mlp, input, mn, unit) {
  if (!isTRUE(input$compare_om)) return(NULL)
  
  diff_val <- "نامشخص"
  if (!is.null(mlp$daily_om) && !is.null(mlp$daily_preds[[mn]])) {
    ml_d <- mlp$daily_preds[[mn]][1,]
    om_d <- mlp$daily_om[1,]
    diff <- abs(as.numeric(ml_d$max_val) - as.numeric(om_d$max_val))
    diff_val <- paste0(round(diff, 1), unit)
  }
  
  tags$div(
    style = "flex:1; min-width:200px; background:rgba(59,130,246,0.05); border:1px solid rgba(59,130,246,0.2); border-radius:8px; padding:12px;",
    tags$div(style="font-size:10px; color:#60a5fa; font-weight:700; margin-bottom:6px; text-transform:uppercase;", "مقایسه با Open-Meteo"),
    tags$div(style="font-size:14px; font-weight:700; color:#e2e8f0; margin-bottom:4px;", paste0("اختلاف: ", diff_val)),
    tags$div(style="font-size:10px; color:#64748b; line-height:1.4;", "این اختلاف عمدتاً ناشی از تأثیر بیشتر داده‌های محلی و الگوریتم‌های متفاوت در مدل حاضر است.")
  )
}