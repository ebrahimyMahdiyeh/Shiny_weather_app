# File: modules/anomaly_module.R
# ماژول تشخیص ناهنجاری — روش‌های مختلف آنالیز آنومالی

# ── توابع تشخیص ناهنجاری ────────────────────────────────────────────────────

# روش ۱: Z-Score فصلی (بر اساس میانگین ماهانه)
detect_seasonal_zscore <- function(df, target = "temperature",
                                    threshold = 2.5) {
  df %>%
    dplyr::mutate(month = lubridate::month(date)) %>%
    dplyr::group_by(month) %>%
    dplyr::mutate(
      monthly_mean = mean(.data[[target]], na.rm = TRUE),
      monthly_sd   = sd(.data[[target]], na.rm = TRUE),
      z_score      = (.data[[target]] - monthly_mean) / (monthly_sd + 1e-6)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      is_anomaly   = abs(z_score) > threshold,
      anomaly_score = abs(z_score),
      method       = "Seasonal Z-Score"
    )
}

# روش ۲: باقیمانده STL
detect_stl_residual <- function(df, target = "temperature",
                                 threshold = 2.5) {
  vals    <- df[[target]]
  ts_data <- ts(vals, frequency = min(7, nrow(df) - 1))

  stl_result <- tryCatch(
    stats::stl(ts_data, s.window = "periodic", na.action = na.approx),
    error = function(e) NULL
  )

  if (is.null(stl_result)) {
    df$residual     <- vals - mean(vals, na.rm = TRUE)
  } else {
    df$residual <- as.numeric(stl_result$time.series[, "remainder"])
  }

  resid_sd <- sd(df$residual, na.rm = TRUE)
  df %>%
    dplyr::mutate(
      z_score      = residual / (resid_sd + 1e-6),
      is_anomaly   = abs(z_score) > threshold,
      anomaly_score = abs(z_score),
      method       = "STL Residual"
    )
}

# روش ۳: IQR متحرک
detect_rolling_iqr <- function(df, target = "temperature",
                                window = 30, threshold = 1.5) {
  vals <- df[[target]]
  n    <- length(vals)

  scores <- numeric(n)
  for (i in seq_len(n)) {
    start_idx <- max(1, i - window + 1)
    window_vals <- vals[start_idx:i]
    q25 <- quantile(window_vals, 0.25, na.rm = TRUE)
    q75 <- quantile(window_vals, 0.75, na.rm = TRUE)
    iqr <- q75 - q25
    if (iqr == 0) {
      scores[i] <- 0
    } else {
      lower <- q25 - threshold * iqr
      upper <- q75 + threshold * iqr
      scores[i] <- max(
        0,
        (vals[i] - upper) / (iqr + 1e-6),
        (lower - vals[i]) / (iqr + 1e-6)
      )
    }
  }

  df %>%
    dplyr::mutate(
      anomaly_score = scores,
      is_anomaly    = anomaly_score > 1,
      z_score       = anomaly_score,
      method        = "Rolling IQR"
    )
}

# روش ۴: Isolation Forest (با پیاده‌سازی ساده R)
detect_isolation_forest <- function(df, target = "temperature",
                                     n_trees = 100, threshold = 0.6) {
  # استفاده از چند ویژگی برای بهبود کیفیت تشخیص
  feat_df <- df %>%
    add_calendar_features() %>%
    dplyr::select(
      dplyr::all_of(target),
      day_of_year, sin_annual, cos_annual
    ) %>%
    tidyr::drop_na()

  # Isolation Forest ساده بر اساس anomaly score
  # (پیاده‌سازی سبک بدون نیاز به پکیج خارجی)
  X <- scale(as.matrix(feat_df))
  n <- nrow(X)
  p <- ncol(X)

  anomaly_scores <- numeric(n)

  set.seed(42)
  for (t in seq_len(n_trees)) {
    # نمونه‌برداری تصادفی
    sample_idx <- sample(seq_len(n), min(256, n))
    X_sub      <- X[sample_idx, , drop = FALSE]

    # محاسبه عمق تقریبی برای هر نقطه
    for (i in seq_len(n)) {
      depth <- 0
      node_pts <- seq_len(nrow(X_sub))
      for (d in seq_len(ceiling(log2(nrow(X_sub))) + 1)) {
        if (length(node_pts) <= 1) break
        feat_idx <- sample(seq_len(p), 1)
        feat_vals <- X_sub[node_pts, feat_idx]
        split_val <- runif(1,
          min = min(feat_vals, na.rm = TRUE),
          max = max(feat_vals, na.rm = TRUE)
        )
        go_left <- X[i, feat_idx] <= split_val
        left_pts  <- node_pts[feat_vals <= split_val]
        right_pts <- node_pts[feat_vals >  split_val]
        node_pts  <- if (go_left) left_pts else right_pts
        depth <- depth + 1
      }
      anomaly_scores[i] <- anomaly_scores[i] + depth / n_trees
    }
  }

  # نرمال‌سازی: نقاط با عمق کمتر = ناهنجارتر
  norm_scores <- 1 - (anomaly_scores - min(anomaly_scores)) /
    (max(anomaly_scores) - min(anomaly_scores) + 1e-6)

  # هم‌راستا کردن با df اصلی (برخی ردیف‌ها ممکن است حذف شده باشند)
  result_df <- df
  result_df$anomaly_score <- NA_real_
  result_df$anomaly_score[!is.na(df[[target]])] <- norm_scores

  result_df %>%
    dplyr::mutate(
      is_anomaly = !is.na(anomaly_score) & anomaly_score > threshold,
      z_score    = anomaly_score,
      method     = "Isolation Forest"
    )
}

# ── UI ───────────────────────────────────────────────────────────────────────
anomalyUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # پانل کنترل
      column(3,
        box(
          title       = "تنظیمات تشخیص",
          width       = 12,
          status      = "warning",
          solidHeader = TRUE,
          selectInput(ns("station"),
            label   = "ایستگاه:",
            choices = NULL
          ),
          selectInput(ns("target_var"),
            label   = "متغیر:",
            choices = c(
              "دما"       = "temperature",
              "رطوبت"    = "humidity",
              "سرعت باد" = "wind_speed",
              "بارش"     = "precipitation"
            )
          ),
          selectInput(ns("method"),
            label   = "روش تشخیص:",
            choices = c(
              "Z-Score فصلی"     = "seasonal_zscore",
              "باقیمانده STL"    = "stl_residual",
              "IQR متحرک"        = "rolling_iqr",
              "Isolation Forest" = "isolation_forest"
            )
          ),
          # آستانه تشخیص
          conditionalPanel(
            condition = paste0(
              "input['", ns("method"), "'] != 'isolation_forest'"
            ),
            sliderInput(ns("threshold"),
              label = "آستانه Z-Score:",
              min   = 1.5,
              max   = 4.0,
              value = 2.5,
              step  = 0.1
            )
          ),
          conditionalPanel(
            condition = paste0(
              "input['", ns("method"), "'] == 'isolation_forest'"
            ),
            sliderInput(ns("if_threshold"),
              label = "آستانه Isolation Forest:",
              min   = 0.4,
              max   = 0.9,
              value = 0.6,
              step  = 0.05
            )
          ),
          hr(),
          actionButton(
            ns("run_detection"),
            label = "تشخیص ناهنجاری",
            class = "btn-warning btn-block",
            icon  = icon("search")
          )
        ),
        # آمار خلاصه
        box(
          title       = "خلاصه ناهنجاری‌ها",
          width       = 12,
          status      = "danger",
          solidHeader = TRUE,
          uiOutput(ns("anomaly_summary"))
        )
      ),

      # محتوای اصلی
      column(9,
        box(
          title       = "نمودار تشخیص ناهنجاری",
          width       = 12,
          status      = "warning",
          solidHeader = TRUE,
          shinycssloaders::withSpinner(
            plotlyOutput(ns("anomaly_plot"), height = "380px"),
            type  = 5,
            color = COLORS$accent
          )
        ),
        box(
          title       = "جدول ناهنجاری‌های شناسایی‌شده",
          width       = 12,
          status      = "danger",
          solidHeader = TRUE,
          collapsible = TRUE,
          DTOutput(ns("anomaly_table"))
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────────
anomalyServer <- function(id, weather_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # تنظیم ایستگاه‌ها
    observe({
      req(weather_data())
      choices <- purrr::imap_chr(weather_data(), function(df, sid) sid) %>%
        purrr::set_names(purrr::imap_chr(weather_data(), function(df, sid) {
          STATIONS[[sid]]$name %||% sid
        }))
      updateSelectInput(session, "station", choices = choices)
    })

    # نتیجه تشخیص
    anomaly_result <- reactiveVal(NULL)

    observeEvent(input$run_detection, {
      req(input$station, input$target_var, input$method, weather_data())
      sid    <- input$station
      target <- input$target_var
      method <- input$method
      req(sid %in% names(weather_data()))
      df <- weather_data()[[sid]] %>%
        dplyr::arrange(date) %>%
        dplyr::filter(!is.na(.data[[target]]))

      if (nrow(df) < 14) {
        showNotification("داده کافی برای تشخیص ناهنجاری وجود ندارد.", type = "error")
        return()
      }

      withProgress(message = "در حال تشخیص ناهنجاری...", value = 0.3, {
        tryCatch({
          result <- switch(method,
            seasonal_zscore  = detect_seasonal_zscore(df, target, input$threshold),
            stl_residual     = detect_stl_residual(df, target, input$threshold),
            rolling_iqr      = detect_rolling_iqr(df, target, threshold = input$threshold / 2),
            isolation_forest = detect_isolation_forest(df, target,
                                                        threshold = input$if_threshold),
            stop("روش ناشناخته")
          )
          anomaly_result(result)
          setProgress(1)
          n_anom <- sum(result$is_anomaly, na.rm = TRUE)
          showNotification(
            paste0(n_anom, " ناهنجاری شناسایی شد."),
            type = "message"
          )
        }, error = function(e) {
          msg <- paste("خطا:", conditionMessage(e))
          message("\n========== خطای ماژول ناهنجاری ==========")
          message(msg)
          message("ایستگاه: ", sid, " | متغیر: ", target, " | روش: ", method)
          message("==========================================\n")
          showNotification(msg, type = "error", duration = NULL)
        })
      })
    })

    # ── نمودار ناهنجاری ──────────────────────────────────────────────────────
    output$anomaly_plot <- renderPlotly({
      req(anomaly_result())
      res    <- anomaly_result()
      target <- input$target_var

      normal_df  <- res %>% dplyr::filter(!is_anomaly)
      anomaly_df <- res %>% dplyr::filter(is_anomaly)

      p <- plotly::plot_ly()

      # سری اصلی
      p <- plotly::add_lines(p,
        data  = res,
        x     = ~date,
        y     = as.formula(paste0("~`", target, "`")),
        name  = "داده عادی",
        line  = list(color = COLORS$primary, width = 1.5)
      )

      # نقاط ناهنجار
      if (nrow(anomaly_df) > 0) {
        # استخراج مقادیر ستون هدف به صورت مستقیم
        target_vals <- anomaly_df[[target]]
        p <- plotly::add_markers(p,
          data   = anomaly_df,
          x      = ~date,
          y      = as.formula(paste0("~`", target, "`")),
          name   = "ناهنجاری",
          marker = list(
            color  = COLORS$secondary,
            size   = 10,
            symbol = "x",
            line   = list(color = "white", width = 1)
          ),
          text      = paste0("تاریخ: ", anomaly_df$date,
                             "<br>مقدار: ", round(target_vals, 2),
                             "<br>نمره: ",  round(anomaly_df$anomaly_score, 3)),
          hoverinfo = "text"
        )
      }

      p %>% plotly::layout(
        title     = paste("تشخیص ناهنجاری —", res$method[1]),
        xaxis     = list(title = "تاریخ"),
        yaxis     = list(title = target),
        hovermode = "closest",
        legend    = list(orientation = "h", y = -0.2)
      )
    })

    # ── جدول ناهنجاری‌ها ─────────────────────────────────────────────────────
    output$anomaly_table <- DT::renderDT({
      req(anomaly_result())
      res <- anomaly_result()
      target <- input$target_var

      tbl <- res %>%
        dplyr::filter(is_anomaly) %>%
        dplyr::select(
          "تاریخ"      = date,
          "مقدار"      = dplyr::all_of(target),
          "نمره ناهنجاری" = anomaly_score,
          "روش"        = method
        ) %>%
        dplyr::mutate(
          "مقدار"      = round(.data[["مقدار"]], 2),
          "نمره ناهنجاری" = round(.data[["نمره ناهنجاری"]], 4)
        ) %>%
        dplyr::arrange(dplyr::desc(.data[["نمره ناهنجاری"]]))

      DT::datatable(
        tbl,
        options  = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE,
        class    = "cell-border stripe"
      ) %>%
        DT::formatStyle(
          "نمره ناهنجاری",
          backgroundColor = DT::styleInterval(
            c(0.5, 1.5, 2.5),
            c("white", "#fff3cd", "#f8d7da", "#dc3545")
          ),
          color = DT::styleInterval(2.5, c("black", "white"))
        )
    })

    # ── خلاصه آماری ──────────────────────────────────────────────────────────
    output$anomaly_summary <- renderUI({
      req(anomaly_result())
      res    <- anomaly_result()
      n_tot  <- nrow(res)
      n_anom <- sum(res$is_anomaly, na.rm = TRUE)
      pct    <- round(100 * n_anom / n_tot, 1)

      tagList(
        tags$p(tags$b("روش: "), res$method[1]),
        tags$p(tags$b("کل روزها: "), n_tot),
        tags$p(
          tags$b("ناهنجاری‌ها: "),
          tags$span(n_anom, style = "color:red; font-weight:bold;"),
          paste0(" (", pct, "%)")
        ),
        if (n_anom > 0) {
          top5 <- res %>%
            dplyr::filter(is_anomaly) %>%
            dplyr::arrange(dplyr::desc(anomaly_score)) %>%
            dplyr::slice_head(n = 3)
          tags$p(
            tags$b("شدیدترین ناهنجاری‌ها:"),
            tags$ul(
              purrr::map(seq_len(nrow(top5)), function(i) {
                tags$li(paste0(top5$date[i], " — نمره: ",
                               round(top5$anomaly_score[i], 3)))
              })
            )
          )
        }
      )
    })

    return(anomaly_result)
  })
}
