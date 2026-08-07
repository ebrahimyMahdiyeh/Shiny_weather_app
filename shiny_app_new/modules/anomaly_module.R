# File: modules/anomaly_module.R
# ماژول کنترل کیفیت و تشخیص ناهنجاری هواشناسی (Advanced Meteorological QC)

# ── توابع کنترل کیفیت (QC) ───────────────────────────────────────────────────

# روش ۱: محدودیت‌های فیزیکی
detect_physical_limits <- function(df, target = "temperature") {
  limits <- list(
    temperature   = c(-40, 55),
    humidity      = c(0, 100),
    wind_speed    = c(0, 150),
    precipitation = c(0, 300)
  )
  lim <- limits[[target]]
  
  df %>%
    dplyr::mutate(
      is_anomaly = !is.na(.data[[target]]) & (.data[[target]] < lim[1] | .data[[target]] > lim[2]),
      anomaly_score = ifelse(is_anomaly, 100, 0),
      method = "بررسی محدودیت‌های فیزیکی"
    )
}

# روش ۲: کنترل کیفیت زمانی (جهش و تکرار)
detect_temporal_qc <- function(df, target = "temperature", window = 24) {
  vals <- df[[target]]
  
  step_threshold <- switch(target, "temperature" = 15, "humidity" = 40, "wind_speed" = 50, "precipitation" = 50, 20)
  diffs <- abs(c(0, diff(vals)))
  step_anomaly <- diffs > step_threshold
  
  roll_var <- zoo::rollapply(vals, width = window, FUN = sd, fill = 0, align = "right")
  stuck_anomaly <- roll_var == 0 & !is.na(vals)
  
  df %>%
    dplyr::mutate(
      is_anomaly = step_anomaly | stuck_anomaly,
      anomaly_score = pmax(diffs / step_threshold, ifelse(stuck_anomaly, 100, 0)),
      method = "کنترل کیفیت زمانی (جهش و تکرار)"
    )
}

# روش ۳: کنترل کیفیت آماری و فصلی
detect_climatological_qc <- function(df, target = "temperature", threshold = 3.0) {
  df %>%
    dplyr::mutate(doy = lubridate::yday(date)) %>%
    dplyr::group_by(doy) %>%
    dplyr::mutate(
      clim_mean = mean(.data[[target]], na.rm = TRUE),
      clim_sd   = sd(.data[[target]], na.rm = TRUE),
      z_score   = abs((.data[[target]] - clim_mean) / (clim_sd + 1e-6))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      is_anomaly = z_score > threshold,
      anomaly_score = z_score,
      method = "کنترل کیفیت آماری و فصلی"
    )
}

# روش ۴: تشخیص تغییر فاز سنسور
detect_change_points <- function(df, target = "temperature", window = 30) {
  vals <- df[[target]]
  roll_mean <- zoo::rollmean(vals, k = window, fill = NA, align = "center")
  
  mean_diff <- abs(c(0, diff(roll_mean)))
  mean_diff_sd <- sd(mean_diff, na.rm = TRUE)
  
  cp_threshold <- mean(mean_diff, na.rm = TRUE) + 3 * mean_diff_sd
  
  df %>%
    dplyr::mutate(
      is_anomaly = mean_diff > cp_threshold & !is.na(mean_diff),
      anomaly_score = ifelse(is_anomaly, mean_diff / (cp_threshold + 1e-6), 0),
      method = "تشخیص تغییر فاز سنسور"
    )
}

# ── UI ───────────────────────────────────────────────────────────────────────
anomalyUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             shinydashboard::box(
               title = tags$span(tags$i(class="fa fa-sliders", style="margin-left:6px;color:var(--blue2);"), "تنظیمات کنترل کیفیت"),
               width = 12, solidHeader = FALSE, status = "primary",
               
               selectInput(ns("station"), label = "ایستگاه:", choices = NULL),
               selectInput(ns("target_var"), label = "متغیر:",
                           choices = c("دما"="temperature", "رطوبت"="humidity", "سرعت باد"="wind_speed", "بارش"="precipitation")),
               selectInput(ns("method"), label = "روش کنترل کیفیت:",
                           choices = c(
                             "بررسی محدودیت‌های فیزیکی" = "physical",
                             "کنترل کیفیت زمانی (جهش و تکرار)" = "temporal",
                             "کنترل کیفیت آماری و فصلی" = "climatological",
                             "تشخیص تغییر فاز سنسور" = "change_point"
                           )),
               conditionalPanel(
                 condition = paste0("input['", ns("method"), "'] == 'climatological'"),
                 sliderInput(ns("threshold"), label = "آستانه زد-اسکور آماری:", min = 2.0, max = 4.0, value = 3.0, step = 0.1)
               ),
               hr(style="border-color:var(--border);"),
               actionButton(ns("run_detection"), label = tagList(tags$i(class="fa fa-magnifying-glass-chart"), " اجرای کنترل کیفیت"), class = "btn-warning btn-block", style="font-size:14px; font-weight:700;")
             ),
             shinydashboard::box(
               title = tags$span(tags$i(class="fa fa-clipboard-check", style="margin-left:6px;color:#22c55e;"), "گزارش کیفیت و خلاصه"),
               width = 12, solidHeader = FALSE, status = "success",
               uiOutput(ns("anomaly_summary"))
             )
      ),
      
      column(9,
             # ── کارت راهنمای روش (Method Explainer) ──
             uiOutput(ns("method_explainer")),
             
             shinydashboard::box(
               title = tags$span(tags$i(class="fa fa-chart-line", style="margin-left:6px;color:#fbbf24;"), "نمودار ناهنجاری‌های هواشناسی"),
               width = 12, solidHeader = FALSE, status = "warning",
               shinycssloaders::withSpinner(plotly::plotlyOutput(ns("anomaly_plot"), height = "380px"), type = 8, color = "#f59e0b", size = 0.6)
             ),
             shinydashboard::box(
               title = tags$span(tags$i(class="fa fa-table", style="margin-left:6px;color:#ef4444;"), "جدول ناهنجاری‌های شناسایی‌شده"),
               width = 12, solidHeader = FALSE, status = "danger",
               collapsible = TRUE,
               DT::DTOutput(ns("anomaly_table"))
             )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────────
anomalyServer <- function(id, weather_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      req(weather_data())
      choices <- purrr::set_names(names(weather_data()), purrr::map_chr(names(weather_data()), ~ STATIONS[[.x]]$name %||% .x))
      updateSelectInput(session, "station", choices = choices)
    })
    
    # ── کارت راهنمای روش (توضیحات داینامیک) ──
    output$method_explainer <- renderUI({
      method <- input$method
      
      info <- switch(method,
                     "physical" = list(
                       icon = "fa-thermometer-half", color = "#ef4444",
                       title = "بررسی محدودیت‌های فیزیکی",
                       desc = "این روش مقادیر غیرممکن فیزیکی را بررسی می‌کند. مثلاً دمای زیر ۴۰- یا بالای ۵۵ درجه، یا رطوبت بیش از ۱۰۰ درصد که نشان‌دهنده خطای سنسور یا ثبت داده است را به عنوان ناهنجاری علامت‌گذاری می‌کند."
                     ),
                     "temporal" = list(
                       icon = "fa-clock", color = "#fbbf24",
                       title = "کنترل کیفیت زمانی (جهش و تکرار)",
                       desc = "این روش دو خطای رایج را بررسی می‌کند: ۱. جهش ناگهانی (مثلاً ۱۵ درجه تغییر دما در یک ساعت) ۲. گیر کردن سنسور (ثبت یک مقدار ثابت برای ساعت‌های متوالی که نشان‌دهنده خرابی سنسور است)."
                     ),
                     "climatological" = list(
                       icon = "fa-chart-bar", color = "#3b82f6",
                       title = "کنترل کیفیت آماری و فصلی",
                       desc = "این روش داده‌ها را با میانگین تاریخی همان روز از سال (Day of Year) مقایسه می‌کند. اگر مقدار ثبت شده بیش از ۳ انحراف معیار با میانگین سال‌های دیگر فاصله داشته باشد، به عنوان ناهنجاری فصلی تشخیص داده می‌شود."
                     ),
                     "change_point" = list(
                       icon = "fa-arrows-split-up-and-left", color = "#a78bfa",
                       title = "تشخیص تغییر فاز سنسور",
                       desc = "این روش زمان‌هایی که رفتار میانگین داده‌ها به طور ناگهانی و دائمی تغییر می‌کند را پیدا می‌کند. این تغییر فاز معمولاً به دلیل جابجایی سنسور، تغییر کالیبراسیون یا ایجاد مانع فیزیکی در مقابل سنسور رخ می‌دهد."
                     )
      )
      
      tags$div(
        style = paste0("background:var(--panel2); border:1px solid ", info$color, "33; border-right:4px solid ", info$color, "; border-radius:10px; padding:16px 20px; margin-bottom:16px; display:flex; gap:15px; align-items:flex-start;"),
        tags$div(
          style = paste0("width:40px; height:40px; border-radius:10px; background:", info$color, "22; display:flex; align-items:center; justify-content:center; flex-shrink:0;"),
          tags$i(class = paste("fa", info$icon), style = paste0("color:", info$color, "; font-size:18px;"))
        ),
        tags$div(
          tags$div(style = paste0("font-size:15px; font-weight:800; color:var(--text); margin-bottom:6px;"), info$title),
          tags$div(style = "font-size:13px; color:var(--text2); line-height:1.7;", info$desc)
        )
      )
    })
    
    anomaly_result <- reactiveVal(NULL)
    
    observeEvent(input$run_detection, {
      req(input$station, input$target_var, input$method, weather_data())
      sid    <- input$station
      target <- input$target_var
      method <- input$method
      
      df <- weather_data()[[sid]] %>% dplyr::arrange(date) %>% dplyr::filter(!is.na(.data[[target]]))
      if (nrow(df) < 14) {
        showNotification("داده کافی برای کنترل کیفیت وجود ندارد.", type = "error")
        return()
      }
      
      withProgress(message = "در حال اجرای کنترل کیفیت...", value = 0.3, {
        tryCatch({
          result <- switch(method,
                           physical       = detect_physical_limits(df, target),
                           temporal       = detect_temporal_qc(df, target),
                           climatological = detect_climatological_qc(df, target, input$threshold),
                           change_point   = detect_change_points(df, target),
                           stop("روش ناشناخته")
          )
          
          anomaly_result(result)
          setProgress(1)
          
          n_anom <- sum(result$is_anomaly, na.rm = TRUE)
          showNotification(paste0(n_anom, " ناهنجاری شناسایی شد."), type = "message")
        }, error = function(e) {
          showNotification(paste("خطا:", conditionMessage(e)), type = "error", duration = 15)
        })
      })
    })
    
    # ── نمودار ناهنجاری ──────────────────────────────────────────────────────
    output$anomaly_plot <- plotly::renderPlotly({
      req(anomaly_result())
      res    <- anomaly_result()
      target <- input$target_var
      
      normal_df  <- res %>% dplyr::filter(!is_anomaly)
      anomaly_df <- res %>% dplyr::filter(is_anomaly)
      
      p <- plotly::plot_ly()
      
      # سری اصلی (داده‌های عادی)
      p <- plotly::add_trace(p, data = normal_df, x = ~date, y = as.formula(paste0("~`", target, "`")),
                             type = 'scatter', mode = 'lines', name = 'داده عادی',
                             line = list(color = 'rgba(59, 130, 246, 0.6)', width = 1.5),
                             hoverinfo = 'text',
                             text = ~paste("تاریخ:", date, "<br>مقدار:", round(.data[[target]], 2)))
      
      # نقاط ناهنجار
      if (nrow(anomaly_df) > 0) {
        p <- plotly::add_trace(p, data = anomaly_df, x = ~date, y = as.formula(paste0("~`", target, "`")),
                               type = 'scatter', mode = 'markers', name = 'داده پرت (Anomaly)',
                               marker = list(color = '#ef4444', size = 10, symbol = 'x', line = list(color = 'white', width = 1)),
                               hoverinfo = 'text',
                               text = ~paste("تاریخ:", date, "<br>مقدار:", round(.data[[target]], 2), "<br>نمره:", round(anomaly_score, 3)))
      }
      
      p %>% plotly::layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font = list(family = "Vazirmatn, Tahoma", color = "#94a3b8", size = 12),
        title = list(text = paste("کنترل کیفیت —", res$method[1]), font = list(color = '#fbbf24', size = 14)),
        xaxis = list(title = "", gridcolor = "rgba(99,143,232,0.08)", tickfont = list(color = "#94a3b8")),
        yaxis = list(title = target, gridcolor = "rgba(99,143,232,0.08)", tickfont = list(color = "#94a3b8")),
        legend = list(orientation = "h", y = -0.2, font = list(color = "#94a3b8")),
        margin = list(l = 50, r = 20, t = 40, b = 40)
      ) %>% plotly::config(displayModeBar = FALSE)
    })
    
    # ── جدول ناهنجاری‌ها ─────────────────────────────────────────────────────
    output$anomaly_table <- DT::renderDT({
      req(anomaly_result())
      res <- anomaly_result()
      target <- input$target_var
      
      tbl <- res %>%
        dplyr::filter(is_anomaly) %>%
        dplyr::select("تاریخ" = date, "مقدار" = dplyr::all_of(target), "نمره ناهنجاری" = anomaly_score, "روش" = method) %>%
        dplyr::mutate("مقدار" = round(.data[["مقدار"]], 2), "نمره ناهنجاری" = round(.data[["نمره ناهنجاری"]], 4)) %>%
        dplyr::arrange(dplyr::desc(.data[["نمره ناهنجاری"]]))
      
      DT::datatable(tbl, options = list(pageLength = 10, dom = "t", scrollX = TRUE), rownames = FALSE, class = "cell-border stripe") %>%
        DT::formatStyle("نمره ناهنجاری",
                        background = DT::styleColorBar(c(0, max(tbl$`نمره ناهنجاری`, 1)), "#ef4444"),
                        backgroundSize = "100% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
    })
    
    # ── خلاصه آماری و گزارش کیفیت ────────────────────────────────────────────
    output$anomaly_summary <- renderUI({
      req(anomaly_result())
      res <- anomaly_result()
      
      n_tot  <- nrow(res)
      n_anom <- sum(res$is_anomaly, na.rm = TRUE)
      pct    <- round(100 * n_anom / n_tot, 1)
      
      tagList(
        tags$div(
          style = "background:rgba(239,68,68,0.05); border:1px solid rgba(239,68,68,0.2); border-radius:8px; padding:12px; margin-bottom:15px;",
          tags$div(style="font-size:14px; font-weight:800; color:#ef4444; margin-bottom:8px;", tags$i(class="fa fa-shield-halved"), " گزارش سلامت داده‌ها:"),
          tags$div(style="font-size:13px; color:var(--text2); display:flex; justify-content:space-between; margin-bottom:5px;",
                   tags$span("کل رکوردهای بررسی شده:"), tags$b(n_tot, " رکورد")),
          tags$div(style="font-size:13px; display:flex; justify-content:space-between;",
                   tags$span("داده‌های آلوده:"), 
                   tags$b(style=ifelse(n_anom > 0, "color:#ef4444;", "color:#22c55e;"), n_anom, " رکورد"))
        ),
        tags$p(style="font-size:14px; color:var(--text);", tags$b("روش اجرا شده: "), res$method[1]),
        tags$p(style="font-size:14px; color:var(--text);",
               tags$b("درصد ناهنجاری: "),
               tags$span(paste0(pct, " %"), style = "color:#fbbf24; font-weight:800;"))
      )
    })
    
    return(anomaly_result)
  })
}