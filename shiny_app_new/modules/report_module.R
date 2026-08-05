# File: modules/report_module.R
# ماژول گزارش — خروجی Excel و HTML

# ── UI ───────────────────────────────────────────────────────────────────────
reportUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             shinydashboard::box(
               title = tags$span(tags$i(class="fa fa-sliders", style="margin-left:6px;color:var(--blue2);"), "تنظیمات گزارش"),
               width = 12, solidHeader = FALSE, status = "primary",
               
               selectInput(ns("report_station"), label = "ایستگاه:", choices = NULL),
               textInput(ns("report_title"), label = "عنوان گزارش:", value = "گزارش تحلیل آب‌وهوا"),
               textInput(ns("analyst_name"), label = "نام تحلیلگر:", value = "سیستم خودکار"),
               dateRangeInput(ns("report_date_range"), label = "بازه زمانی:", start = Sys.Date() - 365, end = Sys.Date() - 1, language = "fa"),
               hr(),
               
               tags$h5(tags$i(class="fa fa-list-check", style="margin-left:6px;color:var(--blue2);"), "محتوای گزارش"),
               
               fluidRow(
                 column(6, tags$div(class="mv-box compare-box", checkboxInput(ns("include_forecast"), label = "پیش‌بینی", value = TRUE, width = "100%"))),
                 column(6, tags$div(class="mv-box compare-box", checkboxInput(ns("include_metrics"), label = "معیارهای ارزیابی", value = TRUE, width = "100%"))),
                 column(6, tags$div(class="mv-box compare-box", checkboxInput(ns("include_anomalies"), label = "ناهنجاری‌ها", value = TRUE, width = "100%"))),
                 column(6, tags$div(class="mv-box compare-box", checkboxInput(ns("include_leaderboard"), label = "رتبه‌بندی مدل‌ها", value = TRUE, width = "100%")))
               )
             )
      ),
      
      column(8,
             shinydashboard::box(
               title = tags$span(tags$i(class="fa fa-download", style="margin-left:6px;color:#22c55e;"), "دانلود گزارش"),
               width = 12, solidHeader = FALSE, status = "success",
               tags$p(style="font-size:14px;color:var(--text3);margin:-4px 0 12px;",
                      "دو قالب خروجی: Excel برای تحلیل داده‌محور، و HTML برای اشتراک‌گذاری سریع و مشاهده در مرورگر بدون نیاز به نصب هیچ نرم‌افزاری."),
               fluidRow(
                 column(6,
                        div(class = "report-dl-card",
                            icon("file-excel", class = "dl-icon", style = "color:#1D6F42;"),
                            tags$div(class = "dl-title", "گزارش Excel"),
                            tags$p(class = "dl-desc", "صفحات: خلاصه، داده تاریخی، پیش‌بینی، ناهنجاری‌ها، رتبه‌بندی"),
                            downloadButton(ns("download_excel"), label = tags$span(tags$i(class="fa fa-download", style="margin-left:5px;"), "Excel"), class = "btn-success")
                        )
                 ),
                 column(6,
                        div(class = "report-dl-card",
                            icon("file-code", class = "dl-icon", style = "color:#3b82f6;"),
                            tags$div(class = "dl-title", "گزارش HTML"),
                            tags$p(class = "dl-desc", "خودکار و همیشه قابل‌اعتماد — قابل چاپ و اشتراک‌گذاری در مرورگر"),
                            downloadButton(ns("download_html"), label = tags$span(tags$i(class="fa fa-download", style="margin-left:5px;"), "HTML"), class = "btn-primary")
                        )
                 )
               )
             ),
             
             shinydashboard::box(
               title = tags$span(tags$i(class="fa fa-eye", style="margin-left:6px;color:#fbbf24;"), "پیش‌نمایش داده گزارش"),
               width = 12, solidHeader = FALSE, status = "warning",
               collapsible = TRUE,
               uiOutput(ns("report_preview"))
             )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────────
reportServer <- function(id, weather_data, forecast_rv, anomaly_rv, leaderboard_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      req(weather_data())
      choices <- purrr::set_names(names(weather_data()), purrr::map_chr(names(weather_data()), ~ STATIONS[[.x]]$name %||% .x))
      updateSelectInput(session, "report_station", choices = choices)
    })
    
    report_data <- reactive({
      req(input$report_station, weather_data())
      sid <- input$report_station
      req(sid %in% names(weather_data()))
      df <- weather_data()[[sid]]
      
      date_range <- input$report_date_range
      if (!is.null(date_range)) {
        df <- df %>% dplyr::filter(date >= date_range[1] & date <= date_range[2])
      }
      
      summary_df <- tibble::tibble(
        "معیار" = c("تعداد روز", "میانگین دما", "حداقل دما", "حداکثر دما", "میانگین رطوبت", "کل بارش", "میانگین سرعت باد"),
        "مقدار" = c(
          nrow(df),
          round(mean(df$temperature, na.rm = TRUE), 1),
          round(min(df$temperature, na.rm = TRUE), 1),
          round(max(df$temperature, na.rm = TRUE), 1),
          round(mean(df$humidity, na.rm = TRUE), 1),
          round(sum(df$precipitation, na.rm = TRUE), 1),
          round(mean(df$wind_speed, na.rm = TRUE), 1)
        )
      )
      
      list(station = sid, station_name = STATIONS[[sid]]$name %||% sid, df = df, summary = summary_df, date_range = date_range)
    })
    
    # ── پیش‌نمایش ────────────────────────────────────────────────────────────
    output$report_preview <- renderUI({
      req(report_data())
      rd <- report_data()
      tagList(
        tags$h5(style="color:var(--text);", paste("ایستگاه:", rd$station_name)),
        tags$p(style="color:var(--text2);", paste("بازه:", rd$date_range[1], "تا", rd$date_range[2])),
        tags$p(style="color:var(--text2);", paste("تعداد روز:", nrow(rd$df))),
        hr(),
        DT::DTOutput(ns("preview_table"))
      )
    })
    
    output$preview_table <- DT::renderDT({
      req(report_data())
      DT::datatable(
        report_data()$summary,
        options = list(dom = "t", pageLength = 10),
        rownames = FALSE, class = "cell-border stripe"
      )
    })
    
    # ── دانلود Excel ──────────────────────────────────────────────────────────
    output$download_excel <- downloadHandler(
      filename = function() { paste0("weather_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx") },
      content = function(file) {
        req(report_data())
        rd <- report_data()
        
        tryCatch({
          wb <- openxlsx::createWorkbook()
          
          openxlsx::addWorksheet(wb, "خلاصه آماری")
          openxlsx::writeData(wb, "خلاصه آماری", x = input$report_title, startRow = 1, startCol = 1)
          openxlsx::addStyle(wb, "خلاصه آماری", style = openxlsx::createStyle(fontSize = 14, fontColour = "#FFFFFF", fgFill = "#3b82f6", halign = "center", bold = TRUE), rows = 1, cols = 1:2, gridExpand = TRUE)
          openxlsx::mergeCells(wb, "خلاصه آماری", rows = 1, cols = 1:2)
          
          meta_df <- data.frame(
            "گزارش" = c("ایستگاه", "بازه زمانی", "تهیه‌کننده", "تاریخ تولید"),
            "جزئیات" = c(rd$station_name, paste(rd$date_range[1], "تا", rd$date_range[2]), input$analyst_name, format(Sys.Date(), "%Y-%m-%d")),
            check.names = FALSE
          )
          openxlsx::writeData(wb, "خلاصه آماری", x = meta_df, startRow = 3)
          openxlsx::writeData(wb, "خلاصه آماری", x = rd$summary, startRow = 9)
          
          openxlsx::addWorksheet(wb, "داده تاریخی")
          openxlsx::writeDataTable(wb, "داده تاریخی", x = rd$df, tableName = "HistoricalData")
          
          if (isTRUE(input$include_forecast) && !is.null(forecast_rv())) {
            fr <- forecast_rv()
            openxlsx::addWorksheet(wb, "پیش‌بینی ۲۴ ساعت")
            fc_df <- tibble::tibble(timestamp = fr$timestamps)
            for (m_name in names(fr$preds_list)) {
              m_data <- fr$preds_list[[m_name]]
              fc_df[[m_data$method]] <- as.numeric(m_data$preds)
            }
            openxlsx::writeDataTable(wb, "پیش‌بینی ۲۴ ساعت", x = fc_df, tableName = "Forecast24h")
            
            if (length(fr$daily_preds) > 0) {
              openxlsx::addWorksheet(wb, "پیش‌بینی ۷ روز")
              daily_df <- tibble::tibble(date = fr$daily_preds[[1]]$date)
              for (m_name in names(fr$daily_preds)) {
                m_daily <- fr$daily_preds[[m_name]]
                daily_df[[paste0(m_name, "_max")]] <- m_daily$max_val
                daily_df[[paste0(m_name, "_min")]] <- m_daily$min_val
              }
              openxlsx::writeDataTable(wb, "پیش‌بینی ۷ روز", x = daily_df, tableName = "Forecast7d")
            }
          }
          
          if (isTRUE(input$include_anomalies) && !is.null(anomaly_rv())) {
            openxlsx::addWorksheet(wb, "ناهنجاری‌ها")
            anom_df <- anomaly_rv() %>% dplyr::filter(is_anomaly) %>% dplyr::select(date, temperature, anomaly_score, method)
            openxlsx::writeDataTable(wb, "ناهنجاری‌ها", x = anom_df, tableName = "Anomalies")
          }
          
          if (isTRUE(input$include_leaderboard) && !is.null(leaderboard_rv())) {
            openxlsx::addWorksheet(wb, "رتبه‌بندی مدل‌ها")
            lb_df <- leaderboard_rv()$metrics
            openxlsx::writeDataTable(wb, "رتبه‌بندی مدل‌ها", x = lb_df, tableName = "Leaderboard")
          }
          
          for (sname in openxlsx::sheets(wb)) {
            openxlsx::setColWidths(wb, sname, cols = 1:15, widths = "auto")
          }
          
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          showNotification("فایل Excel آماده شد.", type = "message")
          
        }, error = function(e) {
          showNotification(paste("خطا در ساخت Excel:", e$message), type = "error", duration = 15)
        })
      }
    )
    
    # ── دانلود HTML ───────────────────────────────────────────────────────────
    output$download_html <- downloadHandler(
      filename = function() { paste0("weather_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html") },
      content = function(file) {
        req(report_data())
        rd <- report_data()
        
        tryCatch({
          html_content <- build_html_report(rd, input, forecast_rv(), anomaly_rv(), leaderboard_rv())
          writeLines(html_content, file, useBytes = TRUE)
          showNotification("فایل HTML آماده شد.", type = "message")
        }, error = function(e) {
          showNotification(paste("خطا در ساخت HTML:", e$message), type = "error", duration = 15)
        })
      }
    )
  })
}

# ── ساخت HTML گزارش ─────────────────────────────────────────────────────────
build_html_report <- function(rd, input, forecast_rv, anomaly_rv, leaderboard_rv) {
  fc_section <- ""; anom_section <- ""; lb_section <- ""
  
  if (!is.null(forecast_rv) && length(forecast_rv$preds_list) > 0) {
    fc_df <- data.frame(Timestamp = format(forecast_rv$timestamps, "%Y-%m-%d %H:%M"))
    for (m_name in names(forecast_rv$preds_list)) {
      m_data <- forecast_rv$preds_list[[m_name]]
      fc_df[[m_data$method]] <- round(as.numeric(m_data$preds), 2)
    }
    
    header <- paste0("<tr><th>", paste(colnames(fc_df), collapse="</th><th>"), "</th></tr>")
    rows <- paste(apply(fc_df, 1, function(row) {
      paste0("<tr><td>", paste(row, collapse="</td><td>"), "</td></tr>")
    }), collapse="")
    
    fc_section <- paste0(
      "<h2>پیش‌بینی ۲۴ ساعت آینده</h2>",
      "<table>", header, rows, "</table>"
    )
  }
  
  if (!is.null(anomaly_rv)) {
    n_a <- sum(anomaly_rv$is_anomaly, na.rm = TRUE)
    anom_section <- paste0(
      "<h2>ناهنجاری‌ها</h2>",
      "<p>تعداد ناهنجاری شناسایی‌شده: <b>", n_a, "</b></p>"
    )
  }
  
  if (!is.null(leaderboard_rv) && !is.null(leaderboard_rv$metrics)) {
    lb_df <- leaderboard_rv$metrics
    best <- lb_df[1, ]
    lb_rows <- tryCatch({
      lb <- utils::head(lb_df, 10)
      cols <- intersect(c("model", "RMSE", "MAE", "R2", "composite_score"), names(lb))
      paste(apply(lb, 1, function(row) {
        cells <- paste(sapply(cols, function(c) paste0("<td>", row[[c]], "</td>")), collapse = "")
        paste0("<tr>", cells, "</tr>")
      }), collapse = "")
    }, error = function(e) "")
    hdr_names <- if (nzchar(lb_rows))
      paste0("<tr><th>", paste(sapply(intersect(c("model", "RMSE", "MAE", "R2", "composite_score"), names(lb_df)), function(c) c), collapse="</th><th>"), "</th></tr>")
    else ""
    lb_section <- paste0(
      "<h2>رتبه‌بندی مدل‌ها</h2>",
      "<p>بهترین مدل: <b>", best$model, "</b> (RMSE: ", round(best$RMSE, 3), ")</p>",
      if (nzchar(lb_rows)) paste0("<table>", hdr_names, lb_rows, "</table>") else ""
    )
  }
  
  paste0(
    "<!DOCTYPE html><html dir='rtl' lang='fa'><head>",
    "<meta charset='UTF-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1.0'>",
    "<title>", input$report_title, "</title>",
    "<style>",
    "body{font-family:Tahoma,Arial,sans-serif;direction:rtl;line-height:1.7;color:#1e293b;background:#f8fafc;max-width:900px;margin:0 auto;padding:32px;}",
    "h1{color:#2E86AB;border-bottom:3px solid #2E86AB;padding-bottom:10px;margin-top:0;}",
    "h2{color:#A23B72;margin-top:28px;}",
    ".meta{background:#fff;border:1px solid #e2e8f0;border-radius:8px;padding:14px 18px;margin:16px 0;}",
    ".meta span{display:inline-block;margin-left:22px;}",
    "table{border-collapse:collapse;width:100%;background:#fff;margin:10px 0;box-shadow:0 1px 3px rgba(0,0,0,0.06);}",
    "td,th{border:1px solid #e2e8f0;padding:9px 12px;text-align:right;}",
    "th{background:#2E86AB;color:white;font-weight:700;}",
    "tr:nth-child(even){background:#f8fafc;}",
    "hr{border:none;border-top:1px solid #e2e8f0;margin:24px 0;}",
    "footer{margin-top:30px;color:#64748b;font-size:13px;text-align:center;}",
    "</style></head><body>",
    "<h1>", input$report_title, "</h1>",
    "<div class='meta'>",
    "<span><b>ایستگاه:</b> ", rd$station_name, "</span>",
    "<span><b>بازه:</b> ", rd$date_range[1], " تا ", rd$date_range[2], "</span>",
    "<span><b>تعداد روز:</b> ", nrow(rd$df), "</span><br>",
    "<span><b>تاریخ تهیه:</b> ", format(Sys.Date(), "%Y-%m-%d"), "</span>",
    "<span><b>تهیه‌کننده:</b> ", input$analyst_name, "</span>",
    "</div>",
    "<h2>آمار توصیفی</h2>",
    "<table><tr><th>معیار</th><th>مقدار</th></tr>",
    paste(apply(rd$summary, 1, function(row) paste0("<tr><td>", row[1], "</td><td>", row[2], "</td></tr>")), collapse = ""),
    "</table>",
    fc_section, anom_section, lb_section,
    "<footer><i>این گزارش به‌صورت خودکار توسط سیستم تحلیل آب‌وهوا تهیه شده است.</i></footer>",
    "</body></html>"
  )
}