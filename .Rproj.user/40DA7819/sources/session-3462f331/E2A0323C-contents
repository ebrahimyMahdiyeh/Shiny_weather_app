# File: modules/report_module.R
# ماژول گزارش — خروجی Excel و PDF

# ── UI ───────────────────────────────────────────────────────────────────────
reportUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        box(
          title       = "تنظیمات گزارش",
          width       = 12,
          status      = "info",
          solidHeader = TRUE,
          selectInput(ns("report_station"),
            label   = "ایستگاه:",
            choices = NULL
          ),
          textInput(ns("report_title"),
            label = "عنوان گزارش:",
            value = "گزارش تحلیل آب‌وهوا"
          ),
          textInput(ns("analyst_name"),
            label = "نام تحلیلگر:",
            value = "سیستم خودکار"
          ),
          dateRangeInput(ns("report_date_range"),
            label     = "بازه زمانی:",
            start     = Sys.Date() - 365,
            end       = Sys.Date() - 1,
            language  = "fa"
          ),
          hr(),
          # چک‌باکس‌های محتوا
          tags$p(tags$b("محتوای گزارش:")),
          checkboxInput(ns("include_forecast"),
            label = "پیش‌بینی",
            value = TRUE
          ),
          checkboxInput(ns("include_metrics"),
            label = "معیارهای ارزیابی",
            value = TRUE
          ),
          checkboxInput(ns("include_anomalies"),
            label = "ناهنجاری‌ها",
            value = TRUE
          ),
          checkboxInput(ns("include_leaderboard"),
            label = "تابلوی رتبه‌بندی",
            value = TRUE
          )
        )
      ),

      column(8,
        # پیش‌نمایش و دانلود
        box(
          title       = "دانلود گزارش",
          width       = 12,
          status      = "primary",
          solidHeader = TRUE,
          fluidRow(
            column(6,
              div(
                style = paste(
                  "background:#f8f9fa; border:2px dashed #dee2e6;",
                  "border-radius:8px; padding:30px; text-align:center;"
                ),
                icon("file-excel", class = "fa-3x", style = "color:#1D6F42;"),
                tags$h4("گزارش Excel"),
                tags$p(
                  "شامل صفحات: پیش‌بینی، معیارها، ناهنجاری‌ها، رتبه‌بندی",
                  style = "color:#666; font-size:13px;"
                ),
                br(),
                downloadButton(
                  ns("download_excel"),
                  label = "دانلود Excel",
                  class = "btn-success"
                )
              )
            ),
            column(6,
              div(
                style = paste(
                  "background:#f8f9fa; border:2px dashed #dee2e6;",
                  "border-radius:8px; padding:30px; text-align:center;"
                ),
                icon("file-pdf", class = "fa-3x", style = "color:#dc3545;"),
                tags$h4("گزارش PDF"),
                tags$p(
                  "خلاصه اجرایی + نمودارها + جداول عملکرد",
                  style = "color:#666; font-size:13px;"
                ),
                br(),
                downloadButton(
                  ns("download_pdf"),
                  label = "دانلود PDF",
                  class = "btn-danger"
                )
              )
            )
          )
        ),

        # پیش‌نمایش آمار برای گزارش
        box(
          title       = "پیش‌نمایش داده گزارش",
          width       = 12,
          status      = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          uiOutput(ns("report_preview"))
        )
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────────
reportServer <- function(id, weather_data, forecast_rv, anomaly_rv,
                          leaderboard_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # تنظیم ایستگاه‌ها
    observe({
      req(weather_data())
      choices <- purrr::imap_chr(weather_data(), function(df, sid) sid) %>%
        purrr::set_names(purrr::imap_chr(weather_data(), function(df, sid) {
          STATIONS[[sid]]$name %||% sid
        }))
      updateSelectInput(session, "report_station", choices = choices)
    })

    # ── آماده‌سازی داده گزارش ────────────────────────────────────────────────
    report_data <- reactive({
      req(input$report_station, weather_data())
      sid <- input$report_station
      req(sid %in% names(weather_data()))
      df <- weather_data()[[sid]]

      # فیلتر بازه زمانی
      date_range <- input$report_date_range
      if (!is.null(date_range)) {
        df <- df %>%
          dplyr::filter(date >= date_range[1] & date <= date_range[2])
      }

      # آمار خلاصه
      summary_df <- tibble::tibble(
        "معیار"   = c("تعداد روز", "میانگین دما", "حداقل دما",
                      "حداکثر دما", "میانگین رطوبت",
                      "کل بارش", "میانگین سرعت باد"),
        "مقدار"   = c(
          nrow(df),
          round(mean(df$temperature, na.rm = TRUE), 1),
          round(min(df$temperature, na.rm = TRUE), 1),
          round(max(df$temperature, na.rm = TRUE), 1),
          round(mean(df$humidity, na.rm = TRUE), 1),
          round(sum(df$precipitation, na.rm = TRUE), 1),
          round(mean(df$wind_speed, na.rm = TRUE), 1)
        )
      )

      list(
        station      = sid,
        station_name = STATIONS[[sid]]$name %||% sid,
        df           = df,
        summary      = summary_df,
        date_range   = date_range
      )
    })

    # ── پیش‌نمایش ────────────────────────────────────────────────────────────
    output$report_preview <- renderUI({
      req(report_data())
      rd <- report_data()
      tagList(
        tags$h5(paste("ایستگاه:", rd$station_name)),
        tags$p(paste("بازه:", rd$date_range[1], "تا", rd$date_range[2])),
        tags$p(paste("تعداد روز:", nrow(rd$df))),
        hr(),
        DT::renderDT({
          DT::datatable(
            rd$summary,
            options  = list(dom = "t", pageLength = 10),
            rownames = FALSE,
            class    = "cell-border stripe"
          )
        })
      )
    })

    # ── دانلود Excel ──────────────────────────────────────────────────────────
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("weather_report_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        req(report_data())
        rd <- report_data()

        tryCatch({
          wb <- openxlsx::createWorkbook()

          # ── صفحه ۱: خلاصه آماری ─────────────────────────────────────────
          openxlsx::addWorksheet(wb, "خلاصه آماری")
          openxlsx::writeData(wb, "خلاصه آماری",
            x        = rd$summary,
            startRow = 3
          )
          # عنوان
          openxlsx::writeData(wb, "خلاصه آماری",
            x        = input$report_title,
            startRow = 1, startCol = 1
          )
          openxlsx::addStyle(wb, "خلاصه آماری",
            style = openxlsx::createStyle(
              fontSize  = 14,
              fontColour = "#FFFFFF",
              fgFill    = COLORS$primary,
              halign    = "center",
              bold      = TRUE
            ),
            rows = 1, cols = 1:2, gridExpand = TRUE
          )
          openxlsx::mergeCells(wb, "خلاصه آماری", rows = 1, cols = 1:2)

          # ── صفحه ۲: داده خام ───────────────────────────────────────────
          openxlsx::addWorksheet(wb, "داده تاریخی")
          openxlsx::writeDataTable(
            wb, "داده تاریخی",
            x         = rd$df,
            tableName = "HistoricalData"
          )

          # ── صفحه ۳: پیش‌بینی ───────────────────────────────────────────
          if (isTRUE(input$include_forecast) && !is.null(forecast_rv())) {
            openxlsx::addWorksheet(wb, "پیش‌بینی")
            fr       <- forecast_rv()
            last_d   <- max(fr$df$date)
            fc_dates <- seq.Date(last_d + 1, by = "day", length.out = fr$horizon)
            fc_df    <- tibble::tibble(
              date        = fc_dates,
              prediction  = round(fr$fc$predictions[seq_len(fr$horizon)], 2),
              lower_95    = if (!is.null(fr$fc$lower))
                round(fr$fc$lower[seq_len(fr$horizon)], 2) else NA,
              upper_95    = if (!is.null(fr$fc$upper))
                round(fr$fc$upper[seq_len(fr$horizon)], 2) else NA,
              model       = fr$fc$method
            )
            openxlsx::writeDataTable(wb, "پیش‌بینی", x = fc_df, tableName = "Forecast")
          }

          # ── صفحه ۴: ناهنجاری‌ها ────────────────────────────────────────
          if (isTRUE(input$include_anomalies) && !is.null(anomaly_rv())) {
            openxlsx::addWorksheet(wb, "ناهنجاری‌ها")
            anom_df <- anomaly_rv() %>%
              dplyr::filter(is_anomaly) %>%
              dplyr::select(date, temperature, anomaly_score, method)
            openxlsx::writeDataTable(
              wb, "ناهنجاری‌ها",
              x         = anom_df,
              tableName = "Anomalies"
            )
          }

          # ── صفحه ۵: تابلوی رتبه‌بندی ────────────────────────────────────
          if (isTRUE(input$include_leaderboard) && !is.null(leaderboard_rv())) {
            openxlsx::addWorksheet(wb, "رتبه‌بندی مدل‌ها")
            openxlsx::writeDataTable(
              wb, "رتبه‌بندی مدل‌ها",
              x         = leaderboard_rv(),
              tableName = "Leaderboard"
            )
          }

          # تنظیم عرض ستون‌ها
          for (sname in openxlsx::sheets(wb)) {
            openxlsx::setColWidths(wb, sname, cols = 1:10, widths = "auto")
          }

          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          showNotification("فایل Excel آماده شد.", type = "message")

        }, error = function(e) {
          showNotification(paste("خطا در ساخت Excel:", e$message),
                           type = "error", duration = 15)
        })
      }
    )

    # ── دانلود PDF ────────────────────────────────────────────────────────────
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("weather_report_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
      },
      content = function(file) {
        req(report_data())
        rd <- report_data()

        tryCatch({
          # ساخت محتوای Markdown برای تبدیل به PDF
          tmp_rmd <- tempfile(fileext = ".Rmd")

          # جمع‌آوری داده‌ها برای گزارش
          fc_text      <- ""
          lb_text      <- ""
          anom_text    <- ""

          if (isTRUE(input$include_forecast) && !is.null(forecast_rv())) {
            fr <- forecast_rv()
            fc_text <- paste0(
              "\n## پیش‌بینی\n",
              "مدل مورد استفاده: **", fr$fc$method, "**\n\n",
              "افق پیش‌بینی: **", fr$horizon, " روز**\n\n",
              "میانگین پیش‌بینی: **",
              round(mean(fr$fc$predictions, na.rm = TRUE), 2), "**\n"
            )
          }

          if (isTRUE(input$include_leaderboard) && !is.null(leaderboard_rv())) {
            lb  <- leaderboard_rv()
            best <- lb[1, ]
            lb_text <- paste0(
              "\n## عملکرد مدل‌ها\n",
              "بهترین مدل: **", best$model, "**\n\n",
              "RMSE: **", round(best$RMSE, 3), "** | ",
              "MAE: **",  round(best$MAE, 3),  "** | ",
              "R²: **",  round(best$R2, 3),   "**\n"
            )
          }

          if (isTRUE(input$include_anomalies) && !is.null(anomaly_rv())) {
            anom <- anomaly_rv()
            n_a  <- sum(anom$is_anomaly, na.rm = TRUE)
            anom_text <- paste0(
              "\n## ناهنجاری‌ها\n",
              "تعداد ناهنجاری‌های شناسایی‌شده: **", n_a, "**\n\n",
              "روش: **", if (nrow(anom) > 0) anom$method[1] else "نامشخص", "**\n"
            )
          }

          # نوشتن فایل Rmd
          rmd_content <- paste0(
'---
title: "', input$report_title, '"
subtitle: "ایستگاه: ', rd$station_name, '"
author: "', input$analyst_name, '"
date: "', format(Sys.Date(), "%Y-%m-%d"), '"
output:
  pdf_document:
    latex_engine: xelatex
    toc: true
    number_sections: true
header-includes:
  - \\usepackage{fontspec}
  - \\usepackage{polyglossia}
  - \\setmainlanguage{persian}
  - \\setotherlanguage{english}
  - \\newfontfamily\\persianfont[Script=Arabic]{FreeSerif}
---

# خلاصه اجرایی

این گزارش توسط سیستم خودکار تحلیل آب‌وهوا تهیه شده است.

**ایستگاه:** ', rd$station_name, '

**بازه زمانی:** ', rd$date_range[1], ' تا ', rd$date_range[2], '

**تعداد روزهای تحلیل:** ', nrow(rd$df), '

# آمار توصیفی

```{r, echo=FALSE}
knitr::kable(rd_summary, caption = "خلاصه آماری", align = "lr")
```

',
            fc_text,
            anom_text,
            lb_text,
'

# نتیجه‌گیری

این گزارش بر اساس داده‌های هواشناسی تاریخی تهیه شده و اطلاعات آن
جهت تصمیم‌گیری‌های آب‌وهوایی قابل استفاده است.
'
          )

          writeLines(rmd_content, tmp_rmd)

          # محیط برای render
          rd_env <- new.env()
          rd_env$rd_summary <- rd$summary

          # تلاش برای render PDF
          pdf_ok <- tryCatch({
            rmarkdown::render(
              tmp_rmd,
              output_file = file,
              envir       = rd_env,
              quiet       = TRUE
            )
            TRUE
          }, error = function(e) {
            message("PDF render خطا: ", e$message)
            FALSE
          })

          # اگر PDF موفق نشد، فایل HTML ساده بسازیم
          if (!pdf_ok) {
            message("ساخت HTML جایگزین PDF...")
            html_content <- build_html_report(rd, input, forecast_rv(),
                                               anomaly_rv(), leaderboard_rv())
            tmp_html <- sub("\\.pdf$", ".html", file)
            writeLines(html_content, file)
          }

          showNotification("فایل PDF آماده شد.", type = "message")

        }, error = function(e) {
          showNotification(
            paste("خطا در ساخت PDF:", e$message),
            type = "error", duration = 15
          )
        })
      }
    )
  })
}

# ── ساخت HTML گزارش (جایگزین PDF در صورت نبود LaTeX) ──────────────────────
build_html_report <- function(rd, input, forecast_rv, anomaly_rv, leaderboard_rv) {
  fc_section   <- ""
  anom_section <- ""
  lb_section   <- ""

  if (!is.null(forecast_rv)) {
    fr <- forecast_rv
    fc_section <- paste0(
      "<h2>پیش‌بینی</h2>",
      "<p>مدل: <b>", fr$fc$method, "</b></p>",
      "<p>افق: <b>", fr$horizon, " روز</b></p>",
      "<p>میانگین پیش‌بینی: <b>",
      round(mean(fr$fc$predictions, na.rm = TRUE), 2), "</b></p>"
    )
  }

  if (!is.null(anomaly_rv)) {
    n_a <- sum(anomaly_rv$is_anomaly, na.rm = TRUE)
    anom_section <- paste0(
      "<h2>ناهنجاری‌ها</h2>",
      "<p>تعداد ناهنجاری: <b>", n_a, "</b></p>"
    )
  }

  if (!is.null(leaderboard_rv)) {
    best <- leaderboard_rv[1, ]
    lb_section <- paste0(
      "<h2>رتبه‌بندی مدل‌ها</h2>",
      "<p>بهترین مدل: <b>", best$model, "</b></p>",
      "<p>RMSE: <b>", round(best$RMSE, 3), "</b></p>"
    )
  }

  paste0(
    "<!DOCTYPE html><html dir='rtl' lang='fa'><head>",
    "<meta charset='UTF-8'>",
    "<title>", input$report_title, "</title>",
    "<style>body{font-family:Tahoma,Arial;direction:rtl;padding:30px;}",
    "h1{color:#2E86AB;} h2{color:#A23B72;} table{border-collapse:collapse;width:100%;}",
    "td,th{border:1px solid #ddd;padding:8px;} th{background:#2E86AB;color:white;}",
    "</style></head><body>",
    "<h1>", input$report_title, "</h1>",
    "<p><b>ایستگاه:</b> ", rd$station_name, "</p>",
    "<p><b>تاریخ تهیه:</b> ", format(Sys.Date(), "%Y-%m-%d"), "</p>",
    "<p><b>تهیه‌کننده:</b> ", input$analyst_name, "</p>",
    "<hr>",
    "<h2>آمار توصیفی</h2>",
    "<table><tr><th>معیار</th><th>مقدار</th></tr>",
    paste(
      apply(rd$summary, 1, function(row) {
        paste0("<tr><td>", row[1], "</td><td>", row[2], "</td></tr>")
      }),
      collapse = ""
    ),
    "</table>",
    fc_section, anom_section, lb_section,
    "<hr><p><i>این گزارش به صورت خودکار توسط سیستم تحلیل آب‌وهوا تهیه شده است.</i></p>",
    "</body></html>"
  )
}
