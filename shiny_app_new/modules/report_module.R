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
          # چک‌باکس‌های محتوا — چیدمان کارت‌مانند
          tags$div(class="rc-sec-title",
                   tags$i(class="fa fa-list-check", style="margin-left:6px;color:var(--blue2);"),
                   "محتوای گزارش"),
          tags$div(class="rc-chip-grid",
            checkboxInput(ns("include_forecast"),
              label = tagList(tags$i(class="fa fa-cloud-sun", style="margin-left:5px;color:#60a5fa;"), "پیش‌بینی"),
              value = TRUE, width = "100%"
            ),
            checkboxInput(ns("include_metrics"),
              label = tagList(tags$i(class="fa fa-ruler-combined", style="margin-left:5px;color:#2dd4bf;"), "معیارهای ارزیابی"),
              value = TRUE, width = "100%"
            ),
            checkboxInput(ns("include_anomalies"),
              label = tagList(tags$i(class="fa fa-triangle-exclamation", style="margin-left:5px;color:#fbbf24;"), "ناهنجاری‌ها"),
              value = TRUE, width = "100%"
            ),
            checkboxInput(ns("include_leaderboard"),
              label = tagList(tags$i(class="fa fa-trophy", style="margin-left:5px;color:#a78bfa;"), "رتبه‌بندی مدل‌ها"),
              value = TRUE, width = "100%"
            )
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
          tags$p(style="font-size:11.5px;color:var(--text3);margin:-4px 0 12px;",
                 "سه قالب خروجی: Excel برای تحلیل داده‌محور، PDF برای گزارش رسمی (نیازمند LaTeX)، و HTML برای اشتراک‌گذاری سریع بدون نیاز به نصب."),
          fluidRow(
            column(4,
              div(
                class = "report-dl-card",
                icon("file-excel", class = "dl-icon", style = "color:#1D6F42;"),
                tags$div(class = "dl-title", "گزارش Excel"),
                tags$p(class = "dl-desc",
                  "صفحات: خلاصه، داده تاریخی، پیش‌بینی، ناهنجاری‌ها، رتبه‌بندی"
                ),
                downloadButton(
                  ns("download_excel"),
                  label = tags$span(tags$i(class="fa fa-download", style="margin-left:5px;"), "Excel"),
                  class = "btn-success"
                )
              )
            ),
            column(4,
              div(
                class = "report-dl-card",
                icon("file-pdf", class = "dl-icon", style = "color:#dc3545;"),
                tags$div(class = "dl-title", "گزارش PDF"),
                tags$p(class = "dl-desc",
                  "خلاصه اجرایی + آمار توصیفی + جداول عملکرد"
                ),
                downloadButton(
                  ns("download_pdf"),
                  label = tags$span(tags$i(class="fa fa-download", style="margin-left:5px;"), "PDF"),
                  class = "btn-danger"
                )
              )
            ),
            column(4,
              div(
                class = "report-dl-card",
                icon("file-code", class = "dl-icon", style = "color:#3b82f6;"),
                tags$div(class = "dl-title", "گزارش HTML"),
                tags$p(class = "dl-desc",
                  "خودکار و همیشه قابل‌اعتماد — بدون نیاز به LaTeX"
                ),
                downloadButton(
                  ns("download_html"),
                  label = tags$span(tags$i(class="fa fa-download", style="margin-left:5px;"), "HTML"),
                  class = "btn-primary"
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
          # ردیف ۱: عنوان گزارش
          openxlsx::writeData(wb, "خلاصه آماری",
            x        = input$report_title,
            startRow = 1, startCol = 1
          )
          openxlsx::addStyle(wb, "خلاصه آماری",
            style = openxlsx::createStyle(
              fontSize   = 14,
              fontColour = "#FFFFFF",
              fgFill     = COLORS$primary,
              halign     = "center",
              bold       = TRUE
            ),
            rows = 1, cols = 1:2, gridExpand = TRUE
          )
          openxlsx::mergeCells(wb, "خلاصه آماری", rows = 1, cols = 1:2)

          # ردیف‌های ۳ تا ۶: متادیتای گزارش (ایستگاه، بازه، تهیه‌کننده، تاریخ تولید)
          meta_df <- data.frame(
            "گزارش" = c("ایستگاه", "بازه زمانی", "تهیه‌کننده", "تاریخ تولید"),
            "جزئیات" = c(rd$station_name,
                         paste(rd$date_range[1], "تا", rd$date_range[2]),
                         input$analyst_name,
                         format(Sys.Date(), "%Y-%m-%d")),
            check.names = FALSE
          )
          openxlsx::writeData(wb, "خلاصه آماری", x = meta_df, startRow = 3)
          openxlsx::addStyle(wb, "خلاصه آماری",
            style = openxlsx::createStyle(textDecoration = "bold", fgFill = "#e8eef7"),
            rows = 3:6, cols = 1, gridExpand = TRUE
          )
          # جدول آمار توصیفی از ردیف ۹
          openxlsx::writeData(wb, "خلاصه آماری",
            x        = rd$summary,
            startRow = 9
          )
          openxlsx::addStyle(wb, "خلاصه آماری",
            style = openxlsx::createStyle(
              fontSize = 11, fontColour = "#FFFFFF",
              fgFill = COLORS$primary, halign = "right", bold = TRUE
            ),
            rows = 9, cols = 1:2, gridExpand = TRUE
          )

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

          # اگر PDF موفق نشد، پیام راهنما بده — کاربر می‌تواند از دکمه HTML استفاده کند
          if (!pdf_ok) {
            showNotification(
              "ساخت PDF ناموفق بود (نیاز به LaTeX/xelatex). از دکمه HTML استفاده کنید.",
              type = "warning", duration = 12
            )
          } else {
            showNotification("فایل PDF آماده شد.", type = "message")
          }

        }, error = function(e) {
          showNotification(
            paste("خطا در ساخت PDF:", e$message),
            type = "error", duration = 15
          )
        })
      }
    )

    # ── دانلود HTML ───────────────────────────────────────────────────────────
    output$download_html <- downloadHandler(
      filename = function() {
        paste0("weather_report_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      },
      content = function(file) {
        req(report_data())
        rd <- report_data()

        tryCatch({
          html_content <- build_html_report(rd, input, forecast_rv(),
                                             anomaly_rv(), leaderboard_rv())
          writeLines(html_content, file, useBytes = TRUE)
          showNotification("فایل HTML آماده شد.", type = "message")
        }, error = function(e) {
          showNotification(
            paste("خطا در ساخت HTML:", e$message),
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
      "<p>مدل: <b>", fr$fc$method, "</b> &nbsp;|&nbsp; افق: <b>", fr$horizon, " روز</b></p>",
      "<p>میانگین پیش‌بینی: <b>",
      round(mean(fr$fc$predictions, na.rm = TRUE), 2), "</b></p>"
    )
  }

  if (!is.null(anomaly_rv)) {
    n_a <- sum(anomaly_rv$is_anomaly, na.rm = TRUE)
    anom_section <- paste0(
      "<h2>ناهنجاری‌ها</h2>",
      "<p>تعداد ناهنجاری شناسایی‌شده: <b>", n_a, "</b></p>"
    )
  }

  if (!is.null(leaderboard_rv)) {
    best <- leaderboard_rv[1, ]
    # جدول کامل رتبه‌بندی (تا ۱۰ مدل برتر)
    lb_rows <- tryCatch({
      lb <- utils::head(leaderboard_rv, 10)
      cols <- intersect(c("model", "RMSE", "MAE", "R2", "composite_score"), names(lb))
      paste(apply(lb, 1, function(row) {
        cells <- paste(sapply(cols, function(c) paste0("<td>", row[[c]], "</td>")), collapse = "")
        paste0("<tr>", cells, "</tr>")
      }), collapse = "")
    }, error = function(e) "")
    hdr_names <- if (nzchar(lb_rows))
      paste(sapply(intersect(c("model", "RMSE", "MAE", "R2", "composite_score"),
                             names(leaderboard_rv)),
                   function(c) paste0("<th>", c, "</th>")), collapse = "")
    else ""
    lb_section <- paste0(
      "<h2>رتبه‌بندی مدل‌ها</h2>",
      "<p>بهترین مدل: <b>", best$model, "</b> (RMSE: ", round(best$RMSE, 3), ")</p>",
      if (nzchar(lb_rows))
        paste0("<table><tr>", hdr_names, "</tr>", lb_rows, "</table>")
      else ""
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
    paste(
      apply(rd$summary, 1, function(row) {
        paste0("<tr><td>", row[1], "</td><td>", row[2], "</td></tr>")
      }),
      collapse = ""
    ),
    "</table>",
    fc_section, anom_section, lb_section,
    "<footer><i>این گزارش به‌صورت خودکار توسط سیستم تحلیل آب‌وهوا تهیه شده است.</i></footer>",
    "</body></html>"
  )
}
