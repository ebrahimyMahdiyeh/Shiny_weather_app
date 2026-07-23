# File: modules/leaderboard_module.R  (نسخه ارتقاءیافته)
# تابلوی رتبه‌بندی مدل‌ها — طراحی علمی

# ── UI ────────────────────────────────────────────────────────────────────────
leaderboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # پانل کنترل
      column(3,
             shinydashboard::box(
               title = tags$span(
                 tags$i(class="fa fa-gears", style="margin-left:7px;color:#4ade80;"),
                 "تنظیمات Benchmark"
               ),
               width = 12, solidHeader = FALSE,
               
               tags$div(class="ctrl-label-sci", "ایستگاه"),
               selectInput(ns("station"), label=NULL, choices=NULL, width="100%"),
               
               tags$div(class="ctrl-label-sci", "متغیر هدف"),
               selectInput(ns("target_var"), label=NULL,
                           choices = c(
                             "دما (°C)"         = "temperature",
                             "رطوبت (%)"        = "humidity",
                             "سرعت باد (km/h)"  = "wind_speed",
                             "بارش (mm)"        = "precipitation"
                           ),
                           width = "100%"
               ),
               
               tags$div(class="ctrl-label-sci", "نسبت داده آزمون"),
               sliderInput(ns("test_ratio"), label=NULL,
                           min=0.10, max=0.30, value=0.15, step=0.05, width="100%"
               ),
               
               tags$hr(),
               
               actionButton(
                 ns("run_benchmark"),
                 label = tags$span(
                   tags$i(class="fa fa-trophy", style="margin-left:6px;"),
                   "اجرای Benchmark"
                 ),
                 class = "btn btn-success btn-block",
                 style = "font-size:13px;font-weight:700;padding:10px;margin-bottom:8px;"
               ),
               
               downloadButton(
                 ns("download_csv"),
                 label = tags$span(
                   tags$i(class="fa fa-download", style="margin-left:5px;"),
                   "دانلود CSV"
                 ),
                 class = "btn btn-default btn-block",
                 style = "font-size:12px;padding:8px;"
               )
             ),
             
             # توضیح نمره ترکیبی
             shinydashboard::box(
               title = tags$span(
                 tags$i(class="fa fa-circle-question", style="margin-left:7px;color:#a78bfa;"),
                 "نمره ترکیبی"
               ),
               width = 12, solidHeader = FALSE,
               tags$div(style="font-size:11px;color:#94a3b8;line-height:1.7;",
                        "نمره ترکیبی میانگین وزنی معیارهای نرمال‌شده است:",
                        tags$div(
                          class = "formula-box",
                          style = "margin:8px 0;font-size:10px;",
                          "0.25×nRMSE + 0.20×nMAE + 0.20×nMAPE + 0.20×nR² + 0.15×nSMAPE"
                        ),
                        tags$div(style="margin-top:6px;",
                                 tags$span(style="display:inline-block;width:10px;height:10px;border-radius:2px;background:#22c55e;margin-left:5px;"),
                                 "< ۰.۲ عالی",
                                 tags$br(),
                                 tags$span(style="display:inline-block;width:10px;height:10px;border-radius:2px;background:#3b82f6;margin-left:5px;"),
                                 "۰.۲–۰.۴ خوب",
                                 tags$br(),
                                 tags$span(style="display:inline-block;width:10px;height:10px;border-radius:2px;background:#f59e0b;margin-left:5px;"),
                                 "۰.۴–۰.۶ متوسط",
                                 tags$br(),
                                 tags$span(style="display:inline-block;width:10px;height:10px;border-radius:2px;background:#ef4444;margin-left:5px;"),
                                 "> ۰.۶ ضعیف"
                        )
               )
             )
      ),
      
      # پانل اصلی
      column(9,
             # جدول رتبه‌بندی
             shinydashboard::box(
               title = tags$span(
                 tags$i(class="fa fa-trophy", style="margin-left:7px;color:#fbbf24;"),
                 "جدول رتبه‌بندی مدل‌ها",
                 tags$span(
                   style="margin-right:10px;font-size:10px;color:#64748b;font-weight:400;",
                   "کلیک روی ستون برای مرتب‌سازی ↕"
                 )
               ),
               width = 12, solidHeader = FALSE,
               shinycssloaders::withSpinner(
                 DT::DTOutput(ns("leaderboard_table")),
                 type = 8, color = "#f59e0b", size = 0.6
               )
             ),
             
             # نمودار مقایسه
             fluidRow(
               column(8,
                      shinydashboard::box(
                        title = tags$span(
                          tags$i(class="fa fa-chart-bar", style="margin-left:7px;color:#60a5fa;"),
                          "مقایسه مدل‌ها — نمودار"
                        ),
                        width = 12, solidHeader = FALSE,
                        tags$div(style="margin-bottom:8px;",
                                 selectInput(
                                   ns("metric_to_plot"),
                                   label = NULL,
                                   choices = c(
                                     "RMSE"          = "RMSE",
                                     "MAE"           = "MAE",
                                     "MAPE (%)"      = "MAPE",
                                     "R²"            = "R2",
                                     "SMAPE (%)"     = "SMAPE",
                                     "نمره ترکیبی"   = "composite_score"
                                   ),
                                   width = "200px"
                                 )
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput(ns("metric_bar_chart"), height = "280px"),
                          type = 8, color = "#3b82f6", size = 0.6
                        )
                      )
               ),
               column(4,
                      shinydashboard::box(
                        title = tags$span(
                          tags$i(class="fa fa-chart-radar", style="margin-left:7px;color:#a78bfa;"),
                          "نمودار رادار"
                        ),
                        width = 12, solidHeader = FALSE,
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput(ns("radar_chart"), height = "280px"),
                          type = 8, color = "#8b5cf6", size = 0.6
                        )
                      )
               )
             )
      )
    )
  )
}

# ── CSS اضافی برای leaderboard ──────────────────────────────────────────────
leaderboardCSS <- "
.ctrl-label-sci {
  font-size:10px; font-weight:700; color:#64748b;
  text-transform:uppercase; letter-spacing:0.8px;
  margin-bottom:4px; margin-top:10px;
}
"

# ── Server ────────────────────────────────────────────────────────────────────
leaderboardServer <- function(id, weather_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # رنگ مدل‌ها (LightGBM و CatBoost اضافه شدند)
    MODEL_COLORS <- c(
      arima="#3b82f6", sarima="#60a5fa", ets="#14b8a6", tbats="#0d9488",
      prophet="#8b5cf6", rf="#f59e0b", xgboost="#d97706", 
      lightgbm="#22d3ee", catboost="#fb7185", 
      svm="#ef4444", naive="#64748b"
    )
    
    # نام مدل‌ها (LightGBM و CatBoost اضافه شدند)
    MODEL_LABELS <- c(
      arima="ARIMA", sarima="SARIMA", ets="ETS", tbats="TBATS",
      prophet="Prophet", rf="Random Forest", xgboost="XGBoost", 
      lightgbm="LightGBM", catboost="CatBoost", 
      svm="SVM", naive="Naïve"
    )
    
    # تنظیم ایستگاه
    observe({
      req(weather_data())
      choices <- purrr::set_names(
        names(weather_data()),
        purrr::map_chr(names(weather_data()), ~ STATIONS[[.x]]$name %||% .x)
      )
      updateSelectInput(session, "station", choices = choices)
    })
    
    benchmark_results <- reactiveVal(NULL)
    
    # ── اجرای Benchmark ────────────────────────────────────────────────────
    observeEvent(input$run_benchmark, {
      req(input$station, input$target_var, weather_data())
      sid    <- input$station
      target <- input$target_var
      req(sid %in% names(weather_data()))
      
      df <- weather_data()[[sid]] %>%
        dplyr::arrange(date) %>%
        dplyr::filter(!is.na(.data[[target]]))
      
      if (nrow(df) < 60) {
        showNotification("حداقل ۶۰ روز داده برای Benchmark نیاز است.", type="error")
        return()
      }
      
      withProgress(message="در حال ارزیابی مدل‌ها...", value=0, {
        tryCatch({
          splits    <- train_test_split(df, test_ratio=input$test_ratio)
          train_df  <- splits$train
          test_df   <- splits$test
          test_vals <- test_df[[target]]
          test_h    <- nrow(test_df)
          
          # اضافه شدن lightgbm و catboost به لیست ارزیابی
          model_names <- c("arima","sarima","ets","tbats","prophet","rf","xgboost","lightgbm","catboost","svm","naive")
          all_metrics <- list()
          
          for (i in seq_along(model_names)) {
            mn <- model_names[i]
            setProgress((i-1)/length(model_names), paste("آموزش:", mn, paste0("(",i,"/",length(model_names),")")))
            tryCatch({
              fc    <- run_model_by_name(mn, train_df, test_h, target)
              preds <- fc$predictions[seq_len(min(test_h, length(fc$predictions)))]
              actual <- test_vals[seq_len(length(preds))]
              m     <- compute_all_metrics(actual, preds, model_name=mn)
              all_metrics[[mn]] <- m
            }, error = function(e) {
              message("خطا در مدل ", mn, ": ", conditionMessage(e))
              all_metrics[[mn]] <<- tibble::tibble(
                model=mn, RMSE=NA_real_, MAE=NA_real_,
                MAPE=NA_real_, R2=NA_real_, SMAPE=NA_real_
              )
            })
          }
          
          combined_metrics <- dplyr::bind_rows(all_metrics)
          ranked           <- compute_composite_score(combined_metrics)
          ranked$rank      <- seq_len(nrow(ranked))
          benchmark_results(ranked)
          setProgress(1, "Benchmark کامل شد!")
          showNotification("Benchmark با موفقیت انجام شد ✓", type="message")
          
        }, error = function(e) {
          msg <- paste("خطا در Benchmark:", conditionMessage(e))
          message(msg)
          showNotification(msg, type="error", duration=NULL)
        })
      })
    })
    
    # ── جدول رتبه‌بندی ────────────────────────────────────────────────────
    output$leaderboard_table <- DT::renderDT({
      req(benchmark_results())
      res <- benchmark_results()
      
      # آیکون رتبه
      rank_icons <- c("🥇","🥈","🥉", rep("",nrow(res)-3))
      if (nrow(res) < 3) rank_icons <- c("🥇","🥈")[seq_len(nrow(res))]
      
      display <- res %>%
        dplyr::mutate(
          model_label = MODEL_LABELS[model] %||% model,
          rank_label  = paste(rank_icons[rank], rank),
          score_bar   = composite_score
        ) %>%
        dplyr::select(
          "رتبه"         = rank_label,
          "مدل"          = model_label,
          "RMSE"         = RMSE,
          "MAE"          = MAE,
          "MAPE%"        = MAPE,
          "R²"           = R2,
          "SMAPE%"       = SMAPE,
          "نمره ترکیبی"  = composite_score
        ) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))
      
      DT::datatable(
        display,
        options = list(
          pageLength = 15, dom = "t", scrollX = TRUE,
          columnDefs = list(
            list(className="dt-center", targets="_all"),
            list(orderable=TRUE, targets="_all")
          ),
          order = list(list(7, "asc"))  # مرتب بر اساس نمره ترکیبی
        ),
        rownames  = FALSE,
        class     = "cell-border stripe hover",
        selection = "none"
      ) %>%
        DT::formatStyle(
          "رتبه",
          fontWeight = "900",
          fontSize   = "14px"
        ) %>%
        DT::formatStyle(
          "مدل",
          fontWeight = "700",
          color      = "#e2e8f0"
        ) %>%
        DT::formatStyle(
          "R²",
          color = DT::styleInterval(c(0.7, 0.85, 0.92),
                                    c("#ef4444","#f59e0b","#3b82f6","#22c55e")),
          fontWeight = "700"
        ) %>%
        DT::formatStyle(
          "نمره ترکیبی",
          background         = DT::styleColorBar(c(0, 1), "#3b82f6"),
          backgroundSize     = "100% 65%",
          backgroundRepeat   = "no-repeat",
          backgroundPosition = "center",
          fontWeight         = "700",
          color = DT::styleInterval(c(0.2, 0.4, 0.6),
                                    c("#22c55e","#3b82f6","#f59e0b","#ef4444"))
        ) %>%
        DT::formatStyle(
          "RMSE",
          color = DT::styleInterval(
            quantile(res$RMSE, c(0.33, 0.66), na.rm=TRUE),
            c("#22c55e","#f59e0b","#ef4444")
          )
        )
    })
    
    # ── نمودار میله‌ای ────────────────────────────────────────────────────
    output$metric_bar_chart <- plotly::renderPlotly({
      req(benchmark_results(), input$metric_to_plot)
      res    <- benchmark_results()
      metric <- input$metric_to_plot
      ascending <- metric != "R2"
      
      res_sorted <- if (ascending)
        dplyr::arrange(res, dplyr::desc(.data[[metric]]))
      else
        dplyr::arrange(res, .data[[metric]])
      
      res_sorted <- dplyr::mutate(res_sorted,
                                  model_label = MODEL_LABELS[model] %||% model,
                                  bar_color   = MODEL_COLORS[model] %||% "#64748b"
      )
      
      plotly::plot_ly(
        res_sorted,
        x      = ~reorder(model_label, if (ascending) -.data[[metric]] else .data[[metric]]),
        y      = as.formula(paste0("~`", metric, "`")),
        type   = "bar",
        marker = list(
          color  = ~bar_color,
          line   = list(color="rgba(0,0,0,0)", width=0),
          opacity = 0.85
        ),
        text          = as.formula(paste0("~round(`", metric, "`, 3)")),
        textposition  = "outside",
        textfont      = list(color="#94a3b8", size=10, family="Vazirmatn"),
        hovertemplate = paste0("<b>%{x}</b><br>", metric, ": %{y:.3f}<extra></extra>")
      ) %>%
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font   = list(family="Vazirmatn,Tahoma", color="#94a3b8", size=11),
          xaxis  = list(title="", gridcolor="transparent",
                        tickfont=list(size=10, color="#94a3b8")),
          yaxis  = list(title=metric, gridcolor="rgba(99,143,232,0.06)",
                        tickfont=list(size=10, color="#64748b")),
          bargap = 0.3,
          margin = list(l=50, r=15, t=30, b=50),
          showlegend = FALSE
        ) %>%
        plotly::config(displayModeBar=FALSE)
    })
    
    # ── نمودار رادار (۳ مدل برتر) ───────────────────────────────────────
    output$radar_chart <- plotly::renderPlotly({
      req(benchmark_results())
      res <- benchmark_results()
      
      # ۳ مدل برتر
      top3 <- head(res, 3)
      if (nrow(top3) == 0) return(plotly::plot_ly() %>% plotly::layout(title="داده‌ای وجود ندارد"))
      
      # نرمال‌سازی ۰–۱ (R² برعکس)
      normalize_metric <- function(x, higher_better=FALSE) {
        mn <- min(x, na.rm=TRUE); mx <- max(x, na.rm=TRUE)
        if (is.na(mn) || mn == mx) return(rep(0.5, length(x)))
        v <- (x - mn) / (mx - mn)
        if (!higher_better) 1 - v else v
      }
      
      all_rmse  <- normalize_metric(res$RMSE,  FALSE)
      all_mae   <- normalize_metric(res$MAE,   FALSE)
      all_mape  <- normalize_metric(res$MAPE,  FALSE)
      all_r2    <- normalize_metric(res$R2,    TRUE)
      all_smape <- normalize_metric(res$SMAPE, FALSE)
      
      cats <- c("RMSE","MAE","MAPE","R²","SMAPE","RMSE")  # بسته شدن چندضلعی
      p <- plotly::plot_ly(type="scatterpolar", mode="lines+markers")
      
      top3_colors <- c("#3b82f6","#22c55e","#f59e0b")
      for (i in seq_len(nrow(top3))) {
        idx   <- which(res$model == top3$model[i])
        vals  <- c(all_rmse[idx], all_mae[idx], all_mape[idx],
                   all_r2[idx], all_smape[idx], all_rmse[idx])
        label <- MODEL_LABELS[top3$model[i]] %||% top3$model[i]
        col   <- top3_colors[i]
        
        p <- plotly::add_trace(p,
                               r    = vals,
                               theta = cats,
                               name = label,
                               line = list(color=col, width=2),
                               marker = list(color=col, size=6),
                               fill  = "toself",
                               fillcolor = paste0(substr(col,1,7), "22")
        )
      }
      
      p %>% plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        polar = list(
          bgcolor = "rgba(0,0,0,0)",
          radialaxis = list(
            visible = TRUE, range=c(0,1),
            gridcolor="rgba(99,143,232,0.12)",
            linecolor="rgba(99,143,232,0.12)",
            tickfont=list(size=9, color="#64748b"),
            tickvals=c(0.25,0.5,0.75,1)
          ),
          angularaxis = list(
            tickfont=list(size=11, color="#94a3b8", family="Vazirmatn"),
            linecolor="rgba(99,143,232,0.2)",
            gridcolor="rgba(99,143,232,0.08)"
          )
        ),
        legend = list(font=list(size=11, family="Vazirmatn", color="#94a3b8"),
                      bgcolor="rgba(0,0,0,0)"),
        margin = list(l=30, r=30, t=20, b=20)
      ) %>%
        plotly::config(displayModeBar=FALSE)
    })
    
    # ── دانلود CSV ────────────────────────────────────────────────────────
    output$download_csv <- downloadHandler(
      filename = function() paste0("leaderboard_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content  = function(file) {
        req(benchmark_results())
        res <- benchmark_results() %>%
          dplyr::mutate(model = MODEL_LABELS[model] %||% model)
        utils::write.csv(res, file, row.names=FALSE, fileEncoding="UTF-8")
      }
    )
    
    return(benchmark_results)
  })
}