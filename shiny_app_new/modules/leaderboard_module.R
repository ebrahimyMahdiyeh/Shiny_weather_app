# File: modules/leaderboard_module.R  (نسخه کامل با UI)
# ماژول رتبه‌بندی مدل‌ها — UI کامل + Server

# ════════════════════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════════════════════
leaderboardUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(class="lb-wrapper",
             tags$style(HTML("
        .lb-wrapper {
          font-size: 18px; /* فونت پایه کلی به شدت افزایش یافت */
          color: #cbd5e1;
        }
        .lb-wrapper .box-title {
          font-size: 20px !important;
          font-weight: 800 !important;
        }
        .lb-wrapper .form-control, .lb-wrapper .selectize-input {
          font-size: 16px !important;
        }
        .lb-wrapper label {
          font-size: 16px !important;
          font-weight: 600;
        }
        
        /* ─── افزایش شدید سایز فونت جداول ─── */
        .lb-wrapper table.dataTable {
          font-size: 17px !important;
        }
        .lb-wrapper table.dataTable thead th {
          font-size: 18px !important;
          font-weight: 800 !important;
          color: #e2e8f0 !important;
          border-bottom: 2px solid rgba(99,143,232,0.3) !important;
        }
        .lb-wrapper table.dataTable tbody td {
          padding: 12px 8px !important;
        }

        .lb-hero{
          background:linear-gradient(135deg,#0d1b35 0%,#0f2347 55%,#0a1628 100%);
          border:1px solid rgba(99,143,232,.15);
          border-radius:16px;
          padding:28px 32px;
          margin-bottom:16px;
          position:relative;
          overflow:hidden;
        }
        .lb-hero::before{
          content:'';position:absolute;top:0;left:0;width:5px;height:100%;background:linear-gradient(to bottom,#3b82f6,#22c55e);
        }
        .lb-hero h2{margin:0 0 10px;color:#f1f5f9;font-size:28px;font-weight:800;}
        .lb-hero p{margin:0 0 24px;color:#94a3b8;font-size:16px;line-height:1.8;max-width:85%;}
        .lb-hero .badge{
          display:inline-flex;align-items:center;gap:6px;
          background:rgba(59,130,246,.1);
          border:1px solid rgba(59,130,246,.25);
          color:#60a5fa;border-radius:20px;
          padding:8px 16px;font-size:14px;font-weight:700;
          margin-bottom:14px;
        }

        .lb-hero-stats{
          display:grid;grid-template-columns:repeat(4,1fr);gap:16px;
          padding-top:20px;
          margin-top:10px;
          border-top:1px solid rgba(99,143,232,.15);
        }
        .lb-hero-stat-item{
          display:flex;align-items:center;gap:14px;
          padding:16px 20px;
          background:rgba(17,24,39,0.6);
          border-radius:12px;
          border:1px solid rgba(99,143,232,.1);
          box-shadow:0 4px 6px rgba(0,0,0,0.1);
        }
        .lb-hero-stat-icon{
          width:50px;height:50px;
          display:flex;align-items:center;justify-content:center;
          border-radius:10px;
          background:linear-gradient(135deg, rgba(59,130,246,0.2), rgba(34,197,94,0.2));
          color:#60a5fa;
          font-size:22px;
        }
        .lb-hero-stat-text{display:flex;flex-direction:column;}
        .lb-hero-stat-val{font-size:22px;font-weight:800;color:#f8fafc;line-height:1.2;}
        .lb-hero-stat-lbl{font-size:14px;color:#94a3b8;margin-top:4px;font-weight:600;}

        .lb-control-card{
          background:#111827;border:1px solid rgba(99,143,232,.15);
          border-radius:10px;padding:14px 16px;
          font-size:16px;
        }

        .reco-card{
          background:linear-gradient(135deg,rgba(34,197,94,.12),rgba(59,130,246,.08));
          border:1px solid rgba(34,197,94,.3);
          border-radius:12px;
          padding:20px;
          box-shadow:0 4px 15px rgba(0,0,0,0.2);
          position:relative;
          overflow:hidden;
        }
        .reco-card::before {
          content:'';position:absolute;top:0;left:0;width:100%;height:5px;
          background:linear-gradient(90deg, #22c55e, #3b82f6);
        }
        .reco-header{
          font-size:16px;font-weight:800;color:#22c55e;
          margin-bottom:14px;display:flex;align-items:center;gap:8px;
          text-transform:uppercase;letter-spacing:0.5px;
        }
        .reco-model{
          font-size:30px;font-weight:800;color:#f8fafc;
          margin-bottom:12px;display:flex;align-items:center;gap:12px;
        }
        .reco-score{
          display:inline-flex;align-items:center;gap:8px;
          font-size:16px;color:#fbbf24;font-weight:700;
          background:rgba(251,191,36,0.1);
          padding:8px 16px;border-radius:20px;
          border:1px solid rgba(251,191,36,0.2);
          margin-bottom:18px;
        }
        .reco-why{
          background:rgba(15,23,42,0.5);
          border-radius:8px;padding:14px 16px;margin-bottom:16px;
        }
        .reco-why div{
          font-size:16px;color:#cbd5e1;margin:10px 0;
          display:flex;align-items:center;gap:10px;
        }
        .reco-why i{color:#22c55e;font-size:16px;width:18px;text-align:center;}
        .reco-alt{
          font-size:15px;color:#94a3b8;
          padding-top:14px;border-top:1px dashed rgba(99,143,232,.2);
        }
        .reco-alt-item {
          display:flex;align-items:center;gap:10px;
          margin-top:10px;background:rgba(30,45,69,0.4);
          padding:12px 14px;border-radius:6px;
          font-size:15px;
        }
        .reco-alt-item i { font-size:16px; }

        .stability-grid{
          display:grid;grid-template-columns:repeat(4,1fr);gap:8px;margin-top:8px;
        }
        .stab-card{
          background:rgba(34,197,94,.05);
          border:1px solid rgba(34,197,94,.15);
          border-radius:6px;padding:10px 12px;text-align:center;
        }
        .stab-val{font-size:20px;font-weight:800;color:#22c55e;}
        .stab-lbl{font-size:12px;color:#64748b;margin-top:2px;}

        .speed-row{margin-bottom:12px;}
        .speed-label{display:flex;justify-content:space-between;font-size:16px;color:#cbd5e1;margin-bottom:5px;font-weight:600;}
        .speed-track{height:10px;background:rgba(99,143,232,.08);border-radius:5px;overflow:hidden;}
        .speed-fill{height:100%;border-radius:5px;transition:width .3s ease;}
      ")),
             
             # ── Hero Banner + Stats (یکپارچه) ────────────────────────────────────────
             fluidRow(
               column(12,
                      tags$div(class="lb-hero",
                               tags$div(class="badge",
                                        tags$i(class="fa fa-trophy"),
                                        "Benchmark · ۱۲ مدل · مقایسه جامع"
                               ),
                               tags$h2("رتبه‌بندی مدل‌های پیش‌بینی"),
                               tags$p(
                                 "ارزیابی جامع ۱۲ مدل پیش‌بینی سری زمانی بر اساس ۵ معیار آماری",
                                 " (RMSE، MAE، MAPE، SMAPE، R²) و نمره ترکیبی.",
                                 " شامل تحلیل پایداری، زمان اجرا، توصیه‌گر هوشمند و Heatmap منطقه‌ای."
                               ),
                               uiOutput(ns("lb_stats_ui"))
                      )
               )
             ),
             
             # ── پنل کنترل (تنظیمات) + توصیه‌گر هوشمند ─────────────────────────────────────
             fluidRow(
               column(3,
                      shinydashboard::box(
                        title = tags$span(
                          tags$i(class="fa fa-sliders", style="margin-left:6px;color:#60a5fa;"),
                          "تنظیمات Benchmark"
                        ),
                        width = 12, solidHeader = FALSE, status = "primary",
                        
                        selectInput(ns("station"),
                                    label = "ایستگاه:",
                                    choices = NULL
                        ),
                        selectInput(ns("target_var"),
                                    label = "متغیر هدف:",
                                    choices = c(
                                      "دما (°C)"        = "temperature",
                                      "رطوبت (%)"       = "humidity",
                                      "سرعت باد (km/h)" = "wind_speed",
                                      "بارش (mm)"       = "precipitation"
                                    )
                        ),
                        sliderInput(ns("test_ratio"),
                                    label = "نسبت داده آزمون:",
                                    min = 0.10, max = 0.30, value = 0.15, step = 0.05
                        ),
                        hr(),
                        actionButton(ns("run_benchmark"),
                                     label = tagList(tags$i(class="fa fa-play"), " اجرای Benchmark"),
                                     class = "btn-primary btn-block",
                                     style = "background:#3b82f6;border:none;color:white;font-weight:800;padding:14px;font-size:17px;"
                        ),
                        hr(),
                        tags$div(class="lb-control-card",
                                 tags$div(style="font-size:14px;color:#64748b;margin-bottom:6px;",
                                          "زمان کل اجرا:"),
                                 uiOutput(ns("bench_time_val"))
                        )
                      ),
                      
                      # ── کارت توصیه‌گر هوشمند ─────────────────────────────────────────────
                      shinydashboard::box(
                        title = tags$span(
                          tags$i(class="fa fa-award", style="margin-left:6px;color:#22c55e;"),
                          "توصیه‌گر هوشمند"
                        ),
                        width = 12, solidHeader = FALSE, status = "success",
                        shinycssloaders::withSpinner(
                          uiOutput(ns("recommendation_card")),
                          type = 8, color = "#22c55e", size = 0.6
                        )
                      )
               ),
               
               # ── جدول رتبه‌بندی اصلی ──────────────────────────────────────────────────
               column(9,
                      fluidRow(
                        column(12,
                               shinydashboard::box(
                                 title = tags$span(
                                   tags$i(class="fa fa-list-ol", style="margin-left:6px;color:#fbbf24;"),
                                   "جدول رتبه‌بندی اصلی"
                                 ),
                                 width = 12, solidHeader = FALSE, status = "warning",
                                 shinycssloaders::withSpinner(
                                   DT::DTOutput(ns("leaderboard_table")),
                                   type = 8, color = "#f59e0b", size = 0.6
                                 )
                               )
                        )
                      ),
                      
                      # ── نمودار میله‌ای + Trade-off ────────────────────────────────────────
                      fluidRow(
                        column(6,
                               shinydashboard::box(
                                 title = tags$span(
                                   tags$i(class="fa fa-chart-bar", style="margin-left:6px;color:#60a5fa;"),
                                   "مقایسه معیارها"
                                 ),
                                 width = 12, solidHeader = FALSE, status = "primary",
                                 selectInput(ns("metric_to_plot"),
                                             label = "معیار:",
                                             choices = c("RMSE", "MAE", "MAPE", "SMAPE", "R2"),
                                             selected = "RMSE"
                                 ),
                                 shinycssloaders::withSpinner(
                                   plotly::plotlyOutput(ns("metric_bar_chart"), height = "300px"),
                                   type = 8, color = "#3b82f6", size = 0.6
                                 )
                               )
                        ),
                        column(6,
                               shinydashboard::box(
                                 title = tags$span(
                                   tags$i(class="fa fa-arrows-left-right", style="margin-left:6px;color:#a78bfa;"),
                                   "Trade-off: سرعت ↔ دقت"
                                 ),
                                 width = 12, solidHeader = FALSE, status = "primary",
                                 shinycssloaders::withSpinner(
                                   plotly::plotlyOutput(ns("tradeoff_scatter"), height = "344px"),
                                   type = 8, color = "#8b5cf6", size = 0.6
                                 )
                               )
                        )
                      )
               )
             ),
             
             # ── تحلیل پایداری + سرعت ────────────────────────────────────────────────
             fluidRow(
               column(6,
                      shinydashboard::box(
                        title = tags$span(
                          tags$i(class="fa fa-shield-halved", style="margin-left:6px;color:#22c55e;"),
                          "تحلیل پایداری مدل‌ها"
                        ),
                        width = 12, solidHeader = FALSE, status = "success",
                        tags$div(style="font-size:14px;color:#64748b;margin-bottom:10px;",
                                 "رتبه‌بندی مدل‌ها در ۳ پنجره زمانی مختلف — پایداری = عملکرد ثابت در طول زمان"),
                        shinycssloaders::withSpinner(
                          uiOutput(ns("stability_cards_ui")),
                          type = 8, color = "#22c55e", size = 0.6
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput(ns("stability_plot"), height = "280px"),
                          type = 8, color = "#22c55e", size = 0.6
                        )
                      )
               ),
               column(6,
                      shinydashboard::box(
                        title = tags$span(
                          tags$i(class="fa fa-gauge-high", style="margin-left:6px;color:#fbbf24;"),
                          "زمان اجرای مدل‌ها"
                        ),
                        width = 12, solidHeader = FALSE, status = "warning",
                        tags$div(style="font-size:14px;color:#64748b;margin-bottom:10px;",
                                 "مدت زمان آموزش هر مدل (ثانیه) — برای مقاینه سرعت نسبی"),
                        shinycssloaders::withSpinner(
                          uiOutput(ns("speed_bars_ui")),
                          type = 8, color = "#f59e0b", size = 0.6
                        )
                      )
               )
             ),
             
             # ── Heatmap منطقه‌ای ──────────────────────────────────────────────────────
             fluidRow(
               column(12,
                      shinydashboard::box(
                        title = tags$span(
                          tags$i(class="fa fa-map-location-dot", style="margin-left:6px;color:#14b8a6;"),
                          "Heatmap منطقه‌ای — مقایسه RMSE در ایستگاه‌ها"
                        ),
                        width = 12, solidHeader = FALSE, status = "info",
                        tags$div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:12px;",
                                 tags$div(style="font-size:14px;color:#64748b;",
                                          "اجرای Benchmark برای همه ایستگاه‌ها با همه مدل‌ها — مقایسه عملکرد منطقه‌ای"),
                                 actionButton(ns("run_regional"),
                                              label = tagList(tags$i(class="fa fa-play"), " اجرای Heatmap منطقه‌ای"),
                                              class = "btn-info btn-sm",
                                              style = "background:#14b8a6;border:none;color:white;font-weight:700;padding:12px 18px;font-size:15px;"
                                 )
                        ),
                        shinycssloaders::withSpinner(
                          plotly::plotlyOutput(ns("regional_heatmap"), height = "370px"),
                          type = 8, color = "#14b8a6", size = 0.6
                        )
                      )
               )
             )
    )
  )
}

# ════════════════════════════════════════════════════════════════════════════
# Server
# ════════════════════════════════════════════════════════════════════════════
leaderboardServer <- function(id, weather_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    MODEL_COLORS <- c(arima="#3b82f6", sarima="#60a5fa", ets="#14b8a6", tbats="#0d9488", prophet="#8b5cf6", rf="#f59e0b", xgboost="#d97706", lightgbm="#22d3ee", catboost="#fb7185", svm="#ef4444", naive="#64748b")
    MODEL_LABELS <- c(arima="ARIMA", sarima="SARIMA", ets="ETS", tbats="TBATS", prophet="Prophet", rf="Random Forest", xgboost="XGBoost", lightgbm="LightGBM", catboost="CatBoost", svm="SVM", naive="Naïve")
    ML_MODELS <- c("rf", "xgboost", "lightgbm", "catboost", "svm")
    
    observe({
      req(weather_data())
      choices <- purrr::set_names(names(weather_data()), purrr::map_chr(names(weather_data()), ~ STATIONS[[.x]]$name %||% .x))
      updateSelectInput(session, "station", choices = choices)
    })
    
    benchmark_data <- reactiveVal(NULL)
    
    observeEvent(input$run_benchmark, {
      req(input$station, input$target_var, weather_data())
      sid <- input$station
      target <- input$target_var
      req(sid %in% names(weather_data()))
      
      df <- weather_data()[[sid]] %>% dplyr::arrange(date) %>% dplyr::filter(!is.na(.data[[target]]))
      if (nrow(df) < 90) { showNotification("برای تحلیل پایداری حداقل ۹۰ روز داده نیاز است.", type="error"); return() }
      
      withProgress(message="در حال ارزیابی مدل‌ها...", value=0, {
        tryCatch({
          start_time <- Sys.time()
          splits <- train_test_split(df, test_ratio=input$test_ratio)
          train_df <- splits$train
          test_df <- splits$test
          test_vals <- test_df[[target]]
          test_h <- nrow(test_df)
          
          # CatBoost اضافه شد
          model_names <- c("arima","sarima","ets","tbats","prophet","rf","xgboost","lightgbm","catboost","svm","naive")
          
          all_metrics <- list()
          all_speed <- list()
          
          window_rmses <- matrix(NA_real_, nrow=length(model_names), ncol=3)
          rownames(window_rmses) <- model_names
          
          window_size <- floor(test_h / 3)
          
          for (i in seq_along(model_names)) {
            mn <- model_names[i]
            setProgress((i-1)/length(model_names), paste("آموزش:", MODEL_LABELS[[mn]]))
            
            t_start <- Sys.time()
            fc <- tryCatch(
              run_model_by_name(mn, train_df, test_h, target),
              error = function(e) {
                message("خطا در مدل ", mn, ": ", conditionMessage(e))
                NULL
              }
            )
            t_end <- Sys.time()
            exec_time <- as.numeric(difftime(t_end, t_start, units = "secs"))
            
            if (!is.null(fc) && !is.null(fc$predictions) && length(fc$predictions) > 0) {
              preds <- fc$predictions[seq_len(min(test_h, length(fc$predictions)))]
              actual <- test_vals[seq_len(length(preds))]
              
              m <- compute_all_metrics(actual, preds, model_name=mn)
              all_metrics[[mn]] <- m
              all_speed[[mn]] <- tibble::tibble(model_id = mn, exec_time = round(exec_time, 3))
              
              for(w in 1:3) {
                start_idx <- (w-1)*window_size + 1
                end_idx <- if(w==3) test_h else (w*window_size)
                w_actual <- as.numeric(actual[start_idx:end_idx])
                w_preds <- as.numeric(preds[start_idx:end_idx])
                w_rmse <- sqrt(mean((w_actual - w_preds)^2, na.rm=TRUE))
                window_rmses[mn, w] <- w_rmse
              }
            } else {
              all_metrics[[mn]] <- tibble::tibble(model=mn, RMSE=NA_real_, MAE=NA_real_, MAPE=NA_real_, R2=NA_real_, SMAPE=NA_real_)
              all_speed[[mn]] <- tibble::tibble(model_id=mn, exec_time=NA_real_)
            }
          }
          
          combined_metrics <- dplyr::bind_rows(all_metrics)
          ranked <- compute_composite_score(combined_metrics)
          ranked$rank <- seq_len(nrow(ranked))
          speed_df <- dplyr::bind_rows(all_speed)
          
          window_ranks <- apply(window_rmses, 2, function(col) rank(col, na.last="keep", ties.method="average"))
          
          stability_df <- tibble::tibble(
            model_id = rownames(window_ranks),
            mean_rank = rowMeans(window_ranks, na.rm=TRUE),
            rank_sd = apply(window_ranks, 1, sd, na.rm=TRUE),
            wins = rowSums(window_ranks == 1, na.rm=TRUE)
          ) %>%
            dplyr::mutate(
              stability_score = round(pmax(0, pmin(100, 100 - ((mean_rank - 1) * 8) - ifelse(is.na(rank_sd), 0, rank_sd * 10))), 1)
            )
          
          total_time <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)
          
          benchmark_data(list(
            metrics = ranked,
            speed = speed_df,
            stability = stability_df,
            info = list(n_models = length(model_names), test_size = test_h, exec_time = total_time, station = STATIONS[[sid]]$name, target = target)
          ))
          
          setProgress(1, "Benchmark کامل شد!")
          showNotification("Benchmark با موفقیت انجام شد ✓", type="message")
          
        }, error = function(e) {
          showNotification(paste("خطا در Benchmark:", conditionMessage(e)), type="error")
        })
      })
    })
    
    # ── Regional Heatmap ─────────────────────────────────────────────────────
    regional_data <- reactiveVal(NULL)
    observeEvent(input$run_regional, {
      req(weather_data(), input$target_var)
      target <- input$target_var
      stations <- names(weather_data())
      model_names <- c("arima","sarima","ets","tbats","prophet","rf","xgboost","lightgbm","catboost","svm","naive")
      
      withProgress(message="در حال اجرای بنچمارک منطقه‌ای (ایستگاه‌ها)...", value=0, {
        res_matrix <- matrix(NA_real_, nrow=length(model_names), ncol=length(stations))
        rownames(res_matrix) <- sapply(model_names, function(m) MODEL_LABELS[[m]])
        colnames(res_matrix) <- sapply(stations, function(s) STATIONS[[s]]$name)
        
        for(i in seq_along(stations)) {
          sid <- stations[i]
          df <- weather_data()[[sid]] %>% dplyr::filter(!is.na(.data[[target]]))
          if (nrow(df) < 60) next
          splits <- train_test_split(df, test_ratio=0.15)
          
          for(j in seq_along(model_names)) {
            mn <- model_names[j]
            fc <- tryCatch(
              run_model_by_name(mn, splits$train, nrow(splits$test), target),
              error = function(e) NULL
            )
            if(!is.null(fc) && !is.null(fc$predictions) && length(fc$predictions) > 0) {
              preds <- fc$predictions[seq_len(min(nrow(splits$test), length(fc$predictions)))]
              actual <- splits$test[[target]][seq_len(length(preds))]
              m <- compute_all_metrics(actual, preds, model_name=mn)
              res_matrix[j, i] <- m$RMSE
            }
          }
          setProgress(i/length(stations))
        }
        regional_data(res_matrix)
        showNotification("Heatmap منطقه‌ای ساخته شد ✓", type="message")
      })
    })
    
    output$regional_heatmap <- plotly::renderPlotly({
      req(regional_data())
      m <- regional_data()
      plotly::plot_ly(
        x = colnames(m), y = rownames(m),
        z = m, type = "heatmap",
        colorscale = "RdYlGn", reversescale = TRUE,
        hovertemplate = "Model: %{y}<br>Station: %{x}<br>RMSE: %{z:.2f}<extra></extra>"
      ) %>%
        plotly::layout(
          paper_bgcolor = "transparent", plot_bgcolor = "transparent",
          font = list(family="Vazirmatn", color="#94a3b8", size=15),
          margin = list(l=110, r=20, t=10, b=60)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$bench_time_val <- renderUI({
      if (is.null(benchmark_data())) {
        tags$span(style="color:#64748b;font-size:18px;", "—")
      } else {
        tags$span(style="color:#60a5fa;font-size:22px;font-weight:800;",
                  paste0(benchmark_data()$info$exec_time, " ثانیه"))
      }
    })
    
    # ── ساختار جدید کارت‌های آمار با آیکون ────────────────────────────────────
    output$lb_stats_ui <- renderUI({
      if (is.null(benchmark_data())) {
        tags$div(class="lb-hero-stats",
                 tags$div(class="lb-hero-stat-item",
                          tags$div(class="lb-hero-stat-icon", tags$i(class="fa fa-layer-group")),
                          tags$div(class="lb-hero-stat-text", tags$div(class="lb-hero-stat-val", "—"), tags$div(class="lb-hero-stat-lbl", "مدل ارزیابی شد"))
                 ),
                 tags$div(class="lb-hero-stat-item",
                          tags$div(class="lb-hero-stat-icon", tags$i(class="fa fa-map-pin")),
                          tags$div(class="lb-hero-stat-text", tags$div(class="lb-hero-stat-val", "—"), tags$div(class="lb-hero-stat-lbl", "ایستگاه"))
                 ),
                 tags$div(class="lb-hero-stat-item",
                          tags$div(class="lb-hero-stat-icon", tags$i(class="fa fa-bullseye")),
                          tags$div(class="lb-hero-stat-text", tags$div(class="lb-hero-stat-val", "—"), tags$div(class="lb-hero-stat-lbl", "متغیر هدف"))
                 ),
                 tags$div(class="lb-hero-stat-item",
                          tags$div(class="lb-hero-stat-icon", tags$i(class="fa fa-stopwatch")),
                          tags$div(class="lb-hero-stat-text", tags$div(class="lb-hero-stat-val", "—"), tags$div(class="lb-hero-stat-lbl", "زمان کل"))
                 )
        )
      } else {
        info <- benchmark_data()$info
        tags$div(class="lb-hero-stats",
                 tags$div(class="lb-hero-stat-item",
                          tags$div(class="lb-hero-stat-icon", tags$i(class="fa fa-layer-group")),
                          tags$div(class="lb-hero-stat-text", tags$div(class="lb-hero-stat-val", info$n_models), tags$div(class="lb-hero-stat-lbl", "مدل ارزیابی شد"))
                 ),
                 tags$div(class="lb-hero-stat-item",
                          tags$div(class="lb-hero-stat-icon", tags$i(class="fa fa-map-pin")),
                          tags$div(class="lb-hero-stat-text", tags$div(class="lb-hero-stat-val", info$station), tags$div(class="lb-hero-stat-lbl", "ایستگاه"))
                 ),
                 tags$div(class="lb-hero-stat-item",
                          tags$div(class="lb-hero-stat-icon", tags$i(class="fa fa-bullseye")),
                          tags$div(class="lb-hero-stat-text", tags$div(class="lb-hero-stat-val", info$target), tags$div(class="lb-hero-stat-lbl", "متغیر هدف"))
                 ),
                 tags$div(class="lb-hero-stat-item",
                          tags$div(class="lb-hero-stat-icon", tags$i(class="fa fa-stopwatch")),
                          tags$div(class="lb-hero-stat-text", tags$div(class="lb-hero-stat-val", paste0(info$exec_time, "s")), tags$div(class="lb-hero-stat-lbl", "زمان کل"))
                 )
        )
      }
    })
    
    output$leaderboard_table <- DT::renderDT({
      req(benchmark_data())
      res <- benchmark_data()$metrics
      rank_icons <- c("🥇","🥈","🥉", rep("",nrow(res)-3))
      if (nrow(res) < 3) rank_icons <- c("🥇","🥈")[seq_len(nrow(res))]
      
      display <- res %>%
        dplyr::mutate(model_label = MODEL_LABELS[model] %||% model, rank_label = paste(rank_icons[rank], rank)) %>%
        dplyr::select("رتبه"=rank_label, "مدل"=model_label, "RMSE"=RMSE, "MAE"=MAE, "R²"=R2, "نمره ترکیبی"=composite_score) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))
      
      DT::datatable(display,
                    options = list(pageLength = 12, dom = "t", scrollX = TRUE,
                                   order = list(list(5, "asc")),
                                   columnDefs = list(list(className = "dt-center", targets = "_all"))),
                    rownames = FALSE, class = "cell-border stripe hover") %>%
        DT::formatStyle("نمره ترکیبی",
                        background = DT::styleColorBar(c(0, 1), "#3b82f6"),
                        backgroundSize = "100% 70%", backgroundRepeat = "no-repeat",
                        backgroundPosition = "center")
    })
    
    output$metric_bar_chart <- plotly::renderPlotly({
      req(benchmark_data(), input$metric_to_plot)
      res <- benchmark_data()$metrics
      metric <- input$metric_to_plot
      ascending <- metric != "R2"
      res_sorted <- if (ascending) dplyr::arrange(res, dplyr::desc(.data[[metric]])) else dplyr::arrange(res, .data[[metric]])
      res_sorted <- dplyr::mutate(res_sorted,
                                  model_label = MODEL_LABELS[model] %||% model,
                                  bar_color = MODEL_COLORS[model] %||% "#64748b",
                                  val = round(as.numeric(.data[[metric]]), 3))
      
      plotly::plot_ly(res_sorted, x = ~model_label, y = ~val, type = "bar",
                      marker = list(color = ~bar_color, opacity = 0.85),
                      hovertemplate = paste0("<b>%{x}</b><br>", metric, ": %{y}<extra></extra>")) %>%
        plotly::layout(
          paper_bgcolor = "transparent", plot_bgcolor  = "transparent",
          font = list(family="Vazirmatn,Tahoma", color="#94a3b8", size=15),
          xaxis = list(title="", gridcolor="transparent", tickfont=list(size=14, color="#cbd5e1")),
          yaxis = list(title=metric, gridcolor="rgba(99,143,232,0.06)", tickfont=list(size=14, color="#cbd5e1")),
          margin = list(l=60, r=20, t=10, b=60), showlegend = FALSE) %>%
        plotly::config(displayModeBar=FALSE)
    })
    
    output$tradeoff_scatter <- plotly::renderPlotly({
      req(benchmark_data())
      metrics_df <- benchmark_data()$metrics
      speed_df <- benchmark_data()$speed
      merged <- dplyr::inner_join(metrics_df, speed_df, by = c("model" = "model_id"))
      merged$model_label <- sapply(merged$model, function(m) MODEL_LABELS[[m]])
      merged$type <- ifelse(merged$model %in% ML_MODELS, "ML", "Statistical")
      
      plotly::plot_ly(merged, x = ~exec_time, y = ~RMSE, type = "scatter", mode = "markers",
                      color = ~type, colors = c("ML" = "#f59e0b", "Statistical" = "#3b82f6"),
                      marker = list(size = 18, opacity = 0.8,
                                    line = list(width = 1, color = "rgba(255,255,255,0.5)")),
                      text = ~model_label,
                      hovertemplate = "<b>%{text}</b><br>Time: %{x}s<br>RMSE: %{y}<extra></extra>") %>%
        plotly::layout(
          paper_bgcolor = "transparent", plot_bgcolor = "transparent",
          font = list(family="Vazirmatn", color="#94a3b8", size=15),
          xaxis = list(title = "زمان اجرا (ثانیه)", gridcolor = "rgba(99,143,232,0.1)", tickfont=list(size=14, color="#cbd5e1")),
          yaxis = list(title = "RMSE", gridcolor = "rgba(99,143,232,0.1)", tickfont=list(size=14, color="#cbd5e1")),
          legend = list(orientation = "h", y = -0.2, font=list(size=14))) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$stability_cards_ui <- renderUI({
      req(benchmark_data())
      stab <- benchmark_data()$stability
      best <- stab[order(-stab$stability_score), ][1, ]
      best_name <- MODEL_LABELS[[best$model_id]]
      
      tags$div(
        tags$div(style="text-align:center; margin-bottom:12px; font-size:18px; color:#22c55e; font-weight:800;",
                 paste0("پایدارترین مدل: ", best_name)),
        tags$div(class="stability-grid",
                 tags$div(class="stab-card", tags$div(class="stab-val", round(best$mean_rank, 1)), tags$div(class="stab-lbl", "Mean Rank")),
                 tags$div(class="stab-card", tags$div(class="stab-val", round(best$rank_sd, 2)), tags$div(class="stab-lbl", "Rank SD")),
                 tags$div(class="stab-card", tags$div(class="stab-val", best$wins), tags$div(class="stab-lbl", "Wins")),
                 tags$div(class="stab-card", tags$div(class="stab-val", best$stability_score), tags$div(class="stab-lbl", "Stability Score"))
        )
      )
    })
    
    output$stability_plot <- plotly::renderPlotly({
      req(benchmark_data())
      stab <- benchmark_data()$stability
      stab$model_label <- sapply(stab$model_id, function(m) MODEL_LABELS[[m]])
      stab <- stab[order(stab$stability_score, decreasing=TRUE),]
      
      plotly::plot_ly(stab, x = ~model_label, y = ~stability_score, type = "bar",
                      marker = list(color = "#22c55e", opacity = 0.8),
                      hovertemplate = "<b>%{x}</b><br>Score: %{y}<extra></extra>") %>%
        plotly::layout(
          paper_bgcolor = "transparent", plot_bgcolor  = "transparent",
          font = list(family="Vazirmatn", color="#94a3b8", size=14),
          xaxis = list(title="", gridcolor="transparent", tickfont=list(size=13, color="#cbd5e1")),
          yaxis = list(title="Score", gridcolor="rgba(99,143,232,0.06)", tickfont=list(size=13, color="#cbd5e1")),
          margin = list(l=50, r=20, t=10, b=50)) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$speed_bars_ui <- renderUI({
      req(benchmark_data())
      speed <- benchmark_data()$speed
      speed$model_label <- sapply(speed$model_id, function(m) MODEL_LABELS[[m]])
      max_time <- max(speed$exec_time, na.rm = TRUE)
      
      bars <- lapply(seq_len(nrow(speed)), function(i) {
        row <- speed[i,]
        width_pct <- round((row$exec_time / max_time) * 100, 1)
        color <- MODEL_COLORS[[row$model_id]]
        
        tags$div(class="speed-row",
                 tags$div(class="speed-label",
                          tags$span(row$model_label),
                          tags$span(paste0(row$exec_time, "s"))
                 ),
                 tags$div(class="speed-track",
                          tags$div(class="speed-fill",
                                   style=paste0("width:", width_pct, "%; background:", color, ";")
                          )
                 )
        )
      })
      do.call(tagList, bars)
    })
    
    output$recommendation_card <- renderUI({
      req(benchmark_data())
      metrics <- benchmark_data()$metrics
      speed <- benchmark_data()$speed
      
      best_acc <- metrics[1, ]
      best_speed <- speed[order(speed$exec_time), ][1, ]
      
      best_acc_name <- MODEL_LABELS[[best_acc$model]]
      best_speed_name <- MODEL_LABELS[[best_speed$model_id]]
      score <- round((1 - best_acc$composite_score) * 10, 1)
      
      tags$div(
        class = "reco-card",
        tags$div(class = "reco-header",
                 tags$i(class="fa fa-wand-magic-sparkles"), "مدل پیشنهادی سیستم"
        ),
        tags$div(class = "reco-model", 
                 tags$i(class="fa fa-trophy", style="color:#fbbf24;"), 
                 best_acc_name
        ),
        tags$div(class = "reco-score", 
                 tags$i(class="fa fa-star"), 
                 tags$span(paste0("امتیاز کلی: ", score, " از 10"))
        ),
        tags$div(class = "reco-why",
                 tags$div(tags$i(class="fa fa-bullseye"),
                          tags$span(paste0("کمترین میزان خطا (RMSE: ", round(best_acc$RMSE, 2), ")"))
                 ),
                 tags$div(tags$i(class="fa fa-chart-line"),
                          tags$span(paste0("بالاترین ضریب تعیین (R²: ", round(best_acc$R2, 3), ")"))
                 ),
                 tags$div(tags$i(class="fa fa-medal"),
                          tags$span("برترین نمره ترکیبی در میان مدل‌ها")
                 )
        ),
        tags$div(class = "reco-alt",
                 tags$strong(style="color:#e2e8f0; display:block; margin-bottom:8px; font-size:16px;", "مدل‌های جایگزین:"),
                 tags$div(class="reco-alt-item",
                          tags$i(class="fa fa-bolt", style="color:#f59e0b;"),
                          tags$span(paste0("اولویت با سرعت: ", best_speed_name, " (", round(best_speed$exec_time, 3), "s)"))
                 ),
                 tags$div(class="reco-alt-item",
                          tags$i(class="fa fa-microscope", style="color:#3b82f6;"),
                          tags$span(paste0("اولویت با دقت: ", best_acc_name))
                 )
        )
      )
    })
    
    return(benchmark_data)
  })
}