# ── Server ────────────────────────────────────────────────────────────────────
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
          
          model_names <- c("arima","sarima","ets","tbats","prophet","rf","xgboost","lightgbm","catboost","svm","naive")
          
          all_metrics <- list()
          all_speed <- list()
          
          # ماتریسی برای ذخیره خطای هر مدل در ۳ پنجره زمانی
          window_rmses <- matrix(NA_real_, nrow=length(model_names), ncol=3)
          rownames(window_rmses) <- model_names
          
          window_size <- floor(test_h / 3)
          
          for (i in seq_along(model_names)) {
            mn <- model_names[i]
            setProgress((i-1)/length(model_names), paste("آموزش:", MODEL_LABELS[[mn]]))
            
            t_start <- Sys.time()
            fc <- tryCatch(run_model_by_name(mn, train_df, test_h, target), error = function(e) NULL)
            t_end <- Sys.time()
            exec_time <- as.numeric(difftime(t_end, t_start, units = "secs"))
            
            if (!is.null(fc) && !is.null(fc$predictions)) {
              preds <- fc$predictions[seq_len(min(test_h, length(fc$predictions)))]
              actual <- test_vals[seq_len(length(preds))]
              
              m <- compute_all_metrics(actual, preds, model_name=mn)
              all_metrics[[mn]] <- m
              all_speed[[mn]] <- tibble::tibble(model_id = mn, exec_time = round(exec_time, 3))
              
              # محاسبه RMSE در ۳ پنجره زمانی مختلف
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
          
          # ── محاسبه صحیح تحلیل پایداری ──
          # رتبه‌بندی مدل‌ها در مقابل یکدیگر در هر پنجره (رتبه ۱ = بهترین)
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
    
    # ── ۵. Regional Heatmap Server ──
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
            fc <- tryCatch(run_model_by_name(mn, splits$train, nrow(splits$test), target), error=function(e) NULL)
            if(!is.null(fc)) {
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
          font = list(family="Vazirmatn", color="#94a3b8", size=11),
          margin = list(l=100, r=20, t=10, b=50)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # ── خروجی زمان اجرا ──
    output$bench_time_val <- renderText({
      req(benchmark_data())
      paste0(benchmark_data()$info$exec_time, " ثانیه")
    })
    
    # ── خروجی آمار بالای جدول ──
    output$lb_stats_ui <- renderUI({
      req(benchmark_data())
      info <- benchmark_data()$info
      tags$div(class="lb-stats",
               tags$div(class="lb-stat-item", tags$div(class="lb-stat-val", info$n_models), tags$div(class="lb-stat-lbl", "مدل ارزیابی شد")),
               tags$div(class="lb-stat-item", tags$div(class="lb-stat-val", info$station), tags$div(class="lb-stat-lbl", "ایستگاه")),
               tags$div(class="lb-stat-item", tags$div(class="lb-stat-val", info$target), tags$div(class="lb-stat-lbl", "متغیر هدف")),
               tags$div(class="lb-stat-item", tags$div(class="lb-stat-val", paste0(info$exec_time, "s")), tags$div(class="lb-stat-lbl", "زمان کل"))
      )
    })
    
    # ── جدول رتبه‌بندی اصلی ──
    output$leaderboard_table <- DT::renderDT({
      req(benchmark_data())
      res <- benchmark_data()$metrics
      rank_icons <- c("🥇","🥈","🥉", rep("",nrow(res)-3))
      if (nrow(res) < 3) rank_icons <- c("🥇","🥈")[seq_len(nrow(res))]
      
      display <- res %>%
        dplyr::mutate(model_label = MODEL_LABELS[model] %||% model, rank_label = paste(rank_icons[rank], rank)) %>%
        dplyr::select("رتبه"=rank_label, "مدل"=model_label, "RMSE"=RMSE, "MAE"=MAE, "R²"=R2, "نمره ترکیبی"=composite_score) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))
      
      DT::datatable(display, options = list(pageLength = 10, dom = "t", scrollX = TRUE, order = list(list(5, "asc")), columnDefs = list(list(className = "dt-center", targets = "_all"))), rownames = FALSE, class = "cell-border stripe hover") %>%
        DT::formatStyle("نمره ترکیبی", background = DT::styleColorBar(c(0, 1), "#3b82f6"), backgroundSize = "100% 65%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
    })
    
    # ── ۲. نمودار میله‌ای ──
    output$metric_bar_chart <- plotly::renderPlotly({
      req(benchmark_data(), input$metric_to_plot)
      res <- benchmark_data()$metrics
      metric <- input$metric_to_plot
      ascending <- metric != "R2"
      res_sorted <- if (ascending) dplyr::arrange(res, dplyr::desc(.data[[metric]])) else dplyr::arrange(res, .data[[metric]])
      res_sorted <- dplyr::mutate(res_sorted, model_label = MODEL_LABELS[model] %||% model, bar_color = MODEL_COLORS[model] %||% "#64748b", val = round(as.numeric(.data[[metric]]), 3))
      
      plotly::plot_ly(res_sorted, x = ~model_label, y = ~val, type = "bar", marker = list(color = ~bar_color, opacity = 0.85), hovertemplate = paste0("<b>%{x}</b><br>", metric, ": %{y}<extra></extra>")) %>%
        plotly::layout(paper_bgcolor = "transparent", plot_bgcolor  = "transparent", font = list(family="Vazirmatn,Tahoma", color="#94a3b8", size=11), xaxis = list(title="", gridcolor="transparent"), yaxis = list(title=metric, gridcolor="rgba(99,143,232,0.06)"), margin = list(l=50, r=15, t=10, b=50), showlegend = FALSE) %>%
        plotly::config(displayModeBar=FALSE)
    })
    
    # ── ۲. نمودار Trade-off ──
    output$tradeoff_scatter <- plotly::renderPlotly({
      req(benchmark_data())
      metrics_df <- benchmark_data()$metrics
      speed_df <- benchmark_data()$speed
      merged <- dplyr::inner_join(metrics_df, speed_df, by = c("model" = "model_id"))
      merged$model_label <- sapply(merged$model, function(m) MODEL_LABELS[[m]])
      merged$type <- ifelse(merged$model %in% ML_MODELS, "ML", "Statistical")
      
      plotly::plot_ly(merged, x = ~exec_time, y = ~RMSE, type = "scatter", mode = "markers", color = ~type, colors = c("ML" = "#f59e0b", "Statistical" = "#3b82f6"), marker = list(size = 15, opacity = 0.7, line = list(width = 1, color = "rgba(255,255,255,0.5)")), text = ~model_label, hovertemplate = "<b>%{text}</b><br>Time: %{x}s<br>RMSE: %{y}<extra></extra>") %>%
        plotly::layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent", font = list(family="Vazirmatn", color="#94a3b8"), xaxis = list(title = "زمان اجرا (ثانیه)", gridcolor = "rgba(99,143,232,0.1)"), yaxis = list(title = "RMSE", gridcolor = "rgba(99,143,232,0.1)"), legend = list(orientation = "h", y = -0.2)) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # ── ۳. Stability UI & Plot ──
    output$stability_cards_ui <- renderUI({
      req(benchmark_data())
      stab <- benchmark_data()$stability
      best <- stab[order(-stab$stability_score), ][1, ]
      best_name <- MODEL_LABELS[[best$model_id]]
      
      tags$div(
        tags$div(style="text-align:center; margin-bottom:10px; font-size:12px; color:#22c55e; font-weight:700;", 
                 paste0("Most Stable Model: ", best_name)),
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
      
      plotly::plot_ly(stab, x = ~model_label, y = ~stability_score, type = "bar", marker = list(color = "#22c55e", opacity = 0.8), hovertemplate = "<b>%{x}</b><br>Score: %{y}<extra></extra>") %>%
        plotly::layout(paper_bgcolor = "transparent", plot_bgcolor  = "transparent", font = list(family="Vazirmatn", color="#94a3b8", size=10), xaxis = list(title="", gridcolor="transparent"), yaxis = list(title="Score", gridcolor="rgba(99,143,232,0.06)"), margin = list(l=40, r=10, t=10, b=40)) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # ── ۴. Speed Benchmark UI (Progress Bars) ──
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
                 tags$div(class="speed-label", tags$span(row$model_label), tags$span(paste0(row$exec_time, "s"))),
                 tags$div(class="speed-track", tags$div(class="speed-fill", style=paste0("width:", width_pct, "%; background:", color, ";")))
        )
      })
      do.call(tagList, bars)
    })
    
    # ── ۷. کارت توصیه‌گر هوشمند ⭐ ──
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
        tags$div(class = "reco-header", tags$i(class="fa fa-award"), "🏆 Recommended Model"),
        tags$div(class = "reco-model", best_acc_name),
        tags$div(class = "reco-score", paste0("Overall Score: ", score, " / 10")),
        tags$div(class = "reco-why",
                 tags$div(tags$i(class="fa fa-check"), paste0("Lowest Error (RMSE: ", round(best_acc$RMSE, 2), ")")),
                 tags$div(tags$i(class="fa fa-check"), paste0("High R² (", round(best_acc$R2, 3), ")")),
                 tags$div(tags$i(class="fa fa-check"), "Best Composite Score")
        ),
        tags$div(class = "reco-alt",
                 tags$strong("Alternatives:"),
                 tags$br(),
                 paste0("👉 If speed is priority: ", best_speed_name, " (", round(best_speed$exec_time, 3), "s)"),
                 tags$br(),
                 paste0("👉 If accuracy is priority: ", best_acc_name)
        )
      )
    })
    
    return(benchmark_data)
  })
}