# File: modules/forecast_module.R  (نسخه نهایی پایدار + AutoML Ensemble + ضدعفونی خروجی)
# ─────────────────────────────────────────────────────────────────────────────

# ── Helperهای تبدیل کد و سرعت ──────────────────────────────────────────────
weather_icon <- function(code, is_day = 1) {
  is_night <- (is_day == 0)
  
  if (is.null(code) || is.na(code)) return(list(icon=if(is_night) "🌙" else "☁️", label="ابری"))
  code <- as.integer(code)
  
  if (is_night) {
    if      (code == 0)       list(icon="🌙",  label="صاف (شب)")
    else if (code %in% 1:2)   list(icon="🌙",  label="نیمه ابری (شب)")
    else if (code == 3)       list(icon="☁️",  label="ابری")
    else if (code %in% 45:48) list(icon="🌫️",  label="مه")
    else if (code %in% 51:57) list(icon="🌦️",  label="نم‌نم باران")
    else if (code %in% 61:65) list(icon="🌧️",  label="باران")
    else if (code %in% 71:77) list(icon="🌨️",  label="برف")
    else if (code %in% 80:82) list(icon="🌧️",  label="رگبار")
    else if (code %in% 85:86) list(icon="🌨️",  label="رگبار برف")
    else if (code %in% 95:99) list(icon="⛈️",  label="طوفان")
    else                      list(icon="🌡️",  label="نامشخص")
  } else {
    if      (code == 0)       list(icon="☀️",  label="آفتابی")
    else if (code %in% 1:2)   list(icon="🌤️",  label="نیمه ابری")
    else if (code == 3)       list(icon="☁️",  label="ابری")
    else if (code %in% 45:48) list(icon="🌫️",  label="مه")
    else if (code %in% 51:57) list(icon="🌦️",  label="نم‌نم باران")
    else if (code %in% 61:65) list(icon="🌧️",  label="باران")
    else if (code %in% 71:77) list(icon="🌨️",  label="برف")
    else if (code %in% 80:82) list(icon="🌧️",  label="رگبار")
    else if (code %in% 85:86) list(icon="🌨️",  label="رگبار برف")
    else if (code %in% 95:99) list(icon="⛈️",  label="طوفان")
    else                      list(icon="🌡️",  label="نامشخص")
  }
}

wind_label <- function(kph) {
  if (is.null(kph) || is.na(kph)) return("نامشخص")
  if (kph < 5) "آرام" else if (kph < 20) "ملایم"
  else if (kph < 40) "متوسط" else if (kph < 60) "تند" else "طوفانی"
}

fetch_hourly_forecast <- function(lat, lon, tz = "Asia/Tehran") {
  url <- paste0(
    "https://api.open-meteo.com/v1/forecast",
    "?latitude=", lat, "&longitude=", lon,
    "&hourly=temperature_2m,relative_humidity_2m,apparent_temperature,",
    "wind_speed_10m,precipitation,precipitation_probability,weather_code,surface_pressure,is_day",
    "&daily=temperature_2m_max,temperature_2m_min,precipitation_sum,weather_code,",
    "sunrise,sunset,wind_speed_10m_max,uv_index_max,precipitation_probability_max",
    "&current=temperature_2m,relative_humidity_2m,apparent_temperature,wind_speed_10m,wind_direction_10m,weather_code,precipitation,surface_pressure,is_day,cloud_cover,visibility,dew_point_2m,uv_index",
    "&past_days=2",
    "&forecast_days=8",
    "&timezone=", utils::URLencode(tz, reserved = TRUE)
  )
  tryCatch({
    resp <- httr::GET(url, httr::timeout(15))
    if (httr::http_error(resp)) return(NULL)
    raw <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    
    h <- raw$hourly
    n <- length(h$time)
    
    safe_vec <- function(x, len) {
      if (is.null(x)) return(rep(NA_real_, len))
      as.numeric(x)
    }
    
    hourly_df <- tibble::tibble(
      time        = as.POSIXct(h$time, format = "%Y-%m-%dT%H:%M", tz = tz),
      temp        = safe_vec(h$temperature_2m, n),
      feels_like  = safe_vec(h$apparent_temperature, n),
      humidity    = safe_vec(h$relative_humidity_2m, n),
      wind_speed  = safe_vec(h$wind_speed_10m, n),
      precip      = safe_vec(h$precipitation, n),
      precip_prob = safe_vec(h$precipitation_probability, n),
      w_code      = if(!is.null(h$weather_code)) as.integer(h$weather_code) else rep(NA_integer_, n),
      pressure    = safe_vec(h$surface_pressure, n),
      is_day      = if(!is.null(h$is_day)) as.integer(h$is_day) else rep(1L, n)
    ) %>% dplyr::filter(!is.na(temp))
    
    d <- raw$daily
    n_d <- length(d$time)
    daily_df <- tibble::tibble(
      date        = as.Date(d$time),
      temp_max    = safe_vec(d$temperature_2m_max, n_d),
      temp_min    = safe_vec(d$temperature_2m_min, n_d),
      precip_sum  = safe_vec(d$precipitation_sum, n_d),
      w_code      = if(!is.null(d$weather_code)) as.integer(d$weather_code) else rep(NA_integer_, n_d),
      sunrise     = d$sunrise,
      sunset      = d$sunset,
      wind_max    = safe_vec(d$wind_speed_10m_max, n_d),
      uv_index    = safe_vec(d$uv_index_max, n_d),
      precip_prob = safe_vec(d$precipitation_probability_max, n_d)
    )
    
    cur <- raw$current
    current <- list(
      temp       = safe_vec(cur$temperature_2m, 1)[1],
      feels_like = safe_vec(cur$apparent_temperature, 1)[1],
      humidity   = safe_vec(cur$relative_humidity_2m, 1)[1],
      wind_speed = safe_vec(cur$wind_speed_10m, 1)[1],
      wind_dir   = safe_vec(cur$wind_direction_10m, 1)[1],
      w_code     = if(!is.null(cur$weather_code)) as.integer(cur$weather_code)[1] else NA_integer_,
      precip     = safe_vec(cur$precip, 1)[1],
      pressure   = safe_vec(cur$surface_pressure, 1)[1],
      is_day     = if(!is.null(cur$is_day)) as.integer(cur$is_day)[1] else 1L,
      cloud_cover= safe_vec(cur$cloud_cover, 1)[1],
      visibility = safe_vec(cur$visibility, 1)[1],
      dew_point  = safe_vec(cur$dew_point_2m, 1)[1],
      uv_index   = safe_vec(cur$uv_index, 1)[1],
      time       = as.POSIXct(cur$time, format = "%Y-%m-%dT%H:%M", tz = tz)
    )
    list(hourly = hourly_df, daily = daily_df, current = current, tz = tz)
  }, error = function(e) { message("Open-Meteo error: ", e$message); NULL })
}

# ── تابع کمکی برای پاک‌سازی مقادیر نامعتبر (NaN, Inf) ──
sanitize_vec <- function(v, len = NULL) {
  v <- as.numeric(v)
  if (any(!is.finite(v))) {
    valid <- v[is.finite(v)]
    rep_val <- if (length(valid) > 0) mean(valid, na.rm = TRUE) else 0
    v[!is.finite(v)] <- rep_val
  }
  if (!is.null(len) && length(v) < len) {
    v <- c(v, rep(v[length(v)], len - length(v)))
  }
  v
}

# ── UI ────────────────────────────────────────────────────────────────────────
forecastUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class="fc-wrapper",
             tags$style(HTML("
        .fc-wrapper {
          font-size: 15px;
          color: var(--text2);
          font-family: 'Vazirmatn', sans-serif;
        }
        .fc-wrapper .box-title { font-size: 16px !important; font-weight: 800 !important; }
        .fc-wrapper label { font-size: 14px !important; font-weight: 600; }
        .fc-wrapper .form-control, .fc-wrapper .selectize-input { font-size: 14px !important; }

        .weather-hero{
          background: var(--hero-grad);
          border:1px solid var(--border);
          border-radius:16px;
          padding:24px 28px;
          margin-bottom:16px;
          display:grid;
          grid-template-columns: 1.2fr 1px 2fr; 
          gap:32px; 
          position:relative;
          overflow:hidden;
        }
        .weather-hero::before{
          content:'';position:absolute;top:-100px;left:-100px;width:300px;height:300px;
          background:radial-gradient(circle,rgba(59,130,246,.08) 0%,transparent 70%);
          border-radius:50%;pointer-events:none;
        }
        .hero-divider{width:1px;background:var(--border);height:100%;}
        .hero-main{display:flex;flex-direction:column;justify-content:space-between;z-index:1;}
        .loc-time{display:flex;align-items:center;gap:8px;margin-bottom:12px;}
        .loc-time .city{font-size:16px;font-weight:800;color:var(--text);}
        .loc-time .time{font-size:12px;color:var(--text3);background:var(--input-bg);padding:3px 10px;border-radius:10px;}
        .temp-block{display:flex;align-items:flex-start;gap:12px;margin-bottom:8px;}
        .big-temp{font-size:68px;font-weight:900;line-height:1;color:var(--text);letter-spacing:-3px;}
        .big-temp sup{font-size:24px;font-weight:400;color:var(--text3);vertical-align:super;}
        .big-icon{font-size:56px;line-height:1;margin-top:5px;}
        .cond-feels{display:flex;align-items:center;gap:12px;margin-bottom:15px;}
        .cond{font-size:16px;font-weight:700;color:var(--text);}
        .chip{font-size:13px;font-weight:600;color:var(--text2);background:var(--input-bg);border:1px solid var(--border);padding:4px 12px;border-radius:20px;}
        .hero-trend{margin-top:auto;border-top:1px solid var(--border);padding-top:10px;}
        .trend-label{font-size:11px;font-weight:700;color:var(--text3);text-transform:uppercase;letter-spacing:1px;margin-bottom:4px;}
        
        .hero-metrics{display:flex;flex-direction:column;z-index:1;}
        .metrics-header{font-size:12px;font-weight:800;color:var(--text3);text-transform:uppercase;letter-spacing:1px;margin-bottom:14px;}
        .today-summary{display:flex;gap:18px;margin-bottom:16px;padding-bottom:16px;border-bottom:1px solid var(--border);}
        .today-item{display:flex;align-items:center;gap:6px;font-size:18px;font-weight:700;color:var(--text);}
        .today-item i{font-size:14px;}
        
        .metric-grid{ display:grid; grid-template-columns:repeat(4, 1fr); gap:18px; }
        .metric-item{display:flex;flex-direction:column;gap:5px;}
        .m-icon-lbl{display:flex;align-items:center;gap:6px;}
        .m-icon{font-size:14px;color:var(--blue);width:16px;text-align:center;}
        .m-lbl{font-size:12px;font-weight:700;color:var(--text3);text-transform:uppercase;letter-spacing:.5px;}
        .m-val{font-size:18px;font-weight:800;color:var(--text);}
        
        @media (max-width: 1100px) {
          .weather-hero { grid-template-columns: 1fr; }
          .hero-divider { display:none; }
        }

        .hourly-strip{display:flex;gap:8px;overflow-x:auto;padding:8px 0 4px;scrollbar-width:thin;scrollbar-color:rgba(99,143,232,.15) transparent;direction:rtl;}
        .hourly-strip::-webkit-scrollbar{height:4px;}
        .hourly-strip::-webkit-scrollbar-thumb{background:rgba(99,143,232,.18);border-radius:2px;}
        .hour-card{flex-shrink:0;background:var(--input-bg);border:1px solid var(--border);border-radius:10px;padding:10px 12px;text-align:center;min-width:65px;transition:all .15s;cursor:default;}
        .hour-card:hover{background:var(--hover-bg);border-color:var(--border2);}
        .hour-card.now-hour{background:rgba(59,130,246,.13);border-color:rgba(59,130,246,.38);}
        .hour-time{font-size:13px;color:var(--text2);font-weight:600;margin-bottom:4px;}
        .hour-icon{font-size:22px;margin:3px 0;}
        .hour-temp{font-size:18px;font-weight:800;color:var(--text);}
        .hour-prob{font-size:11px;color:#60a5fa;margin-top:3px;font-weight:600;}

        .fc-chart-box{background:var(--panel);border:1px solid var(--border);border-radius:12px;padding:16px 18px;margin-bottom:16px;}
        .fc-section-lbl{font-size:13px;font-weight:800;color:var(--text2);text-transform:uppercase;letter-spacing:1px;display:flex;align-items:center;gap:7px;margin-bottom:12px;}
        .fc-section-lbl::after{content:'';flex:1;height:1px;background:var(--border);}

        .week-mini-grid{display:grid;grid-template-columns:repeat(7,1fr);gap:8px;direction:rtl;}
        .wm-card{background:var(--input-bg);border:1px solid var(--border);border-radius:10px;padding:12px 6px;text-align:center;cursor:pointer;transition:all .15s;position:relative;overflow:hidden;}
        .wm-card:hover{border-color:var(--border2);background:var(--hover-bg);}
        .wm-card.sel{background:var(--active-bg);border-color:var(--blue);}
        .wm-card.today-col::before{content:'';position:absolute;top:0;left:0;right:0;height:2px;background:linear-gradient(90deg,#3b82f6,#14b8a6);}
        .wm-name{font-size:12px;font-weight:800;color:var(--text2);text-transform:uppercase;letter-spacing:.6px;margin-bottom:2px;}
        .wm-date{font-size:11px;color:var(--text3);margin-bottom:6px;}
        .wm-icon{font-size:24px;margin:4px 0;}
        .wm-max{font-size:18px;font-weight:900;color:var(--text);}
        .wm-min{font-size:14px;color:var(--text2);margin-top:2px;font-weight:600;}
        .wm-rain{font-size:11px;color:#60a5fa;font-weight:600;display:flex;align-items:center;justify-content:center;gap:2px;margin-top:4px;}
        .wm-bar{height:4px;border-radius:2px;margin:4px auto;width:75%;background:var(--border);overflow:hidden;position:relative;}
        .wm-bar-fill{height:100%;border-radius:2px;position:absolute;background:linear-gradient(90deg,#3b82f6,#f59e0b);}

        .fc-ctrl-box{background:var(--panel);border:1px solid var(--border);border-radius:12px;padding:14px;margin-bottom:12px;}
        .fc-ctrl-lbl{font-size:13px;font-weight:800;color:var(--text2);text-transform:uppercase;letter-spacing:1px;margin-bottom:6px;display:block;}
        
        .mv-box{margin-top:10px;background:rgba(34,211,238,.05);border:1px solid rgba(34,211,238,.2);border-radius:8px;padding:10px;}
        .mv-box .form-group{margin-bottom:0;}
        .mv-box .checkbox{margin:0; padding:0; text-align:center;}
        .mv-box .checkbox label{
          display:flex; flex-direction:row-reverse; justify-content:center; align-items:center;
          gap:8px; padding:0; font-size:13px; font-weight:600; color:var(--text2); cursor:pointer;
        }
        .mv-box .checkbox input[type='checkbox']{margin:0; position:static; cursor:pointer; width:16px; height:16px; accent-color:#22d3ee;}
        .mv-box.compare-box{background:rgba(34,197,94,.05);border:1px solid rgba(34,197,94,.2);}
        .mv-box.compare-box .checkbox input[type='checkbox']{accent-color:#22c55e;}
        .mv-box-disabled{margin-top:10px;background:var(--input-bg);border:1px solid var(--border);border-radius:8px;padding:10px;text-align:center;font-size:12px;color:var(--text3);display:flex;align-items:center;justify-content:center;gap:6px;}
        
        .eval-container{margin-top:12px;background:var(--panel2);border:1px solid var(--border);border-radius:8px;padding:14px;}
        .eval-header{font-size:14px;font-weight:800;color:var(--text);margin-bottom:12px;display:flex;align-items:center;gap:6px;}
        .eval-header i{color:#22c55e;}
        .eval-table{width:100%;border-collapse:collapse;}
        .eval-table th{text-align:center;color:var(--text2);font-size:13px;font-weight:700;padding:8px 4px;border-bottom:2px solid var(--border);}
        .eval-table th:first-child{text-align:right;}
        .eval-table td{text-align:center;color:var(--text);font-size:15px;font-weight:700;padding:10px 4px;border-bottom:1px solid var(--border);}
        .eval-table td:first-child{text-align:right;color:var(--text);font-weight:800;}
        .eval-footer{display:flex;justify-content:space-between;border-top:1px solid var(--border);padding-top:12px;margin-top:8px;color:var(--text2);font-size:13px;font-weight:600;}
        .eval-footer span{display:flex;align-items:center;gap:5px;}
        
        .om-row{margin-top:8px;padding-top:8px;border-top:1px dashed rgba(34,197,94,0.3);}
        .om-label{font-size:10px;color:#22c55e;font-weight:700;text-align:center;margin-bottom:3px;}
        .daily-comp-box{margin-top:15px;background:var(--panel2);border:1px solid rgba(34,197,94,0.2);border-radius:8px;padding:14px;}
        .daily-comp-header{font-size:14px;font-weight:700;color:#22c55e;margin-bottom:12px;display:flex;align-items:center;gap:6px;}
        .comp-table{width:100%;border-collapse:collapse;}
        .comp-table td{padding:8px 0;font-size:14px;border-bottom: 1px solid var(--border);}
        .comp-table tr:last-child td {border-bottom: none;}
        .comp-table td:first-child{text-align:right;color:var(--text2);}
        .comp-table td:last-child{text-align:center;font-weight:800;color:var(--text);font-size:16px;}
        
        .daily-model-selector { display: flex; gap: 8px; margin-bottom: 16px; flex-wrap: wrap; }
        .daily-pill {
          padding: 8px 16px; border-radius: 20px; font-size: 13px; font-weight: 700;
          cursor: pointer; background: var(--input-bg); border: 1px solid var(--border);
          color: var(--text2); transition: all 0.2s; display: flex; align-items: center; gap: 8px;
        }
        .daily-pill:hover { background: var(--hover-bg); color: var(--text); }
        .daily-pill.active { background: var(--active-bg); border-color: var(--blue); color: var(--text); }
        .daily-pill .pill-dot { width: 10px; height: 10px; border-radius: 50%; }
      ")),
             
             fluidRow(
               column(3,
                      tags$div(class="fc-ctrl-box",
                               tags$span(class="fc-ctrl-lbl", "ایستگاه"),
                               selectInput(ns("station"), label=NULL, choices=NULL, width="100%"),
                               tags$hr(style="border-color:var(--border);margin:12px 0;"),
                               
                               tags$span(class="fc-ctrl-lbl", "انتخاب مدل (امکان مقایسه)"),
                               shinyWidgets::pickerInput(
                                 ns("selected_models"), label = NULL,
                                 choices = c(
                                   "ARIMA"="arima", "SARIMA"="sarima", "ETS"="ets", "TBATS"="tbats",
                                   "Prophet"="prophet", "Random Forest"="rf", "XGBoost"="xgboost",
                                   "LightGBM"="lightgbm", "CatBoost"="catboost", "SVM"="svm", "Naïve"="naive",
                                   "AutoML Ensemble"="ensemble"
                                 ),
                                 selected = "xgboost", multiple = TRUE,
                                 options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 2"),
                                 width = "100%"
                               ),
                               tags$hr(style="border-color:var(--border);margin:12px 0;"),
                               
                               tags$span(class="fc-ctrl-lbl", "متغیر"),
                               selectInput(ns("target_var"), label=NULL,
                                           choices=c("دما (°C)"="temperature","رطوبت (%)"="humidity",
                                                     "سرعت باد (km/h)"="wind_speed","بارش (mm)"="precipitation"),
                                           width="100%"),
                               
                               uiOutput(ns("mv_box_ui")),
                               
                               tags$div(class="mv-box compare-box",
                                        checkboxInput(ns("compare_om"), label = "مقایسه با پیش‌بینی Open-Meteo", value = FALSE, width = "100%")
                               ),
                               
                               actionButton(ns("run_model"),
                                            label=tags$span(tags$i(class="fa fa-play",style="margin-left:5px;"),"اجرای مدل"),
                                            class="btn btn-success btn-block",
                                            style="font-size:14px;font-weight:700;padding:10px;margin-top:12px;"),
                               
                               uiOutput(ns("train_res_ui")),
                               
                               tags$hr(style="border-color:var(--border);margin:12px 0;"),
                               
                               actionButton(ns("refresh"),
                                            label=tags$span(tags$i(class="fa fa-rotate",style="margin-left:5px;"),"بروزرسانی آب‌وهوا"),
                                            class="btn btn-primary btn-block",
                                            style="font-size:13px;font-weight:600;padding:10px;"),
                               tags$div(style="margin-top:6px;text-align:center;font-size:12px;color:var(--text3);",
                                        textOutput(ns("last_update"),inline=TRUE))
                      ),
                      uiOutput(ns("error_box"))
               ),
               
               column(9,
                      uiOutput(ns("now_card")),
                      
                      tags$div(class="fc-chart-box",
                               tags$div(style="display:flex;align-items:center;gap:8px;margin-bottom:12px;",
                                        tags$i(class="fa fa-clock",style="color:#60a5fa;font-size:14px;"),
                                        tags$span(class="fc-section-lbl", style="margin-bottom:0;", "۲۴ ساعت آینده —"),
                                        tags$span(style="font-size:14px;font-weight:800;color:#60a5fa;", textOutput(ns("active_model_lbl"), inline=TRUE))
                               ),
                               uiOutput(ns("hourly_strip")),
                               tags$div(style="margin-top:12px;", uiOutput(ns("dynamic_chart_ui")))
                      ),
                      
                      uiOutput(ns("daily_forecast_section")),
                      uiOutput(ns("feat_imp_box"))
               )
             )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
forecastServer <- function(id, weather_data, hourly_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    deg_to_cardinal <- function(deg) {
      if (is.null(deg) || length(deg) == 0 || is.na(deg)) return("—")
      dirs <- c("شمال","شمال‌شرق","شرق","جنوب‌شرق","جنوب","جنوب‌غرب","غرب","شمال‌غرب")
      ix <- round(as.numeric(deg) / 45) %% 8 + 1
      dirs[ix]
    }
    
    observe({
      req(weather_data())
      choices <- purrr::set_names(names(weather_data()), purrr::map_chr(names(weather_data()), ~ STATIONS[[.x]]$name %||% .x))
      updateSelectInput(session, "station", choices=choices)
    })
    
    live_data <- reactiveVal(NULL)
    do_fetch <- function(sid) {
      st <- STATIONS[[sid]]; if (is.null(st)) return(NULL)
      withProgress(message="دریافت از Open-Meteo...", value=.4, d <- fetch_hourly_forecast(st$lat, st$lon))
      d
    }
    observeEvent(input$station, { req(input$station); live_data(do_fetch(input$station)) }, ignoreInit=FALSE)
    observeEvent(input$refresh, { req(input$station); live_data(do_fetch(input$station)) })
    
    output$last_update <- renderText({
      if (is.null(live_data())) "— دریافت نشده" else paste0("بروز: ", format(Sys.time(), "%H:%M"))
    })
    
    MODEL_META <- list(
      arima=list(label="ARIMA",color="#3b82f6"), sarima=list(label="SARIMA",color="#60a5fa"),
      ets=list(label="ETS",color="#14b8a6"), tbats=list(label="TBATS",color="#0d9488"),
      prophet=list(label="Prophet",color="#8b5cf6"), rf=list(label="Random Forest",color="#f59e0b"),
      xgboost=list(label="XGBoost",color="#d97706"), lightgbm=list(label="LightGBM",color="#22d3ee"),
      catboost=list(label="CatBoost",color="#fb7185"), svm=list(label="SVM",color="#ef4444"),
      naive=list(label="Naïve",color="var(--text3)"),
      ensemble=list(label="AutoML Ensemble",color="#22c55e") 
    )
    ML_MODELS <- c("rf", "xgboost", "lightgbm", "catboost", "svm")
    
    output$active_model_lbl <- renderText({
      models <- input$selected_models
      if (is.null(models) || length(models) == 0) return("مدلی انتخاب نشده")
      if (length(models) == 1) return(MODEL_META[[models[1]]]$label)
      paste0("مقایسه ", length(models), " مدل")
    })
    
    output$mv_box_ui <- renderUI({
      models <- input$selected_models %||% "arima"
      mn <- models[1]
      if (mn %in% ML_MODELS) {
        tags$div(class="mv-box",
                 checkboxInput(ns("use_multivariate"), label = "حالت چندمتغیره (اثر متقابل متغیرها)", value = FALSE, width = "100%"))
      } else {
        tags$div(class="mv-box-disabled", tags$i(class="fa fa-lock", style="font-size:12px;"), tags$span("مدل‌های کلاسیک از حالت چندمتغیره پشتیبانی نمی‌کنند"))
      }
    })
    
    hourly_ml_pred <- reactiveVal(NULL)
    last_error     <- reactiveVal(NULL)
    daily_selected_model <- reactiveVal(NULL)
    feat_imp_selected_model <- reactiveVal(NULL) 
    
    observeEvent(input$daily_model_click, { daily_selected_model(input$daily_model_click) })
    observeEvent(input$feat_imp_model_click, { feat_imp_selected_model(input$feat_imp_model_click) })
    
    run_hourly_pred <- function() {
      sid <- input$station
      if (is.null(sid) || sid == "") {
        sid <- names(STATIONS)[1]
      }
      target <- input$target_var %||% "temperature"
      user_selected_models <- input$selected_models
      
      if (is.null(user_selected_models) || length(user_selected_models) == 0) {
        last_error(list(msg = "حداقل یک مدل را انتخاب کنید.", time = format(Sys.time(), "%H:%M")))
        showNotification("حداقل یک مدل را انتخاب کنید", type="error")
        return()
      }
      
      # ── مدیریت مدل انسمبل ──
      run_ensemble <- "ensemble" %in% user_selected_models
      ENSEMBLE_BASE_MODELS <- c("arima", "ets", "xgboost", "lightgbm")
      
      if (run_ensemble) {
        models_to_run <- unique(c(user_selected_models[user_selected_models != "ensemble"], ENSEMBLE_BASE_MODELS))
      } else {
        models_to_run <- user_selected_models
      }
      
      h_data <- if (!is.null(hourly_data) && is.function(hourly_data)) {
        hourly_data()[[sid]]
      } else if (!is.null(hourly_data) && is.list(hourly_data)) {
        hourly_data[[sid]]
      } else if (exists("WEATHER_DATA_HOURLY")) {
        WEATHER_DATA_HOURLY[[sid]]
      } else NULL
      
      if (is.null(h_data) || nrow(h_data) < 48) {
        msg <- if (is.null(h_data)) paste0("داده ساعتی برای ایستگاه «", sid, "» پیدا نشد.") else paste0("فقط ", nrow(h_data), " ردیف داده ساعتی موجود است — حداقل ۴۸ ردیف لازم است.")
        last_error(list(msg = msg, time = format(Sys.time(), "%H:%M")))
        showNotification(msg, type = "error", duration = 6)
        hourly_ml_pred(NULL)
        return()
      }
      
      tryCatch({
        last_error(NULL)
        ld <- live_data()
        now_check <- lubridate::with_tz(Sys.time(), "Asia/Tehran")
        need_fetch <- is.null(ld)
        if (!is.null(ld) && !is.null(ld$hourly) && nrow(ld$hourly) > 0) {
          live_max <- max(ld$hourly$time, na.rm = TRUE)
          if (as.numeric(difftime(now_check, live_max, units = "hours")) > 3) need_fetch <- TRUE
        }
        if (need_fetch) {
          st <- STATIONS[[sid]]
          if (!is.null(st)) {
            ld_new <- tryCatch(fetch_hourly_forecast(st$lat, st$lon), error = function(e) NULL)
            if (!is.null(ld_new)) { ld <- ld_new; live_data(ld) }
          }
        }
        
        live_hourly <- NULL
        if (!is.null(ld) && !is.null(ld$hourly) && nrow(ld$hourly) > 0) {
          lh <- ld$hourly
          now_filter <- lubridate::with_tz(Sys.time(), "Asia/Tehran")
          lh <- lh %>% dplyr::filter(time <= now_filter)
          if (nrow(lh) > 0) {
            live_hourly <- tibble::tibble(timestamp = lh$time)
            col_map <- c(temp="temperature", humidity="humidity", wind_speed="wind_speed", pressure="pressure", precip="precipitation", w_code="w_code")
            for (live_col in names(col_map)) {
              csv_col <- col_map[[live_col]]
              live_hourly[[csv_col]] <- if (live_col %in% names(lh)) lh[[live_col]] else NA_real_
            }
            live_hourly <- live_hourly %>% dplyr::filter(!is.na(timestamp))
          }
        }
        
        h_data_combined <- h_data
        if (!is.null(live_hourly) && nrow(live_hourly) > 0) {
          h_data_combined <- dplyr::bind_rows(h_data, live_hourly) %>% dplyr::arrange(timestamp) %>% dplyr::filter(!duplicated(timestamp))
        }
        
        now <- lubridate::with_tz(Sys.time(), "Asia/Tehran")
        h_data_combined <- h_data_combined %>% dplyr::filter(timestamp <= now + 3600)
        if (nrow(h_data_combined) < 72) stop("داده معتبری تا همین لحظه وجود ندارد. لطفاً «بروزرسانی آب‌وهوا» را بزنید.")
        
        last_ts <- max(h_data_combined$timestamp)
        now_hour <- as.POSIXct(paste(format(now, "%Y-%m-%d"), format(now, "%H:00:00")), tz = "Asia/Tehran")
        gap_hours <- as.integer(difftime(now_hour, last_ts, units = "hours"))
        if (gap_hours < 0) gap_hours <- 0
        if (gap_hours > 100) { gap_hours <- 0; last_ts <- now_hour }
        
        horizon_24h <- 24 + gap_hours
        horizon_7d  <- 168 + gap_hours
        
        start_time <- Sys.time()
        use_mv <- isTRUE(input$use_multivariate)
        
        # ── ارزیابی مدل‌های بک‌اند ──
        eval_metrics_list <- list()
        eval_preds_list <- list() 
        eval_test <- tail(h_data_combined, 24)
        eval_train <- head(h_data_combined, nrow(h_data_combined) - 24)
        
        for (mn in models_to_run) {
          eval_res <- tryCatch(run_hourly_model(mn, eval_train, 24, target, use_mv), error = function(e) NULL)
          if (!is.null(eval_res) && !is.null(eval_res$predictions)) {
            actual_vals <- eval_test[[target]]
            pred_vals <- eval_res$predictions
            len <- min(length(actual_vals), length(pred_vals))
            actual_vals <- actual_vals[1:len]; pred_vals <- pred_vals[1:len]
            valid <- !is.na(actual_vals) & !is.na(pred_vals)
            
            # ── [FIX] فیلتر کردن مقادیر نامعتبر (NaN, Inf) که باعث خرابی ETS می‌شود ──
            if(sum(valid) > 2) {
              a <- as.numeric(actual_vals[valid]); p <- as.numeric(pred_vals[valid])
              finite_idx <- is.finite(a) & is.finite(p)
              if (sum(finite_idx) > 2) {
                a <- a[finite_idx]
                p <- p[finite_idx]
                
                rmse_val <- sqrt(mean((a - p)^2))
                mae_val <- mean(abs(a - p))
                ss_res <- sum((a - p)^2)
                ss_tot <- sum((a - mean(a))^2)
                r2_val <- ifelse(ss_tot == 0, 0, 1 - (ss_res/ss_tot))
                eval_metrics_list[[mn]] <- list(r2 = r2_val, rmse = rmse_val, mae = mae_val, color = MODEL_META[[mn]]$color)
                eval_preds_list[[mn]] <- p 
              }
            }
          }
        }
        
        # ── پیش‌بینی ۲۴ ساعت و ۷ روز آینده ──
        all_preds_24h <- list()
        all_preds_7d  <- list()
        all_feat_imp  <- list() 
        
        for (mn in models_to_run) {
          res_final <- NULL
          is_7d_success <- FALSE
          
          if (target == "temperature") {
            res_7d <- tryCatch(run_hourly_model(mn, h_data_combined, horizon_h = horizon_7d, target = target, use_multivariate = use_mv), error = function(e) NULL)
            if (!is.null(res_7d) && !is.null(res_7d$predictions) && length(res_7d$predictions) >= horizon_7d) {
              res_final <- res_7d
              is_7d_success <- TRUE
            }
          }
          
          if (is.null(res_final)) {
            res_final <- tryCatch(run_hourly_model(mn, h_data_combined, horizon_h = horizon_24h, target = target, use_multivariate = use_mv), error = function(e) NULL)
          }
          
          if (!is.null(res_final) && !is.null(res_final$predictions) && length(res_final$predictions) >= horizon_24h) {
            idx_24_start <- gap_hours + 1
            idx_24_end <- gap_hours + 24
            
            # ── [FIX] پاک‌سازی مقادیر نامعتبر (NaN, Inf) در پیش‌بینی‌ها ──
            preds_24 <- sanitize_vec(res_final$predictions[idx_24_start:idx_24_end])
            
            lower_24 <- if (!is.null(res_final$lower) && length(res_final$lower) >= idx_24_end) {
              sanitize_vec(res_final$lower[idx_24_start:idx_24_end])
            } else {
              preds_24 - 1.5
            }
            upper_24 <- if (!is.null(res_final$upper) && length(res_final$upper) >= idx_24_end) {
              sanitize_vec(res_final$upper[idx_24_start:idx_24_end])
            } else {
              preds_24 + 1.5
            }
            
            # اطمینان از صحت بازه‌های اطمینان
            lower_24 <- pmin(lower_24, preds_24)
            upper_24 <- pmax(upper_24, preds_24)
            
            all_preds_24h[[mn]] <- list(preds = preds_24, lower = lower_24, upper = upper_24, method = MODEL_META[[mn]]$label, color = MODEL_META[[mn]]$color)
            
            if (is_7d_success) {
              preds_168 <- sanitize_vec(res_7d$predictions[idx_24_start:(gap_hours + 168)])
              
              future_ts_7d <- seq(from = now_hour + 3600, by = 3600, length.out = 168)
              df_agg <- data.frame(date = as.Date(future_ts_7d), val = preds_168)
              daily_agg <- df_agg %>%
                dplyr::group_by(date) %>%
                dplyr::summarise(max_val = max(val, na.rm=TRUE), min_val = min(val, na.rm=TRUE), .groups="drop") %>%
                head(7)
              
              if (any(!is.finite(daily_agg$max_val))) daily_agg$max_val <- sanitize_vec(daily_agg$max_val)
              if (any(!is.finite(daily_agg$min_val))) daily_agg$min_val <- sanitize_vec(daily_agg$min_val)
              
              all_preds_7d[[mn]] <- daily_agg
            }
            
            if (!is.null(res_final$feat_imp)) {
              all_feat_imp[[mn]] <- res_final$feat_imp
            }
          }
        }
        
        # ── محاسبه مدل ترکیبی هوشمند (Smart AutoML Ensemble) ──
        if (run_ensemble) {
          valid_models <- intersect(names(all_preds_24h), ENSEMBLE_BASE_MODELS)
          if (length(valid_models) >= 2) {
            rmses <- sapply(valid_models, function(mn) if(!is.null(eval_metrics_list[[mn]]$rmse)) eval_metrics_list[[mn]]$rmse else 1e-6)
            rmses[is.na(rmses) | rmses == 0] <- 1e-6
            
            min_rmse <- min(rmses)
            strong_models <- names(rmses)[rmses <= (min_rmse * 1.5)]
            if (length(strong_models) >= 2) {
              valid_models <- strong_models
              rmses <- rmses[valid_models]
            }
            
            beta <- 5
            exp_vals <- exp(-beta * rmses)
            weights <- exp_vals / sum(exp_vals)
            
            ens_preds <- rowSums(sapply(valid_models, function(mn) sanitize_vec(all_preds_24h[[mn]]$preds) * weights[mn]))
            ens_lower <- rowSums(sapply(valid_models, function(mn) sanitize_vec(all_preds_24h[[mn]]$lower) * weights[mn]))
            ens_upper <- rowSums(sapply(valid_models, function(mn) sanitize_vec(all_preds_24h[[mn]]$upper) * weights[mn]))
            
            all_preds_24h[["ensemble"]] <- list(
              preds = as.numeric(ens_preds),
              lower = as.numeric(ens_lower),
              upper = as.numeric(ens_upper),
              method = "AutoML Ensemble",
              color = MODEL_META$ensemble$color
            )
            
            valid_models_7d <- intersect(valid_models, names(all_preds_7d))
            if (length(valid_models_7d) >= 2) {
              n_days <- nrow(all_preds_7d[[ valid_models_7d[1] ]])
              ens_max <- rowSums(sapply(valid_models_7d, function(mn) sanitize_vec(all_preds_7d[[mn]]$max_val, n_days) * weights[mn]))
              ens_min <- rowSums(sapply(valid_models_7d, function(mn) sanitize_vec(all_preds_7d[[mn]]$min_val, n_days) * weights[mn]))
              
              all_preds_7d[["ensemble"]] <- tibble::tibble(
                date = all_preds_7d[[ valid_models_7d[1] ]]$date,
                max_val = ens_max,
                min_val = ens_min
              )
            }
            
            valid_eval_models <- intersect(valid_models, names(eval_preds_list))
            if (length(valid_eval_models) >= 2) {
              max_len <- max(sapply(valid_eval_models, function(mn) length(eval_preds_list[[mn]])))
              ens_eval_preds <- rowSums(sapply(valid_eval_models, function(mn) sanitize_vec(eval_preds_list[[mn]], max_len) * weights[mn]))
              
              actual_vals <- eval_test[[target]]
              len <- min(length(actual_vals), length(ens_eval_preds))
              a <- as.numeric(actual_vals[1:len])
              p <- as.numeric(ens_eval_preds[1:len])
              valid_idx <- !is.na(a) & !is.na(p) & is.finite(a) & is.finite(p)
              
              if(sum(valid_idx) > 2) {
                a_v <- a[valid_idx]; p_v <- p[valid_idx]
                rmse_val <- sqrt(mean((a_v - p_v)^2))
                mae_val <- mean(abs(a_v - p_v))
                ss_res <- sum((a_v - p_v)^2)
                ss_tot <- sum((a_v - mean(a_v))^2)
                r2_val <- ifelse(ss_tot == 0, 0, 1 - (ss_res/ss_tot))
                eval_metrics_list[["ensemble"]] <- list(r2 = r2_val, rmse = rmse_val, mae = mae_val, color = MODEL_META$ensemble$color)
              }
            }
            
            valid_feat_models <- intersect(valid_models, names(all_feat_imp))
            if (length(valid_feat_models) > 0) {
              all_features <- unique(unlist(lapply(all_feat_imp[valid_feat_models], function(x) x[[1]])))
              ens_imp <- data.frame(Feature = all_features, Gain = 0, stringsAsFactors = FALSE)
              for (mn in valid_feat_models) {
                w <- weights[mn]
                df <- all_feat_imp[[mn]]
                names(df) <- c("Feature", "Gain")
                df$Gain <- as.numeric(df$Gain) * w
                idx <- match(df$Feature, ens_imp$Feature)
                ens_imp$Gain[idx] <- ens_imp$Gain[idx] + df$Gain
              }
              all_feat_imp[["ensemble"]] <- ens_imp
            }
          }
        }
        
        all_preds_24h <- all_preds_24h[names(all_preds_24h) %in% user_selected_models]
        eval_metrics_list <- eval_metrics_list[names(eval_metrics_list) %in% user_selected_models]
        all_preds_7d <- all_preds_7d[names(all_preds_7d) %in% user_selected_models]
        all_feat_imp <- all_feat_imp[names(all_feat_imp) %in% user_selected_models]
        
        if (length(all_preds_24h) == 0) stop("هیچ‌کدام از مدل‌های انتخاب شده خروجی معتبری تولید نکردند.")
        exec_time <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)
        
        if (length(eval_metrics_list) > 0) {
          for(mn in names(eval_metrics_list)) { eval_metrics_list[[mn]]$time <- exec_time }
        }
        
        om_daily_agg <- NULL
        daily_comp_metrics <- list()
        if (isTRUE(input$compare_om) && target == "temperature" && !is.null(ld) && !is.null(ld$hourly)) {
          now_filter <- lubridate::with_tz(Sys.time(), "Asia/Tehran")
          om_future_hourly <- ld$hourly %>% dplyr::filter(time > now_filter, time <= now_filter + 3600*168)
          if (nrow(om_future_hourly) > 0) {
            om_daily_agg <- om_future_hourly %>%
              dplyr::mutate(date = as.Date(time)) %>%
              dplyr::group_by(date) %>%
              dplyr::summarise(max_val = max(temp, na.rm=TRUE), min_val = min(temp, na.rm=TRUE), .groups="drop") %>%
              head(7)
            
            for (mn in names(all_preds_7d)) {
              ml_d <- all_preds_7d[[mn]]
              merged <- dplyr::inner_join(ml_d, om_daily_agg, by="date", suffix=c("_ml", "_om"))
              if (nrow(merged) > 1) {
                diff_max <- abs(as.numeric(merged$max_val_ml) - as.numeric(merged$max_val_om))
                diff_min <- abs(as.numeric(merged$min_val_ml) - as.numeric(merged$min_val_om))
                all_diffs <- c(diff_max, diff_min)
                
                daily_comp_metrics[[mn]] <- list(
                  avg_diff = round(mean(all_diffs, na.rm=TRUE), 1), 
                  max_diff = round(max(all_diffs, na.rm=TRUE), 1), 
                  min_diff = round(min(all_diffs, na.rm=TRUE), 1), 
                  trend_sim = round(tryCatch(cor(merged$max_val_ml, merged$max_val_om, use="complete.obs"), error=function(e) 0) * 100, 0)
                )
              }
            }
          }
        }
        
        future_ts_24h <- seq(from = now_hour + 3600, by = 3600, length.out = 24)
        
        if (length(all_preds_7d) > 0) {
          daily_selected_model(names(all_preds_7d)[1])
        } else {
          daily_selected_model(NULL)
        }
        
        if (length(all_feat_imp) > 0) {
          feat_imp_selected_model(names(all_feat_imp)[1])
        } else {
          feat_imp_selected_model(NULL)
        }
        
        hourly_ml_pred(list(
          timestamps = future_ts_24h, 
          preds_list = all_preds_24h, 
          feat_imp = all_feat_imp, 
          target = target, 
          hist_df = tail(h_data_combined, 48), 
          metrics = eval_metrics_list,
          daily_preds = all_preds_7d,
          daily_om = om_daily_agg,
          daily_comp_metrics = daily_comp_metrics
        ))
        showNotification(paste0(length(all_preds_24h), " مدل با موفقیت اجرا شد ✓"), type = "message", duration = 3)
      }, error = function(e) {
        msg <- conditionMessage(e)
        message("خطا در پیش‌بینی ساعتی: ", msg)
        last_error(list(msg = msg, time = format(Sys.time(), "%H:%M")))
        showNotification(paste("خطا:", msg), type = "error", duration = 8)
        hourly_ml_pred(NULL)
      })
    }
    
    # ── NOW CARD (Premium UI) ──
    output$now_card <- renderUI({
      ld <- live_data(); sid <- input$station %||% names(STATIONS)[1]
      sname <- STATIONS[[sid]]$name %||% sid
      
      if (is.null(ld)) return(tags$div(class="weather-hero", tags$div(style="text-align:center;padding:40px;color:rgba(255,255,255,.3);", "برای دریافت وضعیت فعلی روی بروزرسانی کلیک کنید")))
      
      cur <- ld$current; wi <- weather_icon(cur$w_code, cur$is_day)
      d0  <- if(nrow(ld$daily)>0) ld$daily[1,] else NULL
      
      tags$div(class="weather-hero",
               tags$div(class="hero-main",
                        tags$div(
                          tags$div(class="loc-time",
                                   tags$span(class="city", sname),
                                   tags$span(class="time", paste("بروزرسانی", format(cur$time, "%H:%M")))
                          ),
                          tags$div(class="temp-block",
                                   tags$div(class="big-temp", round(cur$temp, 0), tags$sup("°C")),
                                   tags$div(class="big-icon", wi$icon)
                          ),
                          tags$div(class="cond-feels",
                                   tags$span(class="cond", wi$label),
                                   tags$span(class="chip", paste0("احساس ", round(cur$feels_like, 0), "°"))
                          )
                        ),
                        tags$div(class="hero-trend",
                                 tags$div(class="trend-label", "روند ۲۴ ساعت گذشته"),
                                 plotly::plotlyOutput(ns("hero_sparkline"), height="40px")
                        )
               ),
               tags$div(class="hero-divider"),
               tags$div(class="hero-metrics",
                        tags$div(class="metrics-header", "شرایط فعلی جو"),
                        tags$div(class="today-summary",
                                 tags$div(class="today-item", tags$i(class="fa fa-arrow-up", style="color:#ef4444"), if(!is.null(d0)) paste0(round(d0$temp_max,0),"°") else "—"),
                                 tags$div(class="today-item", tags$i(class="fa fa-arrow-down", style="color:#3b82f6"), if(!is.null(d0)) paste0(round(d0$temp_min,0),"°") else "—"),
                                 tags$div(class="today-item", tags$i(class="fa fa-droplet", style="color:#60a5fa"), paste0(round(cur$precip_prob%||%0,0), "%"))
                        ),
                        tags$div(class="metric-grid",
                                 tags$div(class="metric-item", tags$div(class="m-icon-lbl", tags$i(class="fa fa-droplet m-icon"), tags$span(class="m-lbl", "رطوبت")), tags$div(class="m-val", paste0(round(cur$humidity,0), " %"))),
                                 tags$div(class="metric-item", tags$div(class="m-icon-lbl", tags$i(class="fa fa-wind m-icon"), tags$span(class="m-lbl", "سرعت باد")), tags$div(class="m-val", paste0(round(cur$wind_speed,0), " km/h"))),
                                 tags$div(class="metric-item", tags$div(class="m-icon-lbl", tags$i(class="fa fa-compass m-icon"), tags$span(class="m-lbl", "جهت باد")), tags$div(class="m-val", deg_to_cardinal(cur$wind_dir))),
                                 tags$div(class="metric-item", tags$div(class="m-icon-lbl", tags$i(class="fa fa-gauge-high m-icon"), tags$span(class="m-lbl", "فشار")), tags$div(class="m-val", paste0(round(cur$pressure,0), " hPa"))),
                                 tags$div(class="metric-item", tags$div(class="m-icon-lbl", tags$i(class="fa fa-sun m-icon"), tags$span(class="m-lbl", "UV Index")), tags$div(class="m-val", round(cur$uv_index,1))),
                                 tags$div(class="metric-item", tags$div(class="m-icon-lbl", tags$i(class="fa fa-cloud m-icon"), tags$span(class="m-lbl", "ابری")), tags$div(class="m-val", paste0(round(cur$cloud_cover,0), " %"))),
                                 tags$div(class="metric-item", tags$div(class="m-icon-lbl", tags$i(class="fa fa-eye m-icon"), tags$span(class="m-lbl", "دید")), tags$div(class="m-val", paste0(round(cur$visibility/1000,1), " km"))),
                                 tags$div(class="metric-item", tags$div(class="m-icon-lbl", tags$i(class="fa fa-temperature-half m-icon"), tags$span(class="m-lbl", "نقطه شبنم")), tags$div(class="m-val", paste0(round(cur$dew_point,0), "°")))
                        )
               )
      )
    })
    
    output$hero_sparkline <- plotly::renderPlotly({
      ld <- live_data()
      req(ld, ld$hourly)
      now <- if (!is.null(ld$current) && !is.null(ld$current$time)) ld$current$time else Sys.time()
      hist <- ld$hourly %>% dplyr::filter(time >= now - 3600*24, time <= now)
      if (nrow(hist) == 0) return(plotly::plot_ly(type="scatter", mode="lines") %>% plotly::layout(xaxis=list(visible=FALSE), yaxis=list(visible=FALSE)) %>% plotly::config(displayModeBar=FALSE))
      
      p <- plotly::plot_ly(x = ~hist$time, y = ~hist$temp, type = 'scatter', mode = 'lines',
                           line = list(color = '#60a5fa', width = 2), hoverinfo = 'none') %>%
        plotly::layout(paper_bgcolor = 'transparent', plot_bgcolor = 'transparent', xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), margin = list(l = 0, r = 0, t = 0, b = 0)) %>%
        plotly::config(displayModeBar = FALSE)
      p
    })
    
    output$hourly_strip <- renderUI({
      ld <- live_data()
      if (is.null(ld) || is.null(ld$hourly)) return(tags$div(style="color:var(--text3);text-align:center;padding:10px;font-size:14px;", "داده‌ای دریافت نشده"))
      now <- if (!is.null(ld$current) && !is.null(ld$current$time)) ld$current$time else Sys.time()
      h24 <- ld$hourly %>% dplyr::filter(time >= now - 3600, time <= now + 3600*24) %>% tail(24)
      if (nrow(h24) == 0) return(NULL)
      cards <- purrr::map(seq_len(nrow(h24)), function(i) {
        r <- h24[i,]; wi <- weather_icon(r$w_code, r$is_day)
        lbl <- if(i==1) "الان" else format(r$time,"%H:%M")
        pr  <- if(!is.na(r$precip_prob)&&r$precip_prob>10) paste0(round(r$precip_prob,0),"%") else ""
        tags$div(class=if(i==1)"hour-card now-hour" else "hour-card", 
                 tags$div(class="hour-time",lbl), 
                 tags$div(class="hour-icon",wi$icon), 
                 tags$div(class="hour-temp",paste0(round(r$temp,0),"°")), 
                 if(nchar(pr)>0) tags$div(class="hour-prob", tags$i(class="fa fa-droplet",style="font-size:10px;margin-left:2px;"),pr))
      })
      tags$div(class="hourly-strip",cards)
    })
    
    output$dynamic_chart_ui <- renderUI({
      models <- input$selected_models
      mlp <- hourly_ml_pred()
      if (is.null(models) || length(models) == 0 || is.null(mlp) || length(mlp$preds_list) == 0) {
        return(tags$div(style="text-align:center; padding:90px; color:var(--text3); font-size:14px;", "روی «اجرای مدل» کلیک کنید تا پیش‌بینی ۲۴h ببینید"))
      }
      plotly::plotlyOutput(ns("main_multimodel_chart"), height="350px")
    })
    
    output$main_multimodel_chart <- plotly::renderPlotly({
      ld  <- live_data()
      mlp <- hourly_ml_pred()
      var <- input$target_var %||% "temperature"
      unit <- c(temperature="°C",humidity="%",wind_speed="km/h",precipitation="mm")[[var]]%||%""
      
      now <- if (!is.null(ld) && !is.null(ld$current) && !is.null(ld$current$time)) ld$current$time else Sys.time()
      x_min <- now - 3600*6
      x_max <- now + 3600*24
      p <- plotly::plot_ly()
      
      if (!is.null(ld)) {
        hist_h <- ld$hourly %>% dplyr::filter(time >= x_min, time <= now) %>%
          dplyr::mutate(y_val = switch(var, temperature=temp, humidity=humidity, wind_speed=wind_speed, precipitation=precip, temp))
        if (nrow(hist_h) > 0)
          p <- plotly::add_trace(p, type="scatter", mode="lines", x=hist_h$time, y=hist_h$y_val,
                                 name="۶h گذشته (واقعی)", line=list(color="#64748b",width=2,dash="dot"),
                                 hovertemplate=paste0("واقعی: %{y:.1f}",unit,"<extra></extra>"))
      }
      
      show_om <- isTRUE(input$compare_om) && !is.null(ld) && !is.null(ld$hourly)
      if (show_om) {
        om_var <- switch(var, temperature="temp", humidity="humidity", wind_speed="wind_speed", precipitation="precip", "temp")
        om_future <- ld$hourly %>% dplyr::filter(time > now, time <= x_max) %>% head(24)
        if (nrow(om_future) > 0)
          p <- plotly::add_trace(p, type="scatter", mode="lines+markers",
                                 x=om_future$time, y=om_future[[om_var]], name="Open-Meteo (API)",
                                 line=list(color="#22c55e", width=2, dash="dash"), marker=list(color="#22c55e", size=6, symbol="x"),
                                 hovertemplate=paste0("API: %{y:.1f}",unit,"<extra></extra>"))
      }
      
      has_pred <- !is.null(mlp) && mlp$target == var && length(mlp$preds_list) > 0
      if (has_pred) {
        if (length(mlp$preds_list) == 1) {
          m_name <- names(mlp$preds_list)[1]
          m_data <- mlp$preds_list[[m_name]]
          
          # ── [FIX] جلوگیری از شکست نمودار در صورت نبود lower/upper معتبر ──
          m_lower <- if (!is.null(m_data$lower) && length(m_data$lower) == 24 && all(is.finite(m_data$lower))) m_data$lower else m_data$preds - 1.5
          m_upper <- if (!is.null(m_data$upper) && length(m_data$upper) == 24 && all(is.finite(m_data$upper))) m_data$upper else m_data$preds + 1.5
          
          p <- p %>%
            plotly::add_trace(type="scatter", mode="lines", x=mlp$timestamps, y=m_lower,
                              line=list(color="transparent"), showlegend=FALSE, hoverinfo="skip") %>%
            plotly::add_trace(type="scatter", mode="lines", x=mlp$timestamps, y=m_upper,
                              fill="tonexty", fillcolor=paste0(substr(m_data$color,1,7),"22"),
                              line=list(color="transparent"), name="بازه ۹۵٪",
                              hovertemplate=paste0("حد بالا: %{y:.1f}",unit,"<extra></extra>"))
        }
        
        for (m_name in names(mlp$preds_list)) {
          m_data <- mlp$preds_list[[m_name]]
          p <- plotly::add_trace(p, type="scatter", mode="lines+markers",
                                 x=mlp$timestamps, y=m_data$preds,
                                 name=m_data$method,
                                 line=list(color=m_data$color, width=3),
                                 marker=list(color=m_data$color, size=6),
                                 hovertemplate=paste0("%{x|%H:%M}<br>%{y:.1f}",unit,"<extra></extra>"))
        }
      }
      
      y_vals <- c(
        if (!is.null(ld)) { ld$hourly %>% dplyr::filter(time >= x_min, time <= now) %>% { switch(var, temperature=.$temp, humidity=.$humidity, wind_speed=.$wind_speed, precipitation=.$precip, .$temp) } } else NA_real_,
        if (has_pred) do.call(c, lapply(mlp$preds_list, function(x) c(x$lower, x$upper, x$preds))) else NA_real_,
        if (show_om) om_future[[om_var]] else NA_real_
      )
      y_vals <- y_vals[is.finite(y_vals)]
      if (length(y_vals) > 0) { y_pad <- max(1, diff(range(y_vals) * 0.15)); y_range <- c(min(y_vals) - y_pad, max(y_vals) + y_pad) } else { y_range <- NULL }
      
      p <- p %>% plotly::layout(
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(family="Vazirmatn,Tahoma",color="#94a3b8",size=13),
        xaxis=list(title="", gridcolor="rgba(99,143,232,.04)", tickfont=list(size=12,color="#94a3b8"),
                   tickformat="%H:%M", range=list(x_min, x_max), type="date", dtick=2*60*60*1000, ticklabelmode="instant"),
        yaxis=list(title=unit, gridcolor="rgba(99,143,232,.05)", tickfont=list(size=12,color="#94a3b8"), ticksuffix=unit, range=y_range),
        legend=list(orientation="h",y=-0.2,x=.5,xanchor="center", font=list(size=12),bgcolor="rgba(0,0,0,0)"),
        hovermode="x unified", margin=list(l=45,r=10,t=5,b=50),
        shapes=list(list(type="line",xref="x",yref="paper", x0=now,x1=now,y0=0,y1=1, line=list(color="rgba(255,255,255,.12)",width=1,dash="dot"))),
        annotations = if (has_pred || show_om) {
          list(list(x=now,y=1,xref="x",yref="paper", text="الان",showarrow=FALSE, font=list(color="rgba(255,255,255,.4)",size=12,family="Vazirmatn"), yanchor="bottom"))
        } else {
          list(list(x=now,y=1,xref="x",yref="paper", text="الان",showarrow=FALSE, font=list(color="rgba(255,255,255,.4)",size=12,family="Vazirmatn"), yanchor="bottom"), list(x=0.5, y=0.5, xref="paper", yref="paper", showarrow=FALSE, text="روی «اجرای مدل» کلیک کنید", font=list(color="#64748b",size=14,family="Vazirmatn,Tahoma")))
        }
      )
      p %>% plotly::config(displayModeBar=FALSE)
    })
    
    # ── Evaluation Metrics Box ──
    output$train_res_ui <- renderUI({
      mlp <- hourly_ml_pred()
      if (is.null(mlp) || is.null(mlp$metrics) || length(mlp$metrics) == 0) return(NULL)
      
      metrics <- mlp$metrics
      time_val <- metrics[[1]]$time
      
      rows <- lapply(names(metrics), function(mn) {
        m <- metrics[[mn]]
        r2_color <- ifelse(m$r2 > 0.85, "#22c55e", ifelse(m$r2 > 0.7, "#fbbf24", "#ef4444"))
        tags$tr(
          tags$td(MODEL_META[[mn]]$label),
          tags$td(style=paste0("color:", r2_color, ";"), round(m$r2, 3)),
          tags$td(round(m$rmse, 2)),
          tags$td(round(m$mae, 2))
        )
      })
      
      tags$div(
        class = "eval-container",
        tags$div(class="eval-header", tags$i(class="fa fa-circle-check"), "نتیجه ارزیابی (۲۴h گذشته)"),
        tags$table(class="eval-table",
                   tags$thead(
                     tags$tr(
                       tags$th("مدل"), tags$th("R²"), tags$th("RMSE"), tags$th("MAE")
                     )
                   ),
                   tags$tbody(rows)
        ),
        tags$div(class="eval-footer",
                 tags$span(tags$i(class="fa fa-bolt", style="color:#fbbf24;"), paste0("زمان اجرا: ", time_val, " ثانیه")),
                 tags$span(tags$i(class="fa fa-clock", style="color:#60a5fa;"), "افق: ۲۴ ساعت")
        )
      )
    })
    
    # ── 7-Day Forecast Section ──
    output$daily_forecast_section <- renderUI({
      req(input$target_var)
      if (input$target_var != "temperature") return(NULL)
      
      tagList(
        tags$div(class="fc-chart-box",
                 tags$div(
                   style="display:flex;align-items:center;gap:8px;margin-bottom:12px;",
                   tags$i(class="fa fa-calendar-week",style="color:#fbbf24;font-size:14px;"),
                   tags$span(class="fc-section-lbl", style="margin-bottom:0;", "پیش‌بینی ۷ روز (مدل ML)")
                 ),
                 uiOutput(ns("daily_model_selector")),
                 uiOutput(ns("week_mini")),
                 uiOutput(ns("daily_comp_metrics_ui"))
        )
      )
    })
    
    output$daily_model_selector <- renderUI({
      mlp <- hourly_ml_pred()
      req(mlp, mlp$daily_preds)
      models <- names(mlp$daily_preds)
      
      current_sel <- daily_selected_model()
      if (is.null(current_sel) || !current_sel %in% models) {
        daily_selected_model(models[1])
        current_sel <- models[1]
      }
      
      pills <- lapply(models, function(mn) {
        is_active <- mn == current_sel
        cls <- if(is_active) "daily-pill active" else "daily-pill"
        color <- MODEL_META[[mn]]$color
        
        tags$div(
          class = cls,
          onclick = paste0("Shiny.setInputValue('", ns("daily_model_click"), "', '", mn, "', {priority:'event'})"),
          tags$div(class="pill-dot", style=paste0("background:", color, ";")),
          MODEL_META[[mn]]$label
        )
      })
      
      tags$div(class = "daily-model-selector", pills)
    })
    
    output$week_mini <- renderUI({
      mlp <- hourly_ml_pred()
      req(mlp, mlp$daily_preds)
      
      sel_mn <- daily_selected_model()
      if (is.null(sel_mn) || !sel_mn %in% names(mlp$daily_preds)) {
        sel_mn <- names(mlp$daily_preds)[1]
        daily_selected_model(sel_mn)
      }
      
      ml_daily <- mlp$daily_preds[[sel_mn]]
      om_daily <- mlp$daily_om
      compare_om <- isTRUE(input$compare_om) && !is.null(om_daily) && nrow(om_daily) > 0
      
      if (nrow(ml_daily) == 0) return(NULL)
      
      gmin <- min(ml_daily$min_val, na.rm=TRUE)
      gmax <- max(ml_daily$max_val, na.rm=TRUE)
      if (compare_om) {
        gmin <- min(gmin, min(om_daily$min_val, na.rm=TRUE))
        gmax <- max(gmax, max(om_daily$max_val, na.rm=TRUE))
      }
      rng <- max(1, gmax-gmin)
      
      fa_days <- c("یک‌شنبه","دوشنبه","سه‌شنبه","چهارشنبه","پنج‌شنبه","جمعه","شنبه")
      ld <- live_data()
      
      cards <- purrr::map(seq_len(nrow(ml_daily)), function(i) {
        r_ml <- ml_daily[i,]
        r_om <- if (compare_om && i <= nrow(om_daily)) om_daily[i,] else NULL
        
        w_code <- if (!is.null(ld) && !is.null(ld$daily) && nrow(ld$daily) >= i) ld$daily$w_code[i] else NA
        wi <- weather_icon(w_code)
        
        dow <- as.integer(format(r_ml$date,"%u")) %% 7
        blft_ml <- round((as.numeric(r_ml$min_val)-gmin)/rng*100,0)
        bwid_ml <- max(6,round((as.numeric(r_ml$max_val)-as.numeric(r_ml$min_val))/rng*100,0))
        
        tags$div(class="wm-card",
                 tags$div(class="wm-name", fa_days[dow+1]),
                 tags$div(class="wm-date", format(r_ml$date,"%d/%m")),
                 tags$div(class="wm-icon", wi$icon),
                 tags$div(class="wm-max", paste0(round(as.numeric(r_ml$max_val),0),"°")),
                 tags$div(class="wm-bar", tags$div(class="wm-bar-fill", style=paste0("left:",blft_ml,"%;width:",bwid_ml,"%;"))),
                 tags$div(class="wm-min", paste0(round(as.numeric(r_ml$min_val),0),"°")),
                 
                 if (compare_om) {
                   blft_om <- round((as.numeric(r_om$min_val)-gmin)/rng*100,0)
                   bwid_om <- max(6,round((as.numeric(r_om$max_val)-as.numeric(r_om$min_val))/rng*100,0))
                   tags$div(class="om-row",
                            tags$div(class="om-label", "Open-Meteo"),
                            tags$div(class="wm-max", style="color:#22c55e;", paste0(round(as.numeric(r_om$max_val),0),"°")),
                            tags$div(class="wm-bar", style="margin:3px auto;", tags$div(class="wm-bar-fill", style=paste0("left:",blft_om,"%;width:",bwid_om,"%;background:#22c55e;"))),
                            tags$div(class="wm-min", style="color:#22c55e;", paste0(round(as.numeric(r_om$min_val),0),"°"))
                   )
                 }
        )
      })
      tags$div(class="week-mini-grid", cards)
    })
    
    output$daily_comp_metrics_ui <- renderUI({
      mlp <- hourly_ml_pred()
      req(mlp)
      compare_om <- isTRUE(input$compare_om)
      if (!compare_om || is.null(mlp$daily_comp_metrics) || length(mlp$daily_comp_metrics) == 0) return(NULL)
      
      sel_mn <- daily_selected_model()
      if (is.null(sel_mn) || !sel_mn %in% names(mlp$daily_comp_metrics)) {
        sel_mn <- names(mlp$daily_comp_metrics)[1]
        daily_selected_model(sel_mn)
      }
      
      m <- mlp$daily_comp_metrics[[sel_mn]]
      unit <- c(temperature="°C",humidity="%",wind_speed="km/h",precipitation="mm")[[mlp$target]]%||%""
      
      tags$div(
        class = "daily-comp-box",
        tags$div(class="daily-comp-header", tags$i(class="fa fa-scale-balanced"), "مقایسه با Open-Meteo (۷ روز)"),
        tags$table(class="comp-table",
                   tags$tbody(
                     tags$tr(
                       tags$td("میانگین اختلاف"),
                       tags$td(style="color:var(--text);", paste0(as.numeric(m$avg_diff), unit))
                     ),
                     tags$tr(
                       tags$td("کمترین اختلاف"),
                       tags$td(style="color:#22c55e;", paste0(as.numeric(m$min_diff), unit))
                     ),
                     tags$tr(
                       tags$td("بیشترین اختلاف"),
                       tags$td(style="color:#ef4444;", paste0(as.numeric(m$max_diff), unit))
                     )
                   )
        )
      )
    })
    
    # ── Feature Importance Section ──
    output$feat_imp_box <- renderUI({
      mlp <- hourly_ml_pred()
      req(mlp, mlp$feat_imp)
      
      feat_imps <- mlp$feat_imp
      if (length(feat_imps) == 0) return(NULL)
      
      current_sel <- feat_imp_selected_model()
      if (is.null(current_sel) || !current_sel %in% names(feat_imps)) {
        feat_imp_selected_model(names(feat_imps)[1])
        current_sel = names(feat_imps)[1]
      }
      
      pills <- lapply(names(feat_imps), function(mn) {
        is_active <- mn == current_sel
        cls <- if(is_active) "daily-pill active" else "daily-pill"
        color <- MODEL_META[[mn]]$color
        
        tags$div(
          class = cls,
          onclick = paste0("Shiny.setInputValue('", ns("feat_imp_model_click"), "', '", mn, "', {priority:'event'})"),
          tags$div(class="pill-dot", style=paste0("background:", color, ";")),
          MODEL_META[[mn]]$label
        )
      })
      
      tagList(
        tags$div(class="fc-chart-box",
                 tags$div(
                   style="display:flex;align-items:center;gap:8px;margin-bottom:12px;",
                   tags$i(class="fa fa-chart-simple", style="color:#22d3ee;font-size:14px;"),
                   tags$span(class="fc-section-lbl", style="margin-bottom:0;", "اهمیت ویژگی‌ها (Feature Importance)")
                 ),
                 tags$div(class = "daily-model-selector", pills),
                 plotly::plotlyOutput(ns("feat_imp_chart"), height = "280px")
        )
      )
    })
    
    output$feat_imp_chart <- plotly::renderPlotly({
      mlp <- hourly_ml_pred()
      req(mlp, mlp$feat_imp)
      
      sel_mn <- feat_imp_selected_model()
      if (is.null(sel_mn) || !sel_mn %in% names(mlp$feat_imp)) {
        sel_mn <- names(mlp$feat_imp)[1]
      }
      
      df <- mlp$feat_imp[[sel_mn]]
      if (is.null(df) || nrow(df) == 0) return(plotly::plot_ly())
      
      names(df) <- c("Feature", "Gain")
      df$Gain <- as.numeric(df$Gain)
      df <- df[order(df$Gain, decreasing = FALSE),]
      if (nrow(df) > 15) df <- tail(df, 15)
      
      plotly::plot_ly(data = df, x = ~Gain, y = ~Feature, type = "bar", orientation = "h", 
                      marker = list(color = "#22d3ee", line = list(color = "rgba(0,0,0,0)", width = 0)), 
                      hovertemplate = "<b>%{y}</b><br>Gain: %{x:.4f}<extra></extra>") %>%
        plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", 
                       font = list(family = "Vazirmatn,Tahoma", color = "#94a3b8", size = 13), 
                       xaxis = list(title = "اهمیت (Gain)", gridcolor = "rgba(99,143,232,0.06)", tickfont = list(size = 12, color = "#94a3b8")), 
                       yaxis = list(title = "", gridcolor = "transparent", tickfont = list(size = 13, color = "#cbd5e1")), 
                       margin = list(l = 130, r = 20, t = 10, b = 30)) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # ── Forecast Explainer (XAI) ──
    output$error_box <- renderUI({
      err <- last_error(); if(is.null(err)) return(NULL)
      tags$div(style="background:rgba(239,68,68,.07);border:1px solid rgba(239,68,68,.22);border-radius:8px;padding:10px 12px;margin-top:10px;", 
               tags$div(style="font-size:13px;font-weight:700;color:#f87171;margin-bottom:6px;display:flex;align-items:center;gap:5px;", 
                        tags$i(class="fa fa-triangle-exclamation"),"خطا — ", err$time), 
               tags$pre(style="background:rgba(0,0,0,.2);border-radius:4px;padding:8px;font-size:12px;color:#fca5a5;direction:ltr;text-align:left;white-space:pre-wrap;margin:0;", err$msg))
    })
    
    observeEvent(input$run_model, {
      models <- input$selected_models
      if (is.null(models) || length(models) == 0) { showNotification("حداقل یک مدل را انتخاب کنید", type="error"); return() }
      hourly_ml_pred(NULL)
      withProgress(message = paste("اجرای", length(models), "مدل ساعتی..."), value = 0.2, { run_hourly_pred() })
    })
    
    return(hourly_ml_pred)
  })
}