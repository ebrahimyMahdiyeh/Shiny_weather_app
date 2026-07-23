# File: modules/forecast_module.R  (نسخه ۵ — هماهنگ با modeling_utils جدید)
# ─────────────────────────────────────────────────────────────────────────────

# ── Helperهای تبدیل کد و سرعت ──────────────────────────────────────────────
weather_icon <- function(code) {
  if (is.null(code) || is.na(code)) return(list(icon="☁️", label="ابری"))
  code <- as.integer(code)
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
    "wind_speed_10m,precipitation,precipitation_probability,weather_code,surface_pressure",
    "&daily=temperature_2m_max,temperature_2m_min,precipitation_sum,weather_code,",
    "sunrise,sunset,wind_speed_10m_max,uv_index_max,precipitation_probability_max",
    "&current=temperature_2m,relative_humidity_2m,apparent_temperature,",
    "wind_speed_10m,weather_code,precipitation,surface_pressure",
    "&past_days=2",
    "&forecast_days=8",
    "&timezone=", utils::URLencode(tz, reserved = TRUE)
  )
  tryCatch({
    resp <- httr::GET(url, httr::timeout(15))
    if (httr::http_error(resp)) return(NULL)
    raw <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    
    h <- raw$hourly
    hourly_df <- tibble::tibble(
      time        = as.POSIXct(h$time, format = "%Y-%m-%dT%H:%M", tz = tz),
      temp        = as.numeric(h$temperature_2m),
      feels_like  = as.numeric(h$apparent_temperature),
      humidity    = as.numeric(h$relative_humidity_2m),
      wind_speed  = as.numeric(h$wind_speed_10m),
      precip      = as.numeric(h$precipitation),
      precip_prob = as.numeric(h$precipitation_probability),
      w_code      = as.integer(h$weather_code),
      pressure    = as.numeric(h$surface_pressure)
    ) %>% dplyr::filter(!is.na(temp))
    
    d <- raw$daily
    daily_df <- tibble::tibble(
      date        = as.Date(d$time),
      temp_max    = as.numeric(d$temperature_2m_max),
      temp_min    = as.numeric(d$temperature_2m_min),
      precip_sum  = as.numeric(d$precipitation_sum),
      w_code      = as.integer(d$weather_code),
      sunrise     = d$sunrise,
      sunset      = d$sunset,
      wind_max    = as.numeric(d$wind_speed_10m_max),
      uv_index    = as.numeric(d$uv_index_max),
      precip_prob = as.numeric(d$precipitation_probability_max)
    )
    
    cur <- raw$current
    current <- list(
      temp       = cur$temperature_2m,
      feels_like = cur$apparent_temperature,
      humidity   = cur$relative_humidity_2m,
      wind_speed = cur$wind_speed_10m,
      w_code     = cur$weather_code,
      precip     = cur$precip,
      pressure   = cur$surface_pressure,
      time       = as.POSIXct(cur$time, format = "%Y-%m-%dT%H:%M", tz = tz)
    )
    list(hourly = hourly_df, daily = daily_df, current = current, tz = tz)
  }, error = function(e) { message("Open-Meteo error: ", e$message); NULL })
}

# ── UI ────────────────────────────────────────────────────────────────────────
forecastUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      /* NOW CARD */
      .now-card{background:linear-gradient(135deg,#0d1b35 0%,#0f2347 55%,#0a1628 100%);border:1px solid rgba(99,143,232,.2);border-radius:14px;padding:20px 24px;position:relative;overflow:hidden;margin-bottom:13px;}
      .now-card::before{content:'';position:absolute;top:-70px;right:-70px;width:200px;height:200px;background:radial-gradient(circle,rgba(59,130,246,.1) 0%,transparent 65%);border-radius:50%;pointer-events:none;}
      .now-refresh{position:absolute;top:12px;left:14px;background:rgba(255,255,255,.06);border:1px solid rgba(255,255,255,.1);border-radius:6px;padding:4px 10px;color:rgba(255,255,255,.4);font-size:10px;cursor:pointer;font-family:'Vazirmatn',Tahoma;}
      .now-refresh:hover{background:rgba(255,255,255,.12);color:rgba(255,255,255,.75);}
      .now-inner{display:flex;justify-content:space-between;align-items:flex-start;}
      .now-city{font-size:13px;font-weight:700;color:rgba(255,255,255,.75);display:flex;align-items:center;gap:5px;margin-bottom:5px;}
      .now-big-temp{font-size:66px;font-weight:900;line-height:1;color:#fff;letter-spacing:-2px;}
      .now-big-temp sup{font-size:24px;font-weight:400;color:rgba(255,255,255,.4);vertical-align:super;}
      .now-feels{font-size:12px;color:rgba(255,255,255,.4);margin-top:3px;}
      .now-icon-area{text-align:left;padding-top:4px;}
      .now-big-icon{font-size:52px;line-height:1;}
      .now-weather-label{font-size:14px;font-weight:700;color:#e2e8f0;text-align:left;margin-top:3px;}
      .now-hilo{font-size:11px;color:rgba(255,255,255,.32);text-align:left;margin-top:2px;}
      .now-meta-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:8px;margin-top:14px;padding-top:13px;border-top:1px solid rgba(255,255,255,.07);}
      .now-meta-item{display:flex;flex-direction:column;gap:2px;}
      .now-meta-lbl{font-size:9px;font-weight:700;text-transform:uppercase;letter-spacing:.9px;color:rgba(255,255,255,.28);}
      .now-meta-val{font-size:15px;font-weight:700;color:rgba(255,255,255,.88);}
      .now-meta-sub{font-size:10px;color:rgba(255,255,255,.28);}

      /* HOURLY */
      .hourly-strip{display:flex;gap:6px;overflow-x:auto;padding:8px 0 4px;scrollbar-width:thin;scrollbar-color:rgba(99,143,232,.15) transparent;}
      .hourly-strip::-webkit-scrollbar{height:3px;}
      .hourly-strip::-webkit-scrollbar-thumb{background:rgba(99,143,232,.18);border-radius:2px;}
      .hour-card{flex-shrink:0;background:rgba(255,255,255,.025);border:1px solid rgba(99,143,232,.1);border-radius:10px;padding:9px 10px;text-align:center;min-width:60px;transition:all .15s;cursor:default;}
      .hour-card:hover{background:rgba(59,130,246,.07);border-color:rgba(59,130,246,.22);}
      .hour-card.now-hour{background:rgba(59,130,246,.13);border-color:rgba(59,130,246,.38);}
      .hour-time{font-size:10px;color:#64748b;font-weight:600;}
      .hour-icon{font-size:18px;margin:3px 0;}
      .hour-temp{font-size:14px;font-weight:800;color:#e2e8f0;}
      .hour-prob{font-size:9px;color:#60a5fa;margin-top:2px;}

      /* CHART BOX */
      .fc-chart-box{background:#111827;border:1px solid rgba(99,143,232,.15);border-radius:10px;padding:14px 18px;margin-bottom:13px;}
      .fc-section-lbl{font-size:10px;font-weight:800;color:#64748b;text-transform:uppercase;letter-spacing:1.1px;display:flex;align-items:center;gap:7px;margin-bottom:10px;}
      .fc-section-lbl::after{content:'';flex:1;height:1px;background:rgba(99,143,232,.09);}

      /* WEEK MINI */
      .week-mini-grid{display:grid;grid-template-columns:repeat(7,1fr);gap:6px;}
      .wm-card{background:rgba(255,255,255,.022);border:1px solid rgba(99,143,232,.09);border-radius:9px;padding:9px 5px;text-align:center;cursor:pointer;transition:all .15s;position:relative;overflow:hidden;}
      .wm-card:hover{border-color:rgba(99,143,232,.22);background:rgba(255,255,255,.04);}
      .wm-card.sel{background:rgba(59,130,246,.1);border-color:rgba(59,130,246,.35);}
      .wm-card.today-col::before{content:'';position:absolute;top:0;left:0;right:0;height:2px;background:linear-gradient(90deg,#3b82f6,#14b8a6);}
      .wm-name{font-size:9px;font-weight:800;color:#64748b;text-transform:uppercase;letter-spacing:.6px;margin-bottom:2px;}
      .wm-date{font-size:9px;color:#94a3b8;margin-bottom:4px;}
      .wm-card.today-col .wm-date{color:#60a5fa;font-weight:800;}
      .wm-icon{font-size:18px;margin:2px 0;}
      .wm-max{font-size:13px;font-weight:900;color:#e2e8f0;}
      .wm-min{font-size:10px;color:#64748b;margin-top:1px;}
      .wm-rain{font-size:9px;color:#60a5fa;font-weight:600;display:flex;align-items:center;justify-content:center;gap:2px;margin-top:3px;}
      .wm-bar{height:3px;border-radius:2px;margin:3px auto;width:75%;background:rgba(99,143,232,.1);overflow:hidden;position:relative;}
      .wm-bar-fill{height:100%;border-radius:2px;position:absolute;background:linear-gradient(90deg,#3b82f6,#f59e0b);}

      /* DAY DETAIL */
      .day-detail-row{display:grid;grid-template-columns:repeat(6,1fr);gap:6px;margin-top:9px;}
      .dd-card{background:rgba(0,0,0,.18);border:1px solid rgba(99,143,232,.08);border-radius:8px;padding:8px 5px;text-align:center;}
      .dd-lbl{font-size:8.5px;color:#64748b;font-weight:700;text-transform:uppercase;letter-spacing:.6px;}
      .dd-val{font-size:16px;font-weight:900;color:#e2e8f0;margin:2px 0 1px;}
      .dd-sub{font-size:9px;color:#64748b;}
      .day-divider{display:flex;align-items:center;gap:8px;font-size:9.5px;font-weight:800;color:#64748b;text-transform:uppercase;letter-spacing:1px;margin:10px 0 7px;}
      .day-divider::before,.day-divider::after{content:'';flex:1;height:1px;background:rgba(99,143,232,.08);}

      /* MODEL SELECTOR */
      .model-grid{display:grid;grid-template-columns:1fr 1fr;gap:5px;margin-top:6px;}
      .model-btn{padding:8px 10px;border-radius:7px;background:rgba(255,255,255,.025);border:1px solid rgba(99,143,232,.12);color:#94a3b8;font-family:'Vazirmatn',Tahoma;font-size:11px;font-weight:600;cursor:pointer;text-align:right;transition:all .15s;display:flex;align-items:center;gap:6px;}
      .model-btn:hover{background:rgba(59,130,246,.07);color:#e2e8f0;border-color:rgba(59,130,246,.22);}
      .model-btn.active{background:rgba(59,130,246,.14);border-color:rgba(59,130,246,.4);color:#60a5fa;}
      .model-dot{width:7px;height:7px;border-radius:50%;flex-shrink:0;}
      .model-type-badge{margin-right:auto;font-size:9px;opacity:.7;}

      /* CTRL BOX */
      .fc-ctrl-box{background:#111827;border:1px solid rgba(99,143,232,.15);border-radius:10px;padding:13px;margin-bottom:10px;}
      .fc-ctrl-lbl{font-size:9px;font-weight:800;color:#64748b;text-transform:uppercase;letter-spacing:1px;margin-bottom:4px;display:block;}
    ")),
    
    fluidRow(
      # ── سایدبار: فقط ایستگاه + مدل ─────────────────────────────────────
      column(3,
             tags$div(class="fc-ctrl-box",
                      tags$span(class="fc-ctrl-lbl", "ایستگاه"),
                      selectInput(ns("station"), label=NULL, choices=NULL, width="100%"),
                      
                      tags$hr(style="border-color:rgba(99,143,232,.12);margin:10px 0;"),
                      
                      tags$span(class="fc-ctrl-lbl", "انتخاب مدل"),
                      uiOutput(ns("model_buttons")), # رندر داینامیک دکمه‌ها
                      
                      tags$hr(style="border-color:rgba(99,143,232,.12);margin:10px 0;"),
                      
                      tags$span(class="fc-ctrl-lbl", "متغیر"),
                      selectInput(ns("target_var"), label=NULL,
                                  choices=c("دما (°C)"="temperature","رطوبت (%)"="humidity",
                                            "سرعت باد (km/h)"="wind_speed","بارش (mm)"="precipitation"),
                                  width="100%"
                      ),
                      
                      actionButton(ns("run_model"),
                                   label=tags$span(tags$i(class="fa fa-play",style="margin-left:5px;"),"اجرای مدل"),
                                   class="btn btn-success btn-block",
                                   style="font-size:12px;font-weight:700;padding:9px;"
                      ),
                      
                      tags$hr(style="border-color:rgba(99,143,232,.12);margin:10px 0;"),
                      
                      actionButton(ns("refresh"),
                                   label=tags$span(tags$i(class="fa fa-rotate",style="margin-left:5px;"),"بروزرسانی آب‌وهوا"),
                                   class="btn btn-primary btn-block",
                                   style="font-size:11px;font-weight:600;padding:8px;"
                      ),
                      tags$div(style="margin-top:5px;text-align:center;font-size:10px;color:#64748b;",
                               textOutput(ns("last_update"),inline=TRUE))
             ),
             uiOutput(ns("error_box"))
      ),
      
      # ── محتوای اصلی ─────────────────────────────────────────────────────
      column(9,
             uiOutput(ns("now_card")),
             
             # نمودار یکپارچه: نوار ساعتی + پیش‌بینی مدل
             tags$div(class="fc-chart-box",
                      tags$div(
                        style="display:flex;align-items:center;gap:8px;margin-bottom:10px;",
                        tags$i(class="fa fa-clock",style="color:#60a5fa;font-size:11px;"),
                        tags$span(style="font-size:10px;font-weight:800;color:#64748b;text-transform:uppercase;letter-spacing:1px;",
                                  "۲۴ ساعت آینده —"),
                        tags$span(style="font-size:10px;font-weight:800;color:#60a5fa;",
                                  textOutput(ns("active_model_lbl"), inline=TRUE))
                      ),
                      uiOutput(ns("hourly_strip")),
                      tags$div(style="margin-top:10px;",
                               shinycssloaders::withSpinner(
                                 plotly::plotlyOutput(ns("hourly_chart"), height="220px"),
                                 type=8, color="#3b82f6", size=0.45)
                      )
             ),
             
             # هفتگی کوچک
             tags$div(class="fc-chart-box",
                      tags$div(
                        style="display:flex;align-items:center;gap:8px;margin-bottom:10px;",
                        tags$i(class="fa fa-calendar-week",style="color:#fbbf24;font-size:11px;"),
                        tags$span(style="font-size:10px;font-weight:800;color:#64748b;text-transform:uppercase;letter-spacing:1px;",
                                  "پیش‌بینی ۷ روز"),
                        tags$span(style="margin-right:auto;font-size:9px;color:#64748b;font-style:italic;",
                                  "روی هر روز کلیک کنید")
                      ),
                      uiOutput(ns("week_mini")),
                      tags$div(class="day-divider","جزئیات"),
                      uiOutput(ns("day_detail"))
             )
      )
    )
  )
}

# helper: دکمه مدل (برای استفاده در renderUI)
modelBtn <- function(ns, model_id, color, label, type_lbl, is_active = FALSE) {
  cls <- "model-btn"
  if (is_active) cls <- paste0(cls, " active")
  tags$div(class=cls, id=ns(paste0("mbtn_",model_id)),
           onclick=paste0("Shiny.setInputValue('",ns("selected_model"),"','",model_id,"',{priority:'event'})"),
           tags$div(class="model-dot",style=paste0("background:",color,";")),
           tags$span(label),
           tags$span(class="model-type-badge",type_lbl)
  )
}

nowMeta <- function(lbl,val,sub,ico,col) {
  tags$div(class="now-meta-item",
           tags$div(class="now-meta-lbl",lbl),
           tags$div(style="display:flex;align-items:center;gap:5px;",
                    tags$i(class=paste("fa",ico),style=paste0("color:",col,";font-size:11px;")),
                    tags$span(class="now-meta-val",val)),
           if(!is.null(sub)) tags$div(class="now-meta-sub",sub)
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
forecastServer <- function(id, weather_data, hourly_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── ایستگاه ─────────────────────────────────────────────────────────────
    observe({
      req(weather_data())
      choices <- purrr::set_names(
        names(weather_data()),
        purrr::map_chr(names(weather_data()), ~ STATIONS[[.x]]$name %||% .x)
      )
      updateSelectInput(session, "station", choices=choices)
    })
    
    # ── داده زنده ───────────────────────────────────────────────────────────
    live_data <- reactiveVal(NULL)
    
    do_fetch <- function(sid) {
      st <- STATIONS[[sid]]; if (is.null(st)) return(NULL)
      withProgress(message="دریافت از Open-Meteo...", value=.4,
                   d <- fetch_hourly_forecast(st$lat, st$lon))
      d
    }
    
    observeEvent(input$station,  { req(input$station); live_data(do_fetch(input$station)) }, ignoreInit=FALSE)
    observeEvent(input$refresh,  { req(input$station); live_data(do_fetch(input$station)) })
    
    output$last_update <- renderText({
      if (is.null(live_data())) "— دریافت نشده"
      else paste0("بروز: ", format(Sys.time(), "%H:%M"))
    })
    
    # ── مدل انتخابی ─────────────────────────────────────────────────────────
    MODEL_META <- list(
      arima   =list(label="ARIMA",        color="#3b82f6"),
      sarima  =list(label="SARIMA",       color="#60a5fa"),
      ets     =list(label="ETS",          color="#14b8a6"),
      tbats   =list(label="TBATS",        color="#0d9488"),
      prophet =list(label="Prophet",      color="#8b5cf6"),
      rf      =list(label="Random Forest",color="#f59e0b"),
      xgboost =list(label="XGBoost",      color="#d97706"),
      lightgbm=list(label="LightGBM",     color="#22d3ee"),
      catboost=list(label="CatBoost",     color="#fb7185"),
      svm     =list(label="SVM",          color="#ef4444"),
      naive   =list(label="Naïve",        color="#64748b")
    )
    
    selected_model <- reactiveVal("arima")
    
    # رندر دکمه‌های مدل به صورت داینامیک برای اعمال استایل active
    output$model_buttons <- renderUI({
      mn <- selected_model()
      btns <- list(
        list("arima",   "#3b82f6","ARIMA",    "کلاسیک"),
        list("sarima",  "#60a5fa","SARIMA",   "فصلی"),
        list("ets",     "#14b8a6","ETS",      "Smooth"),
        list("tbats",   "#0d9488","TBATS",    "چند فصل"),
        list("prophet", "#8b5cf6","Prophet",  "Bayesian"),
        list("rf",      "#f59e0b","RF",       "ML"),
        list("xgboost", "#d97706","XGBoost",  "ML"),
        list("lightgbm","#22d3ee","LightGBM", "ML"),
        list("catboost","#fb7185","CatBoost", "ML"),
        list("svm",     "#ef4444","SVM",      "ML"),
        list("naive",   "#64748b","Naïve",    "Baseline")
      )
      tags$div(class="model-grid",
               purrr::map(btns, function(b) {
                 modelBtn(ns, b[[1]], b[[2]], b[[3]], b[[4]], mn == b[[1]])
               })
      )
    })
    
    observeEvent(input$selected_model, {
      new_model <- input$selected_model
      if (!is.null(new_model) && new_model != selected_model()) {
        selected_model(new_model)
        if (!is.null(hourly_ml_pred())) {
          hourly_ml_pred(NULL)
          showNotification(
            paste0("مدل تغییر کرد — برای اجرای ", MODEL_META[[new_model]]$label %||% new_model,
                   " روی «اجرای مدل» کلیک کنید"),
            type = "default", duration = 4
          )
        }
      }
    })
    
    output$active_model_lbl <- renderText({
      MODEL_META[[selected_model()]]$label %||% selected_model()
    })
    
    # ── پیش‌بینی ساعتی ۲۴h با مدل انتخابی ──────────────────────────────────
    hourly_ml_pred <- reactiveVal(NULL)
    last_error     <- reactiveVal(NULL)
    
    run_hourly_pred <- function() {
      sid    <- input$station %||% names(STATIONS)[1]
      target <- input$target_var %||% "temperature"
      mn     <- selected_model() %||% "xgboost"
      
      h_data <- if (!is.null(hourly_data) && is.function(hourly_data)) {
        hourly_data()[[sid]]
      } else if (!is.null(hourly_data) && is.list(hourly_data)) {
        hourly_data[[sid]]
      } else if (exists("WEATHER_DATA_HOURLY")) {
        WEATHER_DATA_HOURLY[[sid]]
      } else NULL
      
      if (is.null(h_data) || nrow(h_data) < 48) {
        msg <- if (is.null(h_data)) paste0("داده ساعتی برای ایستگاه «", sid, "» پیدا نشد.")
        else paste0("فقط ", nrow(h_data), " ردیف داده ساعتی موجود است — حداقل ۴۸ ردیف لازم است.")
        last_error(list(msg = msg, time = format(Sys.time(), "%H:%M")))
        showNotification(msg, type = "error", duration = 6)
        hourly_ml_pred(NULL)
        return()
      }
      
      tryCatch({
        last_error(NULL)
        
        # ── مرحله ۱: اطمینان از داشتن داده زنده ─────────────────────────────────
        ld <- live_data()
        now_check <- lubridate::with_tz(Sys.time(), "Asia/Tehran")
        need_fetch <- is.null(ld)
        if (!is.null(ld) && !is.null(ld$hourly) && nrow(ld$hourly) > 0) {
          live_max <- max(ld$hourly$time, na.rm = TRUE)
          if (as.numeric(difftime(now_check, live_max, units = "hours")) > 3) {
            need_fetch <- TRUE
          }
        }
        
        if (need_fetch) {
          st <- STATIONS[[sid]]
          if (!is.null(st)) {
            ld_new <- tryCatch(fetch_hourly_forecast(st$lat, st$lon), error = function(e) NULL)
            if (!is.null(ld_new)) {
              ld <- ld_new
              live_data(ld)
            }
          }
        }
        
        # ── مرحله ۲: تبدیل داده زنده به فرمت هماهنگ ───────────────────────────
        live_hourly <- NULL
        if (!is.null(ld) && !is.null(ld$hourly) && nrow(ld$hourly) > 0) {
          lh <- ld$hourly
          now_filter <- lubridate::with_tz(Sys.time(), "Asia/Tehran")
          lh <- lh %>% dplyr::filter(time <= now_filter)
          
          if (nrow(lh) > 0) {
            live_hourly <- tibble::tibble(timestamp = lh$time)
            col_map <- c(
              temp="temperature", humidity="humidity", wind_speed="wind_speed",
              pressure="pressure", precip="precipitation", w_code="w_code"
            )
            for (live_col in names(col_map)) {
              csv_col <- col_map[[live_col]]
              live_hourly[[csv_col]] <- if (live_col %in% names(lh)) lh[[live_col]] else NA_real_
            }
            live_hourly <- live_hourly %>% dplyr::filter(!is.na(timestamp))
          }
        }
        
        # ── مرحله ۳: ترکیب CSV + داده زنده ────────────────────────────────────
        h_data_combined <- h_data
        if (!is.null(live_hourly) && nrow(live_hourly) > 0) {
          h_data_combined <- dplyr::bind_rows(h_data, live_hourly) %>%
            dplyr::arrange(timestamp) %>%
            dplyr::filter(!duplicated(timestamp))
        }
        
        # ── مرحله ۴: محاسبه gap و now ──────────────────────────────────────────
        last_ts <- max(h_data_combined$timestamp)
        now <- lubridate::with_tz(Sys.time(), "Asia/Tehran")
        now_hour <- as.POSIXct(paste(format(now, "%Y-%m-%d"), format(now, "%H:00:00")), tz = "Asia/Tehran")
        
        gap_hours <- as.integer(difftime(now_hour, last_ts, units = "hours"))
        if (gap_hours < 0) gap_hours <- 0
        
        if (gap_hours > 500) {
          stop(sprintf(
            "آخرین داده‌ی موجود مربوط به %s است (%d ساعت با الان فاصله دارد) — لطفاً «بروزرسانی آب‌وهوا» را بزنید یا فایل CSV تاریخی را به‌روز کنید.",
            format(last_ts, "%Y-%m-%d %H:%M"), gap_hours
          ))
        }
        
        # ── مرحله ۵: اجرای مدل با horizon بزرگتر ──────────────────────────────
        horizon_needed <- 24 + gap_hours
        
        # استفاده از تابع run_hourly_model از modeling_utils.R
        res <- run_hourly_model(mn, h_data_combined,
                                horizon_h = horizon_needed, target = target)
        
        if (is.null(res) || is.null(res$predictions) || length(res$predictions) < horizon_needed) {
          stop("خروجی مدل نامعتبر است")
        }
        
        # ── مرحله ۶: فقط ۲۴ prediction آخر (از now+1h شروع) ───────────────────
        predictions <- tail(res$predictions, 24)
        lower <- if (!is.null(res$lower) && length(res$lower) >= horizon_needed)
          tail(res$lower, 24) else predictions - 1.5
        upper <- if (!is.null(res$upper) && length(res$upper) >= horizon_needed)
          tail(res$upper, 24) else predictions + 1.5
        
        future_ts <- seq(from = now_hour + 3600, by = 3600, length.out = 24)
        
        hourly_ml_pred(list(
          timestamps = future_ts,
          preds      = as.numeric(predictions),
          lower      = as.numeric(lower),
          upper      = as.numeric(upper),
          method     = res$method,
          target     = target,
          hist_df    = tail(h_data_combined, 48)
        ))
        showNotification(paste0(MODEL_META[[mn]]$label %||% mn, " با موفقیت اجرا شد ✓"), type = "message", duration = 3)
      }, error = function(e) {
        msg <- conditionMessage(e)
        message("خطا در پیش‌بینی ساعتی: ", msg)
        last_error(list(msg = msg, time = format(Sys.time(), "%H:%M")))
        showNotification(paste("خطا:", msg), type = "error", duration = 8)
        hourly_ml_pred(NULL)
      })
    }
    
    output$now_card <- renderUI({
      ld <- live_data(); sid <- input$station %||% names(STATIONS)[1]
      sname <- STATIONS[[sid]]$name %||% sid
      
      if (is.null(ld)) return(tags$div(class="now-card",
                                       tags$div(style="text-align:center;padding:18px;color:rgba(255,255,255,.3);font-size:12px;",
                                                "🌡️  برای دریافت وضعیت فعلی روی «بروزرسانی» کلیک کنید")))
      
      cur <- ld$current; wi <- weather_icon(cur$w_code)
      d0  <- if(nrow(ld$daily)>0) ld$daily[1,] else NULL
      
      tags$div(class="now-card",
               actionButton(ns("refresh"),
                            label=tags$span(tags$i(class="fa fa-rotate",style="margin-left:4px;"), format(cur$time,"%H:%M")),
                            class="now-refresh"),
               tags$div(class="now-inner",
                        tags$div(
                          tags$div(class="now-city",
                                   tags$i(class="fa fa-location-dot",style="color:#60a5fa;font-size:11px;"), sname),
                          tags$div(class="now-big-temp", round(cur$temp,1), tags$sup("°C")),
                          tags$div(class="now-feels", paste0("احساس: ", round(cur$feels_like,1), "°C"))
                        ),
                        tags$div(class="now-icon-area",
                                 tags$div(class="now-big-icon", wi$icon),
                                 tags$div(class="now-weather-label", wi$label),
                                 if (!is.null(d0) && !is.na(d0$temp_max))
                                   tags$div(class="now-hilo", paste0("↑ ",round(d0$temp_max,0),"°  ↓ ",round(d0$temp_min,0),"°"))
                        )
               ),
               tags$div(class="now-meta-grid",
                        nowMeta("رطوبت",paste0(round(cur$humidity,0),"%"),NULL,"fa-droplet","#3b82f6"),
                        nowMeta("باد",paste0(round(cur$wind_speed,0)," km/h"),wind_label(cur$wind_speed),"fa-wind","#8b5cf6"),
                        nowMeta("فشار",paste0(round(cur$pressure,0)," hPa"),NULL,"fa-gauge-high","#14b8a6"),
                        nowMeta("بارش",paste0(round(cur$precip%||%0,1)," mm"),NULL,"fa-cloud-rain","#fbbf24")
               )
      )
    })
    
    # ── نوار ساعتی ──────────────────────────────────────────────────────────
    output$hourly_strip <- renderUI({
      ld <- live_data()
      if (is.null(ld)) return(tags$div(style="color:#64748b;text-align:center;padding:10px;font-size:12px;", "داده‌ای دریافت نشده"))
      now <- Sys.time()
      h24 <- ld$hourly %>%
        dplyr::filter(time >= now, time <= now + 3600*24) %>%
        head(24)
      if (nrow(h24)==0) return(NULL)
      
      cards <- purrr::map(seq_len(nrow(h24)), function(i) {
        r <- h24[i,]; wi <- weather_icon(r$w_code)
        lbl <- if(i==1) "الان" else format(r$time,"%H:%M")
        pr  <- if(!is.na(r$precip_prob)&&r$precip_prob>10) paste0(round(r$precip_prob,0),"%") else ""
        tags$div(class=if(i==1)"hour-card now-hour" else "hour-card",
                 tags$div(class="hour-time",lbl),
                 tags$div(class="hour-icon",wi$icon),
                 tags$div(class="hour-temp",paste0(round(r$temp,0),"°")),
                 if(nchar(pr)>0) tags$div(class="hour-prob", tags$i(class="fa fa-droplet",style="font-size:7px;margin-left:2px;"),pr))
      })
      tags$div(class="hourly-strip",cards)
    })
    
    # ── نمودار ساعتی — پیش‌بینی مدل ML روی داده ساعتی ─────────────────────
    output$hourly_chart <- plotly::renderPlotly({
      ld  <- live_data()
      mlp <- hourly_ml_pred()
      var <- input$target_var %||% "temperature"
      mn  <- selected_model() %||% "xgboost"
      col <- MODEL_META[[mn]]$color %||% "#3b82f6"
      unit <- c(temperature="°C",humidity="%",wind_speed="km/h",precipitation="mm")[[var]]%||%""
      now <- Sys.time()
      
      x_min <- now - 3600*6
      x_max <- now + 3600*24
      
      p <- plotly::plot_ly()
      
      if (!is.null(ld)) {
        hist_h <- ld$hourly %>%
          dplyr::filter(time >= x_min, time <= now) %>%
          dplyr::mutate(y_val = switch(var, temperature=temp, humidity=humidity, wind_speed=wind_speed, precipitation=precip, temp))
        if (nrow(hist_h) > 0)
          p <- plotly::add_trace(p, type="scatter", mode="lines", x=hist_h$time, y=hist_h$y_val,
                                 name="۶h گذشته (واقعی)", line=list(color="#64748b",width=1.5,dash="dot"),
                                 hovertemplate=paste0("واقعی: %{y:.1f}",unit,"<extra></extra>"))
      }
      
      has_pred <- !is.null(mlp) && mlp$target == var && length(mlp$preds) > 0 && !all(is.na(mlp$preds))
      
      if (has_pred) {
        p <- p %>%
          plotly::add_trace(type="scatter", mode="lines", x=mlp$timestamps, y=mlp$lower,
                            line=list(color="transparent"),showlegend=FALSE,hoverinfo="skip") %>%
          plotly::add_trace(type="scatter", mode="lines", x=mlp$timestamps, y=mlp$upper,
                            fill="tonexty", fillcolor=paste0(substr(col,1,7),"22"),
                            line=list(color="transparent"), name="بازه ۹۵٪",
                            hovertemplate=paste0("حد بالا: %{y:.1f}",unit,"<extra></extra>")) %>%
          plotly::add_trace(type="scatter", mode="lines+markers", x=mlp$timestamps, y=mlp$preds,
                            name=paste0("پیش‌بینی — ",mlp$method), line=list(color=col,width=2.5),
                            marker=list(color=col,size=4),
                            hovertemplate=paste0("%{x|%H:%M}<br>%{y:.1f}",unit,"<extra></extra>"))
      }
      
      y_vals <- c(
        if (!is.null(ld)) {
          ld$hourly %>% dplyr::filter(time >= x_min, time <= now) %>%
            { switch(var, temperature=.$temp, humidity=.$humidity, wind_speed=.$wind_speed, precipitation=.$precip, .$temp) }
        } else NA_real_,
        if (has_pred) c(mlp$lower, mlp$upper, mlp$preds) else NA_real_
      )
      y_vals <- y_vals[is.finite(y_vals)]
      if (length(y_vals) > 0) {
        y_pad   <- max(1, diff(range(y_vals)) * 0.15)
        y_range <- c(min(y_vals) - y_pad, max(y_vals) + y_pad)
      } else { y_range <- NULL }
      
      p <- p %>% plotly::layout(
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(family="Vazirmatn,Tahoma",color="#64748b",size=10),
        xaxis=list(title="", gridcolor="rgba(99,143,232,.04)", tickfont=list(size=10,color="#64748b"),
                   tickformat="%H:%M", range=list(x_min, x_max), type="date",
                   dtick=2*60*60*1000, ticklabelmode="instant"),
        yaxis=list(title=unit, gridcolor="rgba(99,143,232,.05)", tickfont=list(size=10,color="#64748b"),
                   ticksuffix=unit, range=y_range),
        legend=list(orientation="h",y=-0.24,x=.5,xanchor="center", font=list(size=10),bgcolor="rgba(0,0,0,0)"),
        hovermode="x unified", margin=list(l=42,r=8,t=4,b=48),
        shapes=list(list(type="line",xref="x",yref="paper", x0=now,x1=now,y0=0,y1=1,
                         line=list(color="rgba(255,255,255,.12)",width=1,dash="dot"))),
        annotations = if (has_pred) {
          list(list(x=now,y=1,xref="x",yref="paper", text="الان",showarrow=FALSE,
                    font=list(color="rgba(255,255,255,.28)",size=10,family="Vazirmatn"), yanchor="bottom"))
        } else {
          list(
            list(x=now,y=1,xref="x",yref="paper", text="الان",showarrow=FALSE,
                 font=list(color="rgba(255,255,255,.28)",size=10,family="Vazirmatn"), yanchor="bottom"),
            list(x=0.5, y=0.5, xref="paper", yref="paper", showarrow=FALSE,
                 text="روی «اجرای مدل» کلیک کنید تا پیش‌بینی ۲۴h ببینید",
                 font=list(color="#64748b",size=12,family="Vazirmatn,Tahoma"))
          )
        }
      )
      p %>% plotly::config(displayModeBar=FALSE)
    })
    
    # ── تقویم هفتگی ─────────────────────────────────────────────────────────
    selected_day <- reactiveVal(1L)
    observeEvent(input$sel_day, { selected_day(as.integer(input$sel_day)) })
    
    output$week_mini <- renderUI({
      ld <- live_data()
      if (is.null(ld)) return(tags$div(style="color:#64748b;text-align:center;padding:12px;font-size:12px;",
                                       "برای دیدن پیش‌بینی هفتگی روی «بروزرسانی» کلیک کنید"))
      
      daily <- head(ld$daily,7); today <- Sys.Date()
      gmin <- min(daily$temp_min,na.rm=TRUE)
      gmax <- max(daily$temp_max,na.rm=TRUE)
      rng  <- max(1, gmax-gmin)
      fa_days <- c("یک‌شنبه","دوشنبه","سه‌شنبه","چهارشنبه","پنج‌شنبه","جمعه","شنبه")
      
      cards <- purrr::map(seq_len(nrow(daily)), function(i) {
        r  <- daily[i,]; wi <- weather_icon(r$w_code)
        is_today  <- r$date == today
        is_sel    <- i == selected_day()
        dow       <- as.integer(format(r$date,"%u")) %% 7
        blft      <- round((r$temp_min-gmin)/rng*100,0)
        bwid      <- max(6,round((r$temp_max-r$temp_min)/rng*100,0))
        cls <- paste0("wm-card", if(is_today) " today-col" else "", if(is_sel) " sel" else "")
        
        tags$div(class=cls,
                 onclick=paste0("Shiny.setInputValue('",ns("sel_day"),"',",i,",{priority:'event'})"),
                 tags$div(class="wm-name", fa_days[dow+1]),
                 tags$div(class="wm-date", format(r$date,"%d/%m")),
                 tags$div(class="wm-icon", wi$icon),
                 tags$div(class="wm-max",  paste0(round(r$temp_max,0),"°")),
                 tags$div(class="wm-bar", tags$div(class="wm-bar-fill", style=paste0("left:",blft,"%;width:",bwid,"%;"))),
                 tags$div(class="wm-min",  paste0(round(r$temp_min,0),"°")),
                 if(!is.na(r$precip_prob)&&r$precip_prob>=15)
                   tags$div(class="wm-rain", tags$i(class="fa fa-droplet",style="font-size:7px;"), paste0(round(r$precip_prob,0),"%"))
        )
      })
      tags$div(class="week-mini-grid", cards)
    })
    
    # ── جزئیات روز انتخابی ──────────────────────────────────────────────────
    output$day_detail <- renderUI({
      ld <- live_data(); if(is.null(ld)) return(NULL)
      idx   <- selected_day() %||% 1L
      daily <- ld$daily; if(idx>nrow(daily)) return(NULL)
      r     <- daily[idx,]; today <- Sys.Date()
      lbl   <- if(r$date==today)"امروز" else if(r$date==today+1)"فردا" else format(r$date,"%A %d")
      
      dd <- function(emoji,l,v,s) tags$div(class="dd-card",
                                           tags$div(class="dd-lbl",paste(emoji,l)), tags$div(class="dd-val",v), tags$div(class="dd-sub",s))
      
      tags$div(class="day-detail-row",
               dd("🌡️","max/min",  paste0(round(r$temp_max,0),"°/",round(r$temp_min,0),"°"), lbl),
               dd("🌧️","بارش",    paste0(round(r$precip_sum%||%0,1)," mm"), paste0("احتمال: ",round(r$precip_prob%||%0,0),"%")),
               dd("💨","باد max",  paste0(round(r$wind_max%||%0,0)," km/h"), wind_label(r$wind_max)),
               dd("☀️","UV",      round(r$uv_index%||%0,1), {uv<-r$uv_index%||%0;if(uv<3)"پایین"else if(uv<6)"متوسط"else if(uv<8)"بالا"else"خیلی بالا"}),
               dd("🌅","طلوع",    if(!is.null(r$sunrise)&&!is.na(r$sunrise))substr(r$sunrise,12,16)else"—","صبح"),
               dd("🌇","غروب",    if(!is.null(r$sunset)&&!is.na(r$sunset))substr(r$sunset,12,16)else"—","عصر")
      )
    })
    
    output$error_box <- renderUI({
      err <- last_error(); if(is.null(err)) return(NULL)
      tags$div(
        style="background:rgba(239,68,68,.07);border:1px solid rgba(239,68,68,.22);border-radius:8px;padding:9px 11px;margin-top:7px;",
        tags$div(style="font-size:11px;font-weight:700;color:#f87171;margin-bottom:4px;",
                 tags$i(class="fa fa-triangle-exclamation",style="margin-left:4px;"),"خطا — ",err$time),
        tags$pre(style="background:rgba(0,0,0,.2);border-radius:4px;padding:6px;font-size:10px;color:#fca5a5;direction:ltr;text-align:left;white-space:pre-wrap;margin:0;", err$msg))
    })
    
    # وقتی run_model کلیک میشه → پیش‌بینی ساعتی اجرا میشه
    observeEvent(input$run_model, {
      mn <- selected_model() %||% "xgboost"
      withProgress(message = paste("اجرای مدل ساعتی —", MODEL_META[[mn]]$label %||% mn, "..."), value = 0.2, {
        run_hourly_pred()
      })
    })
    
    return(hourly_ml_pred)
  })
}