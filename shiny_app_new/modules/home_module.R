# File: modules/home_module.R  (نسخه ارتقاءیافته + پشتیبانی تم + آنالیز کیفیت داده)
# صفحه اول — نمای کلی، آمار داده‌ها، توضیح پروژه

# ── UI ────────────────────────────────────────────────────────────────────────
homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    # ── Hero Banner ─────────────────────────────────────────────────────────
    fluidRow(
      column(12,
             tags$div(class = "hero-banner",
                      tags$div(
                        style = "display:inline-flex;align-items:center;gap:6px;background:var(--input-bg);border:1px solid var(--border);color:var(--blue2);border-radius:20px;padding:5px 14px;font-size:12px;font-weight:700;margin-bottom:12px;",
                        tags$i(class="fa fa-graduation-cap"), " پروژه پژوهشی — IEEE Publication 2025"
                      ),
                      tags$h2(
                        "سامانه هوشمند پیش‌بینی ",
                        tags$span(class="grad", "آب‌وهوای ایران")
                      ),
                      tags$p(
                        "مقایسه جامع ۱۱ مدل سری زمانی شامل روش‌های کلاسیک (ARIMA، SARIMA، ETS)،",
                        " یادگیری ماشین (RF، XGBoost، LightGBM، CatBoost، SVM) و مدل‌های مدرن (Prophet)",
                        " با استفاده از داده‌های هواشناسی ۵ ایستگاه ایران در بازه ۲۰۲۱–۲۰۲۵."
                      ),
                      tags$div(class = "hero-chips",
                               tags$span(class="hchip hchip-blue",  tags$i(class="fa fa-chart-bar"),  " ARIMA · SARIMA"),
                               tags$span(class="hchip hchip-purple", tags$i(class="fa fa-leaf"),       " Prophet"),
                               tags$span(class="hchip hchip-amber",  tags$i(class="fa fa-tree"),       " RF · XGBoost"),
                               tags$span(class="hchip hchip-teal",   tags$i(class="fa fa-bolt"),       " LightGBM · CatBoost"),
                               tags$span(class="hchip hchip-amber",  tags$i(class="fa fa-vector-square")," SVM"),
                               tags$span(class="hchip hchip-green",  tags$i(class="fa fa-layer-group")," AutoML Ensemble"),
                               tags$span(class="hchip hchip-teal",   tags$i(class="fa fa-clock"),      " داده Real-time")
                      )
             )
      )
    ),
    
    # ── کارت‌های آماری ─────────────────────────────────────────────────────
    fluidRow(
      shinydashboard::infoBoxOutput(ns("box_stations"),   width = 3),
      shinydashboard::infoBoxOutput(ns("box_total_days"), width = 3),
      shinydashboard::infoBoxOutput(ns("box_models"),     width = 3),
      shinydashboard::infoBoxOutput(ns("box_years"),      width = 3)
    ),
    
    # ── نمودار روند + میانگین ماهانه ──────────────────────────────────────
    fluidRow(
      shinydashboard::box(
        title = tags$span(
          tags$i(class="fa fa-chart-line", style="margin-left:7px;color:var(--blue2);"),
          "روند دمای تاریخی — همه ایستگاه‌ها"
        ),
        width = 8, solidHeader = FALSE,
        tags$div(style="margin-bottom:10px;display:flex;gap:8px;align-items:center;",
                 tags$span(style="font-size:12px;color:var(--text3);", "میانگین ماهانه دما · ۲۰۲۱–۲۰۲۵"),
                 tags$span(
                   style="background:var(--input-bg);color:var(--blue2);border:1px solid var(--border);border-radius:4px;padding:3px 8px;font-size:11px;font-weight:700;",
                   "Open-Meteo API"
                 )
        ),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("temp_overview_plot"), height = "300px"),
          type = 8, color = "#3b82f6", size = 0.6
        )
      ),
      
      shinydashboard::box(
        title = tags$span(
          tags$i(class="fa fa-chart-bar", style="margin-left:7px;color:#fbbf24;"),
          "میانگین ماهانه"
        ),
        width = 4, solidHeader = FALSE,
        selectInput(
          ns("monthly_station"),
          label   = "ایستگاه:",
          choices = NULL,
          width   = "100%"
        ),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("monthly_avg_plot"), height = "240px"),
          type = 8, color = "#f59e0b", size = 0.6
        )
      )
    ),
    
    # ── جدول ایستگاه‌ها + روش‌شناسی ─────────────────────────────────────────
    fluidRow(
      shinydashboard::box(
        title = tags$span(
          tags$i(class="fa fa-map-location-dot", style="margin-left:7px;color:#2dd4bf;"),
          "ایستگاه‌های هواشناسی"
        ),
        width = 7, solidHeader = FALSE,
        tags$div(style="margin-bottom:10px;font-size:12px;color:var(--text3);",
                 "داده روزانه · ۲۰۲۱–۲۰۲۵ · بدون نشت اطلاعات در تقسیم train/test"
        ),
        DT::DTOutput(ns("station_summary_table"))
      ),
      
      shinydashboard::box(
        title = tags$span(
          tags$i(class="fa fa-route", style="margin-left:7px;color:#a78bfa;"),
          "روش‌شناسی پژوهش"
        ),
        width = 5, solidHeader = FALSE,
        tags$div(
          style = "display:flex;flex-direction:column;gap:0;",
          
          homeStep("1", "fa-download",   "#3b82f6", "جمع‌آوری داده",
                   "دریافت داده‌های هواشناسی از Open-Meteo API: دما، رطوبت، باد، بارش"),
          homeStep("2", "fa-broom",      "#14b8a6", "پیش‌پردازش",
                   "پر کردن مقادیر گمشده، تجمیع ساعتی→روزانه، نرمال‌سازی"),
          homeStep("3", "fa-scissors",   "#8b5cf6", "تقسیم داده",
                   "۸۵٪ آموزش / ۱۵٪ آزمون — تقسیم زمانی (بدون نشت)"),
          homeStep("4", "fa-gears",      "#f59e0b", "آموزش مدل‌ها",
                   "آموزش ۱۱ مدل با پارامترهای بهینه + Ensemble AutoML"),
          homeStep("5", "fa-ruler-combined", "#22c55e", "ارزیابی",
                   "محاسبه RMSE، MAE، MAPE، R²، SMAPE و نمره ترکیبی")
        )
      )
    ),
    

    
    # ── مدل‌ها + توزیع متغیرها ───────────────────────────────────────────────
    fluidRow(
      shinydashboard::box(
        title = tags$span(
          tags$i(class="fa fa-brain", style="margin-left:7px;color:#a78bfa;"),
          "مدل‌های پیش‌بینی — خلاصه"
        ),
        width = 7, solidHeader = FALSE,
        tags$div(
          style = "display:grid;grid-template-columns:1fr 1fr;gap:10px;",
          
          modelMiniCard("ARIMA / SARIMA", "fa-wave-square", "#3b82f6",
                        "کلاسیک · Box-Jenkins",
                        "سری زمانی ایستا یا قابل ایستاسازی با تفاضل"),
          modelMiniCard("ETS", "fa-chart-area", "#14b8a6",
                        "Exponential Smoothing",
                        "مدل‌سازی روند و فصلی‌بودن چندگانه"),
          modelMiniCard("Prophet", "fa-leaf", "#8b5cf6",
                        "Bayesian · Meta AI",
                        "تغییر روند و فصلی‌بودن با سری فوریه"),
          modelMiniCard("XGBoost · RF · LGBM · CatBoost", "fa-tree", "#f59e0b",
                        "یادگیری ماشین (ML)",
                        "ویژگی‌های lag و rolling-window با Early Stopping")
        ),
        tags$div(
          style = "margin-top:10px;background:rgba(34,197,94,0.05);border:1px solid rgba(34,197,94,0.2);border-radius:8px;padding:12px 14px;",
          tags$div(style="display:flex;align-items:center;gap:8px;margin-bottom:6px;",
                   tags$div(style="width:8px;height:8px;border-radius:50%;background:#22c55e;"),
                   tags$span(style="font-size:14px;font-weight:800;color:var(--text);", "AutoML Ensemble (وزن‌دار)"),
                   tags$span(style="margin-right:auto;background:rgba(34,197,94,0.15);color:#4ade80;border-radius:4px;padding:3px 8px;font-size:11px;font-weight:700;", "بهترین عملکرد کلی")
          ),
          tags$div(
            class = "formula-box",
            style = "font-size:14px;margin:4px 0;",
            "ŷ = Σ(w_i × ŷ_i)   |   w_i = exp(-β × RMSE_i) / Σ exp(-β × RMSE_j)"
          ),
          tags$div(style="font-size:13px;color:var(--text2);",
                   "ترکیب وزن‌دار بهترین مدل‌ها بر اساس نمره ترکیبی"
          )
        )
      ),
      
      shinydashboard::box(
        title = tags$span(
          tags$i(class="fa fa-chart-violin", style="margin-left:7px;color:#4ade80;"),
          "توزیع متغیرهای اقلیمی"
        ),
        width = 5, solidHeader = FALSE,
        selectInput(
          ns("dist_variable"),
          label   = "متغیر:",
          choices = c(
            "دما (°C)"        = "temperature",
            "رطوبت (%)"       = "humidity",
            "سرعت باد (km/h)" = "wind_speed",
            "بارش (mm)"       = "precipitation"
          ),
          width = "100%"
        ),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("dist_plot"), height = "280px"),
          type = 8, color = "#22c55e", size = 0.6
        )
      )
    )
  )
}

# ── Helper UI Functions ────────────────────────────────────────────────────────

homeStep <- function(num, icon_name, color, title, desc) {
  tags$div(
    style = "display:flex;gap:12px;padding:10px 0;border-bottom:1px solid var(--border);",
    tags$div(
      style = paste0(
        "width:30px;height:30px;border-radius:50%;flex-shrink:0;",
        "background:", gsub("var\\(--blue\\)", "#3b82f6", color), "22;",
        "border:1.5px solid ", color, ";",
        "display:flex;align-items:center;justify-content:center;",
        "color:", color, ";font-size:12px;"
      ),
      tags$i(class = paste("fa", icon_name))
    ),
    tags$div(
      tags$div(style="font-size:13px;font-weight:700;color:var(--text);margin-bottom:2px;",
               paste0("گام ", num, " — ", title)),
      tags$div(style="font-size:12px;color:var(--text2);line-height:1.5;", desc)
    )
  )
}

modelMiniCard <- function(name, icon_name, color, type_label, desc) {
  tags$div(
    style = paste0(
      "background:var(--input-bg);border:1px solid var(--border);",
      "border-radius:8px;padding:12px 14px;border-right:3px solid ", color, ";"
    ),
    tags$div(
      style = "display:flex;align-items:center;gap:7px;margin-bottom:6px;",
      tags$i(class = paste("fa", icon_name),
             style = paste0("color:", color, ";font-size:14px;")),
      tags$span(style="font-size:14px;font-weight:800;color:var(--text);", name)
    ),
    tags$div(style=paste0("font-size:11px;font-weight:700;color:", color,
                          ";background:", color, "22;display:inline-block;",
                          "padding:3px 8px;border-radius:4px;margin-bottom:6px;"),
             type_label),
    tags$div(style="font-size:12px;color:var(--text2);line-height:1.5;", desc)
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
homeServer <- function(id, weather_data, hourly_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # داده ترکیبی
    combined_data <- reactive({
      req(weather_data())
      dplyr::bind_rows(
        purrr::imap(weather_data(), function(df, sid) {
          df$station_id   <- sid
          df$station_name <- STATIONS[[sid]]$name %||% sid
          df
        })
      )
    })
    
    # ── کارت‌های Info ────────────────────────────────────────────────────────
    output$box_stations <- shinydashboard::renderInfoBox({
      req(weather_data())
      shinydashboard::infoBox(
        title    = "ایستگاه هواشناسی",
        value    = length(weather_data()),
        subtitle = "تهران · اصفهان · مشهد · شیراز · تبریز",
        icon     = icon("location-dot"),
        color    = "aqua",
        fill     = TRUE
      )
    })
    
    output$box_total_days <- shinydashboard::renderInfoBox({
      req(combined_data())
      
      # محاسبه تعداد واقعی رکوردها از داده‌های ساعتی (معادل فایل CSV)
      total_records <- nrow(combined_data()) # پیش‌فرض: داده روزانه
      
      if (!is.null(hourly_data)) {
        hd <- tryCatch(hourly_data(), error = function(e) NULL)
        if (!is.null(hd)) {
          total_records <- sum(sapply(hd, function(x) if(is.data.frame(x)) nrow(x) else 0), na.rm = TRUE)
        }
      } else if (exists("WEATHER_DATA_HOURLY", envir = globalenv())) {
        # اگر پاس داده نشد، از محیط سراسری بگیرد
        hd <- get("WEATHER_DATA_HOURLY", envir = globalenv())
        total_records <- sum(sapply(hd, function(x) if(is.data.frame(x)) nrow(x) else 0), na.rm = TRUE)
      }
      
      total <- format(total_records, big.mark = ",")
      shinydashboard::infoBox(
        title    = "کل رکورد داده",
        value    = total,
        subtitle = "رکورد ساعتی (۵ ایستگاه × ۲۴ ساعت)",
        icon     = icon("database"),
        color    = "green",
        fill     = TRUE
      )
    })
    
    output$box_models <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        title    = "مدل پیش‌بینی",
        value    = "۱۱",
        subtitle = "کلاسیک + ML + مدرن + Ensemble",
        icon     = icon("brain"),
        color    = "yellow",
        fill     = TRUE
      )
    })
    
    output$box_years <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        title    = "بازه زمانی داده",
        value    = "۴ سال",
        subtitle = "۲۰۲۱–۲۰۲۵ · داده روزانه",
        icon     = icon("calendar-days"),
        color    = "red",
        fill     = TRUE
      )
    })
    
    # ── ایستگاه‌ها ─────────────────────────────────────────────────────────
    observe({
      req(weather_data())
      choices <- purrr::set_names(
        names(weather_data()),
        purrr::map_chr(names(weather_data()), ~ STATIONS[[.x]]$name %||% .x)
      )
      updateSelectInput(session, "monthly_station", choices = choices)
    })
    
    # ── نمودار روند تاریخی ───────────────────────────────────────────────────
    STATION_COLORS <- c(
      "#3b82f6", "#14b8a6", "#8b5cf6", "#f59e0b", "#ef4444"
    )
    
    output$temp_overview_plot <- plotly::renderPlotly({
      req(combined_data())
      df <- combined_data()
      
      df_monthly <- df %>%
        dplyr::mutate(month_date = lubridate::floor_date(date, "month")) %>%
        dplyr::group_by(station_id, station_name, month_date) %>%
        dplyr::summarise(temperature = mean(temperature, na.rm = TRUE), .groups = "drop")
      
      sids   <- unique(df_monthly$station_id)
      p <- plotly::plot_ly()
      
      for (i in seq_along(sids)) {
        sid   <- sids[i]
        sdf   <- dplyr::filter(df_monthly, station_id == sid)
        color <- STATION_COLORS[i]
        p <- plotly::add_lines(p,
                               data  = sdf,
                               x     = ~month_date,
                               y     = ~temperature,
                               name  = unique(sdf$station_name),
                               line  = list(color = color, width = 2),
                               hovertemplate = paste0("<b>%{text}</b><br>تاریخ: %{x|%Y-%m}<br>دما: %{y:.1f}°C<extra></extra>"),
                               text  = unique(sdf$station_name)
        )
      }
      
      p %>% plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font          = list(family = "Vazirmatn, Tahoma", color = "#94a3b8", size = 12),
        xaxis = list(
          title      = "",
          gridcolor  = "rgba(99,143,232,0.08)",
          linecolor  = "rgba(99,143,232,0.15)",
          tickfont   = list(size = 11, color = "#94a3b8")
        ),
        yaxis = list(
          title      = "دما (°C)",
          gridcolor  = "rgba(99,143,232,0.08)",
          linecolor  = "rgba(99,143,232,0.15)",
          tickfont   = list(size = 11, color = "#94a3b8"),
          ticksuffix = "°"
        ),
        legend    = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center",
                         font = list(size = 12, color = "#94a3b8")),
        hovermode = "x unified",
        margin    = list(l = 50, r = 10, t = 10, b = 40),
        shapes = list(list(
          type = "line",
          x0 = "2023-01-01", x1 = "2023-01-01",
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "rgba(99,143,232,0.3)", width = 1, dash = "dot")
        ))
      ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # ── میانگین ماهانه ───────────────────────────────────────────────────────
    MONTH_NAMES_FA <- c("فروردین","اردیبهشت","خرداد","تیر","مرداد","شهریور",
                        "مهر","آبان","آذر","دی","بهمن","اسفند")
    
    output$monthly_avg_plot <- plotly::renderPlotly({
      req(input$monthly_station, weather_data())
      sid <- input$monthly_station
      req(sid %in% names(weather_data()))
      df  <- weather_data()[[sid]]
      
      monthly <- df %>%
        dplyr::mutate(month_num = lubridate::month(date)) %>%
        dplyr::group_by(month_num) %>%
        dplyr::summarise(
          avg_temp = mean(temperature, na.rm = TRUE),
          min_temp = mean(temperature - stats::sd(temperature, na.rm = TRUE), na.rm = TRUE),
          max_temp = mean(temperature + stats::sd(temperature, na.rm = TRUE), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(month_label = MONTH_NAMES_FA[month_num])
      
      bar_colors <- ifelse(monthly$avg_temp >= 20, "#ef4444",
                           ifelse(monthly$avg_temp >= 10, "#f59e0b", "#3b82f6"))
      
      plotly::plot_ly(monthly,
                      x    = ~month_label,
                      y    = ~avg_temp,
                      type = "bar",
                      marker = list(
                        color  = bar_colors,
                        line   = list(color = "rgba(0,0,0,0)", width = 0)
                      ),
                      error_y = list(
                        type      = "data",
                        symmetric = FALSE,
                        array     = ~(max_temp - avg_temp),
                        arrayminus = ~(avg_temp - min_temp),
                        color     = "rgba(255,255,255,0.3)",
                        thickness = 1.5,
                        width     = 4
                      ),
                      hovertemplate = "<b>%{x}</b><br>میانگین: %{y:.1f}°C<extra></extra>"
      ) %>%
        plotly::layout(
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor  = "rgba(0,0,0,0)",
          font   = list(family = "Vazirmatn, Tahoma", color = "#94a3b8", size = 11),
          xaxis  = list(title = "", gridcolor = "transparent",
                        tickfont = list(size = 10, color = "#94a3b8")),
          yaxis  = list(title = "°C", gridcolor = "rgba(99,143,232,0.08)",
                        tickfont = list(size = 10, color = "#94a3b8"),
                        ticksuffix = "°"),
          bargap = 0.25,
          margin = list(l = 40, r = 5, t = 5, b = 40)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # ── جدول ایستگاه‌ها ──────────────────────────────────────────────────────
    output$station_summary_table <- DT::renderDT({
      req(weather_data())
      
      summaries <- purrr::imap_dfr(weather_data(), function(df, sid) {
        s <- station_summary(df)
        tibble::tibble(
          "ایستگاه"       = STATIONS[[sid]]$name %||% sid,
          "تعداد روز"     = format(s$n_days, big.mark = ","),
          "از"            = as.character(s$date_range[1]),
          "تا"            = as.character(s$date_range[2]),
          "میانگین دما"   = paste0(s$temp_mean, " °C"),
          "min/max"       = paste0(s$temp_min, " / ", s$temp_max),
          "رطوبت میانگین" = paste0(s$humidity_avg, " %"),
          "کل بارش"       = paste0(s$precip_total, " mm")
        )
      })
      
      DT::datatable(
        summaries,
        options = list(
          pageLength = 10, dom = "t",
          scrollX    = TRUE,
          columnDefs = list(list(className = "dt-right", targets = "_all"))
        ),
        rownames  = FALSE,
        class     = "cell-border stripe hover",
        selection = "none"
      ) %>%
        DT::formatStyle(
          "ایستگاه",
          fontWeight = "700",
          color      = "var(--text)"
        ) %>%
        DT::formatStyle(
          "میانگین دما",
          color = DT::styleInterval(
            c(12, 18),
            c("#60a5fa", "#fbbf24", "#ef4444")
          )
        )
    })
    
    # ── توزیع متغیرها ────────────────────────────────────────────────────────
    VAR_COLORS <- c(
      temperature   = "#ef4444",
      humidity      = "#3b82f6",
      wind_speed    = "#8b5cf6",
      precipitation = "#14b8a6"
    )
    
    output$dist_plot <- plotly::renderPlotly({
      req(combined_data(), input$dist_variable)
      df  <- combined_data()
      var <- input$dist_variable
      col <- VAR_COLORS[[var]]
      
      sids  <- unique(df$station_id)
      p     <- plotly::plot_ly()
      
      for (i in seq_along(sids)) {
        sid   <- sids[i]
        sdf   <- dplyr::filter(df, station_id == sid)
        sname <- unique(sdf$station_name)
        vals  <- sdf[[var]]
        vals  <- vals[!is.na(vals)]
        
        p <- plotly::add_trace(p,
                               type   = "violin",
                               y      = vals,
                               name   = sname,
                               box    = list(visible = TRUE),
                               meanline = list(visible = TRUE, color = col),
                               line   = list(color = STATION_COLORS[i]),
                               fillcolor = paste0(
                                 substr(STATION_COLORS[i], 1, 7),
                                 "33"
                               ),
                               hovertemplate = paste0("<b>", sname, "</b><br>",
                                                      var, ": %{y:.1f}<extra></extra>")
        )
      }
      
      var_unit <- switch(var,
                         temperature   = "°C",
                         humidity      = "%",
                         wind_speed    = "km/h",
                         precipitation = "mm"
      )
      
      p %>% plotly::layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font    = list(family = "Vazirmatn, Tahoma", color = "#94a3b8", size = 11),
        xaxis   = list(title = "", gridcolor = "transparent",
                       tickfont = list(size = 11, color = "#94a3b8")),
        yaxis   = list(title = var_unit,
                       gridcolor = "rgba(99,143,232,0.08)",
                       tickfont  = list(size = 11, color = "#94a3b8")),
        showlegend = FALSE,
        violingap  = 0.2,
        margin     = list(l = 45, r = 10, t = 10, b = 35)
      ) %>%
        plotly::config(displayModeBar = FALSE)
    })
  })
}