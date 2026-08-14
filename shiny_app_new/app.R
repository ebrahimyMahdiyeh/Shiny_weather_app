# File: app.R — نسخه ۴ (Dark/Light mode + سایدبار راست)

# ── اصلاح locale برای پشتیبانی فارسی (UTF-8) ────────────────────────────────د
tryCatch({
  if (.Platform$OS.type == "windows") {
    locs <- c("Persian_Iran.65001", "English_United States.65001",
              "English_United States.1252", "C")
  } else {
    locs <- c("en_US.UTF-8", "C.UTF-8", "fa_IR.UTF-8", "C")
  }
  for (loc in locs) {
    ok <- tryCatch({
      suppressWarnings(Sys.setlocale("LC_ALL", loc))
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) {
      cur <- tryCatch(Sys.getlocale("LC_CTYPE"), error = function(e) "")
      if (grepl("65001|UTF-8|utf8|utf-8", cur, ignore.case = TRUE)) {
        message("✓ Locale set to: ", loc)
        break
      }
    }
  }
}, error = function(e) {
  message("Warning: could not set locale - ", e$message)
})

# ── فعال‌سازی encoding پیش‌فرض UTF-8 ─────────────────────────────────────────
options(encoding = "UTF-8")

# ── اطمینان از بارگذاری global.R ────────────────────────────────────────────
source("global.R", chdir = TRUE, local = FALSE, encoding = "UTF-8")

# ══════════════════════════════════════════════════════════════════════════════
# CSS — متغیرهای تم (dark & light)
# ══════════════════════════════════════════════════════════════════════════════
CUSTOM_CSS <- "
@import url('https://fonts.googleapis.com/css2?family=Vazirmatn:wght@300;400;500;600;700;800;900&display=swap');

*,*::before,*::after { box-sizing:border-box; }

/* ── Dark mode (پیش‌فرض) ── */
:root {
  --bg:        #0a0e1a;
  --bg2:       #0d1326;
  --panel:     #111827;
  --panel2:    #1e2d45;
  --border:    rgba(99,143,232,0.15);
  --border2:   rgba(99,143,232,0.28);
  --text:      #e2e8f0;
  --text2:     #94a3b8;
  --text3:     #64748b;
  --blue:      #3b82f6;
  --blue2:     #60a5fa;
  --teal:      #14b8a6;
  --amber:     #f59e0b;
  --red:       #ef4444;
  --green:     #22c55e;
  --purple:    #8b5cf6;
  --radius:    10px;
  --shadow:    0 2px 12px rgba(0,0,0,0.4);
  --input-bg:  rgba(255,255,255,0.04);
  --hover-bg:  rgba(59,130,246,0.08);
  --active-bg: rgba(59,130,246,0.15);
  --hero-grad: linear-gradient(135deg,rgba(59,130,246,0.08),rgba(20,184,166,0.05),rgba(139,92,246,0.05));
}

/* ── Light mode ── */
body.light-mode {
  --bg:        #f0f4f8;
  --bg2:       #e8edf5;
  --panel:     #ffffff;
  --panel2:    #f7f9fc;
  --border:    rgba(99,143,232,0.18);
  --border2:   rgba(99,143,232,0.32);
  --text:      #1e293b;
  --text2:     #475569;
  --text3:     #94a3b8;
  --shadow:    0 2px 12px rgba(0,0,0,0.08);
  --input-bg:  rgba(0,0,0,0.03);
  --hover-bg:  rgba(59,130,246,0.06);
  --active-bg: rgba(59,130,246,0.10);
  --hero-grad: linear-gradient(135deg,rgba(59,130,246,0.06),rgba(20,184,166,0.04),rgba(139,92,246,0.04));
}

/* ══ BASE ══ */
html, body {
  font-family: 'Vazirmatn', Tahoma, sans-serif !important;
  direction: rtl;
  background: var(--bg) !important;
  color: var(--text) !important;
  transition: background 0.25s, color 0.25s;
}
body, .content-wrapper, .main-footer {
  font-family: 'Vazirmatn', Tahoma, sans-serif !important;
  background: var(--bg) !important;
  color: var(--text) !important;
}

/* ══ HEADER ══ */
.main-header .logo {
  background: var(--panel) !important;
  border-bottom: 1px solid var(--border) !important;
  border-right: 1px solid var(--border) !important;
  font-family: 'Vazirmatn', Tahoma, sans-serif !important;
  font-weight: 800; font-size: 15px;
  color: var(--text) !important;
  display: flex;
  align-items: center;
}
.main-header .navbar {
  background: var(--panel) !important;
  border-bottom: 1px solid var(--border) !important;
  position: relative;
  min-height: 50px;
}
.main-header .navbar .sidebar-toggle {
  position: absolute !important;
  top: 0 !important;
  right: 0 !important;
  left: auto !important;
  float: none !important;
  color: var(--text2) !important;
  height: 50px;
  display: flex !important;
  align-items: center;
  padding: 0 15px !important;
}
.main-header .navbar .sidebar-toggle:hover { background: var(--hover-bg) !important; }
.main-header .navbar-custom-menu,
.main-header .navbar-nav,
.main-header .navbar > .container-fluid {
  margin-right: 50px;
  height: 50px;
  display: flex;
  align-items: center;
}
.main-header .navbar-custom-menu .dropdown {
  height: 50px !important;
  display: flex !important;
  align-items: center !important;
  font-family: 'Vazirmatn', Tahoma, sans-serif !important;
}
.theme-toggle-btn {
  display: inline-flex; align-items: center; gap: 6px;
  padding: 6px 14px; border-radius: 20px; cursor: pointer;
  font-family: 'Vazirmatn', Tahoma, sans-serif;
  font-size: 13px; font-weight: 700;
  background: var(--input-bg); color: var(--text2);
  border: 1px solid var(--border);
  transition: all 0.2s; white-space: nowrap;
}
.theme-toggle-btn:hover { background: var(--hover-bg); color: var(--text); }

/* ══ SIDEBAR (راست) ══ */
.main-sidebar {
  background: var(--panel) !important;
  border-right: none !important;
  border-left: 1px solid var(--border) !important;
  right: 0 !important;
  left: auto !important;
  transform: none !important;
  -webkit-transform: none !important;
  transition: right 0.3s ease-in-out !important;
}
.wrapper { direction: rtl !important; }
.content-wrapper, .main-footer {
  margin-right: 230px !important;
  margin-left: 0 !important;
  transition: margin-right 0.3s ease-in-out;
}
.sidebar-collapse .main-sidebar {
  right: -230px !important;
  left: auto !important;
  transform: none !important;
  -webkit-transform: none !important;
  -ms-transform: none !important;
  margin-left: 0 !important;
  margin-right: 0 !important;
}
.sidebar-collapse .content-wrapper,
.sidebar-collapse .main-footer {
  margin-right: 0 !important;
  margin-left: 0 !important;
  transform: none !important;
  -webkit-transform: none !important;
}
.sidebar-collapse .main-sidebar[style] {
  right: -230px !important;
  left: auto !important;
  transform: none !important;
}
@media (max-width: 767px) {
  .content-wrapper, .main-footer { margin-right: 0 !important; }
  .main-sidebar { right: -230px; transition: right 0.3s; }
  .sidebar-open .main-sidebar { right: 0; }
}
body.light-mode .main-sidebar { box-shadow: -2px 0 10px rgba(0,0,0,0.06) !important; }

.sidebar-menu > li > a {
  color: var(--text2) !important;
  font-family: 'Vazirmatn', Tahoma, sans-serif !important;
  font-size: 14px; border-radius: 8px;
  margin: 2px 8px; padding: 10px 13px !important;
  transition: all 0.18s; display: flex; align-items: center; gap: 8px;
}
.sidebar-menu > li > a:hover { background: var(--hover-bg) !important; color: var(--text) !important; }
.sidebar-menu > li.active > a,
.sidebar-menu > li.active > a:hover {
  background: var(--active-bg) !important;
  color: var(--blue2) !important; font-weight: 700;
}
.sidebar-menu > li > a > .fa { width: 16px; margin-left: 6px; }
.sidebar .sidebar-menu .header {
  color: var(--text3) !important; font-size: 11px;
  letter-spacing: 1.4px; font-weight: 800; padding: 14px 16px 5px;
  text-transform: uppercase;
}
.treeview-menu { background: transparent !important; }

/* ══ CONTENT ══ */
.content-wrapper { background: var(--bg) !important; padding: 20px !important; }
.content-header  { display: none; }

/* ══ BOX ══ */
.box {
  background: var(--panel) !important;
  border: 1px solid var(--border) !important;
  border-radius: var(--radius) !important;
  border-top: none !important;
  box-shadow: var(--shadow) !important;
}
.box.box-primary  { border-top: 3px solid var(--blue) !important; }
.box.box-info     { border-top: 3px solid var(--teal) !important; }
.box.box-success  { border-top: 3px solid var(--green) !important; }
.box.box-warning  { border-top: 3px solid var(--amber) !important; }
.box.box-danger   { border-top: 3px solid var(--red) !important; }
.box-header {
  background: transparent !important;
  border-bottom: 1px solid var(--border) !important;
  color: var(--text) !important;
}
.box-header .box-title {
  font-family: 'Vazirmatn', Tahoma, sans-serif !important;
  font-size: 15px; font-weight: 700; color: var(--text) !important;
}

/* ══ VALUE / INFO BOX ══ */
.value-box {
  border-radius: var(--radius) !important;
  border: 1px solid var(--border) !important; box-shadow: none !important;
}
.value-box.bg-blue   { background: linear-gradient(135deg,#1d4ed8,#2563eb) !important; }
.value-box.bg-green  { background: linear-gradient(135deg,#15803d,#16a34a) !important; }
.value-box.bg-yellow { background: linear-gradient(135deg,#b45309,#d97706) !important; }
.value-box.bg-red    { background: linear-gradient(135deg,#b91c1c,#dc2626) !important; }
.value-box .value-box-number { font-family:'Vazirmatn',Tahoma,sans-serif !important; font-size:24px; font-weight:900; }
.value-box .value-box-text   { font-family:'Vazirmatn',Tahoma,sans-serif !important; font-size:14px; }
.info-box { background: var(--panel) !important; border: 1px solid var(--border) !important; border-radius: var(--radius) !important; box-shadow: none !important; }
.info-box-number { font-family:'Vazirmatn',Tahoma,sans-serif !important; font-weight:900 !important; }
.info-box-text   { font-family:'Vazirmatn',Tahoma,sans-serif !important; }
.bg-aqua   { background: linear-gradient(135deg,#0e7490,#0891b2) !important; }
.bg-green  { background: linear-gradient(135deg,#15803d,#16a34a) !important; }
.bg-yellow { background: linear-gradient(135deg,#b45309,#d97706) !important; }
.bg-red    { background: linear-gradient(135deg,#b91c1c,#dc2626) !important; }

/* ══ INPUTS ══ */
.form-control, select.form-control {
  background: var(--input-bg) !important;
  border: 1px solid var(--border) !important;
  border-radius: 7px !important;
  color: var(--text) !important;
  font-family: 'Vazirmatn', Tahoma, sans-serif !important;
  font-size: 14px !important;
  transition: border-color 0.18s, background 0.18s;
}
.form-control:focus { border-color: var(--blue) !important; box-shadow: 0 0 0 2px rgba(59,130,246,0.12) !important; }
.control-label { color: var(--text3) !important; font-size: 12px !important; font-weight: 700 !important; text-transform: uppercase; letter-spacing: 0.7px; }
select option  { background: var(--panel2) !important; color: var(--text) !important; }

.irs-bar,.irs-bar-edge { background: var(--blue) !important; border-color: var(--blue) !important; }
.irs-single,.irs-from,.irs-to { background: var(--blue) !important; font-family:'Vazirmatn',Tahoma,sans-serif !important; }
.irs-line   { background: var(--border2) !important; border: none !important; }
.irs-min,.irs-max,.irs-grid-text { color: var(--text3) !important; background: transparent !important; font-size: 12px; }

/* ══ BUTTONS ══ */
.btn {
  font-family: 'Vazirmatn', Tahoma, sans-serif !important;
  border-radius: 8px !important; font-weight: 700; transition: all 0.18s;
}
.btn-primary {
  background: linear-gradient(135deg,var(--blue),#2563eb) !important;
  border: none !important; box-shadow: 0 3px 12px rgba(59,130,246,0.22) !important;
}
.btn-primary:hover { transform: translateY(-1px); box-shadow: 0 5px 16px rgba(59,130,246,0.35) !important; }
.btn-success {
  background: linear-gradient(135deg,var(--green),#16a34a) !important;
  border: none !important; box-shadow: 0 3px 12px rgba(34,197,94,0.18) !important;
}
.btn-success:hover { transform: translateY(-1px); box-shadow: 0 5px 16px rgba(34,197,94,0.32) !important; }
.btn-danger {
  background: linear-gradient(135deg,var(--red),#b91c1c) !important;
  border: none !important; box-shadow: 0 3px 12px rgba(239,68,68,0.18) !important;
}
.btn-danger:hover { transform: translateY(-1px); box-shadow: 0 5px 16px rgba(239,68,68,0.32) !important; }
.btn-danger:focus, .btn-success:focus { outline: none !important; }
.btn-default {
  background: var(--input-bg) !important;
  border: 1px solid var(--border) !important; color: var(--text2) !important;
}
.btn-default:hover { background: var(--hover-bg) !important; color: var(--text) !important; }

/* ══ کارت‌های دانلود گزارش ══ */
.report-dl-card {
  background: var(--panel2); border: 1px dashed var(--border2); border-radius: var(--radius);
  padding: 28px 20px; text-align: center; transition: transform .18s ease, border-color .18s ease, box-shadow .18s ease;
  height: 100%; display: flex; flex-direction: column; align-items: center; justify-content: space-between; gap: 10px;
}
.report-dl-card:hover { transform: translateY(-2px); border-color: var(--border2); box-shadow: 0 8px 20px rgba(0,0,0,0.22); }
body.light-mode .report-dl-card:hover { box-shadow: 0 8px 20px rgba(30,41,59,0.10); }
.report-dl-card .dl-icon { font-size: 2.6em; }
.report-dl-card .dl-title { font-size: 16px; font-weight: 800; color: var(--text); margin: 2px 0 0; }
.report-dl-card .dl-desc { font-size: 13px; color: var(--text3); line-height: 1.6; margin: 0; }
.report-dl-card .shiny-download-link { margin-top: 4px; }

.rc-sec-title { font-size: 13px; font-weight: 800; color: var(--text2); text-transform: uppercase; letter-spacing: .6px; margin: 2px 0 9px; display: flex; align-items: center; }
.rc-chip-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 7px; }
.rc-chip-grid .shiny-input-checkbox:not(.shiny-input-container) { display: flex; }
.rc-chip-grid .form-group, .rc-chip-grid .shiny-input-checkbox { margin: 0 !important; display: flex !important; align-items: center; }
.rc-chip-grid .checkbox { margin: 0 !important; width: 100%; }
.rc-chip-grid .checkbox label {
  display: flex !important; align-items: center; gap: 7px;
  width: 100%; margin: 0 !important; padding: 10px 12px;
  background: var(--panel); border: 1px solid var(--border); border-radius: 8px;
  font-size: 13px; font-weight: 700; color: var(--text2); cursor: pointer;
  transition: all .16s ease; min-height: 38px; box-sizing: border-box;
}
.rc-chip-grid .checkbox label:hover { border-color: var(--border2); color: var(--text); background: var(--hover-bg); }
.rc-chip-grid .checkbox label:has(input:checked) { border-color: var(--blue); background: rgba(59,130,246,0.10); color: var(--text); box-shadow: 0 0 0 1px rgba(59,130,246,0.25); }
.rc-chip-grid input[type='checkbox'] {
  appearance: none !important; -webkit-appearance: none !important;
  width: 17px !important; height: 17px !important; border-radius: 5px !important;
  border: 1.5px solid var(--border2) !important; background: transparent !important;
  margin: 0 !important; position: relative !important; flex-shrink: 0 !important; cursor: pointer;
  transition: all .16s ease;
}
.rc-chip-grid input[type='checkbox']:checked { background: var(--blue) !important; border-color: var(--blue) !important; }
.rc-chip-grid input[type='checkbox']:checked::after {
  content: '\2713'; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);
  color: #fff; font-size: 11px; font-weight: 900; line-height: 1;
}

/* ══ DATATABLES ══ */
.dataTables_wrapper { color: var(--text) !important; }
table.dataTable { border-collapse: collapse !important; background: transparent !important; }
table.dataTable thead th {
  background: var(--panel2) !important; color: var(--text3) !important;
  border-bottom: 1px solid var(--border) !important;
  font-family: 'Vazirmatn',Tahoma,sans-serif !important;
  font-size: 13px !important; font-weight: 700 !important;
  letter-spacing: 0.5px; text-transform: uppercase; padding: 10px 12px !important;
}
table.dataTable tbody tr { background: transparent !important; }
table.dataTable tbody tr td {
  color: var(--text2) !important;
  border-bottom: 1px solid rgba(99,143,232,0.06) !important;
  font-family: 'Vazirmatn',Tahoma,sans-serif !important; font-size: 14px; padding: 10px 12px !important;
}
table.dataTable tbody tr:hover td { background: var(--hover-bg) !important; color: var(--text) !important; }
.dataTables_info,.dataTables_length label,.dataTables_filter label { color: var(--text3) !important; font-size: 12px; }
.dataTables_paginate .paginate_button { color: var(--text2) !important; }
.dataTables_paginate .paginate_button.current { background: var(--blue) !important; color: white !important; border-radius: 4px !important; }

/* ══ PLOTLY ══ */
.js-plotly-plot .plotly,.js-plotly-plot .plotly .main-svg { background: transparent !important; }
.plotly .modebar { background: var(--panel2) !important; border-radius: 5px !important; }

/* ══ SCROLLBAR ══ */
::-webkit-scrollbar { width: 4px; height: 4px; }
::-webkit-scrollbar-track { background: transparent; }
::-webkit-scrollbar-thumb { background: rgba(99,143,232,0.2); border-radius: 2px; }
::-webkit-scrollbar-thumb:hover { background: rgba(99,143,232,0.4); }

/* ══ NOTIFICATION ══ */
#shiny-notification-panel { bottom: 20px; right: 20px; left: auto; width: 300px; }
.shiny-notification {
  background: var(--panel) !important; border: 1px solid var(--border) !important;
  color: var(--text) !important; border-radius: 8px !important;
  font-family: 'Vazirmatn',Tahoma,sans-serif !important;
}

/* ══ PROGRESS ══ */
.progress-bar { background: var(--blue) !important; }
.shiny-progress-container { background: var(--panel) !important; border: 1px solid var(--border) !important; border-radius: 8px !important; }

/* ══ HR ══ */
hr { border-color: var(--border) !important; margin: 12px 0; }

/* ══ HOME PAGE ══ */
.hero-banner {
  background: var(--hero-grad);
  border: 1px solid var(--border); border-radius: 14px;
  padding: 26px 30px; margin-bottom: 18px; position: relative; overflow: hidden;
}
.hero-banner::before {
  content:''; position:absolute; top:-50px; left:-50px;
  width:160px; height:160px;
  background: radial-gradient(circle,rgba(59,130,246,0.1) 0%,transparent 70%);
  border-radius: 50%;
}
.hero-banner h2 { font-size:24px; font-weight:900; line-height:1.3; color:var(--text); margin:0 0 9px; }
.hero-banner h2 .grad { background:linear-gradient(90deg,var(--blue2),var(--teal)); -webkit-background-clip:text; -webkit-text-fill-color:transparent; }
.hero-banner p  { font-size:14px; color:var(--text2); line-height:1.7; max-width:700px; margin:0; }
.hero-chips { display:flex; flex-wrap:wrap; gap:6px; margin-top:14px; }
.hchip { display:inline-flex; align-items:center; gap:5px; padding:5px 12px; border-radius:20px; font-size:12px; font-weight:600; }
.hchip-blue   { background:rgba(59,130,246,0.12);  color:#60a5fa; border:1px solid rgba(59,130,246,0.2); }
.hchip-teal   { background:rgba(20,184,166,0.12);  color:#2dd4bf; border:1px solid rgba(20,184,166,0.2); }
.hchip-amber  { background:rgba(245,158,11,0.12);  color:#fbbf24; border:1px solid rgba(245,158,11,0.2); }
.hchip-purple { background:rgba(139,92,246,0.12);  color:#a78bfa; border:1px solid rgba(139,92,246,0.2); }
.hchip-green  { background:rgba(34,197,94,0.12);   color:#4ade80; border:1px solid rgba(34,197,94,0.2); }

/* ══ MODEL CARDS v2 ══ */
.model-card-sci {
  background: var(--panel2); border: 1px solid var(--border); border-radius: 12px;
  padding: 15px 17px 14px; margin-bottom: 12px;
  transition: transform .18s ease, box-shadow .18s ease, border-color .18s ease;
}
.model-card-sci:hover {
  transform: translateY(-2px); border-color: var(--border2);
  box-shadow: 0 10px 22px rgba(0,0,0,0.22);
}
body.light-mode .model-card-sci:hover { box-shadow: 0 10px 22px rgba(30,41,59,0.10); }

.model-card-head { display: flex; align-items: center; gap: 10px; margin-bottom: 9px; }
.model-icon-badge {
  width: 38px; height: 38px; border-radius: 11px; flex-shrink: 0;
  display: flex; align-items: center; justify-content: center;
  box-shadow: 0 3px 10px rgba(0,0,0,0.2);
}
.model-icon-badge i { color: #fff; font-size: 15px; }
.model-icon-badge.ic-blue   { background: linear-gradient(135deg,#3b82f6,#60a5fa); }
.model-icon-badge.ic-amber  { background: linear-gradient(135deg,#d97706,#fbbf24); }
.model-icon-badge.ic-purple { background: linear-gradient(135deg,#7c3aed,#a78bfa); }
.model-icon-badge.ic-green  { background: linear-gradient(135deg,#15803d,#4ade80); }

.model-card-sci h4 {
  font-size: 15px; font-weight: 800; color: var(--text); margin: 0;
  font-family: 'Vazirmatn',Tahoma,sans-serif; display: flex; align-items: center;
  gap: 6px; flex-wrap: wrap; row-gap: 4px;
}
.model-card-sci p, .model-card-sci li { font-size: 13px; color: var(--text2); line-height: 1.65; margin: 0 0 2px; }

.badge-sci { display: inline-block; padding: 3px 9px; border-radius: 20px; font-size: 11px; font-weight: 700; }
.badge-classic { background: rgba(59,130,246,0.14);  color: var(--blue2); }
.badge-ml      { background: rgba(245,158,11,0.14);  color: var(--amber); }
.badge-modern  { background: rgba(139,92,246,0.14);  color: var(--purple); }
.badge-base    { background: rgba(100,116,139,0.14); color: var(--text3); }
.badge-ens     { background: rgba(34,197,94,0.14);   color: var(--green); }

.formula-box {
  background: var(--panel); border: 1px dashed var(--border2); border-radius: 8px;
  padding: 15px 14px 10px; margin: 11px 0 6px; position: relative;
}
.formula-box::before {
  content: 'فرمول'; position: absolute; top: -8px; right: 11px;
  background: var(--panel2); color: var(--text3); font-size: 10px; font-weight: 800;
  letter-spacing: .4px; padding: 0 6px;
}
.formula-box .formula-scroll { overflow-x: auto; direction: ltr; }
.formula-box .katex { color: var(--teal) !important; font-size: 1.05em; }
body.light-mode .formula-box .katex { color: #0d9488 !important; }
.formula-box .katex-display { margin: 0; }
.formula-box .katex-html { direction: ltr; white-space: nowrap; }

.proscons-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 6px 10px; margin-top: 10px; }
.pc-item { display: flex; align-items: flex-start; gap: 6px; font-size: 13px; color: var(--text2); line-height: 1.5; }
.pc-icon {
  width: 15px; height: 15px; border-radius: 50%; flex-shrink: 0; margin-top: 2px;
  display: flex; align-items: center; justify-content: center; font-size: 8px;
}
.pc-icon.good { background: rgba(34,197,94,0.16); color: var(--green); }
.pc-icon.bad  { background: rgba(239,68,68,0.16);  color: var(--red); }
@media (max-width: 1300px) { .proscons-grid { grid-template-columns: 1fr; } }

/* ══ جدول مقایسه سریع ══ */
.compare-wrap { overflow-x: auto; }
.compare-table { width: 100%; border-collapse: collapse; font-size: 13px; min-width: 620px; }
.compare-table th {
  text-align: right; color: var(--text3); font-size: 11px; text-transform: uppercase;
  letter-spacing: .6px; font-weight: 800; padding: 6px 9px; border-bottom: 1px solid var(--border);
}
.compare-table td { padding: 8px 9px; border-bottom: 1px solid rgba(99,143,232,0.06); color: var(--text2); vertical-align: middle; }
.compare-table tr:hover td { background: var(--hover-bg); }
.compare-table td:first-child { display: flex; align-items: center; gap: 7px; font-weight: 800; color: var(--text); white-space: nowrap; }
.compare-table td.mv-cell { text-align: center; }
.mini-dot { width: 7px; height: 7px; border-radius: 50%; flex-shrink: 0; }
.mini-dot.dot-blue   { background: var(--blue); }
.mini-dot.dot-amber  { background: var(--amber); }
.mini-dot.dot-purple { background: var(--purple); }
.mini-dot.dot-green  { background: var(--green); }
.meter { display: inline-flex; gap: 3px; }
.meter .meter-seg { width: 13px; height: 6px; border-radius: 2px; background: var(--border2); }
.meter .meter-seg.on { background: var(--blue2); }

/* ══ کارت معیارها + نوار وزن ══ */
.metric-card { background: var(--panel2); border: 1px solid var(--border); border-radius: 10px; padding: 12px 14px 13px; margin-bottom: 9px; }
.metric-card-head { display: flex; align-items: center; gap: 9px; margin-bottom: 2px; }
.metric-card-head .model-icon-badge { width: 30px; height: 30px; border-radius: 9px; }
.metric-card-head .model-icon-badge i { font-size: 12px; }
.metric-title { font-size: 13px; font-weight: 800; color: var(--text); }
.metric-desc  { font-size: 11px; color: var(--text3); margin-top: 1px; }
.weight-caption { font-size: 10px; color: var(--text3); text-transform: uppercase; letter-spacing: .4px; font-weight: 800; margin-top: 10px; margin-bottom: 4px; }
.weight-row { display: flex; align-items: center; gap: 8px; }
.weight-track { flex: 1; height: 5px; border-radius: 3px; background: var(--border); overflow: hidden; }
.weight-fill  { height: 100%; border-radius: 3px; background: linear-gradient(90deg,var(--blue),var(--teal)); }
.weight-pct   { font-size: 12px; color: var(--text2); font-weight: 800; white-space: nowrap; min-width: 28px; }
.weight-note  { font-size: 10px; color: var(--text3); margin-top: 4px; }

.composite-panel { background: rgba(34,197,94,0.06); border: 1px solid rgba(34,197,94,0.22); border-radius: 10px; padding: 13px 14px 12px; }
.composite-panel .cp-title { font-size: 13px; font-weight: 800; color: var(--green); display: flex; align-items: center; gap: 6px; }
.composite-panel .cp-note { font-size: 11px; color: var(--text3); margin-top: 6px; }
.stack-bar { display: flex; height: 8px; border-radius: 4px; overflow: hidden; margin: 11px 0 9px; box-shadow: inset 0 0 0 1px rgba(0,0,0,0.15); }

/* ══ تب دسته‌های مدل ══ */
.nav-tabs-custom { background: transparent !important; box-shadow: none !important; border-radius: var(--radius) !important; margin-bottom: 0 !important; }
.nav-tabs-custom > .nav-tabs {
  background: var(--panel) !important; border: 1px solid var(--border) !important; border-bottom: none !important;
  border-radius: var(--radius) var(--radius) 0 0 !important; padding: 8px 8px 0 !important; margin: 0 !important;
  display: flex !important; gap: 6px; flex-wrap: wrap;
}
.nav-tabs-custom > .nav-tabs > li { float: none !important; }
.nav-tabs-custom > .nav-tabs > li.header { display: none; }
.nav-tabs-custom > .nav-tabs > li > a {
  border: none !important; background: transparent !important; color: var(--text3) !important;
  font-family: 'Vazirmatn',Tahoma,sans-serif !important; font-size: 14px !important; font-weight: 700;
  border-radius: 8px 8px 0 0 !important; padding: 9px 16px !important;
  display: flex !important; align-items: center; gap: 7px; margin: 0 !important;
}
.nav-tabs-custom > .nav-tabs > li.active > a,
.nav-tabs-custom > .nav-tabs > li.active > a:hover,
.nav-tabs-custom > .nav-tabs > li.active > a:focus {
  background: var(--panel2) !important; color: var(--blue2) !important;
  box-shadow: inset 0 -2px 0 var(--blue) !important; border: none !important;
}
.nav-tabs-custom > .nav-tabs > li:not(.active) > a:hover { color: var(--text) !important; background: var(--hover-bg) !important; }
.nav-tabs-custom > .tab-content {
  background: var(--panel) !important; border: 1px solid var(--border) !important; border-top: none !important;
  border-radius: 0 0 var(--radius) var(--radius) !important; padding: 18px !important; box-shadow: var(--shadow);
}

/* ══ SIDEBAR STATUS ══ */
.sidebar-status { padding:12px 14px; border-top:1px solid var(--border); font-size:13px; color:var(--text3); margin-top:8px; }
.status-dot { display:inline-block; width:6px; height:6px; background:var(--green); border-radius:50%; margin-left:5px; animation:pulse 2s infinite; }
@keyframes pulse { 0%,100%{opacity:1} 50%{opacity:.3} }

.ctrl-label-sci { font-size:12px; font-weight:800; color:var(--text3); text-transform:uppercase; letter-spacing:.8px; margin-bottom:4px; margin-top:8px; font-family:'Vazirmatn',Tahoma,sans-serif; display:block; }

pre.shiny-text-output { background:var(--panel2) !important; border:1px solid var(--border) !important; border-radius:6px !important; color:var(--teal) !important; font-size:12px !important; padding:9px 11px !important; direction:ltr; text-align:left; }

.shiny-download-link { display:block; width:100%; text-align:center; }

.section-title-sci { font-size:16px; font-weight:800; color:var(--text); display:flex; align-items:center; gap:8px; border-bottom:1px solid var(--border); padding-bottom:10px; margin-bottom:14px; }
.section-title-sci .dot { width:3px; height:20px; border-radius:2px; background:var(--blue); flex-shrink:0; }
"

# ══════════════════════════════════════════════════════════════════════════════
# JS
# ══════════════════════════════════════════════════════════════════════════════
THEME_JS <- "
function toggleTheme() {
  var body = document.body;
  var btn  = document.getElementById('theme-btn');
  if (body.classList.contains('light-mode')) {
    body.classList.remove('light-mode');
    btn.innerHTML = '<i class=\"fa fa-moon\"></i> دارک';
    Shiny.setInputValue('current_theme', 'dark');
    localStorage.setItem('wfs_theme','dark');
  } else {
    body.classList.add('light-mode');
    btn.innerHTML = '<i class=\"fa fa-sun\"></i> لایت';
    Shiny.setInputValue('current_theme', 'light');
    localStorage.setItem('wfs_theme','light');
  }
}

function fixSidebarToggle() {
  var toggleBtn = document.querySelector('.main-header .navbar .sidebar-toggle');
  if (!toggleBtn) return;
  toggleBtn.addEventListener('click', function(e) {
    setTimeout(function() {
      var sidebar = document.querySelector('.main-sidebar');
      if (sidebar) {
        sidebar.style.transform = '';
        sidebar.style.webkitTransform = '';
        sidebar.style.marginLeft = '';
        sidebar.style.marginRight = '';
      }
      var content = document.querySelector('.content-wrapper');
      if (content) {
        content.style.transform = '';
        content.style.marginLeft = '';
      }
    }, 50);
  }, true);
}

function renderMathFormulas() {
  if (typeof katex === 'undefined') return;
  document.querySelectorAll('.tex-formula').forEach(function(el) {
    if (el.dataset.rendered === '1') return;
    var tex = el.getAttribute('data-tex');
    if (!tex) return;
    try {
      katex.render(tex, el, { throwOnError: false, displayMode: true });
      el.dataset.rendered = '1';
    } catch (e) {}
  });
}

document.addEventListener('DOMContentLoaded', function() {
  var saved = localStorage.getItem('wfs_theme') || 'dark';
  var btn = document.getElementById('theme-btn');
  if (saved === 'light') {
    document.body.classList.add('light-mode');
    if (btn) btn.innerHTML = '<i class=\"fa fa-sun\"></i> لایت';
  } else {
    if (btn) btn.innerHTML = '<i class=\"fa fa-moon\"></i> دارک';
  }
  fixSidebarToggle();
  renderMathFormulas();
});

 $(document).on('shiny:connected', function() {
  setTimeout(fixSidebarToggle, 100);
  setTimeout(renderMathFormulas, 100);
});

// رندر مجدد فرمول‌ها هنگام کلیک روی منوها (برای تب‌های پنهان)
 $(document).on('click', '.sidebar-menu a', function() {
  setTimeout(renderMathFormulas, 200);
});
"

# ══════════════════════════════════════════════════════════════════════════════
# توابع کمکی — بخش «راهنمای مدل‌ها»
# ══════════════════════════════════════════════════════════════════════════════

mg_meter <- function(rating) {
  tags$span(class = "meter",
            lapply(1:5, function(i)
              tags$span(class = if (i <= rating) "meter-seg on" else "meter-seg")
            )
  )
}

mg_compare_row <- function(name, category, dot_color, accuracy, speed, interpretability) {
  tags$tr(
    tags$td(tags$span(class = paste0("mini-dot dot-", dot_color)), name),
    tags$td(category),
    tags$td(class = "mv-cell", mg_meter(accuracy)),
    tags$td(class = "mv-cell", mg_meter(speed)),
    tags$td(class = "mv-cell", mg_meter(interpretability))
  )
}

mg_model_card <- function(icon, color, title, badges = NULL, desc = NULL,
                          formula_tex = NULL, pros = NULL, cons = NULL) {
  tags$div(class = "model-card-sci",
           tags$div(class = "model-card-head",
                    tags$div(class = paste0("model-icon-badge ic-", color),
                             tags$i(class = paste0("fa fa-", icon))
                    ),
                    tags$h4(title,
                            if (!is.null(badges))
                              lapply(badges, function(b) tags$span(class = paste("badge-sci", b[[2]]), b[[1]]))
                    )
           ),
           if (!is.null(desc)) tags$p(tags$b("توضیح: "), desc),
           if (!is.null(formula_tex))
             tags$div(class = "formula-box",
                      tags$div(class = "formula-scroll tex-formula", `data-tex` = formula_tex)
             ),
           if (!is.null(pros) || !is.null(cons))
             tags$div(class = "proscons-grid",
                      if (!is.null(pros))
                        lapply(pros, function(x) tags$div(class = "pc-item",
                                                          tags$span(class = "pc-icon good", tags$i(class = "fa fa-check")), x)),
                      if (!is.null(cons))
                        lapply(cons, function(x) tags$div(class = "pc-item",
                                                          tags$span(class = "pc-icon bad", tags$i(class = "fa fa-xmark")), x))
             )
  )
}

mg_metric_card <- function(icon, hex, title, formula_tex, desc, weight, note = NULL) {
  tags$div(class = "metric-card",
           tags$div(class = "metric-card-head",
                    tags$div(class = "model-icon-badge",
                             style = paste0("background:linear-gradient(135deg,", hex, ",", hex, "99);"),
                             tags$i(class = paste0("fa fa-", icon))
                    ),
                    tags$div(
                      tags$div(class = "metric-title", title),
                      tags$div(class = "metric-desc", desc)
                    )
           ),
           tags$div(class = "formula-box", style = "margin:10px 0 2px;",
                    tags$div(class = "formula-scroll tex-formula", `data-tex` = formula_tex)
           ),
           tags$div(class = "weight-caption", "وزن در امتیاز نهایی"),
           tags$div(class = "weight-row",
                    tags$div(class = "weight-track",
                             tags$div(class = "weight-fill", style = paste0("width:", round(weight * 100), "%;"))
                    ),
                    tags$span(class = "weight-pct", paste0(round(weight * 100), "%"))
           ),
           if (!is.null(note)) tags$div(class = "weight-note", note)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
ui <- shinydashboard::dashboardPage(
  title = "سامانه پیش بینی آب و هوا",
  skin = "black",
  
  shinydashboard::dashboardHeader(
    title = tags$span(
      style = "font-family:'Vazirmatn',Tahoma,sans-serif;font-weight:800;font-size:15px;color:var(--text);display:flex;align-items:center;height:50px;",
      tags$i(class="fa fa-satellite-dish", style="margin-left:8px;color:#60a5fa;font-size:18px;"),
      "WFS Dashboard"
    ),
    titleWidth = 240,
    
    tags$li(
      class = "dropdown",
      style = "display:flex;align-items:center;height:50px;padding:0 15px;",
      tags$button(
        id    = "theme-btn",
        class = "theme-toggle-btn",
        onclick = "toggleTheme()",
        tags$i(class="fa fa-moon"), " دارک"
      )
    ),
    
    tags$li(
      class = "dropdown",
      style = "display:flex;align-items:center;height:50px;padding:0 20px;gap:20px;font-size:13px;color:var(--text3);font-weight:600;",
      tags$span(style="display:flex;align-items:center;gap:6px;",
                tags$i(class="fa fa-database",style="color:#3b82f6;font-size:14px;"), "250K رکورد"),
      tags$span(style="display:flex;align-items:center;gap:6px;",
                tags$i(class="fa fa-location-dot",style="color:#14b8a6;font-size:14px;"), "۵ ایستگاه"),
      tags$span(
        style="background:rgba(20,184,166,.12);border:1px solid rgba(20,184,166,.3);color:#14b8a6;border-radius:20px;padding:6px 14px;font-size:13px;font-weight:700;display:flex;align-items:center;gap:6px;",
        tags$i(class="fa fa-flask",style="font-size:13px;"), "تحقیقاتی"
      )
    )
  ),
  
  shinydashboard::dashboardSidebar(
    width = 228,
    
    tags$style(HTML(CUSTOM_CSS)),
    tags$script(HTML(THEME_JS)),
    tags$head(
      tags$link(rel="stylesheet",
                href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
      tags$link(rel="stylesheet",
                href="https://cdn.jsdelivr.net/npm/katex@0.17.0/dist/katex.min.css",
                integrity="sha384-vlBdW0r3AcZO/HboRPznQNowvexd3fY8qHOWkBi5q7KGgqJ+F48+DceybYmrVbmB",
                crossorigin="anonymous"),
      tags$script(defer=NA, src="https://cdn.jsdelivr.net/npm/katex@0.17.0/dist/katex.min.js",
                  integrity="sha384-AtrdNsnxl/75rvBneBVH7DtOvCxSVahR2zWqle1coBKd8DEmLoviqNeJSx64gNAs",
                  crossorigin="anonymous")
    ),
    
    tags$div(
      style="padding:16px 14px 13px;border-bottom:1px solid var(--border);",
      tags$div(style="display:flex;align-items:center;gap:9px;",
               tags$div(
                 style="width:36px;height:36px;border-radius:9px;background:linear-gradient(135deg,#3b82f6,#14b8a6);display:flex;align-items:center;justify-content:center;flex-shrink:0;",
                 tags$i(class="fa fa-satellite-dish",style="color:white;font-size:16px;")
               ),
               tags$div(
                 tags$div(style="font-size:14px;font-weight:800;color:var(--text);line-height:1.2;","پیش‌بینی آب‌وهوا"),
                 tags$div(style="font-size:11px;color:var(--text3);margin-top:2px;","Weather Forecast System")
               )
      )
    ),
    
    tags$p(style="padding:12px 15px 4px;font-size:11px;font-weight:800;letter-spacing:1.4px;color:var(--text3);text-transform:uppercase;margin:0;",
           "منو اصلی"),
    
    shinydashboard::sidebarMenu(
      id = "sidebar_menu",
      shinydashboard::menuItem("نمای کلی",         tabName="home",        icon=icon("chart-pie")),
      shinydashboard::menuItem("پیش‌بینی هوا",      tabName="forecast",    icon=icon("cloud-sun")),
      shinydashboard::menuItem("رتبه‌بندی مدل‌ها",  tabName="leaderboard", icon=icon("trophy")),
      shinydashboard::menuItem("تشخیص ناهنجاری",   tabName="anomaly",     icon=icon("triangle-exclamation")),
      shinydashboard::menuItem("راهنمای مدل‌ها",    tabName="model_guide", icon=icon("book")),
      shinydashboard::menuItem("خروجی گزارش",       tabName="report",      icon=icon("file-export"))
    ),
    
    tags$div(class="sidebar-status",
             tags$div(style="margin-bottom:3px;",
                      tags$span(class="status-dot"),
                      tags$span(style="color:var(--text2);","۵ ایستگاه فعال")
             ),
             tags$div(style="font-size:12px;color:var(--text3);","داده: ۲۰۲۱–۲۰۲۶"),
             tags$div(style="margin-top:6px;display:inline-block;background:rgba(59,130,246,.1);color:#60a5fa;border-radius:4px;padding:3px 10px;font-size:11px;font-weight:700;",
                      "v1.0 · R Shiny")
    )
  ),
  
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      
      shinydashboard::tabItem(tabName="home",        homeUI("home")),
      shinydashboard::tabItem(tabName="forecast",    forecastUI("forecast")),
      shinydashboard::tabItem(tabName="leaderboard", leaderboardUI("leaderboard")),
      shinydashboard::tabItem(tabName="anomaly",     anomalyUI("anomaly")),
      shinydashboard::tabItem(tabName="report",      reportUI("report")),
      
      # ── راهنمای مدل‌ها ── (اصلاح کاما جا افتاده)
      shinydashboard::tabItem(tabName="model_guide",
                              
                              fluidRow(column(12,
                                              tags$div(class="hero-banner",
                                                       tags$span(style="display:inline-flex;align-items:center;gap:5px;background:rgba(59,130,246,.1);border:1px solid rgba(59,130,246,.25);color:#60a5fa;border-radius:20px;padding:5px 12px;font-size:12px;font-weight:700;margin-bottom:10px;",
                                                                 tags$i(class="fa fa-book"), "راهنمای جامع"),
                                                       tags$h2("مدل‌های پیش‌بینی و ", tags$span(class="grad","معیارهای ارزیابی")),
                                                       tags$p("توضیح کامل ۱۱ مدل پیش‌بینی، کاربردها، نقاط قوت و ضعف، و فرمول‌های ریاضی — به‌همراه مقایسه سریع و سهم هر معیار در امتیاز نهایی."),
                                                       tags$div(class="hero-chips",
                                                                tags$span(class="hchip hchip-blue",   tags$i(class="fa fa-layer-group"),    "۱۱ مدل پیش‌بینی"),
                                                                tags$span(class="hchip hchip-amber",  tags$i(class="fa fa-shapes"),         "۳ دسته الگوریتمی"),
                                                                tags$span(class="hchip hchip-teal",   tags$i(class="fa fa-ruler-combined"), "۵ معیار ارزیابی"),
                                                                tags$span(class="hchip hchip-purple", tags$i(class="fa fa-trophy"),         "۱ امتیاز ترکیبی")
                                                       )
                                              )
                              )),
                              
                              fluidRow(column(12,
                                              shinydashboard::box(
                                                title=tags$span(tags$i(class="fa fa-list-check",style="margin-left:6px;color:#4ade80;"),"مقایسه سریع مدل‌ها"),
                                                width=12, status="success",
                                                tags$p(style="font-size:13px;color:var(--text3);margin:-4px 0 12px;",
                                                       "مقایسه‌ای کیفی بر پایه ویژگی‌های شناخته‌شده هر روش — برای نتایج واقعی روی داده‌های این پروژه به تب «رتبه‌بندی مدل‌ها» مراجعه کنید. برچسب «دستی» یعنی آن مدل از چرخه‌ی خودکار AutoML/Ensemble خارج است ولی همچنان از صفحه پیش‌بینی قابل انتخاب دستی است."),
                                                tags$div(class="compare-wrap",
                                                         tags$table(class="compare-table",
                                                                    tags$thead(tags$tr(
                                                                      tags$th("مدل"), tags$th("دسته"), tags$th("دقت"), tags$th("سرعت"), tags$th("تفسیرپذیری")
                                                                    )),
                                                                    tags$tbody(
                                                                      mg_compare_row("ARIMA",           "کلاسیک آماری (دستی)",   "blue",   3, 4, 5),
                                                                      mg_compare_row("SARIMA",          "کلاسیک آماری (خودکار)", "blue",   4, 2, 4),
                                                                      mg_compare_row("ETS",             "کلاسیک آماری (دستی)",   "blue",   3, 5, 4),
                                                                      mg_compare_row("TBATS",           "کلاسیک آماری (دستی)",   "blue",   4, 2, 3),
                                                                      mg_compare_row("Random Forest",   "یادگیری ماشین",         "amber",  4, 3, 3),
                                                                      mg_compare_row("XGBoost",         "یادگیری ماشین",         "amber",  5, 4, 2),
                                                                      mg_compare_row("LightGBM",        "یادگیری ماشین",         "amber",  5, 5, 2),
                                                                      mg_compare_row("CatBoost",        "یادگیری ماشین",         "amber",  5, 3, 2),
                                                                      mg_compare_row("SVM",             "یادگیری ماشین",         "amber",  3, 2, 2),
                                                                      mg_compare_row("Prophet",         "مدرن (دستی)",           "purple", 4, 3, 4),
                                                                      mg_compare_row("Naïve",           "بیس‌لاین (خودکار)",     "green",  1, 5, 5),
                                                                      mg_compare_row("AutoML Ensemble", "ترکیبی",                "green",  5, 2, 2)
                                                                    )
                                                         )
                                                )
                                              )
                              )),
                              
                              fluidRow(
                                column(8,
                                       shinydashboard::tabBox(id="model_cat_tabs", width=12,
                                                              
                                                              tabPanel(
                                                                title=tagList(icon("chart-bar"), "کلاسیک آماری"),
                                                                mg_model_card(
                                                                  icon="wave-square", color="blue", title="ARIMA",
                                                                  badges=list(list("دستی","badge-classic")),
                                                                  desc="AutoRegressive Integrated Moving Average — مدل‌سازی خودرگرسیون با میانگین متحرک",
                                                                  formula_tex=r"(y_t = \varphi_1 y_{t-1} + \cdots + \varphi_p y_{t-p} + \varepsilon_t + \theta_1 \varepsilon_{t-1} + \cdots + \theta_q \varepsilon_{t-q})",
                                                                  pros=c("تفسیرپذیر","مبنای آماری محکم"),
                                                                  cons=c("فرض خطی بودن","ضعیف در فصلی‌بودن شدید")
                                                                ),
                                                                mg_model_card(
                                                                  icon="arrows-rotate", color="blue", title="SARIMA",
                                                                  badges=list(list("خودکار","badge-classic")),
                                                                  desc="ARIMA به‌همراه پارامترهای فصلی (P, D, Q, s) — تنظیم خودکار با auto.arima",
                                                                  formula_tex=r"(\text{SARIMA}(p,d,q)(P,D,Q)[s])",
                                                                  pros=c("مدل‌سازی فصلی دقیق","تنظیم خودکار"),
                                                                  cons=c("محاسبه نسبتاً سنگین")
                                                                ),
                                                                mg_model_card(
                                                                  icon="chart-area", color="blue", title="ETS",
                                                                  badges=list(list("دستی","badge-classic"), list("Smoothing","badge-classic")),
                                                                  desc="Exponential Smoothing — مدل‌سازی سطح، روند و فصلیت با ترکیب‌های Additive/Multiplicative",
                                                                  formula_tex=r"(\hat{y}_{t+h} = (l_t + h\,b_t)\times s_{t+h-m})",
                                                                  pros=c("سریع و خودکار","پشتیبانی از روند و فصلیت"),
                                                                  cons=c("فرض ساختار نسبتاً ثابت")
                                                                ),
                                                                mg_model_card(
                                                                  icon="chart-line", color="blue", title="TBATS",
                                                                  badges=list(list("دستی","badge-classic"), list("Multi-season","badge-classic")),
                                                                  desc="Trigonometric Exponential Smoothing با Box-Cox Transform — مدل‌سازی فصلیت‌های چندگانه و غیرصحیح",
                                                                  formula_tex=r"(y_t^{(\omega)} = \ell_t + \sum_{k=1}^{K} b_k\,\sin(2\pi f_k t + \phi_k) + \varepsilon_t)",
                                                                  pros=c("پشتیبانی از چند فصلیت هم‌زمان","مناسب دوره‌های نامنظم"),
                                                                  cons=c("محاسبه سنگین‌تر از ETS")
                                                                )
                                                              ),
                                                              
                                                              tabPanel(
                                                                title=tagList(icon("brain"), "یادگیری ماشین"),
                                                                mg_model_card(
                                                                  icon="bolt", color="amber", title="XGBoost",
                                                                  badges=list(list("Gradient Boost","badge-ml")),
                                                                  desc="Extreme Gradient Boosting با ویژگی‌های lag/rolling",
                                                                  formula_tex=r"(F_m(x) = F_{m-1}(x) + \eta \cdot h_m(x))",
                                                                  pros=c("بهترین عملکرد در بیشتر موارد","سریع","دارای regularization"),
                                                                  cons=c("نیاز به تنظیم دقیق hyperparameter")
                                                                ),
                                                                mg_model_card(
                                                                  icon="tree", color="amber", title="Random Forest",
                                                                  badges=list(list("Ensemble","badge-ml")),
                                                                  desc="جنگل تصادفی با bootstrap sampling",
                                                                  pros=c("مقاوم در برابر outlier","خروجی feature importance"),
                                                                  cons=c("کُند روی داده‌های بزرگ")
                                                                ),
                                                                mg_model_card(
                                                                  icon="feather", color="amber", title="LightGBM",
                                                                  badges=list(list("Gradient Boost","badge-ml"), list("Leaf-wise","badge-ml")),
                                                                  desc="Light Gradient Boosting Machine — رشد برگ‌به‌برگ با هیستوگرام برای آموزش سریع‌تر",
                                                                  formula_tex=r"(F_m(x) = F_{m-1}(x) + \eta \sum_{j \in \text{leaf}} w_j\,\mathbb{I}(x \in \text{leaf}_j))",
                                                                  pros=c("آموزش بسیار سریع","مصرف حافظه کم","مناسب داده‌های بزرگ"),
                                                                  cons=c("ریسک overfitting روی داده کوچک","تنظیم num_leaves حساس")
                                                                ),
                                                                mg_model_card(
                                                                  icon="cat", color="amber", title="CatBoost",
                                                                  badges=list(list("Gradient Boost","badge-ml"), list("Ordered Boost","badge-ml")),
                                                                  desc="Categorical Boosting — مدیریت خودکار ویژگی‌های دسته‌ای با Ordered Boosting برای جلوگیری از data leakage",
                                                                  formula_tex=r"(F_m(x) = F_{m-1}(x) + \eta \cdot h_m(x;\,D^{\text{ordered}}))",
                                                                  pros=c("مدیریت خودکار متغیرهای دسته‌ای","مقاوم در برابر overfitting","تنظیم hyperparameter ساده"),
                                                                  cons=c("آموزش کندتر از LightGBM","نیاز به رم بیشتر")
                                                                ),
                                                                mg_model_card(
                                                                  icon="vector-square", color="amber", title="SVM",
                                                                  badges=list(list("Kernel","badge-ml")),
                                                                  desc="رگرسیون بردار پشتیبان با kernel RBF",
                                                                  pros=c("مؤثر در فضای با ابعاد بالا"),
                                                                  cons=c("کُند روی داده حجیم","نیاز به نرمال‌سازی")
                                                                )
                                                              ),
                                                              
                                                              tabPanel(
                                                                title=tagList(icon("leaf"), "مدرن و ترکیبی"),
                                                                mg_model_card(
                                                                  icon="leaf", color="purple", title="Prophet",
                                                                  badges=list(list("دستی","badge-modern"), list("Bayesian","badge-modern")),
                                                                  desc="مدل Bayesian از Meta با مدیریت تعطیلات و changepoint — تجزیه به روند، فصلیت و تعطیلات",
                                                                  formula_tex=r"(y(t) = g(t) + s(t) + h(t) + \varepsilon_t)",
                                                                  pros=c("مدیریت missing data","تفسیرپذیر","مدیریت تعطیلات"),
                                                                  cons=c("نیاز به کالیبراسیون")
                                                                ),
                                                                mg_model_card(
                                                                  icon="flag", color="purple", title="Naïve",
                                                                  badges=list(list("بیس‌لاین","badge-modern"), list("خودکار","badge-modern")),
                                                                  desc="خط‌مبنا (Baseline) — مقدار آخرین مشاهده به‌عنوان پیش‌بینی آینده؛ مرجع سنجش عملکرد سایر مدل‌ها",
                                                                  formula_tex=r"(\hat{y}_{t+h} = y_t)",
                                                                  pros=c("بسیار سریع","بدون فرض","خط‌مبنا برای مقایسه"),
                                                                  cons=c("بدون یادگیری الگو","ضعیف در داده با روند/فصلیت")
                                                                ),
                                                                mg_model_card(
                                                                  icon="layer-group", color="green", title="AutoML Ensemble",
                                                                  badges=list(list("Smart Softmax","badge-ens")),
                                                                  desc="ترکیب هوشمند خروجی مدل‌های قوی (فیلتر کردن مدل‌های ضعیف) با استفاده از وزن‌دهی Softmax برای تمرکز روی بهترین مدل و اصلاح خطاهای کوچک آن.",
                                                                  formula_tex=r"(\hat{y} = \sum_{i} w_i \hat{y}_i \qquad w_i = \dfrac{e^{-\beta \cdot \text{RMSE}_i}}{\sum_{j} e^{-\beta \cdot \text{RMSE}_j}})",
                                                                  pros=c("پایداری بالا در برابر نوسانات","کاهش خطاهای فاجعه‌بار"),
                                                                  cons=c("زمان اجرا برابر با مجموع مدل‌های پایه")
                                                                )
                                                              )
                                       )
                                ),
                                
                                column(4,
                                       shinydashboard::box(
                                         title=tags$span(tags$i(class="fa fa-ruler-combined",style="margin-left:6px;color:#a78bfa;"),"معیارهای ارزیابی"),
                                         width=12, status="primary",
                                         
                                         mg_metric_card(
                                           icon="ruler", hex="#60a5fa", title="RMSE",
                                           formula_tex=r"(\text{RMSE} = \sqrt{\dfrac{\sum (y-\hat y)^2}{n}})",
                                           desc="جذر میانگین مربعات خطا — حساس به خطاهای بزرگ", weight=0.25
                                         ),
                                         mg_metric_card(
                                           icon="scale-balanced", hex="#2dd4bf", title="MAE",
                                           formula_tex=r"(\text{MAE} = \dfrac{\sum |y-\hat y|}{n})",
                                           desc="میانگین قدر مطلق خطا — مقاوم در برابر outlier", weight=0.20
                                         ),
                                         mg_metric_card(
                                           icon="percent", hex="#fbbf24", title="MAPE",
                                           formula_tex=r"(\text{MAPE} = \dfrac{1}{n}\sum \dfrac{|y-\hat y|}{|y|}\times 100)",
                                           desc="میانگین درصد قدر مطلق خطا — درصد خطای نسبی", weight=0.20
                                         ),
                                         mg_metric_card(
                                           icon="shuffle", hex="#34d399", title="SMAPE",
                                           formula_tex=r"(\text{SMAPE} = \dfrac{1}{n}\sum \dfrac{|y-\hat y|}{(|y|+|\hat y|)/2}\times 100)",
                                           desc="درصد خطای متقارن — پایدارتر از MAPE وقتی y نزدیک صفر است", weight=0.15
                                         ),
                                         mg_metric_card(
                                           icon="superscript", hex="#a78bfa", title="R²",
                                           formula_tex=r"(R^2 = 1 - \dfrac{\sum (y-\hat y)^2}{\sum (y-\bar y)^2})",
                                           desc="ضریب تعیین — بالاتر = بهتر (۰ تا ۱)", weight=0.20
                                         ),
                                         
                                         tags$div(class="composite-panel",
                                                  tags$div(class="cp-title", tags$i(class="fa fa-trophy"), "نمره ترکیبی"),
                                                  tags$div(class="stack-bar",
                                                           tags$div(style="flex:25;background:#60a5fa;"),
                                                           tags$div(style="flex:20;background:#2dd4bf;"),
                                                           tags$div(style="flex:20;background:#fbbf24;"),
                                                           tags$div(style="flex:15;background:#34d399;"),
                                                           tags$div(style="flex:20;background:#a78bfa;")
                                                  ),
                                                  tags$div(class="formula-box", style="margin:0 0 2px;",
                                                           tags$div(class="formula-scroll tex-formula",
                                                                    `data-tex`=r"(\text{Score}=0.25\,n_{RMSE}+0.20\,n_{MAE}+0.20\,n_{MAPE}+0.15\,n_{SMAPE}+0.20\,n_{R^2})")),
                                                  tags$div(class="cp-note", "نرمال‌سازی min-max روی همه مدل‌ها · عدد کمتر = عملکرد بهتر")
                                         )
                                       )
                                )
                              )
      )
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# Server
# ══════════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  weather_data <- reactive({ WEATHER_DATA })
  hourly_data  <- reactive({ WEATHER_DATA_HOURLY })
  
  homeServer("home", weather_data, hourly_data = hourly_data)
  forecast_rv <- forecastServer("forecast", weather_data, hourly_data = hourly_data)
  leaderboard_rv <- leaderboardServer("leaderboard", hourly_data)
  anomaly_rv <- anomalyServer("anomaly", hourly_data)
  reportServer("report", weather_data,
               forecast_rv     = forecast_rv,
               anomaly_rv      = anomaly_rv,
               leaderboard_rv  = leaderboard_rv)
}

# اجرای اپلیکیشن به‌صورت استاندارد
shinyApp(ui, server)