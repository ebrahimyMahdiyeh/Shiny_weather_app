# ════════════════════════════════════════════════════════════════════════════
# start.R — اسکریپت راه‌انداز اپ شاین (نسخه خودکار و هوشمند)
# ════════════════════════════════════════════════════════════════════════════

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║  راه‌انداز اپ شاین با UTF-8 + مرورگر خارجی                       ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# ── مرحله ۰: پیدا کردن خودکار مسیر پروژه ────────────────────────────────────
# این بخش از خطای App dir must contain app.R جلوگیری می‌کند
cat("[۰/۵] تنظیم مسیر پروژه...\n")
tryCatch({
  if (interactive() && "rstudioapi" %in% rownames(installed.packages())) {
    # اگر در RStudio اجرا می‌شود، مسیر فایل فعال را می‌گیرد
    doc_path <- rstudioapi::getSourceEditorContext()$path
    if (length(doc_path) > 0 && grepl("start.R$", doc_path)) {
      setwd(dirname(doc_path))
      cat("  ✓ مسیر پروژه بر اساس فایل start.R تنظیم شد\n")
    } else {
      doc_path <- rstudioapi::getActiveDocumentContext()$path
      if (length(doc_path) > 0 && nchar(doc_path) > 0) {
        setwd(dirname(doc_path))
        cat("  ✓ مسیر پروژه بر اساس فایل باز شده تنظیم شد\n")
      }
    }
  }
}, error = function(e) {
  cat("  ⚠ تنظیم خودکار مسیر ناموفق بود، از مسیر فعلی استفاده می‌شود\n")
})

# بررسی وجود فایل app.R
if (!file.exists("app.R")) {
  cat("\n❌ خطای بحرانی: فایل app.R در پوشه فعلی پیدا نشد!\n")
  cat("   مسیر فعلی شما: ", getwd(), "\n")
  cat("   لطفاً مطمئن شوید فایل start.R را از داخل پوشه پروژه (کنار app.R) باز کرده‌اید.\n")
  stop("App dir must contain either app.R or server.R.")
}

# ── مرحله ۱: تنظیم locale UTF-8 ─────────────────────────────────────────────
cat("[۱/۵] تنظیم locale UTF-8...\n")
if (.Platform$OS.type == "windows") {
  locs <- c("Persian_Iran.65001", "English_United States.65001",
            "English_United States.1252", "C")
} else {
  locs <- c("en_US.UTF-8", "C.UTF-8", "fa_IR.UTF-8", "C")
}
locale_ok <- FALSE
for (loc in locs) {
  ok <- tryCatch({
    suppressWarnings(Sys.setlocale("LC_ALL", loc))
    cur <- Sys.getlocale("LC_CTYPE")
    grepl("65001|UTF-8|utf8|utf-8", cur, ignore.case = TRUE)
  }, error = function(e) FALSE)
  if (isTRUE(ok)) {
    cat("  ✓ Locale ست شد:", loc, "\n")
    locale_ok <- TRUE
    break
  }
}
if (!locale_ok) {
  cat("  ⚠ نتوانستیم locale UTF-8 را تنظیم کنیم — ادامه می‌دهیم...\n")
}
options(encoding = "UTF-8")

# ── مرحله ۲: فعال‌سازی پچ خودکار UTF-8 ────────────────────────────────────────
cat("[۲/۵] فعال‌سازی پچ خودکار UTF-8...\n")
if (file.exists("utf8_patch.R")) {
  tryCatch({
    source("utf8_patch.R", encoding = "UTF-8", local = FALSE)
    cat("  ✓ پچ UTF-8 فعال شد\n")
  }, error = function(e) {
    cat("  ⚠ خطا در فعال‌سازی پچ:", e$message, "\n")
  })
} else {
  cat("  ⚠ فایل utf8_patch.R پیدا نشد — ادامه بدون پچ\n")
}

# ── مرحله ۲.۵: هشدار در مورد فایل debug_sub.R ────────────────────────────────
if (file.exists("debug_sub.R")) {
  cat("  ⚠ فایل debug_sub.R پیدا شد — لطفاً آن را از پوشه پاک کنید!\n")
}

# ── مرحله ۳: تنظیمات مرورگر خارجی ────────────────────────────────────────────
cat("[۳/۵] تنظیم مرورگر خارجی برای اپ...\n")
options(shiny.launch.browser = TRUE)
options(shiny.host = "127.0.0.1")
options(shiny.port = 3838)
cat("  ✓ اپ در مرورگر پیش‌فرض سیستم باز می‌شود\n")
cat("  ✓ آدرس: http://127.0.0.1:3838\n")

# ── مرحله ۴: اجرای اپ ───────────────────────────────────────────────────────
cat("[۴/۵] در حال اجرای اپ...\n")
cat("═══════════════════════════════════════════════════\n")
cat("  اپ در حال اجراست — منتظر باز شدن مرورگر بمانید\n")
cat("  برای توقف: Ctrl+C در کنسول R را بزنید\n")
cat("═══════════════════════════════════════════════════\n\n")

# اجرای مستقیم app.R
shiny::runApp(".", port = 3838, launch.browser = TRUE, host = "127.0.0.1")