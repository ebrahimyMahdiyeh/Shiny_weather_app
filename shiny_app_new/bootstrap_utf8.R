# ════════════════════════════════════════════════════════════════════════════
# bootstrap_utf8.R — اجرای این فایل در کنسول R قبل از runApp()
# ════════════════════════════════════════════════════════════════════════════
# این اسکریپت locale سیستم را روی UTF-8 تنظیم می‌کند تا از خطای
# "input string 1 is invalid UTF-8" در توابع sub/gsub جلوگیری شود.
#
# نحوه استفاده:
#   1. این فایل را در کنسول R اجرا کنید: source("bootstrap_utf8.R")
#   2. سپس اپ را اجرا کنید: shiny::runApp()
#
# اگر روی ویندوز هستید و خطا ادامه داشت، Control Panel → Region →
# Administrative → Change system locale → "Persian (Iran)" و تیک
# "Beta: Use Unicode UTF-8 for worldwide language support" را بزنید،
# سپس کامپیوتر را restart کنید.
# ════════════════════════════════════════════════════════════════════════════

cat("\n═══════════════════════════════════════════════════\n")
cat(" تنظیم locale برای پشتیبانی از فارسی (UTF-8)\n")
cat("═══════════════════════════════════════════════════\n\n")

# ── مرحله ۱: تشخیص سیستم‌عامل ──────────────────────────────────────────────
is_windows <- .Platform$OS.type == "windows"
cat("سیستم‌عامل:", if (is_windows) "Windows" else "Linux/Mac", "\n")
cat("Locale فعلی LC_CTYPE:", Sys.getlocale("LC_CTYPE"), "\n\n")

# ── مرحله ۲: تعریف لیست locale های候选 ─────────────────────────────────────
if (is_windows) {
  candidate_locales <- c(
    "Persian_Iran.65001",            # ایران، UTF-8 (کدپیج 65001)
    "English_United States.65001",   # انگلیسی آمریکا، UTF-8
    "English_United Kingdom.65001",  # انگلیسی بریتانیا، UTF-8
    "C"                              # fallback (ASCII)
  )
} else {
  candidate_locales <- c(
    "en_US.UTF-8",
    "C.UTF-8",
    "fa_IR.UTF-8",
    "en_US.utf8",
    "C"
  )
}

# ── مرحله ۳: امتحان تک‌تک locale ها ─────────────────────────────────────────
success <- FALSE
for (loc in candidate_locales) {
  res <- tryCatch({
    #suppressWarnings برای گرفتن warning های setlocale (که در ویندوز error نمی‌شوند)
    suppressWarnings(Sys.setlocale("LC_ALL", loc))
    TRUE
  }, error = function(e) {
    cat("  ✗ شکست:", loc, "—", conditionMessage(e), "\n")
    FALSE
  })

  if (isTRUE(res)) {
    cur <- tryCatch(Sys.getlocale("LC_CTYPE"), error = function(e) "")
    is_utf8 <- grepl("65001|UTF-8|utf8|utf-8", cur, ignore.case = TRUE)
    if (is_utf8) {
      cat("  ✓ موفق:", loc, "\n")
      cat("    LC_CTYPE =", cur, "\n")
      success <- TRUE
      break
    } else {
      cat("  ~ set شد ولی UTF-8 نیست:", loc, "(LC_CTYPE=", cur, ")\n")
    }
  }
}

# ── مرحله ۴: فعال‌سازی encoding پیش‌فرض ──────────────────────────────────────
options(encoding = "UTF-8")

# ── مرحله ۵: گزارش نهایی ───────────────────────────────────────────────────
cat("\n")
if (success) {
  cat("✓ Locale با موفقیت روی UTF-8 تنظیم شد.\n")
  cat("✓ حالا می‌توانید اپ را اجرا کنید: shiny::runApp()\n\n")
} else {
  cat("✗ نتوانستیم locale UTF-8 را تنظیم کنیم!\n\n")
  cat("راه‌حل‌های دستی:\n")
  if (is_windows) {
    cat("  1. Control Panel → Region → Administrative tab\n")
    cat("  2. 'Change system locale' → Persian (Iran)\n")
    cat("  3. تیک 'Beta: Use Unicode UTF-8 for worldwide language support'\n")
    cat("  4. Restart کامپیوتر\n")
  } else {
    cat("  1. در ترمینال اجرا کنید: sudo locale-gen en_US.UTF-8 fa_IR.UTF-8\n")
    cat("  2. سپس: sudo update-locale\n")
    cat("  3. R را دوباره باز کنید\n")
  }
  cat("\n")
}

# ── مرحله ۶: تست سریع ───────────────────────────────────────────────────────
cat("═══════════════════════════════════════════════════\n")
cat(" تست UTF-8: \n")
test_str <- "تهران — اصفهان — مشهد"
cat("  رشته:", test_str, "\n")
cat("  Encoding:", Encoding(test_str), "\n")
cat("  nchar:", nchar(test_str), "\n")
tryCatch({
  result <- sub("تهران", "Tehran", test_str)
  cat("  sub() test:", result, "\n")
  cat("  ✓ sub() کار می‌کند — آماده runApp()\n")
}, error = function(e) {
  cat("  ✗ sub() خطا:", conditionMessage(e), "\n")
})
cat("═══════════════════════════════════════════════════\n\n")
