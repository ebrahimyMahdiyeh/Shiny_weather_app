# ════════════════════════════════════════════════════════════════════════════
# utf8_patch.R — پچ خودکار برای خطای "input string 1 is invalid UTF-8"
# ════════════════════════════════════════════════════════════════════════════
# این فایل توابع base::sub و base::gsub را با نسخه‌ای جایگزین می‌کند
# که در صورت بروز خطای UTF-8، خودکار با useBytes=TRUE دوباره تلاش می‌کند.
# این روش از دیباگ کردن بهتر است چون:
#   1. هیچ خروجی نویز تولید نمی‌کند
#   2. اپ بدون توقف کار می‌کند
#   3. هیچ تداخلی با فراخوانی‌های داخلی R ندارد
# ════════════════════════════════════════════════════════════════════════════

# ── ذخیره نسخه اصلی ─────────────────────────────────────────────────────────
.original_sub  <- base::sub
.original_gsub <- base::gsub

# ── نسخه پچ‌شده sub ─────────────────────────────────────────────────────────
# استراتژی: اول با UTF-8 امتحان کن. اگر خطا داد، با useBytes=TRUE امتحان کن.
# useBytes=TRUE از بررسی encoding می‌گذرد و مستقیم روی بایت‌ها کار می‌کند.
.patched_sub <- function(pattern, replacement, x, ...) {
  # اول آرگومان useBytes فعلی کاربر رو بگیر (اگر داده)
  args <- list(...)
  user_useBytes <- isTRUE(args$useBytes)

  if (user_useBytes) {
    # کاربر خودش useBytes=TRUE داده، مستقیم اجرا کن
    return(.original_sub(pattern, replacement, x, ...))
  }

  # اول سعی کن نرمال اجرا بشه
  result <- tryCatch({
    .original_sub(pattern, replacement, x, ...)
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("UTF-8|utf-8|utf8|invalid multibyte", msg, ignore.case = TRUE)) {
      # خطای UTF-8! با useBytes=TRUE دوباره امتحان کن
      # ابتدا سعی کن رشته‌ها رو به UTF-8 تبدیل کن
      tryCatch({
        p2 <- if (is.character(pattern))     enc2utf8(pattern)     else pattern
        r2 <- if (is.character(replacement)) enc2utf8(replacement) else replacement
        x2 <- if (is.character(x))           enc2utf8(x)           else x
        .original_sub(p2, r2, x2, ..., useBytes = TRUE)
      }, error = function(e2) {
        # اگر باز هم نشد، فقط useBytes=TRUE رو امتحان کن بدون تبدیل encoding
        tryCatch({
          .original_sub(pattern, replacement, x, ..., useBytes = TRUE)
        }, error = function(e3) {
          # آخرین تلاش: pattern و replacement رو به native تبدیل کن
          tryCatch({
            p3 <- if (is.character(pattern))     enc2native(pattern)     else pattern
            r3 <- if (is.character(replacement)) enc2native(replacement) else replacement
            x3 <- if (is.character(x))           enc2native(x)           else x
            .original_sub(p3, r3, x3, ..., useBytes = TRUE)
          }, error = function(e4) {
            # واقعاً نشد — خطای اصلی رو پرتاب کن
            stop(e)
          })
        })
      })
    } else {
      # خطای غیر UTF-8 — مستقیم پرتاب کن
      stop(e)
    }
  })

  result
}

# ── نسخه پچ‌شده gsub ────────────────────────────────────────────────────────
.patched_gsub <- function(pattern, replacement, x, ...) {
  args <- list(...)
  user_useBytes <- isTRUE(args$useBytes)

  if (user_useBytes) {
    return(.original_gsub(pattern, replacement, x, ...))
  }

  result <- tryCatch({
    .original_gsub(pattern, replacement, x, ...)
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("UTF-8|utf-8|utf8|invalid multibyte", msg, ignore.case = TRUE)) {
      tryCatch({
        p2 <- if (is.character(pattern))     enc2utf8(pattern)     else pattern
        r2 <- if (is.character(replacement)) enc2utf8(replacement) else replacement
        x2 <- if (is.character(x))           enc2utf8(x)           else x
        .original_gsub(p2, r2, x2, ..., useBytes = TRUE)
      }, error = function(e2) {
        tryCatch({
          .original_gsub(pattern, replacement, x, ..., useBytes = TRUE)
        }, error = function(e3) {
          tryCatch({
            p3 <- if (is.character(pattern))     enc2native(pattern)     else pattern
            r3 <- if (is.character(replacement)) enc2native(replacement) else replacement
            x3 <- if (is.character(x))           enc2native(x)           else x
            .original_gsub(p3, r3, x3, ..., useBytes = TRUE)
          }, error = function(e4) {
            stop(e)
          })
        })
      })
    } else {
      stop(e)
    }
  })

  result
}

# ── اعمال پچ روی base namespace ─────────────────────────────────────────────
unlockBinding("sub",  as.environment("package:base"))
unlockBinding("gsub", as.environment("package:base"))
assign("sub",  .patched_sub,  envir = as.environment("package:base"))
assign("gsub", .patched_gsub, envir = as.environment("package:base"))
lockBinding("sub",  as.environment("package:base"))
lockBinding("gsub", as.environment("package:base"))

cat("✓ پچ UTF-8 برای sub/gsub فعال شد\n")
cat("✓ در صورت بروز خطای UTF-8، خودکار با useBytes=TRUE بازیابی می‌شود\n")
