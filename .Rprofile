# اصلاح locale برای پشتیبانی فارسی (UTF-8) روی ویندوز
# این فایل قبل از هر فایل R دیگه‌ای در پروژه اجرا می‌شه
invisible(tryCatch(
  Sys.setlocale("LC_CTYPE", "English_United States.utf8"),
  error = function(e) tryCatch(
    Sys.setlocale("LC_CTYPE", "C.UTF-8"),
    error = NULL
  )
))
