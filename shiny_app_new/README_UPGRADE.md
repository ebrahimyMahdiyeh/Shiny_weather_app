# 🎨 راهنمای نسخه ارتقاءیافته داشبورد

## تغییرات عمده

### طراحی بصری
- تم تاریک علمی (Navy/Dark background)
- فونت Vazirmatn با وزن‌های مختلف
- انیمیشن‌ها و hover effects
- کارت‌های ۲۴ ساعت با نوارهای progress
- نمودار رادار برای مقایسه ۳ مدل برتر

### صفحه اول (home_module.R)
- Hero Banner با chip های رنگی
- کارت‌های آماری (InfoBox) با رنگ‌بندی gradient
- نمودار violin برای توزیع متغیرها
- نمودار میله‌ای ماهانه با error bars
- timeline گام‌های پژوهش

### صفحه پیش‌بینی (forecast_module.R)  
- کارت‌های ۲۴ ساعت آینده با progress bars
- slider عددی نمایش افق
- legend واضح برای نمودار
- خط جداکننده تاریخی/پیش‌بینی

### رتبه‌بندی (leaderboard_module.R)
- رنگ‌بندی شرطی بر اساس کیفیت
- نمودار radar برای ۳ مدل برتر
- توضیح فرمول نمره ترکیبی
- دانلود CSV

## افزودن CSS به app.R
در تابع UI، قبل از sidebar، این خط اضافه کنید:
```r
tags$style(HTML("
  .ctrl-label-sci {
    font-size:10px; font-weight:700; color:#64748b;
    text-transform:uppercase; letter-spacing:0.8px;
    margin-bottom:4px; margin-top:10px;
  }
"))
```

## نکات مهم
- کتابخانه shinyWidgets برای materialSwitch نیاز است
- `install.packages("shinyWidgets")`
