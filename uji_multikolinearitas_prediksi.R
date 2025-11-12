
#install package yang digunakan
install.packages("lme4") #untuk glmm (glmer function)
install.packages("ggplot2") #untuk visualisasi
install.packages("dplyr") #untuk manipulasi data
install.packages("car") #untuk periksa atau diagnosa 
install.packages("psych")
install.packages("corrplot")
install.packages("readxl") #untuk membaca file excel


library(lme4)
library(ggplot2)
library(dplyr)
library(car)
library(psych)
library(corrplot)
library(xlsx)
library(magrittr)

#membaca dataset
imo <- readxl::read_excel("G:/My Drive/006_school/007_compile_data_imo.xlsx",sheet = "compile_data_imo")
View(imo)

# Pilih hanya variabel prediktor yang akan diuji korelasi
pred <- imo %>%
  dplyr::select(luas_desa,luas_phva,pa_phva,luas_gambut,pa_gambut,hotspot,def_luas,pa_def)

# Cek apakah ada NA, jika ada --> hapus sementara (atau lakukan imputasi)
pred <- na.omit(pred)

# 1. Korelasi Pearson (tabel)
# ---------------------------
cor_matrix <- cor(pred, method = "pearson")
cor_matrix   # cetak hasilnya

# Tambahan: nilai signifikansi korelasi
cor_test <- psych::corr.test(pred)
cor_test

# 2. Visualisasi korelasi (heatmap)
# ---------------------------
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.srt = 45,
         tl.col = "black",
         number.cex = 0.7)

#proses pembuatan model
table(imo$pa_interaksi)
str(imo$pa_interaksi)

#menjadikan obectid (desa) menajdi efek acak
imo$objectid <- as.factor(imo$objectid)

imo$luas_phva    <- scale(imo$luas_phva)
imo$luas_gambut  <- scale(imo$luas_gambut)
imo$def_luas     <- scale(imo$def_luas)
imo$hotspot      <- scale(imo$hotspot)

model <- glmer(
  pa_interaksi ~ luas_phva + luas_gambut + def_luas + hotspot +
    (1 | objectid),
  data = imo,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model)



