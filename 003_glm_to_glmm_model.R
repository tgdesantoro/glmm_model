#proses pembuatan model GLM
#seluruh data digunakan untuk pembuatan model ini
#jika berhasil, lanjut dengan K-fold cross validation

install.packages("patchwork") #untuk menggabungkan beberapa plot

library(readxl)
library(lme4)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(magrittr)
library(pROC) #untuk menghitung dan memvisualisasikan ROC curve serta nilai AUC (Area Under the Curve)
library(caret)


#membaca atau memanggil data yang sudah diuji multikolinearitas
#membaca dataset
imo <- readxl::read_excel("D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_produce_r/007_compile_data_imo.xlsx",sheet = "compile_data_imo")
View(imo)




#periska nilai NA
colSums(is.na(imo))

# --- 1️⃣ Buat versi asli dan versi scaled ---
imo_scaled <- imo %>%
  mutate(
    luas_phva   = scale(luas_phva),
    luas_gambut = scale(luas_gambut),
    def_luas    = scale(def_luas),
    hotspot     = scale(hotspot)
  )

# --- 2️⃣ Gabungkan data sebelum dan sesudah untuk perbandingan ---
imo_compare <- bind_rows(
  imo %>% mutate(type = "Sebelum Scaling"),
  imo_scaled %>% mutate(type = "Sesudah Scaling")
)

# --- 3️⃣ Ubah ke format long untuk memudahkan plotting ---
imo_long <- imo_compare %>%
  pivot_longer(cols = c(luas_phva, luas_gambut, def_luas, hotspot),
               names_to = "Variabel",
               values_to = "Nilai")

# --- 4️⃣ Plot distribusi (density plot) ---
ggplot(imo_long, aes(x = Nilai, fill = type)) +
  geom_density(alpha = 0.5) +
  facet_grid(type ~ Variabel, scales = "free") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribusi Variabel Sebelum vs Sesudah Scaling",
    x = "Nilai", y = "Kerapatan", fill = "Tipe Data"
  )


#cek proporsi kelas
table(imo_scaled$pa_interaksi)
prop.table(table(imo_scaled$pa_interaksi))


#Transformasi log variabel luas dan hotspot
imo_log <- imo %>%
  mutate(
    luas_phva_log   = log1p(luas_phva),
    luas_gambut_log = log1p(luas_gambut),
    def_luas_log    = log1p(def_luas),
    hotspot_log     = log1p(hotspot)
  )

colSums(is.na(imo_log))

#lihat hasil log
summary(imo_log$luas_phva_log)
summary(imo_log$hotspot_log)

#visualisasai distribusi log
hist(imo_log$luas_phva_log, main="Distribusi luas_phva_log", xlab="Log(Luas PHVA)")
hist(imo_log$hotspot_log, main="Distribusi hotspot_log", xlab="Log(Hotspot)")


#jalankan ulang GLM dengan variable log
glm_log <- glm(
  pa_interaksi ~ luas_phva_log + luas_gambut_log + def_luas_log + hotspot_log,
  family = binomial(link = "logit"),
  data = imo_log
)

summary(glm_log)

#lanjut membuat model glm dengan memisahkan data training dan data test
# ==============================================
# LOGISTIC REGRESSION dengan PEMBAGIAN 70:30
# ==============================================


# 1️⃣ Split data menjadi training dan testing (70:30)
set.seed(123)
index <- sample(1:nrow(imo_log), 0.70 * nrow(imo_log))
train_data <- imo_log[index, ]
test_data  <- imo_log[-index, ]

# 2️⃣ Buat wadah hasil evaluasi
results_glm <- data.frame(Accuracy = numeric(), AUC = numeric())

# 3️⃣ Bangun model regresi logistik
model <- glm(
  pa_interaksi ~ luas_phva + luas_gambut + def_luas + hotspot,
  family = binomial(link = "logit"),
  data = train_data
)

# 4️⃣ Prediksi probabilitas pada data testing
pred_prob <- predict(model, newdata = test_data, type = "response")

# 5️⃣ Konversi probabilitas ke kelas (threshold 0.5)
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# 6️⃣ Pastikan variabel observasi dalam bentuk numerik 0/1
obs <- as.numeric(as.character(test_data$pa_interaksi))

# 7️⃣ Hitung metrik evaluasi
acc <- mean(pred_class == obs)                # Akurasi
auc_val <- pROC::auc(obs, pred_prob)          # AUC dari ROC Curve

# 8️⃣ Simpan hasil evaluasi ke tabel
results_glm <- rbind(results_glm, data.frame(Accuracy = acc, AUC = auc_val))

# 9️⃣ Tampilkan hasil
print(results_glm)

# 10️⃣ (Opsional) Lihat ringkasan model
summary(model)

# Prediksi probabilitas dari data uji
pred_prob <- predict(model, newdata = test_data, type = "response")

# Pastikan variabel observasi adalah numerik biner (0 dan 1)
obs <- as.numeric(as.character(test_data$pa_interaksi))

# Buat objek ROC
roc_obj <- roc(obs, pred_prob)

# Cetak nilai AUC
auc_value <- auc(roc_obj)
cat("AUC =", auc_value, "\n")

# Plot ROC Curve
plot(roc_obj, col = "#1c61b6", lwd = 3, main = "ROC Curve - GLM Logistic Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # garis diagonal acuan

# Tambahkan teks nilai AUC di grafik
text(0.6, 0.2, paste("AUC =", round(auc_value, 3)), col = "black", cex = 1.2)



##proses model GLM dengan k-fold = 5 dengan pembagian 80:20
# ==========================================
# 1. Persiapan paket dan data
# ==========================================
library(caret)
library(pROC)

# Pastikan target biner berbentuk faktor 0/1
imo_log$pa_interaksi <- as.factor(imo_log$pa_interaksi)

# ==========================================
# 2. Setup K-Fold Cross Validation
# ==========================================

# Pastikan kolom target berupa faktor
imo_log$pa_interaksi <- as.factor(imo_log$pa_interaksi)

# Ganti nama level faktor 0 dan 1 jadi "absence" dan "presence"
levels(imo_log$pa_interaksi) <- c("absence", "presence")

# Pastikan ulang
table(imo_log$pa_interaksi)

# Kontrol 5-fold cross validation
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = TRUE
)

# Jalankan model GLM dengan validasi silang
glm_cv <- train(
  pa_interaksi ~ luas_phva_log + luas_gambut_log + def_luas_log + hotspot_log,
  data = imo_log,
  method = "glm",
  family = binomial(link = "logit"),
  trControl = ctrl,
  metric = "ROC"
)


# ==========================================
# 4. Lihat hasil k-fold
# ==========================================
print(glm_cv)
summary(glm_cv)

# Menampilkan AUC rata-rata dari 5 fold
cat("AUC rata-rata dari 5-fold cross-validation:",
    glm_cv$results$ROC, "\n")

# ==========================================
# 5. Plot ROC Curve
# ==========================================
# Ambil prediksi probabilitas dan nilai aktual
roc_data <- glm_cv$pred

# Perhatikan: di caret, label positif harus didefinisikan dengan benar.
# Pastikan level positif adalah "1"
roc_curve <- roc(response = roc_data$obs,
                 predictor = roc_data$`1`,  # kolom probabilitas kelas 1
                 levels = rev(levels(roc_data$obs)))

# Plot ROC
plot(roc_curve, col = "blue", lwd = 2, main = "ROC Curve - GLM (5-fold CV)")
auc_value <- auc(roc_curve)
cat("AUC keseluruhan dari hasil k-fold:", auc_value, "\n")


