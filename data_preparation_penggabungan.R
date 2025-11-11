
install.packages(c("readxl", "dplyr", "writexl"))

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

#menggabungkan data pa_interaksi dengan phva

phva <- read_excel("D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_lingkungan/phva_per_desa/003_gabung_phva_per_desa/phva_per_desa_kalimantan.xlsx",
                    sheet = "sum_phva_per_desa")   # Sesuaikan nama sheet
pa_interaksi <- read_excel("D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_pa_lingkungan.xlsx",
                    sheet = "data_pa_lingkungan")                  # Sesuaikan sheet

View(phva)
View(pa_interaksi)

pa_interaksi_01 <- pa_interaksi %>%
  left_join(phva %>% select(kecamatan, desa, luas_phva),
            by = c("kecamatan", "desa"))
View(pa_interaksi_01)

pa_interaksi_02 <- pa_interaksi_01 %>%
  left_join(
    phva %>%
      select(kecamatan, desa, pa_phva = presence),  # rename kolom di sini
    by = c("kecamatan", "desa")
  )
View(pa_interaksi_02)


#menggabungkan data pa_interaksi dengan gambut

gambut <- read_excel("D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_lingkungan/gambut_per_desa/gambut_kementan_per_desa_kalimantan.xlsx",
                           sheet = "gambut_kementan")                  # Sesuaikan sheet
View(gambut)

pa_interaksi_03 <- pa_interaksi_02 %>%
  left_join(gambut %>% select(kecamatan, desa, luas_gambut),
            by = c("kecamatan", "desa"))
View(pa_interaksi_03)

pa_interaksi_04 <- pa_interaksi_03 %>%
  left_join(gambut %>% select(kecamatan, desa, pa_gambut),
            by = c("kecamatan", "desa"))
View(pa_interaksi_04)

#menggabungkan data pa_interaksi dengan hotspot/kebakaran

hotspot <- read_excel("D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_lingkungan/kebakaran_per_desa/kebakaran_per_desa-kalimantan.xlsx",
                      sheet = "hotspot_per_desa")

View(hotspot)
#periksa jumlah kolom dan baris pada data hotspot
hotspot %>%
  count(tahun, kecamatan, desa) %>%
  filter(n > 1)

#membuat satu baris per desa, kecamatan per tahun
hotspot_clean <- hotspot %>%
  group_by(tahun, kecamatan, desa) %>%
  summarise(hotspot = sum(hotspot, na.rm = TRUE), .groups = "drop")

View(hotspot_clean)


pa_interaksi_05 <- pa_interaksi_04 %>%
  left_join(hotspot_clean %>% select(tahun, kecamatan, desa, hotspot),
            by = c("tahun","kecamatan", "desa")) %>% 
  mutate(hotspot = replace_na(hotspot, 0))

View(pa_interaksi_05)

#menggabungkan data pa_interaksi dengan deforestasi
#deforestasi provinsi kalimantan barat

def_kalbar <- read_excel("D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_lingkungan/deforestasi_per_desa/provinsi_kalbar/002_sum_def_desa_kalbar/sum_def_desa_kalbar.xlsx",
                      sheet = "sum_def_desa_kalbar")
View(def_kalbar)

#periksa tipe data kolom tahun. numeric/karakter
str(pa_interaksi_05$tahun)
str(def_kalbar$tahun)


pa_interaksi_06 <- pa_interaksi_05 %>%
  mutate(tahun = as.character(tahun)) %>%       # ubah tahun menjadi karakter
  left_join(
    def_kalbar %>% 
      mutate(tahun = as.character(tahun)) %>%   # pastikan tipe sama
      select(tahun, kecamatan, desa, def_luas),
    by = c("tahun", "kecamatan", "desa")
  )

View(pa_interaksi_06)

pa_interaksi_07 <- pa_interaksi_06 %>%
  left_join(def_kalbar %>% select(tahun, kecamatan, desa, pa_def),
            by = c("tahun","kecamatan", "desa"))
View(pa_interaksi_07)

#deforestasi provinsi kalimantan tengah
def_kalteng <- read_excel("D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_lingkungan/deforestasi_per_desa/provinsi_kalteng/deforest_sum_kalteng.xlsx",
                         sheet = "deforest_sum_kalteng")
View(def_kalteng)

#input data luasan dengan mengisi kolom def_luas yang sudah ada
pa_interaksi_08 <- pa_interaksi_07 %>%
  left_join(
    def_kalteng %>% select(tahun, kecamatan, desa, def_luas),
    by = c("tahun", "kecamatan", "desa")
  ) %>%
  mutate(def_luas = coalesce(def_luas.y, def_luas.x)) %>%   # isi kolom eksisting
  select(-def_luas.x, -def_luas.y)                          # hapus kolom baru

View(pa_interaksi_08)

#input data pa_Deforestasi dengan mengisi kolom yang sudah ada
pa_interaksi_09 <- pa_interaksi_08 %>%
  left_join(
    def_kalteng %>% select(tahun, kecamatan, desa, pa_def),
    by = c("tahun", "kecamatan", "desa")
  ) %>%
  mutate(pa_def = coalesce(pa_def.y, pa_def.x)) %>%   # isi kolom eksisting
  select(-pa_def.x, -pa_def.y)                          # hapus kolom baru
View(pa_interaksi_09)

#deforestasi provinsi kalimantan selatan
def_kalsel <- read_excel("D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_lingkungan/deforestasi_per_desa/provinsi_kalsel/002_def_desa_kalsel/def_kalsel.xlsx",
                          sheet = "def_kalsel")
View(def_kalsel)

#input data luasan dengan mengisi kolom def_luas yang sudah ada
pa_interaksi_10 <- pa_interaksi_09 %>%
  left_join(
    def_kalsel %>% select(tahun, kecamatan, desa, def_luas),
    by = c("tahun", "kecamatan", "desa")
  ) %>%
  mutate(def_luas = coalesce(def_luas.y, def_luas.x)) %>%   # isi kolom eksisting
  select(-def_luas.x, -def_luas.y)                          # hapus kolom baru
View(pa_interaksi_10)

#input data pa_Deforestasi dengan mengisi kolom yang sudah ada
pa_interaksi_11 <- pa_interaksi_10 %>%
  left_join(
    def_kalsel %>% select(tahun, kecamatan, desa, pa_def),
    by = c("tahun", "kecamatan", "desa")
  ) %>%
  mutate(pa_def = coalesce(pa_def.y, pa_def.x)) %>%   # isi kolom eksisting
  select(-pa_def.x, -pa_def.y)                          # hapus kolom baru
View(pa_interaksi_11)

#deforestasi provinsi kalimantan timur
def_kaltim <- read_excel("D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_lingkungan/deforestasi_per_desa/provinsi_kaltim/def_per_desa_kaltim/def_desa_kaltim.xlsx",
                         sheet = "def_desa_kaltim")
View(def_kaltim)

#input data luasan dengan mengisi kolom def_luas yang sudah ada
pa_interaksi_12 <- pa_interaksi_11 %>%
  left_join(
    def_kaltim %>% select(tahun, kecamatan, desa, def_luas),
    by = c("tahun", "kecamatan", "desa")
  ) %>%
  mutate(def_luas = coalesce(def_luas.y, def_luas.x)) %>%   # isi kolom eksisting
  select(-def_luas.x, -def_luas.y)                          # hapus kolom baru
View(pa_interaksi_12)


#input data pa_Deforestasi dengan mengisi kolom yang sudah ada
pa_interaksi_13 <- pa_interaksi_12 %>%
  left_join(
    def_kaltim %>% select(tahun, kecamatan, desa, pa_def),
    by = c("tahun", "kecamatan", "desa")
  ) %>%
  mutate(pa_def = coalesce(pa_def.y, pa_def.x)) %>%   # isi kolom eksisting
  select(-pa_def.x, -pa_def.y)                          # hapus kolom baru
View(pa_interaksi_13)

#deforestasi provinsi kalimantan timur
def_kaltara <- read_excel("D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_lingkungan/deforestasi_per_desa/provinsi_kaltara/def_per_desa_kaltara/def_kaltara.xlsx",
                         sheet = "def_kaltara")
View(def_kaltara)

#input data luasan dengan mengisi kolom def_luas yang sudah ada
pa_interaksi_14 <- pa_interaksi_13 %>%
  left_join(
    def_kaltara %>% select(tahun, kecamatan, desa, def_luas),
    by = c("tahun", "kecamatan", "desa")
  ) %>%
  mutate(def_luas = coalesce(def_luas.y, def_luas.x)) %>%   # isi kolom eksisting
  select(-def_luas.x, -def_luas.y)                          # hapus kolom baru
View(pa_interaksi_14)


#input data pa_Deforestasi dengan mengisi kolom yang sudah ada
pa_interaksi_15 <- pa_interaksi_14 %>%
  left_join(
    def_kaltara %>% select(tahun, kecamatan, desa, pa_def),
    by = c("tahun", "kecamatan", "desa")
  ) %>%
  mutate(pa_def = coalesce(pa_def.y, pa_def.x)) %>%   # isi kolom eksisting
  select(-pa_def.x, -pa_def.y)                          # hapus kolom baru
View(pa_interaksi_15)

#memberikan nilai 0 pada baris kolom NA pada def_luas dan pa_def
pa_interaksi_16 <- pa_interaksi_15 %>%
  mutate(def_luas = replace_na(def_luas, 0)) %>% 
  mutate(pa_def = replace_na(pa_def,0))
View(pa_interaksi_16)

#menggabungkan luas desa ke pa_interaksi
#menggabungkan data pa_interaksi dengan luas wilayah desa

luas_desa <- read_excel("D:/006_pascasarjana/008_thesis/004_thesis/data/data_administrasi/desa_kalimantan/luas_desa_kalimantan_utm.xlsx",
                     sheet = "desa_kalimantan_utm")                  # Sesuaikan sheet
View(luas_desa)

pa_interaksi_17 <- pa_interaksi_16 %>%
  left_join(luas_desa %>% select(kabupaten, kecamatan, desa, luas_desa),
            by = c("kabupaten", "kecamatan", "desa")) %>% 
  select(-luas)
View(pa_interaksi_17)


write_xlsx(pa_interaksi_17, "D:/006_pascasarjana/008_thesis/004_thesis/data/data-sebaran-interaksi/data_produce_r/007_compile_data_imo.xlsx")




