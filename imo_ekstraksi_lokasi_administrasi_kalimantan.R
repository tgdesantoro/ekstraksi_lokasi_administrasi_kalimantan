library(readr)
imo <- read.csv2("D:/013_text_mining/imo_text_mining/imo_dateformat_waste_utf.csv") # membaca file data utama
kabupaten_ind <- read.csv2("D:/013_text_mining/kalimantan_bow/kabupaten_bow.csv")   # membaca file data lokasi administrasi level kabupaten format CSV
kabupaten_id <- readLines("D:/013_text_mining/kalimantan_bow/kabupaten_bow.txt")    # membaca file data lokasi administrasi level kabupaten format teks


# ===== profil data =====

View(imo)               # melihat atribut tabel data utama
Encoding(imo$content)   # periksa format data
View(kabupaten_ind)     # melihat atribut tabel lokasi administrasi
print(kabupaten_id)     # melihat data pada console

names(imo)                     # melihat nama kolom
content_imo <- imo$content     # membuat dataframe bagian kolom konten berita
typeof(content_imo)            # periksa format data

# ===========================
# ===== install package =====
# ===========================
library(dplyr)       # manipulasi data
library(stringr)     # manipulasi teks
library(tidytext)    # olah data
library(magrittr)    # aktivasi pipe (fungsi dalam fungsi)
library(stringi)     # manipulasi dan pengolahan string
library(purrr)       # deteksi kesalahan atau nilai NA

# ========================================
# ===== standarisasi teks ================
# ========================================

content_standard <- 
  content_imo %>%
  str_to_lower() %>%                             # change text to lowercase
  str_replace_all("[[:punct:]]", "") %>%         # removing punctuation
  stri_replace_all_regex("\\d+", "") %>%         # deleting numbers with strings
  stri_replace_all_regex("[^[:print:]]", "") %>% # removing non-printable characters with stringi
  str_trim()                                     # Mengurangi spasi berlebih
print(content_standard)
  

# =========================================
# ===== ekstraksi lokasi administrasi kabupaten =====
# =========================================
# ===== #1 ekstraksi kabupaten berdasarkan kamus =======
imo$kabupaten <- NA                                           # membuat kolom baru untuk penempatan ekstraksi nama kabupaten

#Looping for every colom in dataset "imo"                     # mulai pencarian tiap paris konten berita
for (i in 1:length(content_standard)){
  for (j in 1:length(kabupaten_id)) {
    
    # Cek apakah kata yang ingin dicari ada di dalam teks
    if (str_detect(content_standard[i], kabupaten_id[j])){    # menemukan lokasi administrasi berdasarkan kamus
    #ekstrak teks dan simpan di kolom baru
    imo$kabupaten[i] <- kabupaten_id[j]
    break
    }
  }
}

imo$kabupaten                                                             # lihat hasil pencarian                              
imo[is.na(imo$kabupaten), ]                                               # deteksi nilai NA
View(imo)                                                                 # lihat data
write.csv2(imo,"D:/013_text_mining/imo_text_mining/imo_extract_kbpt.csv") # simpan data

# ===== #2 ekstraksi kabupaten berdasarkan penanda kata / linguistik "kabupaten" =====
# ==========================================================================

# Ekspresi reguler untuk mencari 'kabupaten' dan dua kata setelahnya
pola_kabupaten <- "(?i)\\bKabupaten\\b\\s+\\w+\\s+\\w+"

# Ekspresi reguler untuk mencari 'kota' dan dua kata setelahnya
pola_kota <- "(?i)\\bKota\\b\\s+\\w+\\s+\\w+"

# Menggunakan loop untuk memproses setiap baris
for (i in 1:nrow(imo)) {
  # Periksa apakah kolom kabupaten adalah NA
  if (is.na(imo$kabupaten[i])) {
    # Ekstrak dari kolom content_standard menggunakan pola yang sesuai
    imo[i, "kabupaten"] <- str_extract(content_standard[i], pola_kabupaten)
    # Jika hasil ekstraksi adalah NA, coba pola kota
    if (is.na(imo[i, "kabupaten"])) {
      imo[i, "kabupaten"] <- str_extract(content_standard[i], pola_kota)
    }
  }
}

View(imo)
print(imo$kabupaten)

# Menampilkan nilai kolom 'kabupaten' pada baris bernilai NA sebelumnya
imo[is.na(imo$kabupaten), ]

selected_rows <- c(32, 76, 153, 181, 183, 186, 194, 195)
imo[selected_rows, "kabupaten"]

#menghilangkan kata/teks yang tidak sesuai di kolom kabupaten
kata_hilang <- c("kepala", "kalbar", "kalimantan")

imo <- imo %>%
  mutate(kabupaten = str_replace_all(kabupaten, paste(kata_hilang, collapse = "|"), "")) %>% # Menghilangkan kata-kata tersebut dari kolom 'kabupaten'
  mutate(kabupaten = str_trim(kabupaten)) %>%   # Menghapus spasi berlebih
  mutate(kabupaten = str_replace_all(kabupaten, "Kota Palangka Raya", "Kota Palangkaraya")) #menyambung kata

write.csv2(imo,"D:/013_text_mining/imo_text_mining/imo_extract_kbpt_clear.csv")
View(imo)

# ==========================================================
# ===== ekstraksi lokasi administrasi kecamatan =====
# ===================================================
# membaca data pasca ekstraksi kabupaten
imo_kabupaten <- read.csv2("D:/013_text_mining/imo_text_mining/imo_extract_kbpt_clear.csv") # membaca file data utama dengan nama kabupaten
View(imo_kabupaten)                                                                         # lihat data
reg_kalimantan <- read.csv2("D:/013_text_mining/kalimantan_bow/reg_kalimantan.csv")         # membaca file data lokasi administrasi kalimantan
View(reg_kalimantan)                                                                        # lihat file lokasi administrasi kalimantan
content_extract <- imo_kabupaten$content                                                    # membuat dataframe konten
print(content_extract)

# ========================================
# ===== standarisasi teks ================
# ========================================
content_standard2 <- 
  content_extract %>%
  str_to_lower() %>%                             # Mengubah teks menjadi huruf kecil
  str_replace_all("[[:punct:]]", "") %>%         # Menghapus tanda baca
  stri_replace_all_regex("\\d+", "") %>%         # Menghapus nomor dengan stringi
  stri_replace_all_regex("[^[:print:]]", "") %>% # Menghapus karakter non-printable dengan stringi
  str_trim()                                     # Mengurangi spasi berlebih
print(content_standard2)

# ===== membuat dataframe nama kecamatan dari kamus =====
# konten nama kecamatan
kecamatan_borneo <- reg_kalimantan$nama_kecamatan_big
print(kecamatan_borneo)

# Mengambil nama kecamatan yang berbeda pada kolom kecamatan_big
reg_kecamatan <- unique(kecamatan_borneo)
print(reg_kecamatan)

#buat kolom baru untuk penempatan ekstrak data kecamatan
imo_kabupaten$kecamatan <- NA

# ===== #1 ekstraksi kecamatan berdasarkan kamus ======
#Looping for every colom in dataset "imo"
for (i in 1:length(content_standard2)){
  for (j in 1:length(reg_kecamatan)) {
    
    # Cek apakah kata yang ingin dicari ada di dalam teks
    if (str_detect(content_standard2[i], reg_kecamatan[j])){
      #ekstrak teks dan simpan di kolom baru
      imo_kabupaten$kecamatan[i] <- reg_kecamatan[j]
      break
    }
  }
}
imo_kabupaten$kecamatan

#Menampilkan nilai kolom 'kecamatan' pada baris bernilai NA sebelumnya
imo_kabupaten[is.na(imo_kabupaten$kecamatan), ]

#Menghitung jumlah nilai NA pada kolom 'kecamatan'
sum(is.na(imo_kabupaten$kecamatan))

# ===== #2 ekstraksi kecamatan berdasarkan penanda kata / linguistik "kecamatan" ===== 
# Ekspresi reguler untuk mencari 'kecamatan' dan dua kata setelahnya
pola_kecamatan <- "(?i)\\bkecamatan\\b\\s+\\w+\\s+\\w+"

# Menggunakan loop untuk memproses setiap baris
for (i in 1:nrow(imo_kabupaten)) {
  # Periksa apakah kolom kabupaten adalah NA
  if (is.na(imo_kabupaten$kecamatan[i])) {
    # Ekstrak dari kolom content_standard menggunakan pola yang sesuai
    imo_kabupaten[i, "kecamatan"] <- str_extract(content_standard2[i], pola_kecamatan)
  }
}
print(imo_kabupaten$kecamatan)

#Menghitung jumlah nilai NA pada kolom 'kecamatan'
sum(is.na(imo_kabupaten$kecamatan))

#menghilangkan baris dengan nilai NA pada kolom 'kecamatan'
kecamatan_clean_na <- imo_kabupaten %>% 
  filter(!is.na(imo_kabupaten$kecamatan))
print(kecamatan_clean_na)
print(kecamatan_clean_na$kecamatan)

# =======================================
# mengoreksi nama kecamatan dengan kesamaan kata pada kecamatan_clean_na dengan BoW kecamatan
# =======================================
# membuat list nama kecamatan pada kecamata_clean_na
unique_kecamatan_na <- unique(kecamatan_clean_na$kecamatan)
unique_kecamatan_na

# temukan nama kecamatan yang sama
common_kecamatan <- unique_kecamatan_na[unique_kecamatan_na %in% reg_kecamatan]
common_kecamatan
kecamatan_clean_na$match_kecamatan <- kecamatan_clean_na$kecamatan %in% reg_kecamatan
View(kecamatan_clean_na)

# hitung persentase nama-nama kecamatan yang sama untuk setiap baris
total_unique_kecamatan_reg <- length(unique_kecamatan_na)
kecamatan_clean_na$persentase_kecamatan <- ifelse(kecamatan_clean_na$kecamatan %in% reg_kecamatan, 
                                             (1 / total_unique_kecamatan_reg) * 100, 
                                             0)

View(kecamatan_clean_na)

# menghitung jarak kedekatan kata berdasarkan karakter (Jaro-Winkler distance)
install.packages("stringdist")
library(stringdist)

# Buat vektor kosong untuk menyimpan jarak terdekat
kecamatan_clean_na$jw_distance <- NA
kecamatan_clean_na$matched_kecamatan <- NA

# Loop untuk setiap baris di kecamatan_clean_na
for (i in 1:nrow(kecamatan_clean_na)) {
  # Hitung jarak Jaro-Winkler antara kecamatan_clean_na dan semua reg_kecamatan
  distances <- stringdist(kecamatan_clean_na$kecamatan[i], reg_kecamatan, method = "jw")
  
  # Simpan jarak terkecil dan nama kecamatan yang paling mirip
  kecamatan_clean_na$jw_distance[i] <- min(distances)
  kecamatan_clean_na$matched_kecamatan[i] <- reg_kecamatan[which.min(distances)]
}

# Lihat hasilnya
head(kecamatan_clean_na)
View(kecamatan_clean_na)

# koreksi pencilan
# Menghapus baris dengan kata "kecamatan siantan kabupaten" pada kolom 'kecamatan'
kecamatan_clean_na <- kecamatan_clean_na[!grepl("kecamatan siantan kabupaten", kecamatan_clean_na$kecamatan), ]

View(kecamatan_clean_na)

# Update the 'kabupaten' column if 'matched_kecamatan' contains "kecamatan jongkat"
kecamatan_clean_na$kabupaten[kecamatan_clean_na$matched_kecamatan == "kecamatan jongkat"] <- "kabupaten mempawah"

View(kecamatan_clean_na)

# Update the 'kabupaten' and 'matched_kecamatan' columns if 'kecamatan' contains "kecamatan ambawang"
kecamatan_clean_na$kabupaten[kecamatan_clean_na$kecamatan == "kecamatan ambawang kabupaten"] <- "kabupaten kubu raya"
kecamatan_clean_na$matched_kecamatan[kecamatan_clean_na$kecamatan == "kecamatan ambawang kabupaten"] <- "kecamatan sungai ambawang"

View(kecamatan_clean_na)

# Update the 'matched_kecamatan' column if 'kecamatan' contains "kecamatan tumbang jutuh"
kecamatan_clean_na$matched_kecamatan[kecamatan_clean_na$kecamatan == "kecamatan tumbang jutuh"] <- "kecamatan rungan"

# Update the 'matched_kecamatan' column if 'kecamatan' contains "kecamatan kuala pembuang"
kecamatan_clean_na$matched_kecamatan[kecamatan_clean_na$kecamatan == "kecamatan kuala pembuang"] <- "kecamatan seruyan hilir timur"

View(kecamatan_clean_na)

# Update the 'matched_kecamatan' column if 'kecamatan' contains "kecamatan kelam kabupaten"
kecamatan_clean_na$matched_kecamatan[kecamatan_clean_na$kecamatan == "kecamatan kelam kabupaten"] <- "kecamatan kelam permai"

# Update the 'kabupaten' and 'matched_kecamatan'' column if 'kecamatan' contains "kecamatan kahayan kabupaten"
kecamatan_clean_na$kabupaten[kecamatan_clean_na$kecamatan == "kecamatan kahayan kabupaten"] <- "kabupaten pulang pisau"
kecamatan_clean_na$matched_kecamatan[kecamatan_clean_na$kecamatan == "kecamatan kahayan kabupaten"] <- "kecamatan kahayan tengah"

# Update the 'matched_kecamatan' column if 'kecamatan' contains "kecamatan wajok hilir"
kecamatan_clean_na$matched_kecamatan[kecamatan_clean_na$kecamatan == "kecamatan wajok hilir"] <- "kecamatan jongkat"

# Update the 'matched_kecamatan' column if 'kecamatan' contains "kecamatan paduran sebangau"
kecamatan_clean_na$matched_kecamatan[kecamatan_clean_na$kecamatan == "kecamatan paduran sebangau"] <- "kecamatan sebangau kuala"

# Update the 'matched_kecamatan' column if 'kecamatan' contains "kecamatan komam kabupaten"
kecamatan_clean_na$matched_kecamatan[kecamatan_clean_na$kecamatan == "kecamatan komam kabupaten"] <- "kecamatan muara komam"

# Update the 'matched_kecamatan' column if 'kecamatan' contains "kecamatan seranau kabupaten"
kecamatan_clean_na$matched_kecamatan[kecamatan_clean_na$kecamatan == "kecamatan seranau kabupaten"] <- "kecamatan seranau"

write.csv2(kecamatan_clean_na,"D:/013_text_mining/imo_text_mining/imo_step2_kecamatan.csv")

# =====================================================
# ===== ekstraksi lokasi administrasi desa ============
# =====================================================
# membaca data pasca ekstraksi kabupaten
imo_kecamatan <- read.csv2("D:/013_text_mining/imo_text_mining/imo_step2_kecamatan.csv")
View(imo_kecamatan)
reg_kalimantan <- read.csv2("D:/013_text_mining/kalimantan_bow/reg_kalimantan.csv")
View(reg_kalimantan)
content_kecamatan <- imo_kecamatan$content
print(content_kecamatan)

# ========================================
# ===== standarisasi teks ================
# ========================================

content_standard3 <- 
  content_kecamatan %>%
  str_to_lower() %>%                             # Mengubah teks menjadi huruf kecil
  str_replace_all("[[:punct:]]", "") %>%         # Menghapus tanda baca
  stri_replace_all_regex("\\d+", "") %>%         # Menghapus nomor dengan stringi
  stri_replace_all_regex("[^[:print:]]", "") %>% # Menghapus karakter non-printable dengan stringi
  str_trim()                                     # Mengurangi spasi berlebih
print(content_standard3)

# ===== membuat dataframe nama desa dari kamus =====
desa_borneo <- reg_kalimantan$nama_desa_big
print(desa_borneo)

# Mengambil nama desa yang berbeda pada kolom desa_big
reg_desa <- unique(desa_borneo)
print(reg_desa)

#buat kolom baru untuk penempatan ekstrak data desa
imo_kecamatan$desa <- NA

# ===== #1 ekstraksi desa berdasarkan kamus =====
# ===============================================
# ===== Looping for every colom in dataset "imo_kecamatan"
for (i in 1:length(content_standard3)){
  for (j in 1:length(reg_desa)) {
    
    # Cek apakah kata yang ingin dicari ada di dalam teks
    if (str_detect(content_standard3[i], reg_desa[j])){
      #ekstrak teks dan simpan di kolom baru
      imo_kecamatan$desa[i] <- reg_desa[j]
      break
    }
  }
}
imo_kecamatan$desa

#Menampilkan nilai kolom 'desa' pada baris bernilai NA sebelumnya
imo_kecamatan[is.na(imo_kecamatan$desa), ]

#Menghitung jumlah nilai NA pada kolom 'desa'
sum(is.na(imo_kecamatan$desa))

View(imo_kecamatan)

# ===== #2 ekstraksi desa berdasarkan penanda kata/linguistik "desa","kampung" =====
# ======================================================
# mengekstrak nama desa, kelurahan dan kampung
# Ekspresi reguler untuk mencari 'desa' dan dua kata setelahnya
pola_desa <- "(?i)\\bdesa\\b\\s+\\w+\\s+\\w+"

# Ekspresi reguler untuk mencari 'kota' dan dua kata setelahnya
pola_kelurahan <- "(?i)\\bkelurahan\\b\\s+\\w+\\s+\\w+"

# Ekspresi reguler untuk mencari 'kota' dan dua kata setelahnya
pola_kampung <- "(?i)\\bkampung\\b\\s+\\w+\\s+\\w+"

# Menggunakan loop untuk memproses setiap baris
for (i in 1:nrow(imo_kecamatan)) {
  # Periksa apakah kolom desa adalah NA
  if (is.na(imo_kecamatan$desa[i])) {
    # Ekstrak dari kolom content_standard3 menggunakan pola yang sesuai
    imo_kecamatan[i, "desa"] <- str_extract(content_standard3[i], pola_desa)
    # Jika hasil ekstraksi adalah NA, coba pola kelurahan
    if (is.na(imo_kecamatan[i, "desa"])) {
      imo_kecamatan[i, "desa"] <- str_extract(content_standard3[i], pola_kelurahan)
      # Jika hasil ekstraksi adalah NA, coba pola kelurahan
      if (is.na(imo_kecamatan[i, "desa"])) {
        imo_kecamatan[i, "desa"] <- str_extract(content_standard3[i], pola_kampung)
      }
    }
  }
}
print(imo_kecamatan$desa)

# Menghitung jumlah nilai NA pada kolom 'desa'
sum(is.na(imo_kecamatan$desa))

View(imo_kecamatan)

# menghilangkan baris dengan nilai NA pada kolom 'desa'
desa_clean_na <- imo_kecamatan %>% 
  filter(!is.na(imo_kecamatan$desa))
print(desa_clean_na)
print(desa_clean_na$desa)

# mengoreksi nama desa dengan kesamaan kata pada desa_clean_na dengan BoW desa

# membuat list nama kecamatan pada kecamatan_clean_na
unique_desa_na <- unique(desa_clean_na$desa)
unique_desa_na

# temukan nama desa yang sama
common_desa <- unique_desa_na[unique_desa_na %in% reg_desa]
common_desa
desa_clean_na$match_desa <- desa_clean_na$desa %in% reg_desa
View(desa_clean_na)

# Hitung persentase nama-nama desa yang sama untuk setiap baris
total_unique_desa_reg <- length(unique_desa_na)
desa_clean_na$persentase_desa <- ifelse(desa_clean_na$desa %in% reg_desa, 
                                             (1 / total_unique_desa_reg) * 100, 
                                             0)

View(desa_clean_na)

#menghitung jarak kedekatan kata berdasarkan karakter (Jaro-Winkler distance)
install.packages("stringdist")
library(stringdist)

# Buat vektor kosong untuk menyimpan jarak terdekat
desa_clean_na$jw_distance_desa <- NA
desa_clean_na$matched_desa <- NA

# Loop untuk setiap baris di desa_clean_na
for (i in 1:nrow(desa_clean_na)) {
  # Hitung jarak Jaro-Winkler antara desa_clean_na dan semua reg_desa
  distances_desa <- stringdist(desa_clean_na$desa[i], reg_desa, method = "jw")
  
  # Simpan jarak terkecil dan nama kecamatan yang paling mirip
  desa_clean_na$jw_distance_desa[i] <- min(distances_desa)
  desa_clean_na$matched_desa[i] <- reg_desa[which.min(distances_desa)]
}

# Lihat hasilnya
head(desa_clean_na)
View(desa_clean_na)


# Update the 'matched_kecamatan' and 'matched_desa' columns if 'desa' contains "desa sungai ibar"
desa_clean_na$matched_kecamatan[desa_clean_na$desa == "desa sungai ubar"] <- "kecamatan cempaga hulu"
desa_clean_na$matched_desa[desa_clean_na$desa == "desa sungai ubar"] <- "desa sungai ubar"


# Update the 'matched_desa' columns if 'desa' contains "desa kusambi hulu"
desa_clean_na$matched_desa[desa_clean_na$desa == "desa kumai hulu"] <- "kelurahan kumai hulu"

# Update the 'matched_desa' columns if 'desa' contains "kelurahan pasir panjang"
desa_clean_na$matched_desa[desa_clean_na$desa == "kelurahan pasir panjang"] <- "desa pasir panjang"

# Update the 'matched_desa' columns if 'desa' contains "desa batu layang"
desa_clean_na$matched_desa[desa_clean_na$desa == "desa batu layang"] <- "kelurahan batu layang"

# Update the 'matched_desa' columns if 'desa' contains "desa air hitam"
desa_clean_na$matched_desa[desa_clean_na$desa == "desa air hitam hulu"] <- "desa air hitam besar"

# Update the 'matched_desa' columns if 'desa' contains "desa bayuabang kecamatan"
desa_clean_na$matched_desa[desa_clean_na$desa == "desa bayuabang kecamatan"] <- "desa banyu abang"

# Update the 'matched_desa' columns if 'desa' contains "desa kandilo kabupaten"
desa_clean_na$matched_desa[desa_clean_na$desa == "desa kandilo kabupaten"] <- "desa kandolo"

# Update the 'matched_desa' columns if 'desa' contains "desa nehes liah"
desa_clean_na$matched_desa[desa_clean_na$desa == "desa nehes liah"] <- "desa nehas liah bing"

# Update the 'matched_desa' columns if 'desa' contains "desa santan kecamatan"
desa_clean_na$matched_desa[desa_clean_na$desa == "desa nehes liah"] <- "desa santan tengah"

# Update the 'matched_desa' columns if 'desa' contains "desa sepaso selatan"
desa_clean_na$matched_desa[desa_clean_na$matched_desa == "desa sepaso selatan"] <- "desa sepati selatan"

# update Menghapus baris dengan isi kolom desa 'kelurahan basir kecamatan'
desa_clean_na <- desa_clean_na %>% 
  filter(desa != "kelurahan basir kecamatan")

View(desa_clean_na)

# Update the 'matched_desa' columns if 'desa' contains "desa baamang barat"
desa_clean_na$matched_desa[desa_clean_na$desa == "desa baamang barat"] <- "kelurahan baamang barat"

# Update the 'kabupaten' and 'matched_desa' columns if 'kecamatan telaga antang
desa_clean_na$kabupaten[desa_clean_na$kecamatan == "kecamatan telaga antang"] <- "kabupaten kotawaringin timur"
desa_clean_na$matched_desa[desa_clean_na$kecamatan == "kecamatan telaga antang"] <- "desa tumbang sangai"

# Update the 'kabupaten' and 'matched_desa' columns if 'desa taringin kecamatan'
desa_clean_na$matched_desa[desa_clean_na$desa == "desa taringin kecamatan"] <- "desa taringen"

# ============================================
# ===== simpan ekstraksi lokasi administrasi kabupaten, kecamatan, desa =====
# ============================================
write.csv2(desa_clean_na,"D:/013_text_mining/imo_text_mining/imo_step3_desa.csv")
# ============================================

# ===== analisasi deskriptif terjadinya IMO =====
# ===============================================
library(dplyr)
library(magrittr)

data_interaksi <- read.csv2("D:/013_text_mining/imo_text_mining/imo_step3_desa.csv")

# Count orangutan incidents by kecamatan and kabupaten
incidents_by_kecamatan_kabupaten_desa <- 
  data_interaksi %>%  
  group_by(kabupaten, matched_kecamatan,matched_desa) %>% 
  summarise(topic = n())
print(incidents_by_kecamatan_kabupaten_desa)
View(incidents_by_kecamatan_kabupaten_desa)

incident_ou <- incidents_by_kecamatan_kabupaten_desa$incident_ou <- 1


#membuat grafik dengan treemap
install.packages("treemapify")
library(ggplot2)
library(treemapify)

# Create a treemap

incidents_by_kecamatan_kabupaten_desa <- incidents_by_kecamatan_kabupaten_desa %>%
  filter(topic > 0)
summary(incidents_by_kecamatan_kabupaten_desa$topic)

ggplot(incidents_by_kecamatan_kabupaten_desa, 
       aes(area = topic, fill = kabupaten, label = matched_kecamatan)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre", grow = TRUE) +
  theme_classic() +
  labs(fill = "Kabupaten")


ggplot(incidents_by_kecamatan_kabupaten_desa, 
       aes(area = topic, fill = kabupaten)) +
  geom_treemap() +
  theme_classic() +
  labs(fill = "Kabupaten")


