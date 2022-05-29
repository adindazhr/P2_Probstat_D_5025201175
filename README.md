# Praktikum 2 Probstat

## 1

Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel sebanyak 9 responden. Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut diminta melakukan aktivitas 𝐴. Setelah 15 menit, peneliti tersebut mencatat kembali kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas 𝐴

| Responden | X  | Y  |
| :---:   | :-: | :-: |
| 1 | 78 | 100 |
| 2 | 75 | 95 |
| 3 | 67 | 70 |
| 4 | 77 | 90 |
| 5 | 70 | 90 |
| 6 | 72 | 90 |
| 7 | 78 | 89 |
| 8 | 74 | 90 |
| 9 | 77 | 100 |

Berdasarkan data pada tabel diatas, diketahui kadar saturasi oksigen dari responden ke-3 ketika belum melakukan aktivitas 𝐴 sebanyak 67, dan setelah melakukan aktivitas 𝐴 sebanyak 70.

### a. Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel diatas

```
x <- c(78,75,67,77,70,72,78,74,77)
y <- c(100,95,70,90,90,90,89,90,100)
selisih <- y-x
standardeviasi <- sd(selisih)
```
Pertama, akan dicari selisih dari kolom x dan y. Lalu, dicari standar deviasinya dengan menggunakan function sd()

![image](https://user-images.githubusercontent.com/89954689/170876659-d062be5f-c7be-434c-8417-8667eaa9b24a.png)

### b. Carilah nilai t (p-value)

```
library(BSDA)
t.test(selisih, alternative = 'two.sided', sigma.x=standardeviasi, mu=15)
```
Menggunakan library BSDA yang didalamnya terdapat fungsi t.test() akan dicari nilai t.

![image](https://user-images.githubusercontent.com/89954689/170876615-3bfb710d-9962-4757-a400-735ff3ee0098.png)

### c. Tentukanlah apakah terdapat pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴 jika diketahui tingkat signifikansi 𝛼 = 5% serta H0 : “tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”

```
qt(p = 0.025, df = 8, lower.tail = FALSE)
```
Kita dapat menggunakan fungsi qt untuk mencari tahu berapa nilai t.
Kita menerima h0 atau kita menolak h1, dikarenakan hasil uji statistik terletak di selang nilai kritikal

![image](https://user-images.githubusercontent.com/89954689/170876932-6e567d54-6266-4189-9a9d-962bb03c8a32.png)

## 2

Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun. Untuk menguji klaim ini, 100 pemilik mobil yang dipilih secara acak diminta untuk
mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata 23.500 kilometer dan standar deviasi 3900 kilometer. (Kerjakan menggunakan library seperti referensi pada modul).

### a. Apakah Anda setuju dengan klaim tersebut?

```
library(BSDA)
zsum.test(mean.x=23500, sigma.x=3900, n.x=100, alternative="greater", mu=20000)
```
Dengan menggunakan library BSDA yang didalamnya terdapat fungsi zsum.test(), kita dapat mendapatkan kesimpulan dari permasalahan ini. Kita menggunakan z test dikarenakan n lebih besar dari 30. Dari hasil yang didapatkan maka kami setuju dengan klaim tersebut.

![image](https://user-images.githubusercontent.com/89954689/170877254-427e14af-f4e0-435a-ac2c-efe5f00ab946.png)

### b. Jelaskan maksud dari output yang dihasilkan!

Berdasarkan output yang dihasilkan, diperoleh nilai z hitung = 8.9744 dan nilai p-value < 2.2e-16. Dari hasil tersebut peneliti dapat menolak hipotesis nol dan disimpulkan bahwa terdapat cukup bukti di mana rata-rata secara signifikan lebih besar dari 20000. Selain itu, dari output di atas, kita juga peroleh selang kepercayaan rata-rata dari sampel adalah 22858.51 atau dapat dinyatakan bahwa dengan selang  kepercayaan 95% kita yakin rata-rata weight akan lebih besar sama dengan 22858.51.

### c. Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!

p-value adalah probabilitas untuk memperoleh hasil setidaknya sama ekstremnya dengan yang sekarang,  dengan asumsi bahwa hipotesis nol benar. p-value adalah sebuah pengukuran yang dapat memberi tahu kita seberapa banyak data yang diamati tidak sesuai dengan hipotesis nol. ketika nilai p sangat rendah, data kita tidak sesuai dengan hipotesis nol maka kita akan menolak hipotesis nol. Begitu juga sebaliknya. Dikarenakan pada data ini p-valuenya sangat kecil maka kita akan menolak hipotesis nol.

## 3

Diketahui perusahaan memiliki seorang data analyst ingin memecahkan permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya didapatkanlah data berikut dari perusahaan saham tersebut.

| Nama Kota/Atribut | Bandung  | Bali  |
| :---:   | :-: | :-: |
| Jumlah Saham | 19 | 27 |
| Sampel Mean | 3.64 | 2.79 |
| Sampel Standar Deviasi | 1.67 | 1.32 |

Dari data diatas berilah keputusan serta kesimpulan yang didapatkan dari hasil diatas. Asumsikan nilai variancenya sama, apakah ada perbedaan pada rata-ratanya (α= 0.05)? Buatlah :

### a. H0 dan H1

H0: μ Bali = μ Bandung
H1: μ Bali ≠ μ Bandung

### b. Hitung Sampel Statistik

```
library(BSDA)
tsum.test(mean.x = 3.64, s.x = 1.67, n.x = 19, mean.y = 2.79, s.y = 1.32, n.y = 27, conf.level = 0.95)
```
Menggunakan library BSDA yang didalamnya terdapat fungsi t.test() akan dicari sampel statistik.

![image](https://user-images.githubusercontent.com/89954689/170877826-03021b0c-039c-477e-8b57-eca034712ef6.png)

### c. Lakukan Uji Statistik (df =2)

```
spkuadrat <- (((27 - 1)*(1.32)^2) + ((19 - 1)*(1.67)^2))/(27 + 19 - 2)
sp <- sqrt(spkuadrat)
xkuadrat <- (1/27) + (1/19)
x <- sqrt(xkuadrat)
t <- (2.79 - 3.64)/(sp*x)
t
```

![image](https://user-images.githubusercontent.com/89954689/170878358-6f4bafd6-e6c4-4ec2-9c33-b54c3fa45c4c.png)


### d. Nilai Kritikal

```
qt(p = 0.025, df = 2, lower.tail = FALSE)
```
Dengan menggunakan fungsi qt, kita dapat mencari nilai kritikal.

![image](https://user-images.githubusercontent.com/89954689/170878419-addf2efd-9a92-4e24-8626-cedc3a166494.png)

### e. Keputusan

Kita menerima h0 atau kita menolak h1, dikarenakan hasil uji statistik terletak di selang nilai kritikal

### f. Kesimpulan

Berdasarkan hasil pengujian, diketahui bahwa rata-rata saham Bali dan Bandung adalah sama.

## 4

Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya ia mengumpulkan data tiga spesies kucing yaitu kucing oren, kucing hitam dan kucing putih dengan panjangnya masing-masing. Jika :

diketahui dataset https://intip.in/datasetprobstat1
H0 : Tidak ada perbedaan panjang antara ketiga spesies atau rata-rata panjangnya sama

### a. Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1,grup 2,grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan lihat apakah ada outlier utama dalam homogenitas varians.

```
df4 <- read.delim("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt")
library(ggpubr)
ggboxplot(df4, x = "Group", y = "Length", 
          color = "Group",
          ylab = "Length", xlab = "Group")
```
Plot kuantil normal akan digambarkan dengan fungsi ggboxplot() dari library ggpubr. Lalu, di group sesuai dengan kolom Group. Tidak terdapat Outliers dikarenakan tidak ada titik yang diluar boxplot.

![image](https://user-images.githubusercontent.com/89954689/170878793-2e21634c-50c2-4074-b0bc-0bc74faaa79f.png)

### b. Carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?

```
library(onewaytests)
bartlett.test(Length ~ Group, data = df4)
```
Dengan bartlett.test didapatkan nilai p nya adalah 0.8054. Dikarenakan nilai pnya adalah 0.8054 atau nilai p lebih besar dari 0.05, maka dapat disimpulkan bahwa h0 tidak ditolak.

![image](https://user-images.githubusercontent.com/89954689/170879108-3d1d0d44-df1e-42b4-b11d-0e0fc625b798.png)

### c. Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus Grup dan beri nama model tersebut model 1.

```
Model1 <- lm(formula = Group ~ Length, data = df4)
print(Model1)
```
Untuk membuat model linier, kita menggunakan fungsi lm. Dan model tersebut diberi nama model1

![image](https://user-images.githubusercontent.com/89954689/170879221-d2031e52-87ca-4f41-9fb1-d0210ab301de.png)

### d. Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan dari H0?

```
summary(Model1)$coefficients[2,4]
```
Dikarenakan hasil dari p masih lebih besar dari 0,05 maka h0 tidak ditolak

![image](https://user-images.githubusercontent.com/89954689/170879274-e8a92c7c-e801-48b6-b315-9b854583e083.png)

### e. Verifikasilah jawaban model 1 dengan Post-hoc test Tukey HSD, dari nilai p yang didapatkan apakah satu jenis kucing lebih panjang dari yang lain?

```
library(multcomp)
post.hoc <- glht(Model1, linfct = mcp(Group = 'Tukey'))
summary(post.hoc)
```
Masih terdapat error sehingga tidak bisa diverifikasi

### f. Visualisasikan data dengan ggplot2

```
ggplot(data = df4, mapping = aes(x = Group, y = Length)) +
    geom_point()
```

![image](https://user-images.githubusercontent.com/89954689/170880199-c33e5c5f-19ce-42e2-8b82-425adbd079bf.png)


## 5

Data yang digunakan merupakan hasil eksperimen yang dilakukan untuk mengetahui pengaruh suhu operasi (100˚C, 125˚C dan 150˚C) dan tiga jenis kaca pelat muka (A, B dan C) pada keluaran cahaya tabung osiloskop. Percobaan dilakukan sebanyak 27 kali dan didapat data sebagai berikut: Data Hasil Eksperimen. Dengan data tersebut:

### a. Buatlah plot sederhana untuk visualisasi data

```
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

GTL <- read.csv('https://drive.google.com/u/0/uc?id=1aLUOdw_LVJq6VQrQEkuQhZ8FW43FemTJ&export=download')
head(GTL)

str(GTL)

qplot(x = Temp, y = Light, geom = "point", data = GTL) +
  facet_grid(.~Glass, labeller = label_both)
```
Plot sederhana ini dibuat dengan fungsi qplot()

![image](https://user-images.githubusercontent.com/89954689/170880317-b2c6ded3-7fe7-41c2-b567-4fd91fb73e09.png)

![image](https://user-images.githubusercontent.com/89954689/170880322-525edd03-8d46-4f38-ba4c-de0335d0ec5b.png)

### b. Lakukan uji ANOVA dua arah

```
GTL$Glass <- as.factor(GTL$Glass)
GTL$Temp_Factor <- as.factor(GTL$Temp)
str(GTL)

anova <- aov(Light ~ Glass*Temp_Factor, data = GTL)
summary(anova)
```
Uji anova dua arah dilakukan dengan fungsi aov() lalu di summary

![image](https://user-images.githubusercontent.com/89954689/170880360-a57e2220-2f1a-458a-8e3e-1b3342130a7d.png)

### c. Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)

```
data_summary <- group_by(GTL, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)
```
Mean dan standar deviasi dihitung dengan fungsi mean() dan fungsi sd()

![image](https://user-images.githubusercontent.com/89954689/170880407-57c999a1-843e-4cd1-8a56-cdd914f9ae32.png)

### d. Lakukan uji Tukey

```
tukey <- TukeyHSD(anova)
print(tukey)
```
Uji Tukey dilakukan dengan menggunakan fungsi TukeyHSD()

![image](https://user-images.githubusercontent.com/89954689/170880442-4cb1c7b8-bdea-44f2-92e6-e6ff674fff1b.png)


### e. Gunakan compact letter display untuk menunjukkan perbedaan signifikan antara uji Anova dan uji Tukey

```
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data_summary$Tukey <- cld$Letters
print(data_summary)
```
Fungsi multcompLetters4() digunakan untuk menunjukkan perbedaan signifikan antara uji Anova dan uji Tukey

![image](https://user-images.githubusercontent.com/89954689/170880481-af8dd104-3b49-48af-a05f-bd269fba9aa2.png)
