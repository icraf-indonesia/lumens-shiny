---
title: "Modul SCIENDO Train"
output: 
  html_document: 
    toc: false
    toc_float: true
    number_sections: true
    df_print: kable
    math_method:
      engine: mathjax
---

# Gambaran Umum

Modul SCIENDO Train memungkinkan pengguna untuk melakukan pelatihan model, sebuah langkah untuk merekam pola perubahan tutupan/penggunaan lahan beserta faktor-faktor yang memengaruhinya sebagai pembelajaran untuk melakukan proyeksi tutupan/penggunaan lahan di masa depan pada modul SCIENDO Simulate.

# Konsep Utama

Modul ini menggunakan Dinamica Ego sebagai mesin analisis dan mengimplementasikan sejumlah komponen dalam metode proyeksi tutupan/penggunaan lahan, di antaranya:

1. **Matriks Probabilitas Transisi (TPM)**: Metode identifikasi kemungkinan luas tutupan/penggunaan lahan yang berubah pada suatu periode waktu.

2. **Weight of Evidence (WoE)**: Metode identifikasi faktor-faktor yang memberikan pengaruh pada terjadinya perubahan tutupan/penggunaan lahan.

# Data dan Variabel Input

Sebelum menggunakan alat SCIENDO Train, siapkan hal-hal berikut:

1. **Peta Tutupan/Penggunaan Lahan**: Dua file raster (format GeoTIFF) yang merepresentasikan penggunaan/tutupan lahan pada dua titik waktu yang berbeda.
2. **Peta Unit Perencanaan**: File raster (format GeoTIFF) atau shapefile yang berisi zona administratif atau zona pengelolaan.
3. **Peta Faktor**: Sekumpulan file raster yang menggambarkan faktor pendorong atau variabel pemicu terjadinya perubahan tutupan/penggunaan lahan.
4. **Path DINAMICA EGO (Opsional)**: Path khusus ke direktori DINAMICA EGO yang terpasang di komputer Anda, jika DINAMICA EGO diinstal pada direktori kustom. Jika tidak ditentukan, path default akan digunakan dengan asumsi DINAMICA EGO terinstal di direktori standar.

<font size="2">Sebelum menjalankan modul SCIENDO Train, pastikan Anda telah mengunduh dan menginstal perangkat lunak DINAMICA EGO. DINAMICA EGO adalah sebuah platform gratis dan non-komersial untuk pemodelan lingkungan dengan berbagai kemungkinan luar biasa untuk perancangan. Anda dapat mengunduh versi 6 atau yang lebih rendah [**di sini**](https://csr.ufmg.br/dinamica/dinamica-6/), dan tetap kompatibel dengan versi yang lebih lama. Untuk deskripsi lebih lanjut, silakan kunjungi [Dokumentasi DINAMICA EGO](https://dinamicaego.com/dokuwiki/doku.php?id=guidebook_start).</font>


## Menggunakan Modul SCIENDO Train

<video controls style="max-width:80%;height:auto;">
    <source src="sciendo_train.mp4" type="video/mp4">
    Browser Anda tidak mendukung tag video.
</video>

Ikuti langkah-langkah berikut untuk menggunakan modul ini:

1.  **Peta Tutupan/Penggunaan Lahan T1**
    -   Klik "Browse" dan pilih file raster tutupan lahan untuk titik waktu pertama (T1).
2.  **Tahun T1**
    -   Masukkan tahun yang sesuai dengan data tutupan lahan T1.
3.  **Peta Tutupan/Penggunaan Lahan T2**
    -   Klik "Browse" dan pilih file raster tutupan lahan untuk titik waktu kedua (T2).
4.  **Tahun T2**
    -   Masukkan tahun yang sesuai dengan data tutupan lahan T2.
5.  **Unit Perencanaan**
    -   Klik "Browse" dan pilih shapefile atau raster unit perencanaan.
6.  **Folder Faktor**
    -   Klik "Factor(s) Folder Path" untuk memilih lokasi folder tempat peta faktor disiapkan.
7.  **Direktori Output**
    -   Klik "Select output directory" untuk memilih lokasi penyimpanan hasil analisis.
8.  **Jalankan Analisis**
    -   Klik "Run" untuk memulai proses.


# Hasil analisis

Setelah analisis selesai, Anda akan menemukan hal-hal berikut di direktori output yang dipilih:

1. Matriks Transisi Baseline: Kumpulan file matriks transisi untuk setiap kelas unit perencanaan yang dimodelkan.
2. Direktori WoE: Direktori ini berisi kumpulan hasil pelatihan model WoE dalam file DCF dan CSV, yang diorganisir berdasarkan unit perencanaan.
3. Faktor SCIENDO: File ERS yang memuat citra dan metadata peta faktor yang digunakan dalam ER Mapper Raster.
4. File EGOML: Format file yang berisi model DINAMICA EGO.
5. Laporan Pelatihan SCIENDO: Ringkasan hasil analisis dalam format HTML, yang menyediakan interpretasi terperinci serta visualisasi dari output tersebut.


