---
title: "Modul SCIENDO Simulate"
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

Modul SCIENDO Simulate memungkinkan pengguna untuk melakukan proses simulasi proyeksi tutupan/penggunaan lahan masa depan berdasarkan informasi pembelajaran model yang diperoleh pada modul SCIENDO Train.

# Konsep Utama

Modul ini melakukan simulasi perubahan tutupan/penggunaan lahan menggunakan pendekatan *Cellular Automata* (CA), di mana transisi suatu sel tidak hanya dipengaruhi oleh karakteristiknya sendiri, tetapi juga oleh pengaruh sel di sekitarnya (*neighborhood effect*). Kemudian model menetapkan probabilitas transisi dengan mempertimbangkan informasi matriks transisi dan bobot bukti (*weight of evidence*).

# Data dan Variabel Input

Sebelum menggunakan alat SCIENDO Simulate, siapkan hal-hal berikut:

1.  **Peta Tutupan Lahan pada T1**: File raster (format GeoTIFF) yang merepresentasikan penggunaan/tutupan lahan pada T1.
2.  **Tahun Awal**: Masukkan tahun yang menjadi titik awal periode simulasi.
3.  **Peta Unit Perencanaan**: File raster (format GeoTIFF) atau shapefile berisi zona administratif atau zona pengelolaan.
4.  **Tabel Lookup Tutupan Lahan**: File CSV yang menjelaskan nilai raster beserta kelas tutupan lahan yang sesuai.
5.  ***Raster Cube***: File raster (format GeoTIFF) atau file ERS yang berisi peta faktor yang dihasilkan oleh SCIENDO Train.
6.  **Repetisi**: Angka yang menunjukkan jumlah periode yang akan disimulasikan (e.g. 2x repetisi dari hasil pelatihan model 2015-2020 akan menghasilkan simulasi tahun 2025 dan 2030 jika tahun awal simulasi adalah 2020).
7.  **Alokasi Memori**: konfigurasi teknis tentang mekanisme mesin mengatur penyimpanan data temporer yang dihasilkan selama proses simulasi. Parameter ini menentukan performa mesin menjalankan simulasi.
8.  **Matriks Transisi**: Daftar file CSV yang berisi matriks probabilitas transisi tutupan lahan antara T1 dan T2 pada setiap unit perencanaan, dihasilkan oleh SCIENDO Scenario Builder.
9.  ***Weight of Evidence***: Daftar file DCF yang berisi bobot faktor peta yang dihasilkan oleh SCIENDO Train.
10. **Path DINAMICA EGO (Opsional)**: Jalur khusus ke direktori DINAMICA EGO yang terpasang di komputer Anda jika DINAMICA EGO diinstal pada direktori kustom. Jika tidak ditentukan, jalur default akan digunakan dengan asumsi DINAMICA EGO terinstal di direktori standar.

> <font size="2">Sebelum menjalankan modul SCIENDO Simulate, pastikan Anda telah mengunduh dan menginstal perangkat lunak DINAMICA EGO. DINAMICA EGO adalah platform gratis dan non-komersial untuk pemodelan lingkungan dengan berbagai kemungkinan luar biasa untuk perancangan. Anda dapat mengunduh versi 6 atau yang lebih rendah [**di sini**](https://csr.ufmg.br/dinamica/dinamica-6/) yang tetap kompatibel dengan versi lama. Untuk deskripsi lebih lanjut, silakan kunjungi [Dokumentasi DINAMICA EGO](https://dinamicaego.com/dokuwiki/doku.php?id=guidebook_start).</font>

## Menggunakan Modul SCIENDO Simulate

Ikuti langkah-langkah berikut untuk menggunakan alat ini:

<video controls style="max-width:80%;height:auto;">
    <source src="sciendo-sim.mp4" type="video/mp4">
    Browser Anda tidak mendukung tag video.
</video>

1.  **Tutupan/Penggunaan Lahan T1**
    -   Klik "Browse" dan pilih file raster tutupan lahan untuk titik waktu pertama (T1).
2.  **Unit Perencanaan**
    -   Klik "Browse" dan pilih shapefile unit perencanaan Anda.
3.  **Tabel Lookup Tutupan Lahan**
    -   Klik "Browse" dan pilih file CSV yang telah Anda siapkan berisi kelas tutupan lahan.
4.  **Raster Cube**
    -   Klik "Browse" dan pilih sciendo_factors.tif atau dua file ERS terkait.
5.  **Repetisi**
    -   Masukkan jumlah repetisi untuk menentukan jumlah peta proyeksi yang akan dihasilkan.
6.  **Folder Matriks Transisi**
    -   Klik "Transition Matrix Folder Path" untuk memilih lokasi folder output TPM yang disimpan atau diperoleh dari SCIENDO Scenario Builder.
7.  **Folder Weight of Evidence**
    -   Klik "Weight of Evidence Path" untuk memilih lokasi folder output DCF yang disimpan atau diperoleh dari SCIENDO Train.
8.  **Direktori Output**
    -   Klik "Select output directory" untuk memilih lokasi penyimpanan hasil analisis.
9.  **Jalankan Analisis**
    -   Klik "Run Analysis" untuk memulai proses.

# Hasil Analisis

Setelah analisis selesai, Anda akan menemukan hal-hal berikut di direktori output yang dipilih:

1.  **Peta proyeksi penggunaan/tutupan lahan**: Daftar file TIF yang berisi proyeksi penggunaan/tutupan lahan.
2.  **Tabel lookup proyeksi penggunaan/tutupan lahan**: File CSV yang berisi seluruh area hasil proyeksi penggunaan/tutupan lahan.
3.  **File EGOML**: Format file yang berisi model DINAMICA EGO.
4.  **Laporan SCIENDO Simulate**: Ringkasan hasil analisis dalam format HTML, yang menyediakan interpretasi terperinci serta visualisasi dari output tersebut.
