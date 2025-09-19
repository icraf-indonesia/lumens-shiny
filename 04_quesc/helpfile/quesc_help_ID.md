# Gambaran Umum

Modul *Quantification of Environmental Services for Carbon* (QUES-C) menghitung jumlah emisi gas rumah kaca (GRK) dari perubahan penggunaan/tutupan lahan dengan menggunakan pendekatan *stock difference*. Pendekatan *stock difference* memperkirakan emisi atau serapan GRK dari perubahan penggunaan lahan dengan menghitung perubahan stok karbon dari waktu ke waktu. Hal ini dilakukan dengan membandingkan karbon yang tersimpan pada berbagai jenis penggunaan lahan pada dua titik waktu, menggunakan *Activity Data* yang mengukur luas perubahan penggunaan lahan, dan *Emission Factors* yang merepresentasikan perubahan stok karbon terkait dengan setiap transisi penggunaan lahan. Dengan mengalikan *Activity Data* dengan *Emission Factors* yang sesuai, kita dapat menentukan emisi atau serapan bersih GRK yang dihasilkan dari perubahan penggunaan lahan dalam suatu wilayah dan periode waktu tertentu.

# Konsep Utama:

1.  **Analisis dinamika stok karbon** dilakukan untuk melihat perubahan stok karbon di suatu area selama satu periode, menggunakan metode *Stock Difference*. Stok karbon yang dipertimbangkan adalah rata-rata karbon atas permukaan tanah sepanjang waktu.
2.  **Emisi** dihitung sebagai penurunan jumlah stok karbon akibat perubahan tutupan lahan, jika jumlah stok karbon awal lebih tinggi dibandingkan dengan stok karbon setelah perubahan lahan.
3.  **Sekuestrasi** dihitung sebagai penambahan jumlah stok karbon akibat perubahan tutupan lahan (artinya stok karbon pada penggunaan lahan awal lebih rendah dibandingkan setelah perubahan lahan).

# Persiapan Data

Sebelum menggunakan alat Analisis QUES-C, siapkan data berikut:

1.  **Peta Tutupan Lahan**: Dua file raster (format GeoTIFF) yang merepresentasikan penggunaan/tutupan lahan pada dua titik waktu yang berbeda.
2.  **Peta Unit Perencanaan**: File raster (format GeoTIFF) atau shapefile berisi zona administratif atau zona pengelolaan.
3.  **Tabel Lookup Stok Karbon**: File CSV yang menjelaskan *emission factors* beserta kelas tutupan lahan yang sesuai.

## Menggunakan Alat Analisis QUES-C

Ikuti langkah-langkah berikut untuk menggunakan alat ini:

<video controls style="max-width:80%;height:auto;">
    <source src="quesc.mp4" type="video/mp4">
    Browser Anda tidak mendukung pemutar video.
</video>

1.  **Tutupan Lahan T1**
    -   Klik "Browse" dan pilih file raster tutupan lahan untuk titik waktu pertama (T1).
2.  **Tahun T1**
    -   Masukkan tahun yang sesuai dengan data tutupan lahan T1.
3.  **Tutupan Lahan T2**
    -   Klik "Browse" dan pilih file raster tutupan lahan untuk titik waktu kedua (T2).
4.  **Tahun T2**
    -   Masukkan tahun yang sesuai dengan data tutupan lahan T2.
5.  **Tabel Lookup Stok Karbon**
    -   Klik "Browse" dan pilih file CSV yang telah disiapkan berisi *emission factors* beserta kelas tutupan lahannya.
6.  **Unit Perencanaan**
    -   Klik "Browse" dan pilih shapefile unit perencanaan Anda.
7.  **Direktori Output**
    -   Klik "Select output directory" untuk memilih lokasi penyimpanan hasil analisis.
8.  **Jalankan Analisis**
    -   Klik "Run" untuk memulai proses analisis.

# Akses Hasil Analisis

Setelah analisis selesai, Anda akan menemukan output berikut pada direktori output yang dipilih:

1.  **Peta Karbon**: Peta raster dalam format GeoTIFF yang menggambarkan distribusi dan jumlah karbon yang tersimpan dalam data penggunaan/tutupan lahan.
2.  **Peta Emisi**: Peta raster dalam format GeoTIFF yang menunjukkan penurunan stok karbon dari tutupan lahan T1 ke tutupan lahan T2.
3.  **Peta Sekuestrasi**: Peta raster dalam format GeoTIFF yang menunjukkan peningkatan stok karbon dari tutupan lahan T1 ke tutupan lahan T2.
4.  **Database QUES-C**: File CSV berisi transisi tutupan lahan antara T1 dan T2 pada setiap unit perencanaan beserta nilai stok karbon, emisi, dan sekuestrasi.
5.  **Laporan QUES-C**: Ringkasan hasil analisis dalam format HTML, yang menyediakan interpretasi detail dan visualisasi dari output tersebut.

---

Untuk informasi lebih detail atau pemecahan masalah, lihat dokumentasi lengkap QUES-C atau hubungi pengembang alat ini.  

<font size="1">LUMENS adalah perangkat lunak gratis dan hadir TANPA JAMINAN APA PUN. Pengguna bertanggung jawab atas hasil yang dihasilkan. Hasil sangat bergantung pada kualitas data input ("garbage in, garbage out") dan dapat bervariasi atau sensitif terhadap parameter yang digunakan. Silakan laporkan setiap masalah yang ditemui saat menggunakan LUMENS sebagai [GitHub issue](https://github.com/icraf-indonesia/lumens-shiny/issues). Umpan balik dan pertanyaan dipersilakan [Contact Us](mailto:lumens@cifor-icraf.org).</font>