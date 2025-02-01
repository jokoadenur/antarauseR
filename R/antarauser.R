#' Fungsi antarauseR untuk mencari data berita tertentu dalam wilayah tertentu berdasarkan kata kunci dan rentang waktu.
#'
#' @param wilayahantara Karakter. Nama cabang berita Antara di wilayah Anda (misalnya "jatim").
#' @param keyword Karakter. Kata kunci pencarian (misalnya "ekonomi").
#' @param awal Karakter. Tanggal awal pencarian (misalnya "2025-01-01").
#' @param akhir Karakter. Tanggal akhir pencarian (misalnya "2025-01-31").
#' @import rvest
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @import tibble
#' @import tidyr
#' @importFrom stats filter
#' @importFrom rvest read_html html_nodes html_text
#' @importFrom dplyr mutate case_when
#' @importFrom lubridate dmy_hm
#' @importFrom stringr str_squish
#' @importFrom tibble tribble
#' @importFrom tidyr separate_rows
#' @return Data yang sesuai dengan pencarian.
#' @export

utils::globalVariables(c("katakunci", "kategori", "kategori2", "tanggal", "lokasi",
                         "isi", "|>", "fotocap", "endpage", "coba",
                         "filter", "case_when"))

antarauser <- function(wilayahantara, keyword, awal, akhir) {
  if(wilayahantara == "jatim"){
    keyword <- gsub(" ", "+", keyword)

    # end page
    url_awal <- paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/1")
    laman_awal <- rvest::read_html(url_awal)

    # cek elemen end page
    endpage <- laman_awal |> rvest::html_nodes(".pagination-sm a") |> rvest::html_text() |> trimws() |> max(na.rm = TRUE) |> as.numeric()

    #LOOPING
    isi <- function(x){
      teks <- rvest::read_html(x) |> rvest::html_nodes('#print_content .clearfix') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(teks)
    }

    fotocap <- function(x){
      narfoto <- rvest::read_html(x) |> rvest::html_nodes('.flex-caption') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(narfoto)
    }

    antarajatim <- data.frame()

    for(hasil in seq(from = 1, to = endpage, by = 1)){
      url <-paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/", hasil)
      laman <- rvest::read_html(url)

      judul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_text()
      tgl <- laman |> rvest::html_nodes('.simple-big span') |> rvest::html_text()
      linkjudul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_attr("href") |>
        paste(sep = "")

      isiberita <- sapply(linkjudul, FUN = isi, USE.NAMES = FALSE)
      captionfoto <- sapply(linkjudul, FUN = fotocap, USE.NAMES = FALSE)

      antarajatim <- rbind(antarajatim, data.frame(judul, tgl, isiberita, captionfoto, linkjudul, stringsAsFactors = FALSE))
      print(paste("Page ke-", hasil))

    }

    #Mengimport Data Labelisasi katakunci
    judulku <- antarajatim$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas",
      "partai", "lnprt",
      "parpol", "lnprt",
      "komunitas", "lnprt",
      "ormas", "lnprt",
      "apbn", "belanja pemerintah",
      "apbd", "belanja pemerintah",
      "apbdes", "belanja pemerintah"
    )

    #Data yang akan dilabelisasi
    judulantara <- antarajatim

    #Labelisasi sesuai katelapus
    judulantara |>
      dplyr::mutate(katakunci = isiberita) |>
      tidyr::separate_rows(katakunci, sep = ' ') |>
      dplyr::mutate(katakunci = tolower(katakunci)) |>
      dplyr::left_join(katelapus) |>
      filter(!is.na(kategori)) |>
      dplyr::select(-katakunci) -> judulantara2

    #Mendapatkan kategori teks berdasarkan labelisasi kata paling sering muncul
    judulantara2 |>
      dplyr::group_by(judul) |>
      dplyr::summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
                       kategori = toString(kategori),
                       .groups = "drop") |>
      dplyr::select(judul, kategori, kategori2) -> oke

    #Menggabungkan antara data awal dengan data yang berhasil dilabelisasi
    coba <- dplyr::inner_join(judulantara, oke, by = "judul")

    coba$wilayah <- sub("^\\s*(\\w+).*", "\\1", coba$isiberita) # Wilayah setelah koma dan spasi
    coba$wilayah <- sub(".*?,\\s*(\\w+).*", "\\1", coba$wilayah)
    coba <- coba[grepl("Pacitan|Ponorogo|Trenggalek|Tulungagung|Blitar|Kediri|Malang|Lumajang|Jember|Banyuwangi|Bondowoso|Situbondo|Probolinggo|Pasuruan|Sidoarjo|Mojokerto|Jombang|Nganjuk|Madiun|Magetan|Ngawi|Bojonegoro|Tuban|Lamongan|Gresik|Bangkalan|Sampang|Pamekasan|Sumenep|Kota Kediri|Kota Blitar|Kota Malang|Kota Probolinggo|Kota Pasuruan|Kota Mojokerto|Kota Madiun|Kota Surabaya|Kota Batu|Surabaya", coba$wilayah),]

    coba$wilayah <- ifelse(grepl("Kota Kediri|Kota Blitar|Kota Malang|Kota Probolinggo|Kota Pasuruan|Kota Mojokerto|Kota Madiun|Kota Surabaya|Kota Batu|Surabaya", coba$isiberita),
                           paste("Kota", coba$wilayah),
                           coba$wilayah)

    coba <- coba |>
      dplyr::mutate(
        tanggal = tgl,
        tanggal = gsub("Januari", "January", tanggal),
        tanggal = gsub("Februari", "February", tanggal),
        tanggal = gsub("Maret", "March", tanggal),
        tanggal = gsub("Mei", "May", tanggal),
        tanggal = gsub("Juni", "June", tanggal),
        tanggal = gsub("Juli", "July", tanggal),
        tanggal = gsub("Agustus", "August", tanggal),
        tanggal = gsub("Oktober", "October", tanggal),
        tanggal = gsub("Desember", "December", tanggal)
      )

    # Mengubah format tanggal
    coba <- coba |>
      dplyr::mutate(
        tanggal = dplyr::case_when(
          grepl("menit lalu|jam lalu|hari lalu", tgl) ~ as.character(Sys.Date()),
          TRUE ~ format(lubridate::dmy_hm(tanggal, quiet = TRUE), "%Y-%m-%d")
        )
      )

    antarajatim <- coba[,c(1, 3, 5, 8, 9, 7)]
    colnames(antarajatim)[colnames(antarajatim) == "kategori2"] <- "estimasi_lapus"
    assign("antarajatim", antarajatim, envir = .GlobalEnv)
    return(antarajatim)
  } else if(wilayahantara == "jateng"){
    keyword <- gsub(" ", "+", keyword)

    # end page
    url_awal <- paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/1")
    laman_awal <- rvest::read_html(url_awal)

    # cek elemen end page
    endpage <- laman_awal |> rvest::html_nodes(".pagination-sm a") |> rvest::html_text() |> trimws() |> max(na.rm = TRUE) |> as.numeric()

    #LOOPING
    isi <- function(x){
      teks <- rvest::read_html(x) |> rvest::html_nodes('.font17 p') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(teks)
    }

    fotocap <- function(x){
      narfoto <- rvest::read_html(x) |> rvest::html_nodes('.flex-caption') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(narfoto)
    }

    antarajatim <- data.frame()

    for(hasil in seq(from = 1, to = endpage, by = 1)){
      url <-paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/", hasil)
      laman <- rvest::read_html(url)

      judul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_text()
      tgl <- laman |> rvest::html_nodes('.simple-big span') |> rvest::html_text()
      linkjudul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_attr("href") |>
        paste(sep = "")

      isiberita <- sapply(linkjudul, FUN = isi, USE.NAMES = FALSE)
      captionfoto <- sapply(linkjudul, FUN = fotocap, USE.NAMES = FALSE)

      antarajatim <- rbind(antarajatim, data.frame(judul, tgl, isiberita, captionfoto, linkjudul, stringsAsFactors = FALSE))
      print(paste("Page ke-", hasil))

    }

    #Mengimport Data Labelisasi katakunci
    judulku <- antarajatim$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas",
      "partai", "lnprt",
      "parpol", "lnprt",
      "komunitas", "lnprt",
      "ormas", "lnprt",
      "apbn", "belanja pemerintah",
      "apbd", "belanja pemerintah",
      "apbdes", "belanja pemerintah"
    )

    #Data yang akan dilabelisasi
    judulantara <- antarajatim

    #Labelisasi sesuai katelapus
    judulantara |>
      dplyr::mutate(katakunci = isiberita) |>
      tidyr::separate_rows(katakunci, sep = ' ') |>
      dplyr::mutate(katakunci = tolower(katakunci)) |>
      dplyr::left_join(katelapus) |>
      filter(!is.na(kategori)) |>
      dplyr::select(-katakunci) -> judulantara2

    #Mendapatkan kategori teks berdasarkan labelisasi kata paling sering muncul
    judulantara2 |>
      dplyr::group_by(judul) |>
      dplyr::summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
                       kategori = toString(kategori),
                       .groups = "drop") |>
      dplyr::select(judul, kategori, kategori2) -> oke

    #Menggabungkan antara data awal dengan data yang berhasil dilabelisasi
    coba <- dplyr::inner_join(judulantara, oke, by = "judul")

    coba$wilayah <- sub("^\\s*(\\w+).*", "\\1", coba$isiberita) # Wilayah setelah koma dan spasi
    coba$wilayah <- sub(".*?,\\s*(\\w+).*", "\\1", coba$wilayah)
    coba <- coba[grepl("Cilacap|Banyumas|Purbalingga|Banjarnegara|Kebumen|Purworejo|Wonosobo|Magelang|Boyolali|Klaten|Sukoharjo|Wonogiri|Karanganyar|Sragen|Grobogan|Blora|Rembang|Pati|Kudus|Jepara|Demak|Semarang|Temanggung|Kendal|Batang|Pekalongan|Pemalang|Tegal|Brebes|Kota Magelang|Kota Surakarta|Kota Salatiga|Kota Semarang|Kota Pekalongan|Kota Tegal", coba$wilayah),]

    coba$wilayah <- ifelse(grepl("Kota Magelang|Kota Surakarta|Kota Salatiga|Kota Semarang|Kota Pekalongan|Kota Tegal", coba$isiberita),
                           paste("Kota", coba$wilayah),
                           coba$wilayah)

    coba <- coba |>
      dplyr::mutate(
        tanggal = tgl,
        tanggal = gsub("Januari", "January", tanggal),
        tanggal = gsub("Februari", "February", tanggal),
        tanggal = gsub("Maret", "March", tanggal),
        tanggal = gsub("Mei", "May", tanggal),
        tanggal = gsub("Juni", "June", tanggal),
        tanggal = gsub("Juli", "July", tanggal),
        tanggal = gsub("Agustus", "August", tanggal),
        tanggal = gsub("Oktober", "October", tanggal),
        tanggal = gsub("Desember", "December", tanggal)
      )

    # Mengubah format tanggal
    coba <- coba |>
      dplyr::mutate(
        tanggal = case_when(
          grepl("menit lalu|jam lalu|hari lalu", tgl) ~ as.character(Sys.Date()),
          TRUE ~ format(lubridate::dmy_hm(tanggal, quiet = TRUE), "%Y-%m-%d")
        )
      )

    antarajateng <- coba[,c(1, 3, 5, 8, 9, 7)]
    colnames(antarajateng)[colnames(antarajateng) == "kategori2"] <- "estimasi_lapus"
    assign("antarajateng", antarajateng, envir = .GlobalEnv)
    return(antarajatim)
  } else if(wilayahantara == "jabar"){
    keyword <- gsub(" ", "+", keyword)

    # end page
    url_awal <- paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/1")
    laman_awal <- rvest::read_html(url_awal)

    # cek elemen end page
    endpage <- laman_awal |> rvest::html_nodes(".pagination-sm a") |> rvest::html_text() |> trimws() |> max(na.rm = TRUE) |> as.numeric()

    #LOOPING
    isi <- function(x){
      teks <- rvest::read_html(x) |> rvest::html_nodes('#print_content .clearfix') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(teks)
    }

    fotocap <- function(x){
      narfoto <- rvest::read_html(x) |> rvest::html_nodes('.flex-caption') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(narfoto)
    }

    antarajatim <- data.frame()

    for(hasil in seq(from = 1, to = endpage, by = 1)){
      url <-paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/", hasil)
      laman <- rvest::read_html(url)

      judul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_text()
      tgl <- laman |> rvest::html_nodes('.simple-big span') |> rvest::html_text()
      linkjudul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_attr("href") |>
        paste(sep = "")

      isiberita <- sapply(linkjudul, FUN = isi, USE.NAMES = FALSE)
      captionfoto <- sapply(linkjudul, FUN = fotocap, USE.NAMES = FALSE)

      antarajatim <- rbind(antarajatim, data.frame(judul, tgl, isiberita, captionfoto, linkjudul, stringsAsFactors = FALSE))
      print(paste("Page ke-", hasil))

    }

    #Mengimport Data Labelisasi katakunci
    judulku <- antarajatim$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas",
      "partai", "lnprt",
      "parpol", "lnprt",
      "komunitas", "lnprt",
      "ormas", "lnprt",
      "apbn", "belanja pemerintah",
      "apbd", "belanja pemerintah",
      "apbdes", "belanja pemerintah"
    )

    #Data yang akan dilabelisasi
    judulantara <- antarajatim

    #Labelisasi sesuai katelapus
    judulantara |>
      dplyr::mutate(katakunci = isiberita) |>
      tidyr::separate_rows(katakunci, sep = ' ') |>
      dplyr::mutate(katakunci = tolower(katakunci)) |>
      dplyr::left_join(katelapus) |>
      filter(!is.na(kategori)) |>
      dplyr::select(-katakunci) -> judulantara2

    #Mendapatkan kategori teks berdasarkan labelisasi kata paling sering muncul
    judulantara2 |>
      dplyr::group_by(judul) |>
      dplyr::summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
                       kategori = toString(kategori),
                       .groups = "drop") |>
      dplyr::select(judul, kategori, kategori2) -> oke

    #Menggabungkan antara data awal dengan data yang berhasil dilabelisasi
    coba <- dplyr::inner_join(judulantara, oke, by = "judul")

    coba$wilayah <- sub("^\\s*(\\w+).*", "\\1", coba$isiberita) # Wilayah setelah koma dan spasi
    coba$wilayah <- sub(".*?,\\s*(\\w+).*", "\\1", coba$wilayah)
    coba <- coba[grepl("Bogor|Sukabumi|Cianjur|Bandung|Garut|Tasikmalaya|Ciamis|Kuningan|Cirebon|Majalengka|Sumedang|Indramayu|Subang|Purwakarta|Karawang|Bekasi|Bandung Barat|Pangandaran|Kota Bogor|Kota Sukabumi|Kota Bandung|Kota Cirebon|Kota Bekasi|Kota Depok|Kota Cimahi|Kota Tasikmalaya|Kota Banjar", coba$wilayah),]

    coba$wilayah <- ifelse(grepl("Kota Bogor|Kota Sukabumi|Kota Bandung|Kota Cirebon|Kota Bekasi|Kota Depok|Kota Cimahi|Kota Tasikmalaya|Kota Banjar", coba$isiberita),
                           paste("Kota", coba$wilayah),
                           coba$wilayah)

    coba <- coba |>
      dplyr::mutate(
        tanggal = tgl,
        tanggal = gsub("Januari", "January", tanggal),
        tanggal = gsub("Februari", "February", tanggal),
        tanggal = gsub("Maret", "March", tanggal),
        tanggal = gsub("Mei", "May", tanggal),
        tanggal = gsub("Juni", "June", tanggal),
        tanggal = gsub("Juli", "July", tanggal),
        tanggal = gsub("Agustus", "August", tanggal),
        tanggal = gsub("Oktober", "October", tanggal),
        tanggal = gsub("Desember", "December", tanggal)
      )

    # Mengubah format tanggal
    coba <- coba |>
      dplyr::mutate(
        tanggal = case_when(
          grepl("menit lalu|jam lalu|hari lalu", tgl) ~ as.character(Sys.Date()),
          TRUE ~ format(lubridate::dmy_hm(tanggal, quiet = TRUE), "%Y-%m-%d")
        )
      )

    antarajabar <- coba[,c(1, 3, 5, 8, 9, 7)]
    colnames(antarajabar)[colnames(antarajabar) == "kategori2"] <- "estimasi_lapus"
    assign("antarajabar", antarajabar, envir = .GlobalEnv)
    return(antarajabar)
  } else if(wilayahantara == "banten"){
    keyword <- gsub(" ", "+", keyword)

    # end page
    url_awal <- paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/1")
    laman_awal <- rvest::read_html(url_awal)

    # cek elemen end page
    endpage <- laman_awal |> rvest::html_nodes(".pagination-sm a") |> rvest::html_text() |> trimws() |> max(na.rm = TRUE) |> as.numeric()

    #LOOPING
    isi <- function(x){
      teks <- rvest::read_html(x) |> rvest::html_nodes('#print_content .clearfix') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(teks)
    }

    fotocap <- function(x){
      narfoto <- rvest::read_html(x) |> rvest::html_nodes('.flex-caption') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(narfoto)
    }

    antarajatim <- data.frame()

    for(hasil in seq(from = 1, to = endpage, by = 1)){
      url <-paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/", hasil)
      laman <- rvest::read_html(url)

      judul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_text()
      tgl <- laman |> rvest::html_nodes('.simple-big span') |> rvest::html_text()
      linkjudul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_attr("href") |>
        paste(sep = "")

      isiberita <- sapply(linkjudul, FUN = isi, USE.NAMES = FALSE)
      captionfoto <- sapply(linkjudul, FUN = fotocap, USE.NAMES = FALSE)

      antarajatim <- rbind(antarajatim, data.frame(judul, tgl, isiberita, captionfoto, linkjudul, stringsAsFactors = FALSE))
      print(paste("Page ke-", hasil))

    }

    #Mengimport Data Labelisasi katakunci
    judulku <- antarajatim$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas",
      "partai", "lnprt",
      "parpol", "lnprt",
      "komunitas", "lnprt",
      "ormas", "lnprt",
      "apbn", "belanja pemerintah",
      "apbd", "belanja pemerintah",
      "apbdes", "belanja pemerintah"
    )

    #Data yang akan dilabelisasi
    judulantara <- antarajatim

    #Labelisasi sesuai katelapus
    judulantara |>
      dplyr::mutate(katakunci = isiberita) |>
      tidyr::separate_rows(katakunci, sep = ' ') |>
      dplyr::mutate(katakunci = tolower(katakunci)) |>
      dplyr::left_join(katelapus) |>
      filter(!is.na(kategori)) |>
      dplyr::select(-katakunci) -> judulantara2

    #Mendapatkan kategori teks berdasarkan labelisasi kata paling sering muncul
    judulantara2 |>
      dplyr::group_by(judul) |>
      dplyr::summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
                       kategori = toString(kategori),
                       .groups = "drop") |>
      dplyr::select(judul, kategori, kategori2) -> oke

    #Menggabungkan antara data awal dengan data yang berhasil dilabelisasi
    coba <- dplyr::inner_join(judulantara, oke, by = "judul")

    coba$wilayah <- sub("^\\s*(\\w+).*", "\\1", coba$isiberita) # Wilayah setelah koma dan spasi
    coba$wilayah <- sub(".*?,\\s*(\\w+).*", "\\1", coba$wilayah)
    coba <- coba[grepl("Pandeglang|Lebak|Tangerang|Serang|Kota Tangerang|Kota Cilegon|Kota Serang|Kota Tangerang Selatan", coba$wilayah),]

    coba$wilayah <- ifelse(grepl("Kota Tangerang|Kota Cilegon|Kota Serang|Kota Tangerang Selatan", coba$isiberita),
                           paste("Kota", coba$wilayah),
                           coba$wilayah)

    coba <- coba |>
      dplyr::mutate(
        tanggal = tgl,
        tanggal = gsub("Januari", "January", tanggal),
        tanggal = gsub("Februari", "February", tanggal),
        tanggal = gsub("Maret", "March", tanggal),
        tanggal = gsub("Mei", "May", tanggal),
        tanggal = gsub("Juni", "June", tanggal),
        tanggal = gsub("Juli", "July", tanggal),
        tanggal = gsub("Agustus", "August", tanggal),
        tanggal = gsub("Oktober", "October", tanggal),
        tanggal = gsub("Desember", "December", tanggal)
      )

    # Mengubah format tanggal
    coba <- coba |>
      dplyr::mutate(
        tanggal = case_when(
          grepl("menit lalu|jam lalu|hari lalu", tgl) ~ as.character(Sys.Date()),
          TRUE ~ format(lubridate::dmy_hm(tanggal, quiet = TRUE), "%Y-%m-%d")
        )
      )

    antarabanten <- coba[,c(1, 3, 5, 8, 9, 7)]
    colnames(antarabanten)[colnames(antarabanten) == "kategori2"] <- "estimasi_lapus"
    assign("antarabanten", antarabanten, envir = .GlobalEnv)
    return(antarabanten)
  } else if(wilayahantara == "jogja"){
    keyword <- gsub(" ", "+", keyword)

    # end page
    url_awal <- paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/1")
    laman_awal <- rvest::read_html(url_awal)

    # cek elemen end page
    endpage <- laman_awal |> rvest::html_nodes(".pagination-sm a") |> rvest::html_text() |> trimws() |> max(na.rm = TRUE) |> as.numeric()

    #LOOPING
    isi <- function(x){
      teks <- rvest::read_html(x) |> rvest::html_nodes('.font17 p') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(teks)
    }

    fotocap <- function(x){
      narfoto <- rvest::read_html(x) |> rvest::html_nodes('.flex-caption') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(narfoto)
    }

    antarajatim <- data.frame()

    for(hasil in seq(from = 1, to = endpage, by = 1)){
      url <-paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/", hasil)
      laman <- rvest::read_html(url)

      judul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_text()
      tgl <- laman |> rvest::html_nodes('.simple-big span') |> rvest::html_text()
      linkjudul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_attr("href") |>
        paste(sep = "")

      isiberita <- sapply(linkjudul, FUN = isi, USE.NAMES = FALSE)
      captionfoto <- sapply(linkjudul, FUN = fotocap, USE.NAMES = FALSE)

      antarajatim <- rbind(antarajatim, data.frame(judul, tgl, isiberita, captionfoto, linkjudul, stringsAsFactors = FALSE))
      print(paste("Page ke-", hasil))

    }

    #Mengimport Data Labelisasi katakunci
    judulku <- antarajatim$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas",
      "partai", "lnprt",
      "parpol", "lnprt",
      "komunitas", "lnprt",
      "ormas", "lnprt",
      "apbn", "belanja pemerintah",
      "apbd", "belanja pemerintah",
      "apbdes", "belanja pemerintah"
    )

    #Data yang akan dilabelisasi
    judulantara <- antarajatim

    #Labelisasi sesuai katelapus
    judulantara |>
      dplyr::mutate(katakunci = isiberita) |>
      tidyr::separate_rows(katakunci, sep = ' ') |>
      dplyr::mutate(katakunci = tolower(katakunci)) |>
      dplyr::left_join(katelapus) |>
      filter(!is.na(kategori)) |>
      dplyr::select(-katakunci) -> judulantara2

    #Mendapatkan kategori teks berdasarkan labelisasi kata paling sering muncul
    judulantara2 |>
      dplyr::group_by(judul) |>
      dplyr::summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
                       kategori = toString(kategori),
                       .groups = "drop") |>
      dplyr::select(judul, kategori, kategori2) -> oke

    #Menggabungkan antara data awal dengan data yang berhasil dilabelisasi
    coba <- dplyr::inner_join(judulantara, oke, by = "judul")

    coba$wilayah <- sub("^\\s*(\\w+).*", "\\1", coba$isiberita) # Wilayah setelah koma dan spasi
    coba$wilayah <- sub(".*?,\\s*(\\w+).*", "\\1", coba$wilayah)
    coba <- coba[grepl("Kulon Progo|Bantul|Gunungkidul|Sleman|Kota Yogyakarta", coba$wilayah),]

    coba$wilayah <- ifelse(grepl("Kota Yogyakarta", coba$isiberita),
                           paste("Kota", coba$wilayah),
                           coba$wilayah)

    coba <- coba |>
      dplyr::mutate(
        tanggal = tgl,
        tanggal = gsub("Januari", "January", tanggal),
        tanggal = gsub("Februari", "February", tanggal),
        tanggal = gsub("Maret", "March", tanggal),
        tanggal = gsub("Mei", "May", tanggal),
        tanggal = gsub("Juni", "June", tanggal),
        tanggal = gsub("Juli", "July", tanggal),
        tanggal = gsub("Agustus", "August", tanggal),
        tanggal = gsub("Oktober", "October", tanggal),
        tanggal = gsub("Desember", "December", tanggal)
      )

    # Mengubah format tanggal
    coba <- coba |>
      dplyr::mutate(
        tanggal = case_when(
          grepl("menit lalu|jam lalu|hari lalu", tgl) ~ as.character(Sys.Date()),
          TRUE ~ format(lubridate::dmy_hm(tanggal, quiet = TRUE), "%Y-%m-%d")
        )
      )

    antarajogja <- coba[,c(1, 3, 5, 8, 9, 7)]
    colnames(antarajogja)[colnames(antarajogja) == "kategori2"] <- "estimasi_lapus"
    assign("antarajogja", antarajogja, envir = .GlobalEnv)
    return(antarajogja)
  } else if(wilayahantara == "makassar"){
    keyword <- gsub(" ", "+", keyword)

    # end page
    url_awal <- paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/1")
    laman_awal <- rvest::read_html(url_awal)

    # cek elemen end page
    endpage <- laman_awal |> rvest::html_nodes(".pagination-sm a") |> rvest::html_text() |> trimws() |> max(na.rm = TRUE) |> as.numeric()

    #LOOPING
    isi <- function(x){
      teks <- rvest::read_html(x) |> rvest::html_nodes('#print_content .clearfix') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(teks)
    }

    fotocap <- function(x){
      narfoto <- rvest::read_html(x) |> rvest::html_nodes('.flex-caption') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(narfoto)
    }

    antarajatim <- data.frame()

    for(hasil in seq(from = 1, to = endpage, by = 1)){
      url <-paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/", hasil)
      laman <- rvest::read_html(url)

      judul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_text()
      tgl <- laman |> rvest::html_nodes('.simple-big span') |> rvest::html_text()
      linkjudul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_attr("href") |>
        paste(sep = "")

      isiberita <- sapply(linkjudul, FUN = isi, USE.NAMES = FALSE)
      captionfoto <- sapply(linkjudul, FUN = fotocap, USE.NAMES = FALSE)

      antarajatim <- rbind(antarajatim, data.frame(judul, tgl, isiberita, captionfoto, linkjudul, stringsAsFactors = FALSE))
      print(paste("Page ke-", hasil))

    }

    #Mengimport Data Labelisasi katakunci
    judulku <- antarajatim$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas",
      "partai", "lnprt",
      "parpol", "lnprt",
      "komunitas", "lnprt",
      "ormas", "lnprt",
      "apbn", "belanja pemerintah",
      "apbd", "belanja pemerintah",
      "apbdes", "belanja pemerintah"
    )

    #Data yang akan dilabelisasi
    judulantara <- antarajatim

    #Labelisasi sesuai katelapus
    judulantara |>
      dplyr::mutate(katakunci = isiberita) |>
      tidyr::separate_rows(katakunci, sep = ' ') |>
      dplyr::mutate(katakunci = tolower(katakunci)) |>
      dplyr::left_join(katelapus) |>
      filter(!is.na(kategori)) |>
      dplyr::select(-katakunci) -> judulantara2

    #Mendapatkan kategori teks berdasarkan labelisasi kata paling sering muncul
    judulantara2 |>
      dplyr::group_by(judul) |>
      dplyr::summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
                       kategori = toString(kategori),
                       .groups = "drop") |>
      dplyr::select(judul, kategori, kategori2) -> oke

    #Menggabungkan antara data awal dengan data yang berhasil dilabelisasi
    coba <- dplyr::inner_join(judulantara, oke, by = "judul")

    coba$wilayah <- sub("^\\s*(\\w+).*", "\\1", coba$isiberita) # Wilayah setelah koma dan spasi
    coba$wilayah <- sub(".*?,\\s*(\\w+).*", "\\1", coba$wilayah)
    coba <- coba[grepl("Kepulauan Selayar|Bulukumba|Bantaeng|Jeneponto|Takalar|Gowa|Sinjai|Maros|Pangkajene dan Kepulauan|Barru|Bone|Soppeng|Wajo|Sidrap|Pinrang|Enrekang|Luwu|Tana Toraja|Luwu Utara|Luwu Timur|Toraja Utara|Kota Makassar|Kota Parepare|Kota Palopo|Mamuju|Makassar", coba$wilayah),]
    coba$wilayah <- ifelse(grepl("Kota Makassar|Kota Parepare|Kota Palopo", coba$isiberita),
                           paste("Kota", coba$wilayah),
                           coba$wilayah)

    coba <- coba |>
      dplyr::mutate(
        tanggal = tgl,
        tanggal = gsub("Januari", "January", tanggal),
        tanggal = gsub("Februari", "February", tanggal),
        tanggal = gsub("Maret", "March", tanggal),
        tanggal = gsub("Mei", "May", tanggal),
        tanggal = gsub("Juni", "June", tanggal),
        tanggal = gsub("Juli", "July", tanggal),
        tanggal = gsub("Agustus", "August", tanggal),
        tanggal = gsub("Oktober", "October", tanggal),
        tanggal = gsub("Desember", "December", tanggal)
      )

    # Mengubah format tanggal
    coba <- coba |>
      dplyr::mutate(
        tanggal = case_when(
          grepl("menit lalu|jam lalu|hari lalu", tgl) ~ as.character(Sys.Date()),
          TRUE ~ format(lubridate::dmy_hm(tanggal, quiet = TRUE), "%Y-%m-%d")
        )
      )

    antarasulsel <- coba[,c(1, 3, 5, 8, 9, 7)]
    colnames(antarasulsel)[colnames(antarasulsel) == "kategori2"] <- "estimasi_lapus"
    assign("antarasulsel", antarasulsel, envir = .GlobalEnv)
    return(antarasulsel)
  } else if(wilayahantara == "bali"){
    keyword <- gsub(" ", "+", keyword)

    # end page
    url_awal <- paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/1")
    laman_awal <- rvest::read_html(url_awal)

    # cek elemen end page
    endpage <- laman_awal |> rvest::html_nodes(".pagination-sm a") |> rvest::html_text() |> trimws() |> max(na.rm = TRUE) |> as.numeric()

    #LOOPING
    isi <- function(x){
      teks <- rvest::read_html(x) |> rvest::html_nodes('#print_content .clearfix') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(teks)
    }

    fotocap <- function(x){
      narfoto <- rvest::read_html(x) |> rvest::html_nodes('.flex-caption') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(narfoto)
    }

    antarajatim <- data.frame()

    for(hasil in seq(from = 1, to = endpage, by = 1)){
      url <-paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/", hasil)
      laman <- rvest::read_html(url)

      judul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_text()
      tgl <- laman |> rvest::html_nodes('.simple-big span') |> rvest::html_text()
      linkjudul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_attr("href") |>
        paste(sep = "")

      isiberita <- sapply(linkjudul, FUN = isi, USE.NAMES = FALSE)
      captionfoto <- sapply(linkjudul, FUN = fotocap, USE.NAMES = FALSE)

      antarajatim <- rbind(antarajatim, data.frame(judul, tgl, isiberita, captionfoto, linkjudul, stringsAsFactors = FALSE))
      print(paste("Page ke-", hasil))

    }

    #Mengimport Data Labelisasi katakunci
    judulku <- antarajatim$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas",
      "partai", "lnprt",
      "parpol", "lnprt",
      "komunitas", "lnprt",
      "ormas", "lnprt",
      "apbn", "belanja pemerintah",
      "apbd", "belanja pemerintah",
      "apbdes", "belanja pemerintah"
    )

    #Data yang akan dilabelisasi
    judulantara <- antarajatim

    #Labelisasi sesuai katelapus
    judulantara |>
      dplyr::mutate(katakunci = isiberita) |>
      tidyr::separate_rows(katakunci, sep = ' ') |>
      dplyr::mutate(katakunci = tolower(katakunci)) |>
      dplyr::left_join(katelapus) |>
      filter(!is.na(kategori)) |>
      dplyr::select(-katakunci) -> judulantara2

    #Mendapatkan kategori teks berdasarkan labelisasi kata paling sering muncul
    judulantara2 |>
      dplyr::group_by(judul) |>
      dplyr::summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
                       kategori = toString(kategori),
                       .groups = "drop") |>
      dplyr::select(judul, kategori, kategori2) -> oke

    #Menggabungkan antara data awal dengan data yang berhasil dilabelisasi
    coba <- dplyr::inner_join(judulantara, oke, by = "judul")

    coba$wilayah <- sub("^\\s*(\\w+).*", "\\1", coba$isiberita) # Wilayah setelah koma dan spasi
    coba$wilayah <- sub(".*?,\\s*(\\w+).*", "\\1", coba$wilayah)
    coba <- coba[grepl("Jembrana|Tabanan|Badung|Gianyar|Klungkung|Bangli|Karangasem|Buleleng|Kota Denpasar|Bali", coba$wilayah),]
    coba$wilayah <- ifelse(grepl("Kota Denpasar", coba$isiberita),
                           paste("Kota", coba$wilayah),
                           coba$wilayah)

    coba <- coba |>
      dplyr::mutate(
        tanggal = tgl,
        tanggal = gsub("Januari", "January", tanggal),
        tanggal = gsub("Februari", "February", tanggal),
        tanggal = gsub("Maret", "March", tanggal),
        tanggal = gsub("Mei", "May", tanggal),
        tanggal = gsub("Juni", "June", tanggal),
        tanggal = gsub("Juli", "July", tanggal),
        tanggal = gsub("Agustus", "August", tanggal),
        tanggal = gsub("Oktober", "October", tanggal),
        tanggal = gsub("Desember", "December", tanggal)
      )

    # Mengubah format tanggal
    coba <- coba |>
      dplyr::mutate(
        tanggal = case_when(
          grepl("menit lalu|jam lalu|hari lalu", tgl) ~ as.character(Sys.Date()),
          TRUE ~ format(lubridate::dmy_hm(tanggal, quiet = TRUE), "%Y-%m-%d")
        )
      )

    antarabali <- coba[,c(1, 3, 5, 8, 9, 7)]
    colnames(antarabali)[colnames(antarabali) == "kategori2"] <- "estimasi_lapus"
    assign("antarabali", antarabali, envir = .GlobalEnv)
    return(antarabali)
  } else if(wilayahantara == "lampung"){
    keyword <- gsub(" ", "+", keyword)

    # end page
    url_awal <- paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/1")
    laman_awal <- rvest::read_html(url_awal)

    # cek elemen end page
    endpage <- laman_awal |> rvest::html_nodes(".pagination-sm a") |> rvest::html_text() |> trimws() |> max(na.rm = TRUE) |> as.numeric()

    #LOOPING
    isi <- function(x){
      teks <- rvest::read_html(x) |> rvest::html_nodes('.font17 p') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(teks)
    }

    fotocap <- function(x){
      narfoto <- rvest::read_html(x) |> rvest::html_nodes('.flex-caption') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(narfoto)
    }

    antarajatim <- data.frame()

    for(hasil in seq(from = 1, to = endpage, by = 1)){
      url <-paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/", hasil)
      laman <- rvest::read_html(url)

      judul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_text()
      tgl <- laman |> rvest::html_nodes('.simple-big span') |> rvest::html_text()
      linkjudul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_attr("href") |>
        paste(sep = "")

      isiberita <- sapply(linkjudul, FUN = isi, USE.NAMES = FALSE)
      captionfoto <- sapply(linkjudul, FUN = fotocap, USE.NAMES = FALSE)

      antarajatim <- rbind(antarajatim, data.frame(judul, tgl, isiberita, captionfoto, linkjudul, stringsAsFactors = FALSE))
      print(paste("Page ke-", hasil))

    }

    #Mengimport Data Labelisasi katakunci
    judulku <- antarajatim$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas",
      "partai", "lnprt",
      "parpol", "lnprt",
      "komunitas", "lnprt",
      "ormas", "lnprt",
      "apbn", "belanja pemerintah",
      "apbd", "belanja pemerintah",
      "apbdes", "belanja pemerintah"
    )

    #Data yang akan dilabelisasi
    judulantara <- antarajatim

    #Labelisasi sesuai katelapus
    judulantara |>
      dplyr::mutate(katakunci = isiberita) |>
      tidyr::separate_rows(katakunci, sep = ' ') |>
      dplyr::mutate(katakunci = tolower(katakunci)) |>
      dplyr::left_join(katelapus) |>
      filter(!is.na(kategori)) |>
      dplyr::select(-katakunci) -> judulantara2

    #Mendapatkan kategori teks berdasarkan labelisasi kata paling sering muncul
    judulantara2 |>
      dplyr::group_by(judul) |>
      dplyr::summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
                       kategori = toString(kategori),
                       .groups = "drop") |>
      dplyr::select(judul, kategori, kategori2) -> oke

    #Menggabungkan antara data awal dengan data yang berhasil dilabelisasi
    coba <- dplyr::inner_join(judulantara, oke, by = "judul")

    coba$wilayah <- sub("^\\s*(\\w+).*", "\\1", coba$isiberita) # Wilayah setelah koma dan spasi
    coba$wilayah <- sub(".*?,\\s*(\\w+).*", "\\1", coba$wilayah)
    coba <- coba[grepl("Lampung Barat|Tanggamus|Lampung Selatan|Lampung Timur|Lampung Tengah|Lampung Utara|Way Kanan|Pesawaran|Pringsewu|Mesuji|Tulang Bawang|Tulang Bawang Barat|Pesisir Barat|Kota Bandar Lampung|Kota Metro|Lampung|Bandarlampung", coba$wilayah),]
    coba$wilayah <- ifelse(grepl("Kota Bandar Lampung|Kota Metro", coba$isiberita),
                           paste("Kota", coba$wilayah),
                           coba$wilayah)

    coba <- coba |>
      dplyr::mutate(
        tanggal = tgl,
        tanggal = gsub("Januari", "January", tanggal),
        tanggal = gsub("Februari", "February", tanggal),
        tanggal = gsub("Maret", "March", tanggal),
        tanggal = gsub("Mei", "May", tanggal),
        tanggal = gsub("Juni", "June", tanggal),
        tanggal = gsub("Juli", "July", tanggal),
        tanggal = gsub("Agustus", "August", tanggal),
        tanggal = gsub("Oktober", "October", tanggal),
        tanggal = gsub("Desember", "December", tanggal)
      )

    # Mengubah format tanggal
    coba <- coba |>
      dplyr::mutate(
        tanggal = case_when(
          grepl("menit lalu|jam lalu|hari lalu", tgl) ~ as.character(Sys.Date()),
          TRUE ~ format(lubridate::dmy_hm(tanggal, quiet = TRUE), "%Y-%m-%d")
        )
      )

    antaralampung <- coba[,c(1, 3, 5, 8, 9, 7)]
    colnames(antaralampung)[colnames(antaralampung) == "kategori2"] <- "estimasi_lapus"
    assign("antaralampung", antaralampung, envir = .GlobalEnv)
    return(antaralampung)
  } else if(wilayahantara == "bengkulu"){
    keyword <- gsub(" ", "+", keyword)

    # end page
    url_awal <- paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/1")
    laman_awal <- rvest::read_html(url_awal)

    # cek elemen end page
    endpage <- laman_awal |> rvest::html_nodes(".pagination-sm a") |> rvest::html_text() |> trimws() |> max(na.rm = TRUE) |> as.numeric()

    #LOOPING
    isi <- function(x){
      teks <- rvest::read_html(x) |> rvest::html_nodes('#print_content .clearfix') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(teks)
    }

    fotocap <- function(x){
      narfoto <- rvest::read_html(x) |> rvest::html_nodes('.flex-caption') |> rvest::html_text() |> stringr::str_squish() |>
        paste(collapse = ",")
      return(narfoto)
    }

    antarajatim <- data.frame()

    for(hasil in seq(from = 1, to = endpage, by = 1)){
      url <-paste0("https://", wilayahantara, ".antaranews.com/search/", keyword, "/", awal, "/", akhir, "/", hasil)
      laman <- rvest::read_html(url)

      judul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_text()
      tgl <- laman |> rvest::html_nodes('.simple-big span') |> rvest::html_text()
      linkjudul <- laman |> rvest::html_nodes('.simple-big h3 a') |> rvest::html_attr("href") |>
        paste(sep = "")

      isiberita <- sapply(linkjudul, FUN = isi, USE.NAMES = FALSE)
      captionfoto <- sapply(linkjudul, FUN = fotocap, USE.NAMES = FALSE)

      antarajatim <- rbind(antarajatim, data.frame(judul, tgl, isiberita, captionfoto, linkjudul, stringsAsFactors = FALSE))
      print(paste("Page ke-", hasil))

    }

    #Mengimport Data Labelisasi katakunci
    judulku <- antarajatim$judul

    katelapus <- tibble::tribble(
      ~katakunci, ~kategori,
      "pabrik", "industri",
      "kredit", "bank, jasa keuangan",
      "penumpang kereta", "transportasi darat",
      "penumpang pesawat", "transportasi udara",
      "kapal", "transportasi laut",
      "nissan", "industri kendaraan",
      "investasi", "investasi dan konstruksi",
      "bendungan", "konstruksi",
      "pembangunan tol", "konstruksi",
      "panen", "pertanian",
      "hutan", "kehutanan",
      "tambak", "perikanan",
      "perikanan", "perikanan",
      "pelabuhan", "transportasi laut",
      "infrastruktur", "investasi dan konstruksi",
      "resmikan kantor", "konstruksi",
      "penjualan hewan", "perdagangan",
      "perdagangan", "perdagangan",
      "kawasan industri", "industri, investasi, konstruksi",
      "banyak pesanan", "perdagangan",
      "potensi pasar", "perdagangan",
      "ekspor", "ekspor",
      "peluang ekspor", "ekspor",
      "impor", "impor",
      "bangun rumah", "konstruksi",
      "bangun kantor", "konstruksi",
      "bangun gedung", "konstruksi",
      "bangun tol", "konstruksi",
      "tkdn", "ekspor, impor",
      "mampu jual", "perdagangan",
      "untuk jual", "perdagangan",
      "panen raya", "pertanian",
      "curah hujan", "pertanian, cek",
      "pasarkan produk", "perdagangan",
      "pasar sapi", "perdagangan",
      "pasar hewan", "perdagangan",
      "pasar saham", "investasi",
      "penjualan", "perdagangan",
      "omset", "perdagangan",
      "tambah operasional", "transportasi",
      "tambah gerbong", "transportasi",
      "armada", "transportasi",
      "kawasan khusus industri", "industri, investasi, konstruksi",
      "kawasan industri khusus", "industri, investasi, konstruksi",
      "bazar", "perdagangan",
      "festival kuliner", "penyediaan makan minum",
      "proyek", "konstruksi",
      "pupuk", "pertanian",
      "benih", "pertanian",
      "emas antam", "perdagangan",
      "usaha", "industri",
      "pasar ponsel", "perdagangan",
      "harga mobil", "perdagangan",
      "harga sepeda motor", "perdagangan",
      "harga motor", "perdagangan",
      "harga kendaraan", "perdagangan",
      "harga", "perdagangan",
      "berinvestasi", "investasi",
      "pekerjaan drainase", "konstruksi",
      "pekerjaan jembatan", "konstruksi",
      "perumahan", "konstruksi",
      "pekerjaan bangunan", "konstruksi",
      "daur ulang", "pengelolaan sampah, daur ulang",
      "pengelolaan sampah", "pengelolaan sampah, daur ulang",
      "manfaatkan limbah", "pengelolaan sampah, daur ulang",
      "manfaatkan ampas", "pengelolaan sampah, daur ulang",
      "besi bekas", "pengelolaan sampah, daur ulang",
      "plastik bekas", "pengelolaan sampah, daur ulang",
      "pasang jaringan", "konstruksi",
      "pengerjaan", "konstruksi",
      "selesai dibangun", "konstruksi",
      "jembatan", "konstruksi",
      "pasokan", "perdagangan",
      "penanaman mangrove", "kehutanan",
      "penanaman kayu", "kehutanan",
      "penanaman modal", "investasi",
      "layanan kesehatan", "jasa kesehatan",
      "pasaran", "perdagangan",
      "sembako murah", "perdagangan",
      "pln", "pengadaan listrik",
      "pdam", "pengadaan air",
      "hortikultura", "pertanian",
      "pembayaran elektronik", "keuangan",
      "wisata", "akomodasi",
      "hotel", "akomodasi, cek lagi",
      "rumah makan", "penyediaan makan minum",
      "restoran", "penyediaan makan minum",
      "peternak", "peternakan",
      "petani", "pertanian",
      "nelayan", "perikanan",
      "penerbangan", "transportasi udara",
      "petrokimia", "industri",
      "pasokan bahan baku", "industri",
      "gerai", "perdagangan",
      "bisnis emas", "perdagangan",
      "nasabah", "keuangan",
      "diekspor", "ekspor",
      "diimpor", "impor",
      "disnak", "peternakan",
      "dishub", "transportasi",
      "perizinan", "investasi",
      "investor", "investasi",
      "perbaiki", "konstruksi",
      "kerajinan", "industri kreatif",
      "pedagang", "perdagangan",
      "produksi", "industri",
      "dibangun", "konstruksi",
      "potensi perikanan", "perikanan",
      "perajin", "industri kreatif",
      "hama", "pertanian",
      "pertamina", "pengadaan minyak dan gas, cek lagi",
      "daihatsu", "perdagangan",
      "danamon", "jasa keuangan bank",
      "pembangunan smelter", "konstruksi",
      "rumah kreatif", "industri kreatif",
      "bangun", "konstruksi",
      "kafe", "penyediaan makan minum",
      "harga cabai", "perdagangan",
      "harga telur", "perdagangan",
      "harga susu", "perdagangan",
      "harga bawang", "perdagangan",
      "harga minyak", "perdagangan",
      "harga beras", "perdagangan",
      "harga gula", "perdagangan",
      "harga tempe", "perdagangan",
      "harga tahu", "perdagangan",
      "harga tomat", "perdagangan",
      "harga kedelai", "perdagangan",
      "musim giling", "pertanian",
      "pemesanan", "perdagangan",
      "pasar tradisional", "perdagangan",
      "pasar moderen", "perdagangan",
      "pasar modern", "perdagangan",
      "penggantian", "konstruksi",
      "bisnis", "industri",
      "kai daop", "transportasi darat",
      "ptpn", "industri",
      "daya listrik industri", "pengadaan listrik",
      "tanaman", "pertanian",
      "jualan", "perdagangan",
      "ternak", "peternakan",
      "jalan", "konstruksi",
      "segmen", "perdagangan",
      "sentra ikan", "perikanan",
      "sentra industri", "industri",
      "kursi kereta", "industri",
      "inka", "transportasi",
      "kai", "transportasi",
      "krl", "transportasi",
      "stasiun", "transportasi darat",
      "terminal", "transportasi darat",
      "bandara", "transportasi udara",
      "promosikan", "perdagangan, ekspor",
      "promosi", "perdagangan, ekspor",
      "pelayaran", "transportasi laut",
      "okupansi hotel", "akomodasi",
      "desa wisata", "akomodasi",
      "tiket", "transportasi, cek darat, laut, udaranya",
      "lintasi", "transportasi",
      "melintas", "transportasi",
      "jumlah kendaraan", "transportasi",
      "permintaan", "perdagangan",
      "yamaha", "perdagangan motor",
      "honda", "perdagangan mobil atau motor",
      "tangkapan", "perikanan",
      "swalayan", "perdagangan",
      "nikmati listrik", "pengadaan listrik",
      "furniture", "perdagangan",
      "pembuatan gerbong", "industri alat angkutan",
      "perancang busana", "industri kreatif",
      "perbaikan", "konstruksi",
      "optimalisasi jaringan", "konstruksi",
      "kuliner", "penyediaan makan minum",
      "angkringan", "penyediaan makan minum",
      "makanan jadi", "penyediaan makan minum",
      "lesehan", "penyediaan makan minum",
      "minuman", "penyediaan makan minum",
      "kelontong", "penyediaan makan minum",
      "spklu", "pengadaan listrik",
      "plts", "pengadaan listrik",
      "gas", "pengadaan gas",
      "gas kota", "pengadaan gas",
      "partai", "lnprt",
      "parpol", "lnprt",
      "komunitas", "lnprt",
      "ormas", "lnprt",
      "apbn", "belanja pemerintah",
      "apbd", "belanja pemerintah",
      "apbdes", "belanja pemerintah"
    )

    #Data yang akan dilabelisasi
    judulantara <- antarajatim

    #Labelisasi sesuai katelapus
    judulantara |>
      dplyr::mutate(katakunci = isiberita) |>
      tidyr::separate_rows(katakunci, sep = ' ') |>
      dplyr::mutate(katakunci = tolower(katakunci)) |>
      dplyr::left_join(katelapus) |>
      filter(!is.na(kategori)) |>
      dplyr::select(-katakunci) -> judulantara2

    #Mendapatkan kategori teks berdasarkan labelisasi kata paling sering muncul
    judulantara2 |>
      dplyr::group_by(judul) |>
      dplyr::summarise(kategori2 = names(sort(table(kategori), decreasing = TRUE))[1],
                       kategori = toString(kategori),
                       .groups = "drop") |>
      dplyr::select(judul, kategori, kategori2) -> oke

    #Menggabungkan antara data awal dengan data yang berhasil dilabelisasi
    coba <- dplyr::inner_join(judulantara, oke, by = "judul")

    coba$wilayah <- sub("^\\s*(\\w+).*", "\\1", coba$isiberita) # Wilayah setelah koma dan spasi
    coba$wilayah <- sub(".*?,\\s*(\\w+).*", "\\1", coba$wilayah)
    coba <- coba[grepl("Bengkulu Selatan|Rejang Lebong|Bengkulu Utara|Kaur|Seluma|Mukomuko|Lebong|Kepahiang|Bengkulu Tengah|Kota Bengkulu|Bengkulu", coba$wilayah),]
    coba$wilayah <- ifelse(grepl("Kota Bengkulu", coba$isiberita),
                           paste("Kota", coba$wilayah),
                           coba$wilayah)

    coba <- coba |>
      dplyr::mutate(
        tanggal = tgl,
        tanggal = gsub("Januari", "January", tanggal),
        tanggal = gsub("Februari", "February", tanggal),
        tanggal = gsub("Maret", "March", tanggal),
        tanggal = gsub("Mei", "May", tanggal),
        tanggal = gsub("Juni", "June", tanggal),
        tanggal = gsub("Juli", "July", tanggal),
        tanggal = gsub("Agustus", "August", tanggal),
        tanggal = gsub("Oktober", "October", tanggal),
        tanggal = gsub("Desember", "December", tanggal)
      )

    # Mengubah format tanggal
    coba <- coba |>
      dplyr::mutate(
        tanggal = case_when(
          grepl("menit lalu|jam lalu|hari lalu", tgl) ~ as.character(Sys.Date()),
          TRUE ~ format(lubridate::dmy_hm(tanggal, quiet = TRUE), "%Y-%m-%d")
        )
      )

    antarabengkulu <- coba[,c(1, 3, 5, 8, 9, 7)]
    colnames(antarabengkulu)[colnames(antarabengkulu) == "kategori2"] <- "estimasi_lapus"
    assign("antarabengkulu", antarabengkulu, envir = .GlobalEnv)
    return(antarabengkulu)
  } else{
    cat("Maaf, wilayah berita Antara masih mencakup Jawa, Bengkulu, Lampung, Sulsel, dan Bali")
  }
}
