![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg)
![CRAN Version](https://img.shields.io/badge/CRAN-7.3.2-brightgreen)
![Open Issues](https://img.shields.io/badge/open%20issues-0-brightgreen)
![License](https://img.shields.io/badge/License-MIT-blue)
![License](https://img.shields.io/badge/blog-jokoding.com-brightgreen)

![WhatsApp Image 2025-02-01 at 19 02 17](https://github.com/user-attachments/assets/d47ab993-5fad-4bea-ba31-06ca7bc82520)



# antarauseR

`antarauseR` is an R package designed for scraping news articles from various Antara News branches in Indonesia. With this package, you can easily retrieve news data for a specified number of pages.

## Installation

To install the `antarauseR` package, run the following code in your R script:

```R
# Install package from GitHub
devtools::install_github("jokoadenur/scraperadaR")
```

> **Note:** If prompted to update certain packages (options like 1. All, 2. CRAN, etc.), simply press **ENTER** to skip. Wait until the installation process is complete and the message `DONE (scraperadaR)` appears.

After installation, activate the package with the following code:

```R
# Activate the package
library(scraperadaR)
```

## Usage

To scrape news articles from a Radar branch, use the `scraperadar()` function with the following format:

```R
scraperadar("radar branch name", number_of_pages)
```

### Examples:

1. Scraping 5 pages of news from Radar Tuban:
   ```R
   scraperadar("tuban", 5)
   ```

2. Scraping 2 pages of news from Radar Solo:
   ```R
   scraperadar("solo", 2)
   ```

## List of Radar Branches

### East Java
- Radar Madiun
- Radar Jember
- Radar Banyuwangi
- Radar Bromo
- Radar Malang
- Radar Mojokerto
- Radar Lamongan
- Radar Surabaya
- Radar Kediri
- Radar Tulungagung
- Radar Bojonegoro
- Radar Lawu
- Radar Madura

### Central Java
- Radar Semarang
- Radar Solo
- Radar Kudus
- Radar Jogja

With `scraperadaR`, scraping news becomes easier and more efficient. Happy scraping!
