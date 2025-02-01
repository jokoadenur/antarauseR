![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg)
![CRAN Version](https://img.shields.io/badge/CRAN-7.3.2-brightgreen)
![Open Issues](https://img.shields.io/badge/open%20issues-0-brightgreen)
![License](https://img.shields.io/badge/License-MIT-blue)
![License](https://img.shields.io/badge/blog-jokoding.com-brightgreen)

![WhatsApp Image 2025-02-01 at 19 02 17](https://github.com/user-attachments/assets/d47ab993-5fad-4bea-ba31-06ca7bc82520)



# antarauseR

`antarauseR` is an R package designed for scraping news articles from various Antara News in Indonesia. With this package, you can easily retrieve news data for a specified number of pages.

## Installation

To install the `antarauseR` package, run the following code in your R script:

```R
# Install package from GitHub
devtools::install_github("jokoadenur/antarauseR")
```

> **Note:** If prompted to update certain packages (options like 1. All, 2. CRAN, etc.), simply press **ENTER** to skip. Wait until the installation process is complete and the message `DONE (antarauseR)` appears.

After installation, activate the package with the following code:

```R
# Activate the package
library(antarauseR)
```

## Usage

To scrape news articles from Antara News in Indonesia, use the `antarauser()` function with the following format:

```R
antarauser("Antara Nesws sites name", "keywords", "start_period", "end_period")
```

### Examples:

   Scraping news from Antara Jawa Timur (jatim.antaranews.com) with keyword "ekonomi", start from "2025-01-01" to "2025-01-31":
   ```R
   antarauser("jatim", "ekonomi", "2025-01-01", "2025-01-31")
   ```
   As a disclaimer: this package still cover jatim.antaranews.com, jateng.antaranews.com, jogja.antaranews.com, bali.antaranews.com, jabar.antaranews.com,
   lampung.antaranews.com, makassar.antaranews.com, and bengkulu.antaranews.com

With `antarauseR`, scraping news becomes easier and more efficient. Happy scraping!
