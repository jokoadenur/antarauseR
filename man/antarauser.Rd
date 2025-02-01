\name{antarauser}
\alias{antarauser}
\title{Scrape Data from Antara News}
\usage{
antarauser(wilayahantara, keyword, awal, akhir)
}
\description{
Scrape data from Antara News which is spread across Indonesia.
}
\arguments{
  \item{wilayahantara}{Character. The region or area to scrape news from.}
  \item{keyword}{Character. The keyword to search for news articles.}
  \item{awal}{Integer. The starting year for the search.}
  \item{akhir}{Integer. The ending year for the search.}
}
\examples{
# Example usage
antarauser("Papua", "ekonomi", "2025-01-01", "2025-01-31")
}
