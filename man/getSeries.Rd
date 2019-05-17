\name{getSeries}
\alias{getSeries}
\alias{bundesbank-package}
\alias{bundesbank}
\title{
  Download Time-Series Data from Bundesbank
}
\description{
  Download time-series from the website of the Bundesbank.
}
\usage{
getSeries(series,
          start = NULL,
          end = format(Sys.Date(), "\%Y-\%m"),
          return.class = "data.frame",
          verbose = TRUE, dest.dir = NULL)
}
\arguments{

  \item{series}{The series name as given by the
    Bundesbank (e.g. \sQuote{\acronym{BBK01.ST0316}}
    for 3-Month \acronym{EURIBOR}).}

  \item{start}{character in format
    \sQuote{\acronym{YYYY}-\acronym{MM}} or
    \sQuote{\acronym{YYYY}}.  If omitted, the function
    downloads data from the earliest available
    date. Currently ignored for real-time datasets.}

  \item{end}{character in format
    \sQuote{\acronym{YYYY}.\acronym{MM}} or
    \sQuote{\acronym{YYYY}}.  If omitted, the function
    downloads data up to the most recent available
    date. Currently ignored for real-time datasets.}

  \item{return.class}{character or
    \code{NULL}. Currently supported are \code{"zoo"}
    and \code{"data.frame"}. If \code{NULL} or
    \code{"list"}, a \code{\link{list}} is returned for
    time-series. Real-time data are always returned as a
    \code{\link{data.frame}}.}

  \item{verbose}{logical}

  \item{dest.dir}{\code{NULL} or
  \code{\link{character}}. If character, it must be the
  path to an existing directory. See Details.}

}
\details{

  The data can be downloaded in CSV-format from the
  Bundesbank's website \url{http://www.bundesbank.de} .

  If \code{dest.dir} is provided, the downloaded
  dataset gets a date prefix (today in format
  \code{YYYYMMDD}) and is stored in directory
  \code{dest.dir}. Before any download is attempted,
  the function checks whether a file with today's
  prefix exist in \code{dest.dir}; if yes, the file is
  used.

}
\value{

  A data.frame with two (or more) columns:
  \item{dates}{dates (of class \code{Date})}
  \item{values}{numerical values}

  The result for single time-series may have an
  attribute \code{info}, which is a character vector
  and holds additional information for \code{series}
  (such as its unit). See Examples.

  Real-time datasets are always organised as a
  \code{\link{data.frame}},
  in which the rows correspond to the reporting period
  and the columns to the publication date.
  Real-time  datasets always have several attributes,
  in particular \code{date}, which corresponds to the
  publication date.

}
\author{

  Maintainer: Enrico Schumann <es@enricoschumann.net>

}

\examples{
\donttest{
## (Internet connection required)
  
series <- "BBK01.ST0304" ## Eonia
res <- getSeries(series) ## retrieve all available data
res <- getSeries(series, start = "2012-01")
res <- getSeries(series, end   = "2012-01")
res <- getSeries(series, start = "2012-01", end = "2012-05")

## make 'zoo' series
if (require("zoo")) {
    Eonia <- zoo(res$values, res$dates)
    plot(Eonia)
}

## check comments
writeLines(strwrap(paste("- ", attr(res, "info")), width = 60, exdent = 2))

## real-time dataset (Gross domestic product)
gdp <- getSeries("BBKRT.A.DE.N.A.AG1.CA010.V.A")

## use caching
## ==> the example uses a temporary directory, but
##     better is to use a less ephemeral destination,
##     e.g. '~/Downloads/bundesbank'
gdp <- getSeries("BBKRT.A.DE.N.A.AG1.CA010.V.A",
                 dest.dir = tempdir())
### Downloading data from Bundesbank ... Done.

gdp <- getSeries("BBKRT.A.DE.N.A.AG1.CA010.V.A",
                 dest.dir = tempdir())
### Using cache ... Done.
}
}
