\name{getSeries}
\alias{getSeries}
\title{
  Download Time-Series Data from Bundesbank
}
\description{
  Download time series from the website of the Bundesbank.
}
\usage{
getSeries(series, start = NULL, end = format(Sys.Date(), "\%Y-\%m"),
          return.class = NULL, verbose = TRUE, dest.dir = NULL)
}
\arguments{
  \item{series}{The series name as given by the Bundesbank (e.g.
    \sQuote{\acronym{BBK01.ST0316}} for 3-Month \acronym{EURIBOR}).}
  \item{start}{character in format
    \sQuote{\acronym{YYYY}-\acronym{MM}}. If omitted, the function
    downloads data from the earliest available date.}
  \item{end}{character in format
    \sQuote{\acronym{YYYY}.\acronym{MM}}. If omitted, the function
    downloads data up to the most recent available date.}

  \item{return.class}{character or
    \code{NULL}. Currently supported are \code{"zoo"} and
    \code{"data.frame"}. If \code{NULL} (the default), a
    \code{\link{list}} is returned for time series, and a
    \code{\link{data.frame}} for real-time data.}
  
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
  prefix exist in \code{dest.dir}; if yes, the file
  used.

}
\value{
  A list:
  \item{dates}{dates (of class \code{Date})}
  \item{values}{numerical values}

  The list has an attribute \code{info}, which is a character vector and
  holds additional information for \code{series} (such as its unit).

}
\author{
  Enrico Schumann
}

\examples{
\dontrun{
series <- "BBK01.QUY229"

series <- "BBK01.ST0304" ## Eonia
res <- getSeries(series) ## retrieve all available data
res <- getSeries(series, start = "2012-01")
res <- getSeries(series, end   = "2012-01")
res <- getSeries(series, start = "2012-01", end = "2012-05")

## create a data.frame
df <- data.frame(res)
head(df)

## make zoo series
require("zoo")
eonia <- zoo(res$values, res$dates)
plot(eonia)

## check comments
writeLines(strwrap(paste("- ", attr(res, "info")), width = 60, exdent = 2))

## realtime dataset (Gross domestic product)
getSeries("BBKRT.A.DE.N.A.AG1.CA010.V.A")

}
}
