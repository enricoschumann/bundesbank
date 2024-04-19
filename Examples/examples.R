## EXAMPLES

library("bundesbank")

dl.dir <- "~/Downloads/bundesbank"

series <- "BBK01.ST0304" ## Eonia

res <- getSeries(series, dest.dir = dl.dir) ## retrieve all available data
##        dates values
## 1 1999-01-04   3.20
## 2 1999-01-05   3.20
## 3 1999-01-06   3.21
## 4 1999-01-07   3.21
## 5 1999-01-08   3.21
## 6 1999-01-11   3.20

## check comments
writeLines(strwrap(paste("- ", attr(res, "info")),
                   width = 60, exdent = 2))
## - Money market rates / EONIA / Daily quotations
## - unit: % p.a.
## - unit multiplier: one
## - last update: 2023-03-01 13:36:53
## - General: Discontinued. The Euro short-term rate (€STR) as
##   published by the European Central Bank is designated as
##   the replacement rate for the Euro overnight index average
##   (EONIA). Starting on 2 October 2019 the EONIA has been
##   calculated as the €STR plus a spread of 8.5 basis points.
##   Before: Euro OverNight Index Average: weighted average
##   overnight rate for interbank operations calculated by the
##   European Central Bank since 4 January 1999 on the basis
##   of real turnover according to the act/360 method and
##   published via Reuters. Published with 2 decimal points
##   until 31 August 2007. Data became fee liable from 1
##   January 2014 and therefore was be published with the
##   delay of one working day. Neither Euribor EBF, nor
##   Euribor ACI, nor the Euribor Panel Banks, nor the Euribor
##   Steering Committee, nor Reuters, nor the European Central
##   Bank, nor the Deutsche Bundesbank can be held liable for
##   any irregularity or inaccuracy of the EONIA rate.


res <- getSeries(series, dest.dir = dl.dir, column1.only = FALSE)
##                 dates values.BBK01.ST0304 values.BBK01.ST0304_FLAGS
## 1999-01-01 1999-01-01                  NA        No value available
## 1999-01-02 1999-01-02                  NA        No value available
## 1999-01-03 1999-01-03                  NA        No value available
## 1999-01-04 1999-01-04                3.20
## 1999-01-05 1999-01-05                3.20
## 1999-01-06 1999-01-06                3.21

res <- getSeries(series, start = "2012-01", dest.dir = dl.dir)
res <- getSeries(series, end   = "2012-01", dest.dir = dl.dir)
res <- getSeries(series, start = "2012-01", end = "2012-05",
                 dest.dir = dl.dir)
##                 dates values
## 2012-01-02 2012-01-02  0.390
## 2012-01-03 2012-01-03  0.396
## 2012-01-04 2012-01-04  0.395
## 2012-01-05 2012-01-05  0.369
## 2012-01-06 2012-01-06  0.357
## 2012-01-09 2012-01-09  0.372
## ....
##                 dates values
## 2012-05-24 2012-05-24  0.335
## 2012-05-25 2012-05-25  0.323
## 2012-05-28 2012-05-28  0.322
## 2012-05-29 2012-05-29  0.335
## 2012-05-30 2012-05-30  0.333
## 2012-05-31 2012-05-31  0.329


library("zoo")
library("plotseries")
Eonia <- zoo(res$values, res$dates)
plotseries(Eonia)

## check comments
writeLines(strwrap(paste("- ", attr(res, "info")),
                   width = 60, exdent = 2))

## REAL-TIME DATASET (GROSS DOMESTIC PRODUCT)
gdp <- getSeries("BBKRT.A.DE.N.A.AG1.CA010.V.A",
                 dest.dir = dl.dir)




### OUTPUT IN THE PRODUCTION SECTOR / GERMANY
res <- getSeries("BBDE1.M.DE.Y.BAA1.A2P000000.G.C.I21.A",
                 dest.dir = dl.dir)
writeLines(strwrap(paste("- ", attr(res, "info")),
                   width = 60, exdent = 2))
plotseries(res$values, res$dates)

res <- getSeries("BBDE1.M.DE.Y.BAA1.A2P000000.G.C.I21.A",
                 dest.dir = dl.dir, column1.only = FALSE)
tail(res)
##                 dates values.BBDE1.M.DE.Y.BAA1.A2P000000.G.C.I21.A values.BBDE1.M.DE.Y.BAA1.A2P000000.G.C.I21.A_FLAGS
## 2023-09-30 2023-09-30                                         95.0                                  Provisional value
## 2023-10-31 2023-10-31                                         94.6                                  Provisional value
## 2023-11-30 2023-11-30                                         94.7                                  Provisional value
## 2023-12-31 2023-12-31                                         92.8                                  Provisional value
## 2024-01-31 2024-01-31                                         94.0                                  Provisional value
## 2024-02-29 2024-02-29                                         96.0                                  Provisional value




## INTEREST RATES
res <- getSeries("BBSIS.D.I.ZST.ZI.EUR.S1311.B.A604.R10XX.R.A.A._Z._Z.A",
                 dest.dir = dl.dir)
plotseries(res$values, res$dates)
writeLines(strwrap(paste("- ", attr(res, "info")),
                   width = 60, exdent = 2))

res <- getSeries("BBSIS.D.I.ZAR.ZI.EUR.S1311.B.A604.R10XX.R.A.A._Z._Z.A",
                 dest.dir = dl.dir,
                 return.class = "zoo")
plotseries(res)




## GOLD
gold <- getSeries("BBEX3.D.XAU.USD.EA.AC.C05",
                  dest.dir = dl.dir,
                  return.class = "zoo")
plotseries(gold)
writeLines(strwrap(paste("- ", attr(gold, "info")),
                   width = 60, exdent = 2))
## - Gold price in London / afternoon fixing / 1 ounce of fine
##   gold = USD ...
## - unit: USD
## - unit multiplier: one
## - last update: 2024-04-19 13:37:18
## - General: 1 ounce of fine gold = 31.1034768 g. First
##   quoted on 1 April 1968.
## - Source: April 1968 - March 1974: Financial Times (FT);
##   April 1974 - December 1980: Samuel Montagu & Co. Ltd.;
##   January 1981 - December 1998: FT; January 1999 - present:
##   The London Bullion Market Association.
