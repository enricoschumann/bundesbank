getSeries <- function(series, start = NULL,
                      end = format(Sys.Date(), "%Y-%m")) {
    on.exit(return(invisible(NULL)))
    if (grepl("^BBKRT", series)) {
        ## REAL TIME
        site <- paste0("https://www.bundesbank.de/statistic-rmi/",
                      "StatisticDownload?tsId=",
                      series,
                      "&rtd_csvFormat=en",
                      "&rtd_fileFormat=csv",
                      "&mode=rtd")
        con <- url(site)
        message("Downloading data from Bundesbank ... ")
        txt <- try(readLines(con), silent = TRUE)
        em <- geterrmessage()
        close(con)
        if (inherits(txt, "try-error")) {
            message("Failed.")
            message("Error message: ", em)
            return(invisible(NULL))
        } else
            message("Done.")

        txt.head <- txt[1:5]
        txt.csv <- txt[-c(1:5)]
        tb <- read.table(text = txt.csv,
                         header = FALSE,
                         sep = ",",
                         stringsAsFactors = FALSE)
        row.names(tb) <- tb[[1]]
        tb <- tb[, -1]

        h.split <- strsplit(txt.head, " *, *")
        colnames(tb)                <- h.split[[1]][-1L]
        attr(tb, "a1")      <- as.Date(h.split[[1]][-1L])
        attr(tb, "unit")            <- h.split[[2]][-1L]
        attr(tb, "unit multiplier") <- h.split[[3]][-1L]
        attr(tb, "Baseyear")        <- h.split[[4]][-1L]
        attr(tb, "Record meth")     <- h.split[[5]][-1L]
        on.exit()
        return(tb)
    }

    if (!is.null(start)) {
        if (nchar(start) != 7L) {
            warning("'start' not in format YYYY-MM")
            tmp <- as.Date(as.character(start))
            if (!is.na(tmp))
                start <- strftime(tmp, "%Y-%m") else
            stop("'start' not in required format")
        }
    }
    if (nchar(end) != 7L) {
        warning("'end' not in format YYYY-MM")
        tmp <- as.Date(as.character(end))
        if (!is.na(tmp))
            end <- strftime(tmp, "%Y-%m") else
        stop("'end' not in required format")
    }

    sstart <- ifelse(is.null(start), "",
                     paste("&its_from=", start, sep = ""))
    sto <- paste("&its_to=", end, sep = "")
    site <- paste("http://www.bundesbank.de/cae/servlet/CsvDownload?",
                  "tsId=", series, "&mode=its&its_csvFormat=en",
                  "&its_currency=default&its_dateFormat=dateOfDay&",
                  sstart, "&", sto, sep = "")
    con <- url(site)
    message("Downloading data from Bundesbank ... ")
    dats <- try(readLines(con), silent = TRUE)
    em <- geterrmessage()
    close(con)
    if (inherits(dats, "try-error")) {
        message("Failed.")
        message("Error message: ", em)
        return(NULL)
    } else {
        message("Done.")
    }
    dats <- read.csv(textConnection(dats), stringsAsFactors = FALSE)
    ## is last line a comment?
    if (dats[NROW(dats), 1L] == "") {
        doc <- dats[NROW(dats), 2L]
        dats <- dats[-NROW(dats), ]
    } else {
        doc <- NULL
    }

    doc0 <- dats[2:4,]
    doc0 <- paste(doc0[ ,1L], doc0[ ,2L], sep = ": ")
    doc0 <- c(dats[1L, 2L], doc0)
    doc <- c(doc0, doc)
    dats <- dats[-(1:4), ]


    dates <- as.Date(dats[ ,1L])
    values <- dats[, 2L]
    NAs <- is.na(dates)
    dates <- dates[!NAs]
    values <- values[!NAs]
    missing <- values == "."
    dates <- dates[!missing]
    values <- as.numeric(values[!missing])
    on.exit()
    result <- list(dates = dates, values = values)
    attr(result, "info") <-  doc
    result
}
