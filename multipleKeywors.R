if (!require("devtools")) install.packages("devtools")
devtools::install_github("PMassicotte/gtrendsR")
install.packages("gtrendsR")
library(gtrendsR)
gtrends
methods(gtrends)
getAnywhere(gtrends.default)
function (query = "", geo = "", cat = "0", gprop = c("", "news", 
                                                     "images", "froogle", "youtube"), session, 
          res = c(NA, "1h", "4h", "1d", "7d"), 
          start_date = as.Date("2017-09-01"), end_date = as.Date(Sys.time()), 
          ...) 
{
  query = c("a","b")
  geo = c("US")
  gprop = "web"
  res = "1h"
  
  if (missing(session)) 
    session <- .getDefaultConnection()
  stopifnot(is.character(query), is.vector(query), length(query) <= 
              5, length(geo) <= 5, cat %in% categories$id)
  res <- match.arg(res, several.ok = FALSE)
  gprop <- match.arg(gprop, several.ok = FALSE)
  if (length(query) > 1 & length(geo) > 1) {
    stop("Can not specify multiple keywords and geo at the same time.", 
         call. = FALSE)
  }
  cmpt <- ifelse(length(query) > 1, "q", "geo")
  if (is.null(session)) 
    stop("You are not signed in. Please log in using gconnect().", 
         call. = FALSE)
  start_date <- as.Date(start_date, "%Y-%m-%d")
  end_date <- as.Date(end_date, "%Y-%m-%d")
  if (is.na(start_date)) {
    stop("start_date is not a valid date. Please use yyyy-mm-dd format.", 
         call. = FALSE)
  }
  if (is.na(end_date)) {
    stop("end_date is not a valid date. Please use yyyy-mm-dd format.", 
         call. = FALSE)
  }
  stopifnot(start_date < end_date, start_date >= as.Date("2004-01-01"), 
            end_date <= as.Date(Sys.time()))
  nmonth <- length(seq(from = start_date, to = end_date, by = "month"))
  if (nmonth >= 1) {
    date <- paste(format(start_date, "%m/%Y"), paste(nmonth, 
                                                     "m", sep = ""))
  }
  if (!is.na(res)) {
    resolution_code <- data.frame(code = c("1-H", "4-H", 
                                           "1-d", "7-d"), 
                                  res = c("1h", "4h", "1d", "7d"), stringsAsFactors = FALSE)
    res <- resolution_code$code[resolution_code$res == res]
    date <- paste("now", res)
  }
  query <- paste(query, collapse = ",")
  if (!(Encoding(query) == "UTF-8")) {
    query <- iconv(query, "latin1", "utf-8", sub = "byte")
  }
  if (geo != "" && !all(geo %in% countries[, "country_code"]) && 
      !all(geo %in% countries[, "sub_code"])) {
    stop("Country code not valid. Please use 'data(countries)' to retreive valid codes.", 
         call. = FALSE)
  }
  geo <- paste(geo, sep = "", collapse = ", ")
  trendsURL <- "https://www.google.com/trends/trendsReport?"
  pp <- list(q = query, cat = cat, cmpt = cmpt, content = 1, 
             export = 1, date = date, geo = geo, gprop = gprop)
  trendsURL <- paste(trendsURL, paste(names(pp), pp, sep = "=", 
                                      collapse = "&"), sep = "")
  trendsURL <- URLencode(trendsURL)
  resultsText <- rvest::jump_to(session, trendsURL)
  if (any(grep("quota", resultsText, ignore.case = TRUE))) {
    stop("Reached Google Trends quota limit! Please try again later.")
  }
  queryparams <- c(query = query, cat = cat, geo = geo, time = format(Sys.time()))
  resultsText <- rawToChar(resultsText$response$content)
  res <- .processResults(resultsText, queryparams)
  class(res) <- c("gtrends", "list")
  res
}