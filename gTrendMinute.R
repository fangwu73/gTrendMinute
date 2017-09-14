check.time <- function (time) 
{
  stopifnot(is.character(time))
  fixed_format <- c("now 1-H", "now 4-H", "now 1-d", "now 7-d", 
                    "today 1-m", "today 3-m", "today 12-m", "today+5-y", 
                    "all")
  if (time %in% fixed_format) {
    return(TRUE)
  }
  time <- unlist(strsplit(time, " "))
  if (length(time) != 2) {
    return(FALSE)
  }
  start_date <- anytime::anydate(time[1])
  end_date <- anytime::anydate(time[2])
  if (is.na(start_date) | is.na(end_date)) {
    return(FALSE)
  }
  if (start_date >= end_date) {
    return(FALSE)
  }
  if (start_date < as.Date("2004-01-01")) {
    return(FALSE)
  }
  if (end_date > Sys.Date()) {
    return(FALSE)
  }
  return(TRUE)
}
get_widget <- function (comparison_item, category, gprop) {
  token_payload <- list()
  token_payload$comparisonItem <- comparison_item
  token_payload$category <- category
  token_payload$property <- gprop
  url <- URLencode(paste0("https://www.google.com/trends/api/explore?property=&req=", 
                          jsonlite::toJSON(token_payload, auto_unbox = TRUE), "&tz=0&hl=en-US"))
  widget <- curl::curl_fetch_memory(url)
  stopifnot(widget$status_code == 200)
  myjs <- jsonlite::fromJSON(substring(rawToChar(widget$content), 
                                       first = 6))
  widget <- myjs$widgets
}
interest_over_time <- function (widget, comparison_item) {
  payload2 <- list()
  payload2$locale <- widget$request$locale[1]
  payload2$comparisonItem <- widget$request$comparisonItem[[1]]
  payload2$resolution <- widget$request$resolution[1]
  payload2$requestOptions$category <- widget$request$requestOptions$category[1]
  payload2$requestOptions$backend <- widget$request$requestOptions$backend[1]
  payload2$time <- widget$request$time[1]
  payload2$requestOptions$property <- widget$request$requestOptions$property[1]
  url <- paste0("https://www.google.fr/trends/api/widgetdata/multiline/csv?req=", 
                jsonlite::toJSON(payload2, auto_unbox = T), "&token=", 
                widget$token[1], "&tz=0")
  res <- curl::curl_fetch_memory(URLencode(url))
  stopifnot(res$status_code == 200)
  con <- textConnection(rawToChar(res$content))
  df <- read.csv(con, skip = 1, stringsAsFactors = FALSE)
  close(con)
  if (nrow(df) < 1) {
    return(NULL)
  }
  n <- nrow(df)
  df <- reshape(df, varying = names(df)[2:ncol(df)], v.names = "hits", 
                direction = "long", timevar = "temp", times = names(df)[2:ncol(df)])
  df$temp <- NULL
  df <- cbind(df, comparison_item[rep(seq_len(nrow(comparison_item)), 
                                      each = n), 1:2], row.names = NULL)
  df$geo <- ifelse(df$geo == "", "world", df$geo)
  df$gprop <- ifelse(widget$request$requestOptions$property[1] == 
                       "", "web", widget$request$requestOptions$property[1])
  df$category <- widget$request$requestOptions$category[1]
  names(df)[1] <- "date"
  df$id <- NULL
  if (unique(comparison_item$time) == "all") {
    df$date <- anytime::anydate(df$date)
  }
  else {
    df$date <- anytime::anytime(df$date)
  }
  return(df)
}

interest_by_region <- function (widget, comparison_item) {
  i <- which(grepl("Interest by", widget$title) == TRUE)
  resolution <- ifelse("world" %in% widget$geo, "COUNTRY", 
                       "REGION")
  if (any(grepl("region", widget$title))) {
    region <- lapply(i, create_geo_payload, widget = widget, 
                     resolution = resolution)
    region <- do.call(rbind, region)
  }
  else {
    region <- NULL
  }
  dma <- lapply(i, create_geo_payload, widget = widget, resolution = "DMA")
  dma <- do.call(rbind, dma)
  city <- lapply(i, create_geo_payload, widget = widget, resolution = "CITY")
  city <- do.call(rbind, city)
  res <- list(region = region, dma = dma, city = city)
  return(res)
} 

related_topics <- function (widget, comparison_item, hl) {
  i <- which(grepl("topics", widget$title) == TRUE)
  res <- lapply(i, create_related_topics_payload, widget = widget, 
                hl = hl)
  res <- do.call(rbind, res)
  return(res)
}
interest <- function (keyword, geo = "", time = "today+5-y", gprop = c("web", 
                                                                       "news", "images", "froogle", "youtube"), category = 0, hl = "en-US") 
{
  # time = "2017-09-08T09 2017-09-08T13"
  # geo = "US"
  # keyword = "cat"
  # gprop = "web"
  # stopifnot((length(keyword)%%length(geo) == 0) || (length(geo)%%length(keyword) == 
  #                                                     0), is.vector(keyword), length(keyword) <= 5, length(geo) <= 
  #             5, length(time) == 1, length(hl) == 1, is.character(hl), 
  #           hl %in% language_codes$code)
  # if (geo != "" && !all(geo %in% countries[, "country_code"]) && 
  #     !all(geo %in% countries[, "sub_code"])) {
  #   stop("Country code not valid. Please use 'data(countries)' to retreive valid codes.", 
  #        call. = FALSE)
  # }
  # if (!all(category %in% categories[, "id"])) {
  #   stop("Category code not valid. Please use 'data(categories)' to retreive valid codes.", 
  #        call. = FALSE)
  # }
  # if (!check_time(time)) {
  #   stop("Can not parse the supplied time format.", call. = FALSE)
  # }
  gprop <- match.arg(gprop, several.ok = FALSE)
  gprop <- ifelse(gprop == "web", "", gprop)
  comparison_item <- data.frame(keyword, geo, time, stringsAsFactors = FALSE)
  widget <- get_widget(comparison_item, category, gprop)
  interest_over_time <- interest_over_time(widget, comparison_item)
  return(interest_over_time)
  # interest_by_region <- interest_by_region(widget, comparison_item)
  # related_topics <- related_topics(widget, comparison_item, 
  #                                  hl)
  # related_queries <- related_queries(widget, comparison_item)
  # res <- list(interest_over_time = interest_over_time, interest_by_region = interest_by_region$region, 
  #             interest_by_dma = interest_by_region$dma, interest_by_city = interest_by_region$city, 
  #             related_topics = related_topics, related_queries = related_queries)
  # class(res) <- c("gtrends", "list")
  # return(res)
}

interest(keyword = "dog", geo = "US", time = "2017-09-08T0 2017-09-08T4", gprop = c("web", "news", "images", "froogle", "youtube"), category = 0, hl = "en-US")


start_date = strptime(
  '2017-08-12 00:00:00',
  format = "%Y-%m-%d %H:%M:%S", tz = 'GMT')
end_date = strptime(
  '2017-08-13 00:00:00',
  format = "%Y-%m-%d %H:%M:%S", tz = 'GMT')
timeStamp = seq(start_date, end_date, by = '4 hour')

timeSeq = paste(as.Date(timeStamp, tz= attributes(timeStamp)$tzone), lubridate::hour(timeStamp), sep='T')
timePair1 = timeSeq[-1]
timePair2 = timeSeq[-length(timeSeq)]
timePair = paste(timePair2, timePair1)

tmpL <- lapply(timePair, function(x) interest(keyword = "zillow", geo = "US", time = x, gprop = c("web", "news", "images", "froogle", "youtube"), category = 0, hl = "en-US"))
tmpdf <- do.call(rbind, tmpL)
tmpdf[duplicated(tmpdf$date), ]
head(tmpdf)
View(tmpdf)

tmpdf$date = as.character(tmpdf$date)
tmpdf2 = data.frame(date = as.character(seq(start_date, end_date, by = 'min')))

merge  = tmpdf %>% left_join(tmpdf2, by = 'date')
merge[!complete.cases(merge), ]

length(unique(tmpdf$date))
which(table(tmpdf$date)!=1
      )
