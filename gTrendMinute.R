get_widget = 
  #
  # Function to retreive url with curl and save in memory
  #
  function (comparison_item, category, gprop) {
    # prepare token_payload and URL
    token_payload = list()
    token_payload$comparisonItem = comparison_item
    token_payload$category = category
    token_payload$property = gprop
    url = URLencode(
     paste0(
        "https://www.google.com/trends/api/explore?property=&req=", 
        jsonlite::toJSON(token_payload, auto_unbox = TRUE), 
        "&tz=0&hl=en-US")
      )
    
    # Get widget through curl with the URL
    widget = curl::curl_fetch_memory(url)
    stopifnot(widget$status_code == 200)
    myjs = jsonlite::fromJSON(
      substring(
        rawToChar(widget$content), 
        first = 6)
      )
    
    # return widget
    widget <- myjs$widgets
  }

interest_over_time =
  #
  # Function to fetch data frame in terms of insterest over time
  #
  function (widget, comparison_item) {
    # Prepare payload2 and URL by using widget
    payload2 = list()
    payload2$locale = widget$request$locale[1]
    payload2$comparisonItem = widget$request$comparisonItem[[1]]
    payload2$resolution = widget$request$resolution[1]
    payload2$requestOptions$category = widget$request$requestOptions$category[1]
    payload2$requestOptions$backend = widget$request$requestOptions$backend[1]
    payload2$time = widget$request$time[1]
    payload2$requestOptions$property = widget$request$requestOptions$property[1]
    url = paste0(
      "https://www.google.fr/trends/api/widgetdata/multiline/csv?req=", 
      jsonlite::toJSON(payload2, auto_unbox = T), 
      "&token=", 
      widget$token[1], 
      "&tz=0"
      )
    
    # Get data back through the URL
    res = curl::curl_fetch_memory(URLencode(url))
    stopifnot(res$status_code == 200)
    
    # Read text file into csv format 
    con = textConnection(
      rawToChar(res$content)
      )
    df = read.csv(con, skip = 1, stringsAsFactors = FALSE)
    close(con)
    if (nrow(df) < 1) {
      return(NULL)
    }
    
    # Prepare data frame to return 
    n = nrow(df)
    df = reshape(df, 
                 varying = names(df)[2 : ncol(df)], 
                 v.names = "hits", 
                 direction = "long", 
                 timevar = "temp", 
                 times = names(df)[2 : ncol(df)])
    df$temp = NULL
    df = cbind(df, 
               comparison_item[rep(seq_len(nrow(comparison_item)), each = n), 1:2], 
               row.names = NULL
               )
    df$geo = ifelse(df$geo == "", "world", df$geo)
    df$gprop = ifelse(widget$request$requestOptions$property[1] == "", 
                      "web", 
                      widget$request$requestOptions$property[1]
                      )
    df$category = widget$request$requestOptions$category[1]
    names(df)[1] = "date"
    df$id = NULL
    
    # Parse date in df, set all 'tz' equal to GMT
    if (unique(comparison_item$time) == "all") {
      df$date = anytime::anydate(df$date, tz = 'GMT')
    } else {
      df$date = anytime::anytime(df$date, tz = 'GMT')
    }
    
    return(df)
  
}


interest = 
  # 
  # Main function to query google trend data in terms of interest over time 
  # Arg:
  #    gprop: only be accepted single value 
  # 
  function (keyword, geo = "", time = "today+5-y", gprop = c("web", "news", "images", "froogle", "youtube"), category = 0, hl = "en-US") {
    # check whether `gprop` matches any candidates
    gprop = match.arg(gprop, several.ok = FALSE)
    gprop = ifelse(gprop == "web", "", gprop)
    
    # Prepare args `comparison_item` and `widget`
    comparison_item = data.frame(keyword, 
                                 geo, 
                                 time, 
                                 stringsAsFactors = FALSE
                                 )
    widget = get_widget(comparison_item, 
                        category, 
                        gprop
                        )
    
    # Fetch data
    interest_over_time = interest_over_time(widget, comparison_item)
    return(interest_over_time)
  }

interest(keyword = "dog", geo = "US", time = "2017-09-08T0 2017-09-08T4", gprop = c( "news"), category = 0, hl = "en-US")


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
which(table(tmpdf$date)!=1)
