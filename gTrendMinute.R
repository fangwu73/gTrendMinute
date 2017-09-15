
# Get widget --------------------------------------------------------------

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


# Data over time ----------------------------------------------------------

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

# Check data interval -----------------------------------------------------

checkTimeinterval =
  #
  # Function to find time interval✌️
  #
  function(time = "2017-01-01T00 2017-01-02T00") {
    keyword = 'wut'
    geo = 'US'
    gprop = 'web'
    category = 0
    hl = "en-US"
    comparison_item = data.frame(keyword, 
                                 geo, 
                                 time, 
                                 stringsAsFactors = FALSE
    )
    widget = get_widget(comparison_item, 
                        category, 
                        gprop
    )
    return(widget$request$resolution)
  }


# Sub main fun ------------------------------------------------------------

interest = 
  # 
  # Main function to query google trend data in terms of interest over time 
  # Arg:
  #    gprop: only be accepted single value 
  # 
  function (keyword, geo = "US", time = "today+5-y", gprop = c("web", "news", "images", "froogle", "youtube"), category = 0, hl = "en-US") {
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


# Main fun ----------------------------------------------------------------

longTermQuery = 
  #
  # Rolling pull long term minute-level data
  # 1 minute overlapping
  #
  function(start_date, end_date, keyword, geo = 'US', gprop = 'web', category = 0, hl = 'en-US') {
    require(parallel)
    # change date format
    start_date = strptime(
      start_date,
      format = "%Y-%m-%d %H:%M:%S", 
      tz = 'GMT')
    
    end_date = strptime(
      end_date,
      format = "%Y-%m-%d %H:%M:%S", 
      tz = 'GMT')
    
    # create time stamp sequence
    diff.time = difftime(end_date, start_date, units = 'hours')
    if(diff.time < 4) {
      time.stamp = c(start_date, end_date)
    } else {
      time.stamp = seq(start_date, end_date, by = '4 hour') 
    }
    
    # create time pair
    time.seq = paste(as.Date(time.stamp, 
                             tz= attributes(time.stamp)$tzone
                             ), 
                     lubridate::hour(time.stamp), 
                     sep='T')
    time.pair.1 = time.seq[-1]
    time.pair.2 = time.seq[-length(time.seq)]
    time.pair = paste(time.pair.2, time.pair.1)
    
    # paralle querying data
    cl = makeCluster(4)
    clusterExport(cl, varlist = c('interest', 'get_widget', 'interest_over_time'), envir = globalenv())
    clusterExport(cl, varlist = c('keyword', 'geo', 'gprop', 'category', 'hl'), envir = environment())
    tmp.list = parLapply(cl, 
                         time.pair,
                         function(x) {
                           interest(keyword = keyword, 
                                    geo = geo, 
                                    time = x, 
                                    gprop = gprop, 
                                    category = category,
                                    hl = hl)
                         })
    stopCluster(cl)
    
    # make result
    tmpdf = do.call(rbind, tmp.list)
    
    return(tmpdf)
    
  }



# test
df = longTermQuery('2017-01-01 00:00:00', '2017-01-02 01:00:00', keyword = c('zillow', 'trulia'), geo = c("US","CA"))
