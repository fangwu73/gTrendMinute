.pkgenv <- new.env(parent=emptyenv())

# Get Google API cookies handler
get_api_cookies =
  #
  # get the requisite cookies from Google's API and assign cookie handler to .pkgenv
  #
  function(cookie_url) {
  # create new handler
  cookie_handler <- curl::new_handle()
  # fetch API cookies
  cookie_req <- curl::curl_fetch_memory(cookie_url, handle = cookie_handler)
  curl::handle_cookies(cookie_handler)
  # assign handler to .pkgenv environment
  .pkgenv[["cookie_handler"]] <- cookie_handler
  return(NULL)
}

# Get widget --------------------------------------------------------------

get_widget = 
  #
  # Function to retreive url with curl and save in memory
  #
  function (comparison_item, category, gprop, hl) {
    # prepare token_payload and URL
    token_payload = list()
    token_payload$comparisonItem = comparison_item
    token_payload$category = category
    token_payload$property = gprop
    url <- URLencode(
      paste0(
      "https://www.google.com/trends/api/explore?property=&req=",
      jsonlite::toJSON(token_payload, auto_unbox = TRUE),
      "&tz=0&hl=", hl
      )
    ) ## The tz part is unclear but different
    ## valid values do not change the result:
    ## clarification needed.
    
    # NOT working: Get widget through curl with the URL
    # widget = curl::curl_fetch_memory(url)
    # stopifnot(widget$status_code == 200)
    # myjs = jsonlite::fromJSON(
    #   substring(
    #     rawToChar(widget$content), 
    #     first = 6)
    #   )
    
    #url <- encode_keyword(url)
    
    # if cookie_handler hasn't been set up, get the requisite cookies from Google's API
    if(!exists("cookie_handler", envir = .pkgenv)){ get_api_cookies(cookie_url="http://trends.google.com/Cookies/NID") }
    # get the tokens etc., using the URL and the cookie_handler
    widget <- curl::curl_fetch_memory(url, 
                                      handle = .pkgenv[["cookie_handler"]])
    
    stopifnot(widget$status_code == 200)
    
    ## Fix encoding issue for keywords like österreich"
    temp <- rawToChar(widget$content)
    Encoding(temp) <- "UTF-8"
    
    myjs <- jsonlite::fromJSON(substring(temp, first = 6))
    
    # return widget
    widget <- myjs$widgets
  }


# Data over time ----------------------------------------------------------

interest_over_time =
  #
  # Function to fetch data frame: insterest over time
  #
  function (widget, comparison_item, tz) {
    # Prepare payload2 and URL by using widget
    payload2 = list()
    payload2$locale = widget$request$locale[1]
    payload2$comparisonItem = widget$request$comparisonItem[[1]]
    payload2$resolution = widget$request$resolution[1]
    payload2$requestOptions$category = widget$request$requestOptions$category[1]
    payload2$requestOptions$backend = widget$request$requestOptions$backend[1]
    payload2$time = widget$request$time[1]
    payload2$requestOptions$property = widget$request$requestOptions$property[1]
    url <- URLencode(paste0(
      "https://www.google.com/trends/api/widgetdata/multiline/csv?req=",
      jsonlite::toJSON(payload2, auto_unbox = T),
      "&token=", widget$token[1],
      "&tz=0"
    ))
    
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
    # print(head(df)) check if the raw output is in corresponding UTC/GMT
    # Parse date in df, set all 'tz' equal to GMT
    if (unique(comparison_item$time) == "all") {
      df$date = anytime::anydate(df$date, tz = 'GMT',asUTC = TRUE)
    } else {
      df$date = anytime::anytime(df$date, tz = 'GMT',asUTC = TRUE)
      df$date = lubridate::with_tz(df$date,tz)
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
  function (keyword, geo = "US", time = "today+5-y", gprop = c("web", "news", "images", "froogle", "youtube"), category = 0, hl = "en-US", tz) {
    # check whether `gprop` matches any candidates
    gprop = match.arg(gprop, several.ok = FALSE)
    gprop = ifelse(gprop == "web", "", gprop)
    
    # Prepare args `comparison_item` and `widget`
    comparison_item = data.frame(keyword, 
                                 geo, 
                                 time, 
                                 stringsAsFactors = FALSE)
    widget = get_widget(comparison_item, 
                        category, 
                        gprop,
                        hl)
    
    # Fetch data
    interest_over_time = interest_over_time(widget, comparison_item, tz)
    return(interest_over_time)
  }


# Main fun ----------------------------------------------------------------

long_term_query_raw = 
  #
  # Pull long term minute-level data with consecutive 4-hour window
  # with 1 minute overlapping
  # need to adjust due to rescaling issue with Google Trends
  #
  function(start_date, end_date, keyword, geo = 'US', gprop = 'web', category = 0, hl = 'en-US', tz="America/Los_Angeles") {
    # require(parallel)
    # change date format
    start_date = strptime(
      start_date,
      format = "%Y-%m-%d %H:%M:%S", 
      tz = tz)
    
    end_date = strptime(
      end_date,
      format = "%Y-%m-%d %H:%M:%S", 
      tz = tz)
    
    # timezone in Google API is set to GMT, to confirm 
    start_date_GMT <- lubridate::with_tz(start_date, tzone = "GMT")
    end_date_GMT <- lubridate::with_tz(end_date, tzone = "GMT")
    
    # create time stamp sequence
    diff.time = difftime(end_date_GMT, start_date_GMT, units = 'hours')
    if(diff.time < 4) {
      time.stamp = seq(start_date_GMT, end_date_GMT, by = diff.time)
    } else {
      time.stamp = seq(start_date_GMT, end_date_GMT, by = '4 hour') 
    }
    
    # create time pair
    time.seq = paste(as.Date(time.stamp, 
                             tz= attributes(time.stamp)$tzone), 
                     lubridate::hour(time.stamp), 
                     sep='T')
    time.pair.1 = time.seq[-1]
    time.pair.2 = time.seq[-length(time.seq)]
    time.pair = paste(time.pair.2, time.pair.1)
    
    tmp.list <- vector('list', length = length(time.pair))
    tmp.list <- lapply(time.pair,
             function(x) {
               interest(keyword = keyword, 
                        geo = geo, 
                        time = x, 
                        gprop = gprop, 
                        category = category,
                        hl = hl,
                        tz)})
    # paralle querying data
    # cl = makeCluster(4)
    # clusterExport(cl, varlist = c('interest', 'get_widget', 'interest_over_time'), envir = globalenv())
    # clusterExport(cl, varlist = c('keyword', 'geo', 'gprop', 'category', 'hl'), envir = environment())
    
    # tmp.list = parLapply(cl, 
    #                      time.pair,
    #                      function(x) {
    #                        interest(keyword = keyword, 
    #                                 geo = geo, 
    #                                 time = x, 
    #                                 gprop = gprop, 
    #                                 category = category,
    #                                 hl = hl)
    #                      })
    # stopCluster(cl)
    
    # make result
    tmpdf = do.call(rbind, tmp.list)
    
    return(tmpdf)
    
  }



# test
df = long_term_query_raw('2017-01-01 00:00:00', '2017-01-02 01:00:00', 
                         keyword = c('zillow', 'trulia'), geo = c("US","CA"),tz="GMT")
attr(df$date,"tzone")

df2 = long_term_query_raw(start_date = '2018-07-10 18:21:00', end_date = '2018-07-10 22:04:00', 
                          keyword = c('groupon'), geo = c("US"), tz="America/Los_Angeles")
attr(df2$date,"tzone")  
