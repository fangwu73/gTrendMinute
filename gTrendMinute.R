
# Do NOT source the whole script. Run through all the functions before tests, then run the test one by one
.pkgenv <- new.env(parent=emptyenv())

# Get Google API cookies handler ----

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

# Get widget ----

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


# Data over time ----

interest_over_time =
  #
  # Function to fetch data frame: insterest over time
  # pull the raw data in UTC/GMT
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
    url <- URLencode(paste0(
      "https://www.google.com/trends/api/widgetdata/multiline/csv?req=",
      jsonlite::toJSON(payload2, auto_unbox = T),
      "&token=", widget$token[1],
      "&tz=0" #UTC
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
    }
    
    return(df)
  
}

# Utility functions ----

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

naToZero = 
  #
  # NA/Inf to 0
  #
  function(x) {
    x[is.na(x)] <- 0
    x[!is.finite(x)] <- 0
    return(x)
  }

# Sub main fun ----

interest = 
  # 
  # Main function to query google trend data in terms of interest over time in GMT/UTC 
  # Arg:
  #    gprop: only accepts single value 
  #    time: e.g. "today+5-y", "2018-06-15T3 2018-06-15T7"
  # 
  function (keyword, geo = "US", time = "today+5-y", gprop = c("web", "news", "images", "froogle", "youtube"), category = 0, hl = "en-US") {
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
    interest_over_time = interest_over_time(widget, comparison_item)
    # interest_over_time$date = lubridate::with_tz(interest_over_time$date,tz)
    
    return(interest_over_time)
  }


# Consecutive pull ----

long_query_raw = 
  #
  # This function has more control than interest() with consecutive pulls with specified time windows
  # Pull long term minute-level data with consecutive 4-hour window with 1 minute overlapping
  # Pull consecutive 1 day/3 day/7 day window with corresponding resolution and overlapping records between windows
  # need to adjust due to rescaling issue with Google Trends
  # Arg:
  #    window: the time window you pull from Google Trends at once, the scale within this window is 0-100 and
  #            the resolution depends on the window length.
  #    tz: the timezone of start_date and end_date. e.g. "America/Los_Angeles". default "GMT".
  # Output:
  #    date: GMT/UTC
  #
  function(start_date, end_date, window = c('4h'), keyword, geo = 'US', gprop = 'web', category = 0, hl = 'en-US', tz="GMT") {
    # window = match.arg(arg = window, choices = c('4h','1d','3d','7d'), several.ok = FALSE)
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
    window.length <- dplyr::case_when(
      window =='4h' ~ '4 hour',
      window =='1d' ~ '24 hour',
      window =='3d' ~ '72 hour',
      window =='7d' ~ '168 hour',
      TRUE ~ 'NA')
    # create time stamp sequence
    diff.time = difftime(end_date_GMT, start_date_GMT, units = 'hours')
    if (diff.time < 4) {
      time.stamp = seq(start_date_GMT, end_date_GMT, by = diff.time)
      } else {
      time.stamp = seq(start_date_GMT, end_date_GMT, by = window.length) 
      } 
      # create time pair
      time.seq = paste(as.Date(time.stamp, 
                               tz= attributes(time.stamp)$tzone), 
                       lubridate::hour(time.stamp), 
                       sep='T')
      time.pair.1 = time.seq[-1]
      time.pair.2 = time.seq[-length(time.seq)]
      time.pair = paste(time.pair.2, time.pair.1)
      pb <- progress_estimated(length(time.pair))
  
      #tmp.list <- vector('list', length = length(time.pair))
      rounds <- (length(time.pair)-1)%/%60
      pb$tick()$print()
      start.list <- list(interest(keyword = keyword,
                             geo = geo,
                             time = time.pair[1],
                             gprop = gprop,
                             category = category,
                             hl = hl))
      if (rounds>=1){
        for(i in 1:rounds){
          pb$tick()$print()
          Sys.sleep(200)
          
          tmp.list <- vector('list', length = 60)
          # print(time.pair[((i-1)*60+2):(i*60+1)])
          tmp.list <- lapply(time.pair[((i-1)*60+2):(i*60+1)],
                                                  function(x) {
                                                    interest(keyword = keyword,
                                                             geo = geo,
                                                             time = x,
                                                             gprop = gprop,
                                                             category = category,
                                                             hl = hl)}
          )
          start.list <- c(start.list, tmp.list)
          
        }
      }
      pb$tick()$print()
      tmp.list <- lapply(time.pair[(rounds*60+2):length(time.pair)],
                                                        function(x) {
                                                          interest(keyword = keyword,
                                                                   geo = geo,
                                                                   time = x,
                                                                   gprop = gprop,
                                                                   category = category,
                                                                   hl = hl)}
      )
      start.list <- c(start.list, tmp.list)
      
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
    tmpdf = do.call(rbind, start.list)
    
    return(tmpdf)
    
  }

# Daily window minute level - expect to run once and save it for future rescaling with longer windows ----
daily_minute_gmt_raw = 
  #
  # Arg: tz is the timezone of start_date and end_date. e.g. "America/Los_Angeles". default "GMT". 
  # Steps:
  # 1. Pull long term minute-level data with consecutive 4-hour window with 1 minute overlapping
  # 2. Pull consecutive 1-day window with 1 record overlapping
  # 3. Rescale to get 1-day window minute level trend index
  # Need to adjust again due to rescaling procedure with Google Trends
  # Output: 
  #     date: UTC/GMT 
  #     raw hits and rescaled hits with 1 day window
  #
  function(start_date = '2018-01-01 00:00:00', end_date = '2018-08-01 00:00:00', keyword, 
           geo = 'US', gprop = 'web', category = 0, hl = 'en-US', tz = "GMT") {
    
    
    # timezone in Google API is set to GMT with tz=0 in the url 
    start_date_GMT <- as.character(lubridate::with_tz(as.POSIXct(start_date), tzone = "GMT"))
    end_date_GMT <- as.character(lubridate::with_tz(as.POSIXct(end_date), tzone = "GMT"))
    
    df_a <- long_query_raw(start_date = start_date_GMT, 
                           end_date = end_date_GMT, 
                           window = "4h",
                           keyword = keyword, 
                           geo = geo, 
                           gprop = gprop, 
                           category = category,
                           hl = hl, 
                           tz="GMT")
    df_b <- long_query_raw(start_date = start_date_GMT, 
                           end_date = end_date_GMT, 
                           window = "1d",
                           keyword = keyword, 
                           geo = geo, 
                           gprop = gprop, 
                           category = category,
                           hl = hl, 
                           tz="GMT")
    # df_a and df_b
    resolution_a <- df_a$date[3] - df_a$date[2]
    overlapping_a <- df_a %>% group_by(date, keyword, geo, gprop, category) %>% summarise(n=n()) %>% filter(n >1) %>% 
      select(date, keyword, geo, gprop, category) %>% arrange(keyword, geo, gprop, category,date) %>% as.data.frame()
    window_length_a <- ifelse(difftime(overlapping_a[2,1], overlapping_a[1,1], units = "hour") <= 24, 
                              paste0(difftime(overlapping_a[2,1], overlapping_a[1,1], units = "hour"), "hours"),
                              paste0(difftime(overlapping_a[2,1], overlapping_a[1,1], units = "day"), "days"))
    resolution_b <- df_b$date[3] - df_b$date[2]
    stopifnot(resolution_b > resolution_a)
    if (is.na(window_length_a)) {
      print("no need to rescale since the data is pulled from one window")
      outputdf <- df_a
    } else {
      df_a_windows <- df_a %>% group_by(date, keyword, geo, gprop, category) %>% filter(duplicated(date) | n() == 1) %>% 
        mutate(windows = lubridate::floor_date(date, unit = window_length_a))
      
      df_b_windows <- df_b %>% group_by(date, keyword, geo, gprop, category) %>% filter(duplicated(date) | n() == 1) %>% 
        mutate(windows = lubridate::floor_date(date, unit = window_length_a))
      scale_index <- inner_join(df_a_windows %>% group_by(keyword, geo, gprop, category, windows) %>% 
                                  summarise(hits.avg.a = mean(hits, na.rm = TRUE)), 
                                df_b_windows %>% group_by(keyword, geo, gprop, category, windows) %>% 
                                  summarise(hits.avg.b = mean(hits, na.rm = TRUE))) %>% 
        mutate(scale.index = naToZero(hits.avg.b/hits.avg.a))
      outputdf <- df_a_windows %>% left_join(scale_index, by = c('keyword', 'geo', 'gprop', 'category', "windows")) %>%
        mutate(hits.scaled = hits*scale.index) %>% as.data.frame() 
    }
    return(outputdf)
  }

# Final rescale from daily window to any longer window -  expected to run it frequently ----
minute_gmt_rescale =
  #
  # Final rescale from daily window to any longer window
  #
  function(daily_minute_gmt_rawdf, hl='en-US') {
    keywords <- unique(daily_minute_gmt_rawdf$keyword)
    geos <- unique(daily_minute_gmt_rawdf$geo)
    timespan <- paste(strptime(min(daily_minute_gmt_rawdf$date), format = "%Y-%m-%d", tz = "GMT"), 
                      strptime(max(daily_minute_gmt_rawdf$date), format = "%Y-%m-%d", tz = "GMT"))
    gprops <- unique(daily_minute_gmt_rawdf$gprop)
    categories <- unique(daily_minute_gmt_rawdf$category)
    
    df_b <- interest(keyword = keywords, geo = geos, time = timespan, gprop = gprops, category = categories, hl = hl)
    # plot(df_candidco_real$hits,type="l")
    
    resolution_a <- daily_minute_gmt_rawdf$date[3] - daily_minute_gmt_rawdf$date[2]
    window_length_a <-'1day'
    resolution_b <- df_b$date[3] - df_b$date[2]
    stopifnot(resolution_b > resolution_a)
    
    df_a_windows <- daily_minute_gmt_rawdf %>% group_by(date, keyword, geo, gprop, category) %>% filter(duplicated(date) | n()==1) %>%
      mutate(windows =lubridate::floor_date(date, unit = window_length_a))
    df_b_windows <- df_b %>% group_by(date, keyword, geo, gprop, category) %>% filter(duplicated(date) | n()==1) %>%
      mutate(windows = lubridate::floor_date(date, unit = window_length_a))
    
    scale_index_2 <- inner_join(df_a_windows %>% group_by(keyword, geo, gprop, category, windows) %>% summarise(hits.avg.scaled.a = mean(hits.scaled)),
                                df_b_windows %>% group_by(keyword, geo, gprop, category, windows) %>% summarise(hits.avg.b.2 = mean(hits))) %>%
      mutate(scale.index.2 = naToZero(hits.avg.b.2/hits.avg.scaled.a)) 
    outputdf_2 <- df_a_windows %>% left_join(scale_index_2, by = c('keyword', 'geo', 'gprop', 'category',"windows")) %>% 
      mutate(hits.scaled.2 = hits.scaled*scale.index.2) %>% as.data.frame()
    return(outputdf_2)
  }



# Test 1 ----
df_candid_co <- daily_minute_gmt_raw(start_date = '2018-04-23 00:00:00', 
                                     end_date = '2018-05-02 00:00:00', 
                                     keyword = "candid co", 
                                     geo = 'US', 
                                     gprop = 'web', 
                                     category = 0, 
                                     hl = 'en-US', 
                                     tz = "GMT")
# daily_minute_gmt_rawdf <- df_candid_co

finaldf <- minute_gmt_rescale(daily_minute_gmt_rawdf = df_candid_co)
plot(finaldf$hits.scaled.2, type = "l")
lines(1:length(finaldf$hits.scaled), finaldf$hits.scaled, col = "blue")
lines(1:length(finaldf$hits.avg.b.2), finaldf$hits.avg.b.2, col = "red")

# Test 2 ----
# multiple keywords, geo and a different timezone
df = long_query_raw('2017-01-01 00:00:00', '2017-01-02 01:00:00', 
                    keyword = c('zillow', 'trulia'), geo = c("US","CA"), tz = "America/Los_Angeles")
attr(df$date,"tzone")

# Test 3 ----
# runtime tests
a <- Sys.time()
df_1 <- long_query_raw(start_date = '2018-04-23 00:00:00', end_date = '2018-05-02 00:00:00', 
                     window="4h",
                     keyword = c('23andme'), geo = c("US"), tz = "GMT")
b <- Sys.time()
b-a
attr(df_1$date, "tzone")  
plot(df_1$hits, type = "l")

# Test 4 ----
# 1 day window raw data
a <- Sys.time()
df_2 <- long_query_raw(start_date = '2018-01-01 00:00:00', 
                       end_date = '2018-08-01 00:00:00', 
                       window ="1d",
                       keyword = c('groupon'), geo = c("US"))
b <- Sys.time()
b-a
plot(df_2$hits,type="l")

# Test 5 ----
# 3 day window raw data and a different timezone
df_4 = long_query_raw(start_date = '2018-06-01 00:00:00', 
                      end_date = '2018-06-10 00:00:00', 
                      window = "3d",
                      keyword = c('groupon'), geo = c("US"), tz = "America/Los_Angeles")
plot(df_4$hits,type = "l")
tail(df_4)

# Test 6 ----
# output: minute level in GMT/UTC 
daytest <- interest(keyword = "groupon", 
                    geo = "US", 
                    time = "2018-06-15T3 2018-06-15T7" , 
                    gprop = 'web', category = 0, hl = 'en-US')
head(daytest)

# Test 7 ----
# output: hour level in GMT/UTC 
daytest_1 <- interest(keyword = "groupon", geo = "US", time = "2018-06-15T3 2018-06-18T7", 
                      gprop = 'web', category = 0, hl = 'en-US')
attr(daytest$date,"tzone") 

# Test 8 ----
# output: daily level in GMT/UTC 
daytest_2 <- interest(keyword = c('groupon'), geo = "US", 
                      time = "2018-06-01 2018-06-15", 
                      gprop = c("web"), category = 0, hl = "en-US") 
plot(daytest_2$hits,type="l")

