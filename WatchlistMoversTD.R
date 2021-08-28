require("reticulate");require("httr");require("rvest");require("jsonlite")
PASS <- new.env()
assign("apikey","**********************",envir = PASS)
assign("acctNum","*********************",envir = PASS)
callback = "https://127.0.0.1" # from TD App
request_GET <- function(x, url, ...) {
  x$response <- httr::GET(url, x$config, ..., handle = x$handle)
  x$html <- new.env(parent = emptyenv(), hash = FALSE)
  x$url <- x$response$url
  
  httr::warn_for_status(x$response)
  
  x
}
source_python("authTD.py")

getWatchlistIDs = function()
{
  url = paste0("https://api.tdameritrade.com/v1/accounts/",PASS$acctNum,"/watchlists")
  token = readRDS("token90.rds")
  token = access_token(refresh_token=token$refresh_token, client_id=PASS$apikey)
  
  btoken = paste0("Bearer ",token$access_token)
  pg = html_session(url) %>% suppressWarnings
  # get data by passing in url and cookies
  pg <- pg %>% request_GET(url,
                           config = httr::add_headers(`Authorization` = btoken))
  NAME <- fromJSON(rawToChar(pg$response$content))[[1]] %>% as.data.frame
  colnames(NAME) <- "Name"
  ID <- fromJSON(rawToChar(pg$response$content))[[2]] %>% as.data.frame
  colnames(ID) <- "ID"
  cbind(NAME,ID)
}

WATCHLISTS <- getWatchlistIDs()

getWatchlistTD = function(watchlistId)
{
  
  url = paste0("https://api.tdameritrade.com/v1/accounts/",PASS$acctNum,"/watchlists/",watchlistId)
  # data_raw = read_json(url,simplifyVector = TRUE)
  
  token = readRDS("token90.rds")
  token = access_token(refresh_token=token$refresh_token, client_id=PASS$apikey)
  
  btoken = paste0("Bearer ",token$access_token)
  pg = html_session(url) %>% suppressWarnings
  # get data by passing in url and cookies
  pg <- pg %>% request_GET(url,
                           config = httr::add_headers(`Authorization` = btoken))
  #data_raw <- httr::content(pg$response)
  data_raw <- fromJSON(rawToChar(pg$response$content))$watchlistItems
  data_raw
}
df <- getWatchlistTD("1418656402")
##************************************************************************************************
##                                            MOVERS
##************************************************************************************************
# Top 10 (up or down) movers by value or percent for a particular market
# https://developer.tdameritrade.com/movers/apis/get/marketdata/%7Bindex%7D/movers
# INDEX     -- $COMPX | $DJI | $SPX.X
# direction -- up/down
# change    -- percent/value
getMoversTD = function(INDX, direction, change)
{
  
  url = paste0("https://api.tdameritrade.com/v1/marketdata/",INDX,"/movers?",
               "apikey=",PASS$apikey,"&direction=",direction,"&change=",change)
  
  token = readRDS("token90.rds")
  token = access_token(refresh_token=token$refresh_token, client_id=PASS$apikey)
  
  btoken = paste0("Bearer ",token$access_token)
  pg = html_session(url) %>% suppressWarnings
  # get data by passing in url and cookies
  pg <- pg %>% request_GET(url, config = httr::add_headers(`Authorization` = btoken))
  #data_raw <- httr::content(pg$response)
  data_raw <- fromJSON(rawToChar(pg$response$content))
  data_raw$timeStamp <- Sys.time()
  data_raw$index   <- INDX
  data_raw
}

df = getMoversTD(INDX="$SPX.X", direction="down",change="value")
df = getMoversTD(INDX="$COMPX", direction="up",change="percent")
df = getMoversTD(INDX="$DJI", direction="down",change="percent")
