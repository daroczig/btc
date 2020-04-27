#' Forint formatter
#'
#' Takes a number and returns as a nicely formatted string.
#' @param x number
#' @return string
#' @export
#' @importFrom scales dollar
forint <- function(x) {
  dollar(x, prefix = '', suffix =' Ft')
}


#' Get BTC price
#' @param retried number of times previously retried (and failed) the API query
#' @return number
#' @export
#' @importFrom binancer binance_coins_prices
#' @importFrom checkmate assert_number
#' @importFrom logger log_info log_error
get_bitcoin_price <- function(retried = 0) {
  tryCatch({
    ## fall back to data.frame syntax not to load data.table
    btcusd <- subset(binance_coins_prices(), symbol == 'BTC')$usd
    assert_number(btcusd, lower = 3000, upper = 10000)
    log_info('Current price of a BTC is {dollar(btcusd)}')
    btcusd
  },
  error = function(e) {
    log_error(e$message)
    if (retried > 5) {
      stop('gave up')
    }
    Sys.sleep(1 + retried^2)
    get_bitcoin_price(retried = retried + 1)
  }
  )
}


#' Get USD/HUF rate
#' @return number
#' @importFrom httr GET content
#' @importFrom logger log_info
#' @export
get_usdhuf <- function() {
  usdhuf <- content(GET('https://api.exchangeratesapi.io/latest?base=USD'))$rates$HUF
  log_info('Current USD/HUF rate is {usdhuf}')
  usdhuf
}
