#' Title
#'
#' @param data 
#' @param max_mins_wait 
#' @param orderside_long 
#' @param tick_size 
#' @param point_value 
#' @param size 
#' @param trade_comm 
#'
#' @return
#' @export
#'
#' @examples
backtest = function(data=NULL, max_mins_wait=3, orderside_long=NA, tick_size=NA, point_value=NA, size=1, trade_comm=NA) {
  
  # create column needed
  data$Enter.Price = NA
  data$Enter.Date = NA
  data$Exit.Price = NA
  data$Exit.Date = NA
  data$Enter.Tape = NA
  data$Exit.Tape = NA
  
  # get backtest from cpp function
  out = bias_cpp(data = data, max_mins_wait = max_mins_wait, orderside_long = orderside_long)
  
  # align entry and exit
  out = out[!is.na(out$Enter.Tape) | !is.na(out$Exit.Tape), ]
  out$Enter.Date = as.POSIXct(as.integer(out$Enter.Date), origin="1970-01-01") # convert entry
  out$Exit.Date = dplyr::lead(as.POSIXct(as.integer(out$Exit.Date), origin="1970-01-01")) # convert exit and pull back
  out$Exit.Price = dplyr::lead(out$Exit.Price) # pull back info exit
  out$Exit.Tape = dplyr::lead(out$Exit.Tape) # pull back info exit
  out = na.omit(out)
  
  # calculate commissions and spread
  out$Size = size
  out$Commissions = 2 * out$Size * -trade_comm
  out$Spread = 0 
  out$Spread[out$Exit.Tape == 'EXIT.MKT'] = -(tick_size * point_value * out$Size)
  out$Spread[out$Enter.Tape == 'ENTER.MKT'] = out$Spread[out$Enter.Tape == 'ENTER.MKT'] - (tick_size * point_value * out$Size)
  
  # calculate gross profit
  if (orderside_long){
    out$Net.PL = (out$Exit.Price - out$Enter.Price) * (out$Size * point_value)
  } else {
    out$Net.PL = -(out$Exit.Price - out$Enter.Price) * (out$Size * point_value)
  }
  
  # calculate net profit
  out$Net.PL = out$Net.PL + out$Commissions + out$Spread
  
  # sort output
  out %<>% dplyr::rename(Start = Enter.Date, End = Exit.Date) %>%
    dplyr::select(Start, End, Enter.Price, Exit.Price, Enter.Tape, Exit.Tape,
                  Size, Commissions, Net.PL)
  
  return(out)
}


#' Riceve data.frame fillato con tutti i minuti mancanti:
#' date, open, high, low, close, year, month, mday, wday, day, hhmm, bday
#' 
#' Torna lo stesso con la colonna actions
#'   1 entrata LMT (default), 2 entrata MKT (sempre se sfrutta il gap di sessione compra MKT ultimo minuto giorno prima)
#'   -2 uscita MKT
#'   
#' @param data data.frame
#' @param params 
#'
#' @return data.frame
#' @export
#'
#' @examples
bias2signals = function(data=NULL, params=NULL) {
  enter = data %>%
    dplyr::filter(year %in% params$years, month %in% params$months, mday %in% params$mdays,
                  bday %in% params$bdays, wday %in% params$wdays, hhmm == dplyr::first(params$hhmms)) %>%
    dplyr::mutate(enter=dplyr::if_else(dplyr::first(unique(data$hhmm)) == dplyr::first(params$hhmms), 2, 1), exit=NA) %>% # entrata segnale settata a 1 (entrata LMT) se condizione FALSE, altrimenti 2 se TRUE (Gap)
    dplyr::select(date, enter, exit) 
  exit = data %>%
    dplyr::filter(year %in% params$years, month %in% params$months, mday %in% params$mdays,
                  bday %in% params$bdays, wday %in% params$wdays, hhmm == dplyr::last(params$hhmms)) %>%
    dplyr::mutate(exit=-2, enter=NA) %>% # uscita segnale settata a -2 (in quanto uscita MKT)
    dplyr::select(date, enter, exit) 
  
  out = dplyr::left_join(data, rbind(enter, exit)) %>%
    dplyr::select(date, open, high, low, close, enter, exit)
  out$enter = dplyr::lead(out$enter, n=1)
  out %<>% tidyr::replace_na(list(enter=0, exit=0)) %>%
    dplyr::mutate(actions=enter+exit) %>%
    dplyr::select(date, open, high, low, close, actions)
  
  if (dplyr::first(out$actions[out$actions != 0]) %in% c(-1, -2))
    out$actions[which(out$actions %in% c(-1, -2))[1]] = 0
  if (dplyr::last(out$actions[out$actions != 0]) %in% c(1, 2))
    out$actions[which(out$actions %in% c(1,2))[length(which(out$actions %in% c(1,2)))]] = 0
  
  values = rle(out[out$actions != 0, ]$actions)$values
  values[values>0] = 1
  values[values<0] = -1
  check = sum(values) == 0
  
  if (check)
    return (out)
  else
    return (NULL)
}
