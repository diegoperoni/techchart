#' Title
#'
#' @param data 
#' @param max_mins_wait 
#' @param orderside_long 
#' @param tick_size 
#' @param point_value 
#' @param size 
#' @param trade_comm 
#' @param mkt_slippage_ticks
#' @param delta_shift_hours
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
backtest = function(data=NULL, max_mins_wait=3, orderside_long=NA, tick_size=NA, point_value=NA, size=1, trade_comm=NA, mkt_slippage_ticks=1, delta_shift_hours=0, verbose=FALSE) {
  
  start = Sys.time()
  
  data$Enter.Price = NA
  data$Enter.Date = NA
  data$Exit.Price = NA
  data$Exit.Date = NA
  data$Enter.Tape = NA
  data$Exit.Tape = NA
  data$True.Price = NA
  
  out = backtest_cpp(data=data, max_mins_wait=max_mins_wait, orderside_long=orderside_long)
  
  # align entry and exit
  out = out[!is.na(out$Enter.Tape) | !is.na(out$Exit.Tape), ]

  exchange_tz = lubridate::tz(data$date) # il tz e' sempre quello di exchange ma orario e' virtualmente shiftato in avanti
  out$Enter.Date = as.POSIXct((as.integer(out$Enter.Date) - 60*60*delta_shift_hours), tz=exchange_tz, origin='1970-01-01') # convert entry
  out$Exit.Date = dplyr::lead(as.POSIXct((as.integer(out$Exit.Date) - 60*60*delta_shift_hours), tz=exchange_tz, origin='1970-01-01')) # convert exit and pull back
  
  out$Exit.Price = dplyr::lead(out$Exit.Price) # pull back info exit
  out$Exit.Tape = dplyr::lead(out$Exit.Tape) # pull back info exit
  out = tibble::as_tibble(stats::na.omit(out)) # evita se vuoto poi vada in errore
  
  # calculate commissions and splippage
  out$Size = size
  out$Commissions = - 2 * out$Size * trade_comm
  out$Slippage = 0 
  out$Slippage[out$Exit.Tape=='EXIT.MKT'] = - (mkt_slippage_ticks * tick_size * point_value * out$Size)
  out$Slippage[out$Enter.Tape=='ENTER.MKT'] = out$Slippage[out$Enter.Tape=='ENTER.MKT'] - (mkt_slippage_ticks * tick_size * point_value * out$Size)
  
  # calculate gross profit
  if (orderside_long)
    out$Net.PL = (out$Exit.Price - out$Enter.Price) * out$Size * point_value
  else
    out$Net.PL = - (out$Exit.Price - out$Enter.Price) * out$Size * point_value
  
  # calculate net profit and net value
  out$Net.PL = out$Net.PL + out$Commissions + out$Slippage
  out$Net.Value = out$True.Price * (out$Size * point_value)

  processing_time = paste0('(', round(difftime(Sys.time(), start, units='secs'), 1), ' secs)')
  if (verbose)
    print(paste('backtest...', processing_time, ' N.Trades', nrow(out), ' Net.PL', sum(out$Net.PL)))
  
  out %>% dplyr::select(Start=Enter.Date, End=Exit.Date, Enter.Price, Exit.Price, Enter.Tape, Exit.Tape, Size, Commissions, Slippage, Net.PL, Net.Value)
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
#' @param enter_mkt
#' @param verbose
#'
#' @return data.frame
#' @export
#'
#' @examples
bias2signals = function(data=NULL, params=NULL, enter_mkt=FALSE, verbose=FALSE) {
  start = Sys.time()
  
  lead_enter_bars = 1
  #signal_at_first_hhmm = dplyr::first(unique(data$hhmm)) == dplyr::first(params$hhmms) # force ENTER.MKT signal (session_tz)
  signal_at_first_hhmm = params$hhmms_exchange_tz[1] == params$hhmms_range[1] # force ENTER.MKT signal (exchange_tz)

  if (signal_at_first_hhmm && !params$hhmm_prev_day=='') { # ho indicato un orario di ingresso del giorno prima
    lead_enter_bars = abs(match(params$hhmm_prev_day, params$hhmms_exchange_tz) - length(params$hhmms_exchange_tz)) + 2 # tutto in exchange_tz
    signal_at_first_hhmm = FALSE
  }
  
  enter = data %>%
    dplyr::filter(year %in% !!params$years, month %in% !!params$months, mday %in% !!params$mdays,
                  bday %in% !!params$bdays, wday %in% !!params$wdays, hhmm == dplyr::first(!!params$hhmms)) %>%
    dplyr::mutate(enter = if (!!enter_mkt || !!signal_at_first_hhmm) 2 else 1, exit=NA) %>% # entrata segnale settata a 1 (entrata LMT) se condizione FALSE, altrimenti 2 se TRUE (Gap)
    dplyr::select(date, enter, exit)
  
  exit = data %>%
    dplyr::filter(year %in% !!params$years, month %in% !!params$months, mday %in% !!params$mdays,
                  bday %in% !!params$bdays, wday %in% !!params$wdays, hhmm == dplyr::last(!!params$hhmms)) %>%
    dplyr::mutate(exit=-2, enter=NA) %>% # uscita segnale settata a -2 (in quanto uscita MKT)
    dplyr::select(date, enter, exit) 
  
  out = dplyr::left_join(data, rbind(enter, exit), by='date') %>%
    dplyr::select(date, open, high, low, close, true.close, enter, exit)
  
  out$enter = dplyr::lead(out$enter, n=lead_enter_bars) # shitfo minimo 1 minuto (o piu con hhmms_exchange_tz) indietro l'ingresso

  out %<>% tidyr::replace_na(list(enter=0, exit=0)) %>%
    dplyr::mutate(actions=enter+exit) %>%
    dplyr::select(date, open, high, low, close, true.close, actions)
  
  if (dplyr::first(out$actions[out$actions != 0]) %in% c(-1, -2))
    out$actions[which(out$actions %in% c(-1, -2))[1]] = 0
  if (dplyr::last(out$actions[out$actions != 0]) %in% c(1, 2))
    out$actions[which(out$actions %in% c(1,2))[length(which(out$actions %in% c(1,2)))]] = 0
  
  values = rle(out[out$actions != 0, ]$actions)$values
  values[values>0] = 1
  values[values<0] = -1
  check = sum(values) == 0
  
  processing_time = paste0('(', round(difftime(Sys.time(), start, units='secs'), 1), ' secs)')
  if (verbose)
    print(paste("bias2signals...", processing_time))
  
  if (check)
    return (out)
  else
    return (NULL)
}

#' Prepare asset xts for backtest C++
#'
#' @param asset xts
#' @param ticker
#' @param delta_shift_hours 
#' @param fix.TS.bias 
#' @verbose
#'
#' @return data.frame
#' @export
#'
#' @examples
asset4Backtest = function(asset=NULL, ticker=NULL, delta_shift_hours=0, fix.TS.bias=FALSE, verbose=FALSE) {
  start = Sys.time()
  if (fix.TS.bias)
    asset = xts::shift.time(asset, n=60*(-1)) 
  asset = asset[, c('open', 'high', 'low', 'close', 'true.close')]
  asset = xts::shift.time(asset, n=60*60*delta_shift_hours)
  exchange_tz = lubridate::tz(zoo::index(asset))
  asset = data.frame(zoo::coredata(asset), date=zoo::index(asset))
  asset$day = format(asset$date, '%Y-%m-%d')
  asset$hhmm = format(asset$date, '%H:%M')
  hhmm = sort(unique(asset$hhmm))
  if (ticker %in% c('ES', 'NQ', 'RTY', 'YM'))
    hhmm = setdiff(hhmm, c('22:16', '22:17', '22:18', '22:19', '22:20', '22:21', '22:22', '22:23', '22:24', '22:25', '22:26', '22:27', '22:28', '22:29'))
  combinations = expand.grid(hhmm, unique(asset$day)) %>% setNames(c('hhmm', 'day'))
  asset = dplyr::left_join(combinations, asset, by=c('hhmm', 'day')) %>% 
    tidyr::fill(dplyr::everything(), .direction='downup') %>% 
    dplyr::select(hhmm, day, open, high, low, close, true.close)
  
  #pos = lubridate::ymd_hm(paste(asset$day, asset$hhmm), tz='UTC') # sempre UTC!!!
  pos = lubridate::ymd_hm(paste(asset$day, asset$hhmm), tz=exchange_tz) # DEVO RIPRISTINARE IL TZONE ORIGINALE ANCHE SE SHIFTATO IN AVANTI
  
  data = xts::xts(asset[, 3:ncol(asset)], pos)
  data$year  = xts::.indexyear(data) + 1900
  data$month = xts::.indexmon(data)
  data$mday  = xts::.indexmday(data)
  data$wday  = xts::.indexwday(data)
  data = data.frame(zoo::coredata(data), date=stats::time(data))
  data$day = asset$day
  data$hhmm = asset$hhmm
  # bdays = data %>% dplyr::group_by(year, month, mday) %>% dplyr::filter(dplyr::row_number()==1) %>% dplyr::ungroup() %>%
  #   dplyr::group_by(year, month) %>% dplyr::mutate(bday=dplyr::row_number()) %>% dplyr::ungroup() %>% dplyr::select(day, bday)
  bdays = quantF::calculateBusinessDays(dplyr::last(data$date)) %>%
    dplyr::mutate(day=format(date, '%Y-%m-%d')) %>% dplyr::select(-date)
  data %<>% dplyr::left_join(bdays[, c('day', 'bday')], by='day') %>% 
    dplyr::mutate(year=as.integer(year), month=as.integer(month), mday=as.integer(mday), wday=as.integer(wday), bday=as.integer(bday))
  if (min(data$low, na.rm=TRUE)<=0) {
    delta_2_zero = abs(min(data$low, na.rm=TRUE)) * 2
    print(paste('WARNING: Negative Values found on Adjusted Data: adjusting values to Reach All Positives Values...'))
    data %<>% dplyr::mutate(open = open + !!delta_2_zero, high = high + !!delta_2_zero, low = low + !!delta_2_zero, close = close + !!delta_2_zero)
  }
  
  processing_time = paste0('(', round(difftime(Sys.time(), start, units='secs'), 1), ' secs)')
  if (verbose)
    print(paste("asset4Backtest...", processing_time))
  
  data
}
