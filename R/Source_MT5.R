#' Account information
#'
#' @description
#' This function is useful for obtaining MT5's account information.
#'
#' @return
#' \emph{Data.frame} \eqn{[1x11]} is returned, with follow informations:
#' \itemize{
#'   \item Company \code{{character}}: company name.
#'   \item Acc_server \code{{character}}: server name that your MT5 is connected to.
#'   \item Acc_number \code{{int}}: is the account login as well.
#'   \item Balance \code{{numeric}}: same used in MT5 Trade tab.
#'   \item Equity  \code{{numeric}}: is the sum of Balance and current Profit.
#'   \item Profit \code{{numeric}}: sum of all open profit/loss positions.
#'   \item Credit \code{{numeric}}: same used in MT5 Trade tab.
#'   \item Margin \code{{numeric}}: is the amount of money necessary to cover your possible losses during margin trading.
#'   \item Margin_Free \code{{numeric}}: is the amount availabe to open next trades. Free margin equals equity minus margin.
#'   \item {Account_type \code{{int}}:
#'          \itemize{
#'                  \item{0: ACCOUNT_TRADE_MODE_REAL (real account)}
#'                  \item{1: ACCOUNT_TRADE_MODE_DEMO (demo account)}
#'                  \item{2: ACCOUNT_TRADE_MODE_CONTEST}
#'                  }
#'          }
#'   \item {Margin_type \code{{int}}:
#'          \itemize{
#'                  \item{0: ACCOUNT_MARGIN_MODE_RETAIL_NETTING (Netting)}
#'                  \item{1: ACCOUNT_MARGIN_MODE_EXCHANGE (Exchange)}
#'                  \item{2: ACCOUNT_MARGIN_MODE_RETAIL_HEDGING (Hedging)}
#'                  }
#'          }
#' }
#'
#' @seealso
#' [mt5R::MT5.ShowPositions()], [mt5R::MT5.ShowOrders()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @references
#' \url{https://www.mql5.com/en/docs/constants/environment_state/accountinformation}
#'
#' \url{https://hercules.finance/faq/what-is-balance-equity-margin-and-free-margin-and-margin-call/}
MT5.AccountInfo <- function()
{
  sRequest <- utils::read.table(text = MT5.Connect("O0"), sep = "?", colClasses = c("character"))
  if(base::length(sRequest) == 11)
  {
    colnames(sRequest) <- c("Company", "Acc_server", "Acc_number", "Balance", "Equity", "Profit",
                           "Credit", "Margin", "Margin_Free", "Account_type", "Margin_type")
    sRequest[1:2] <- as.character(sRequest[1:2])
    sRequest[4:9] <- as.numeric(sRequest[4:9])
    sRequest[c(3,10,11)] <- as.integer(sRequest[c(3,10,11)])
  }

  return(sRequest)
}


#' MT5GetClosedEntries
#' @description get the list of closed entries from the MT5 History tab
#' @details The Package MT5R does not include the functionality to extract the history data from Metatrader5. this function provides so.
#'
#' @export
#'
#' @examples
#' MT5GetClosedEntries ()
MT5GetClosedEntries <- function()
{
  sRequest <- MT5.Connect("H0")
  df_final <- data.frame(sSymbol = character(),
                         PositionID = integer(),
                         iCmd = integer(),
                         fVolume = numeric(),
                         fProfit = numeric(),
                         Ticket = integer(),
                         Date = character())
  if (is.null(sRequest)) {
    return(df_final)
  }
  for (i in seq_len(base::length(sRequest))) {
    Linha <- utils::read.table(text = sRequest[i], sep = "?")
    df <- data.frame(sSymbol = as.character(Linha[1][[1]]),
                     PositionID = as.integer(Linha[2][[1]]),
                     iCmd = as.integer(Linha[3][[1]]),
                     fVolume = as.numeric(Linha[4][[1]]),
                     fProfit = as.numeric(Linha[5][[1]]),
                     Ticket  = as.numeric(Linha[6][[1]]),
                     Date = as.character(Linha[7][[1]]))
    df_final <- rbind(df_final, df)
  }
  if (base::dim(df_final)[1] > 0) {
    df_final$sSymbol <- as.character(df_final$sSymbol)
  }
  return(df_final)
}


#' Order book
#'
#' @description
#' This function is for obtaining Bid or Ask order book with specified book depth.
#'
#' @param sSymbol character; target symbol
#' @param iBidAsk int; if \code{iBidAsk = 0} Bid is given, \code{iBidAsk = 1} Ask is given (default \code{0}).
#' @param iBookDepth int; book depth (default \code{5})
#'
#' @return
#' \emph{Data.frame} \eqn{[nx2]} is returned, with follow informations:
#' \itemize{
#'   \item fPrice \code{{numeric}}: price
#'   \item Volume \code{{int}}: bid/ask grouped by price
#' }
#'
#' (Stocks market) If its returns a line with \code{fPrices = 0} the market is in auction. Check references.
#'
#' (Stocks market) In auctions this function cannot reflect the book properly.
#'
#' @seealso
#' [mt5R::MT5.BidAskSpread()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @references
#' \url{https://www.investopedia.com/terms/o/order-book.asp}
#' \url{https://www.investopedia.com/terms/a/auctionmarket.asp}
MT5.BidAskBook <- function(sSymbol, iBidAsk = 0, iBookDepth = 5)
{
  sSymbol <- as.character(sSymbol)
  iBidAsk <- as.integer(iBidAsk)
  iBookDepth <- as.integer(iBookDepth)

  iBookDepth <- ifelse(iBookDepth < 1, 1, iBookDepth)

  if(iBidAsk != 0 &
     iBidAsk != 1)
  {
    stop("Error: iBidAsk should be 0 or 1!")
  }

  sRequest <- MT5.Connect(base::paste0("O4 ", paste(
    sSymbol, iBidAsk, iBookDepth,
    sep = "?")))

  sRequest <- utils::read.table(text = sRequest, sep = "?")
  if(as.character(sRequest[1][[1]]) != "O4OK")
  {
    stop("Error: book was not accessible")
  }

  if(as.character(sRequest[3][[1]]) == "O4ERROR")
  {
    return(data.frame(fPrice = as.numeric(),
                      Volume = as.integer()))
  }

  sRequest <- sRequest[3:(base::length((sRequest))-1)]
  df <- data.frame(fPrice = as.numeric(sRequest[seq(1,base::length(sRequest),2)]),
                  Volume = as.integer(sRequest[seq(2,base::length(sRequest),2)]))
  if(any(df$fPrice == 0))warning("This symbol can be in auction. \n Check ?MT5.BidAskBook");
  return(df)
}

#' Order book Spread
#'
#' @description
#' This function is for obtaining the higher bid, the lower ask and the spread between them.
#'
#'
#' @param sSymbol character; target symbol.
#'
#' @return
#' \emph{Data.frame} \eqn{[1x4]} is returned, with follow informations:
#' \itemize{
#'   \item sSymbol \code{{character}}: symbol used.
#'   \item fBid \code{{numeric}}: higher bid.
#'   \item fAsk \code{{numeric}}: lower ask.
#'   \item Spread \code{{numeric}}: difference between lower ask and higher bid.
#' }
#'
#' @details
#' For same result, use \code{MT5.BidAskBook()} and calculate the difference between the best bid and ask.
#'
#' For Forex brokers this function will return an empty \code{data.frame}.
#'
#' @seealso
#' [mt5R::MT5.BidAskBook()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @references
#' \url{https://www.investopedia.com/terms/o/order-book.asp}
MT5.BidAskSpread <- function(sSymbol)
{
  stopifnot(is.character(sSymbol))
  sRequest <- MT5.Connect(mt5R::SUP_MT5_StackRequests(sSymbol, "O5"))

  df_final <- data.frame(sSymbol = character(),
                         fBid = numeric(),
                         fAsk = numeric())

  for(i in seq_len(base::length(sRequest)))
  {
    Vet <- utils::read.table(text = sRequest[i], sep = "?", stringsAsFactors = F)

    if(is.na(Vet[3][[1]]))
    {
      df <- data.frame(sSymbol = Vet[2][[1]], fBid = NA, fAsk = NA)
    }else
    {
      df <- data.frame(sSymbol = as.character(Vet[2][[1]]), fBid = as.numeric(Vet[3][[1]]), fAsk = as.numeric(Vet[4][[1]]))
    }

    df_final <- rbind(df_final, df)
  }
  if(base::dim(df_final)[1] > 0)
  {
    df_final$Spread <- df_final$fAsk - df_final$fBid
  }

  if(df_final$Spread<0)warning("This symbol can be in auction. \n Check ?MT5.BidAskBook");
  return(df_final)
}

#' Change socket door
#'
#' @description
#' This function is for change the socket door used in mt5R plataform in MT5. Need to be used every time when the door is changed from default \code{2345}.
#'
#' @param iDoor int; new door. If \code{iDoor} is not defined, the door is defined to default \code{2345}.
#'
#' @return
#' Always returns \code{TRUE}.
#'
#' @seealso
#' [mt5R::MT5.Connect()]
#'
#' @details
#' The default door is \code{2345}.
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
MT5.ChangeDoorSocket <- function(iDoor = NULL)
{
  if(is.null(iDoor))
  {
    if(exists("MT5_GLOBALVARIABLE_PORT", envir = .GlobalEnv))
    {
      rm("MT5_GLOBALVARIABLE_PORT", envir = .GlobalEnv)
      base::print("mt5R now is using default door!")
    }else
    {
      base::print("mt5R is already using default door!")
    }
  }else
  {
    assign("MT5_GLOBALVARIABLE_PORT", value = iDoor, envir = .GlobalEnv)
    base::print("mt5R door changed!")
  }

  return(TRUE)
}

#' Close opened position
#'
#' @description
#' Send a request to close positions in MT5.
#'
#' @param iTickers int(); vector of tickets positions to close.
#'
#' @return
#' Returns \code{TRUE} if position was successful closed.
#'
#' @seealso
#' [mt5R::MT5.SingleOrder()], [mt5R::MT5.MultipleOrders()], [mt5R::MT5.ShowPositions()], [mt5R::MT5.ShowOrders()], [mt5R::MT5.ModifyOrder()], [mt5R::MT5.DeleteOrder()], [mt5R::MT5.ModifyPosition()]
#'
#' @details
#' For iTickers users can use [mt5R::MT5.ShowPositions()] to obtain it.
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' # Close position 732176419
#' MT5.ClosePosition(732176419)
#'
#' }
MT5.ClosePosition <- function(iTickers)
{
  if(length(iTickers)<1)stop("Error: iTickers length not positive?");
  iTickers <- as.integer(iTickers)
  sTextToSend <- mt5R::SUP_MT5_StackRequests(iTickers, "O9")
  sRequest <- utils::read.table(text = MT5.Connect(sTextToSend), sep = ";", colClasses = c("character"))

  return(as.logical(unlist(sRequest) == "O9OK"))
}

#' Socket connection with MT5
#'
#' @description
#' The main function of mt5R, its make the connection by socket with MT5. Used in all mt5R functions.
#'
#' This function was only viable by JC's package (Socket library for MT4 and MT5). See reference.
#'
#' @param sReq character; requisition sent to MT5.
#' @param iPort int; port used to create socket connection. (default \code{23456})
#' @param bMsg bool; if \code{TRUE} it will print all return messages from MT5. (default \code{FALSE})
#' @param timeout int; timeout used in \code{\link[base:connections]{base::socketConnection()}}. (default \code{60})
#'
#' @return
#' Returns \code{character}, it will be entirely dependent of the function of the other side in the mt5R package running in MT5. Error due connection will be returned \code{NULL}.
#'
#' @details
#' It uses <socket-library-mt4-mt5.mqh> library.
#'
#' This function still has some experiments concepts, especially with delayed functions that MT5 requires time to run.
#'
#' @seealso
#' [mt5R::MT5.ChangeDoorSocket()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @references
#' \url{https://www.mql5.com/en/blogs/post/706665}
MT5.Connect <- function(sReq, iPort = 23456, bMsg = FALSE, timeout = getOption("timeout"))
{
  if(sReq == "")return(NULL)
  server_resp <- ""
  bError <- TRUE

  if(exists("MT5_GLOBALVARIABLE_PORT")) iPort <- as.integer(MT5_GLOBALVARIABLE_PORT)


  try({
    ## TryCatch do not work here for some unknown reason. Using Try instead
    con <- base::socketConnection(host="localhost", port = iPort, blocking=TRUE,
                                         server=FALSE, open="r+", timeout = timeout)
    bError <- F
  })

  if(!bError)
  {
    on.exit(close(con))

    write_resp <- base::writeLines(base::paste0(sReq, "\r\n"), con)
    server_resp <- base::readLines(con, 1)
  }

  if(base::length(server_resp) == 0 & base::substr(sReq, 1, 2) == "C3")
  {
    ## Experimental
    return("C3OK")
  }
  if(bError) return(NULL)
  if(bMsg) base::print(paste("MT5: '", server_resp,"'"))

  # iLen <- nchar(server_resp)
  # server_resp <- substr(server_resp, 1, iLen - 1)
  if(server_resp == "") return (NULL)

  ## Everything is fine, continue
  sTexto <- utils::read.table(text = server_resp, sep = "@", skipNul = TRUE, quote= "", fill = TRUE)
  sTexto <- sTexto[!is.na(sTexto)]
  return(sTexto)
}

#' Delete pending order
#'
#' @description
#' Send a request to delete orders in MT5.
#'
#' @param int int(); vector of tickets orders to delete.
#'
#' @return
#' Returns \code{TRUE} if order was successful deleted, \code{FALSE} otherwise.
#'
#' @details
#' For iTickers users can use [mt5R::MT5.ShowOrders()] to obtain it.
#'
#' (STOCKS ONLY) WARNING - Poor written MT5's servers
#'
#' I had some problems running this function in some brokers and doesn't in others.
#' All brokers that are winged by XP Investimentos (BR broker), the MT5's server try to close another nonexistent order (order #0)
#' The same error don't occurs in Modal (BR broker) server. In all cases, the order is deleted anyway.
#' Use this function very carefully if you notice that the MT5's server broker was poor written.
#' I already tried to fix into MQL code in every way. All forms to remove the order result in the same outcome.
#' For who is experiencing this problem, take note that the mt5R plataform takes approximately 3 minutes to recover itself.
#' Poor fix: the trader can use [mt5R::MT5.ModifyOrder()] to change fPrice to unviable levels and waits to order expire.
#'
#' @seealso
#' [mt5R::MT5.SingleOrder()], [mt5R::MT5.MultipleOrders()], [mt5R::MT5.ShowPositions()], [mt5R::MT5.ShowOrders()], [mt5R::MT5.ModifyOrder()], [mt5R::MT5.ClosePosition()], [mt5R::MT5.ModifyPosition()]
#'
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' # Delete pending order 832136429
#' MT5.DeleteOrder(832136429)
#'
#' }
MT5.DeleteOrder <- function(iTickets)
{
  sTextToSent <- mt5R::SUP_MT5_StackRequests(iTickets, "O7")
  return(as.logical(unlist(MT5.Connect(sTextToSent)) == "O7OK"))
}

#' Draw horizontal line
#'
#' @description
#' Draw horizontal line into MT5 chart(s).
#'
#' @param sSymbol character; target symbol.
#' @param fPrice numeric; at which price will be draw the line.
#' @param iWidth int; which width will be draw the line (default \code{1}).
#' @param iColor int; See details (default \code{1}: \code{clrGreen}).
#' @param sName character; unique name if you want further modifications in lines parameters.
#' @param iTF int; its always in minutes. If \code{iTF = 0} it will be draw on all time frames (default \code{0}). See details.
#'
#' @return
#' Returns \code{TRUE} if the line was drawn, \code{FALSE} otherwise.
#'
#' @details
#' Supported colors (\code{iColor}). See url below for even more details.
#' \itemize{
#' \item{1: clrGreen (GREEN)}
#' \item{2: clrBlue (BLUE)}
#' \item{3: clrRed (RED)}
#' \item{4: clrNavy	}
#' \item{5: clrPurple	}
#' \item{6: clrIndigo	}
#' \item{7: clrOliveDrab	}
#' \item{8: clrDarkSlateBlue	}
#' \item{9: clrLawnGreen	}
#' \item{10: clrOrangeRed	}
#' \item{11: clrGold	}
#' \item{12: clrYellow	}
#' \item{13: clrAqua	}
#' \item{14: clrMagenta	}
#' \item{15: clrLightSlateGray	}
#' \item{16: clrPaleVioletRed	}
#' \item{17: clrHotPink	}
#' \item{18: clrKhaki	}
#' \item{19: clrSilver	}
#' \item{20: clrLightGray	}
#' \item{21: clrKhaki	}
#' \item{22: clrBeige	}
#' }
#' \url{https://www.mql5.com/en/docs/constants/objectconstants/webcolors}
#'
#'
#' Supported time frames (\code{iTF}). See url below for even more details.
#' \itemize{
#' \item{0: all	(it will be draw on all time frames)}
#' \item{1: \code{PERIOD_M1}	}
#' \item{2: \code{PERIOD_M2}	}
#' \item{5: \code{PERIOD_M5}	}
#' \item{15: \code{PERIOD_M15}	}
#' \item{30: \code{PERIOD_M30}	}
#' \item{60: \code{PERIOD_H1}	}
#' \item{120: \code{PERIOD_H2}	}
#' \item{240: \code{PERIOD_H4}	}
#' \item{480: \code{PERIOD_H8}	}
#' \item{1440: \code{PERIOD_D1}	}
#' \item{7200: \code{PERIOD_W1}	}
#' \item{216000: \code{PERIOD_MN1}	}
#' }
#' \url{https://www.mql5.com/en/docs/constants/chartconstants/enum_timeframes}
#'
#' @seealso
#' [mt5R::MT5.RemoveAllChartsObjects()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' # Red line in all EURUSD charts at 1.1772
#' MT5.DrawHorizontalLine("EURUSD", 1.1772, iColor = 3)
#'
#' }
MT5.DrawHorizontalLine <- function(sSymbol, fPrice, iWidth = 1, iColor = 1, sName = "", iTF = 0)
{
  iColor <- as.integer(iColor)
  iWidth <- as.integer(iWidth)
  iTF <- as.integer(iTF)
  fPrice <- as.numeric(fPrice)
  stopifnot(is.character(sSymbol))

  if(sName == "")
  {
    sName <- base::paste0(rnorm(1, 1), Sys.time()) ## Random name with date
    sName <- gsub("[^0-9]", "", sName)
  }
  if(iWidth < 1)iWidth <- 1;

  sRequest <- utils::read.table(text = MT5.Connect(base::paste0("C1 ", paste(
    sSymbol, sName, fPrice, iWidth, iColor, iTF,
    sep = "?"))), sep = ";")

  return(as.logical(unlist(sRequest) == "C1OK"))
}

#' MT5 File folder
#'
#' @description
#' Used in functions that need to use files generated from MT5.
#'
#' @return
#' Returns file path of MT5 folder.
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
MT5.FileWorkingFolder <- function()
{
  return(as.character(utils::read.table(text = MT5.Connect("S2"), sep = ";")[[1]]))
}

#' Load data from MT5
#'
#' @description
#' Function to load \code{sSymbol} in \code{Data.frame} or \code{xts} if specified. It uses \code{csv} created by MT5 to manipulate the data, this function was created to manage reasonable size of data, for tiny sizes of data use \code{MT5.Quick_GetSymbol()} instead.
#'
#' The \code{csv} created can be used by other softwares, by \emph{default} is deleted after use \code{bDeletecsv = TRUE}.
#'
#' Data is loaded silently and will take longer as defined \code{iRows} is.
#'
#' @param sSymbol character; target symbol.
#' @param iTF int; target time frame. See details.
#' @param iRows int; how many rows. It's start from last. (default: \code{5})
#' @param xts bool; if \code{xts = TRUE} the function will return a xts instead. See details.
#' @param iWait int; how long to wait for \code{csv} from MT5 (default \code{Inf})
#' @param bDeletecsv bool; delete \code{csv} after use (default \code{TRUE})
#' @param xts_tz character; time zone specification to be used when `xts = TRUE` (default \code{""})
#'
#' @return
#' Always returns \code{OHCLV} format. Date will be exhibited as it \code{xts} parameter is defined.
#'
#' \describe{
#'   \item{\code{xts = FALSE} (default)}{Returns \eqn{[nx10]} \code{{data.frame} {Year, Month, Day, Hour, Minute, Open, High, Low, Close, Volume}}}
#'   \item{\code{xts = TRUE}}{Returns \eqn{[nx5]} \code{{xts} {Open, High, Low, Close, Volume}}}
#' }
#'
#' On failure attempts it will return a empty \code{data.frame}.
#'
#' @details
#' It should be used only for reasonable table sizes.
#'
#' Even after stop this function on running or \code{iWait} ran out, mt5R running in MT5 will keep running to create the table. To a complete stop, proceed to kill both process. For very big sizes (\code{iRow}) the user should be patient.
#'
#' [mt5R::MT5.Quick_GetSymbol()] can be used instead for smaller tables, it not uses \code{csv}. Preliminary tests indicate to up 80x speed gain.
#'
#' Supported time frames (\code{iTF}). See references for even more details.
#' \itemize{
#' \item{1: \code{PERIOD_M1}	}
#' \item{2: \code{PERIOD_M2}	}
#' \item{5: \code{PERIOD_M5}	}
#' \item{15: \code{PERIOD_M15}	}
#' \item{30: \code{PERIOD_M30}	}
#' \item{60: \code{PERIOD_H1}	}
#' \item{120: \code{PERIOD_H2}	}
#' \item{240: \code{PERIOD_H4}	}
#' \item{480: \code{PERIOD_H8}	}
#' \item{1440: \code{PERIOD_D1}	}
#' \item{7200: \code{PERIOD_W1}	}
#' \item{216000: \code{PERIOD_MN1}	}
#' }
#'
#' @seealso
#' [mt5R::MT5.Quick_GetSymbol()], [mt5R::MT5.QuickLastRow_GetSymbol()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Open-high-low-close_chart}
#'
#' \url{https://www.mql5.com/en/docs/constants/chartconstants/enum_timeframes}
#'
#' @export
#' @examples
#' \dontrun{
#'
#' MT5.GetSymbol("EURUSD", iTF = 5, iRows = 3)
#'
#' ## Returns
#' ##   Year Month Day Hour Minute    Open    High     Low   Close Volume
#' ## 1 2020    12  11    1      5 1.21411 1.21414 1.21405 1.21409     39
#' ## 2 2020    12  11    1     10 1.21408 1.21419 1.21404 1.21418     56
#' ## 3 2020    12  11    1     15 1.21421 1.21421 1.21405 1.21408     63
#'
#' MT5.GetSymbol("EURUSD", iTF = 5, iRows = 3, xts = TRUE)
#'
#' ## Returns
#' ##                        Open    High     Low   Close Volume
#' ## 2020-12-11 01:05:00 1.21411 1.21414 1.21405 1.21409     39
#' ## 2020-12-11 01:10:00 1.21408 1.21419 1.21404 1.21418     56
#' ## 2020-12-11 01:15:00 1.21421 1.21421 1.21405 1.21415     82
#'
#' }
MT5.GetSymbol <- function(sSymbol, iTF, iRows = 5, xts = FALSE, iWait = Inf, bDeletecsv = TRUE, xts_tz = "")
{
  stopifnot(is.character(sSymbol))
  iTF <- as.integer(iTF)

  if(iRows==Inf)iRows <- 999999999;
  iRows <- ifelse(iRows < 1, 1, iRows)

  MT5.Files <- MT5.FileWorkingFolder()
  sFile <- base::paste0(MT5.Files, sSymbol, "_", iTF, "_TABLE.csv")

  if(file.exists(sFile))
  {
    file.remove(sFile) ## For safety
  }

  sRequest <- utils::read.table(text = MT5.Connect(base::paste0("M1 ", paste(
    sSymbol, iTF, iRows,
    sep = "?")), timeout = 9999999), sep = "?", stringsAsFactors = F)

  if(sRequest[[1]] == "M1ERROR2")
  {
    warning(base::paste0(sSymbol, ": symbol was not found? \nCheck if symbol is in MT5's marketwatch. Check ?MT5.Marketwatch, ?MT5.SymbolInMarketwatch, ?MT5.MarketwatchAdd"))
    return(data.frame())
  }else if(sRequest[[1]] == "M1ERROR3")
  {
    warning(base::paste0(sSymbol, ": time frame not supported: ", iTF," \nCheck supported time frames ?MT5.GetSymbol"))
    return(data.frame())
  }else if(sRequest[[1]] != "M1OK") return(NULL);

  SFileDownloading <- base::paste0(MT5.Files, "PegandoDados.txt")

  iCnt <- 0
  while(TRUE)
  {
    Sys.sleep(0.01)
    iCnt <- iCnt + 1
    if(iCnt > iWait) return(NULL);
    if(!file.exists(SFileDownloading))break;
  }

  if(!file.exists(sFile))
  {
    warning(base::paste0(sSymbol, ": not available data?"))
    return(data.frame())
  }

  Unprocessed_Table <- utils::read.table(sFile, sep = ';', col.names = c("Year", "Month", "Day", "Hour", "Minute", "Open", "High", "Low", "Close", "Volume"))
  if(bDeletecsv) file.remove(sFile);

  if(xts == TRUE)
  {
    Data_string <- base::paste0(Unprocessed_Table[,1],"/", Unprocessed_Table[,2],"/",Unprocessed_Table[,3], " ", Unprocessed_Table[,4],":", Unprocessed_Table[,5])
    Dates <- as.POSIXct(Data_string, format = "%Y/%m/%d %H:%M", tz = xts_tz)

    iDates_AnyNA <- which(is.na(Dates))
    if(length(iDates_AnyNA)>0)
    {
      warning(paste0("Some dates were not been successfully converted. The following rows have been removed: \n", paste(Data_string[iDates_AnyNA], collapse = "\n"), "\nTry another xts_tz?"))
      Symbol_xts <- xts::xts(Unprocessed_Table[-iDates_AnyNA, c(6:10)], order.by = Dates[-iDates_AnyNA])
    }else{
      Symbol_xts <- xts::xts(Unprocessed_Table[,c(6:10)], order.by = Dates)
    }

    return(Symbol_xts)
  }

  return(Unprocessed_Table)
}

#' Load data quickly from MT5
#'
#' @description
#' Function to load \code{sSymbol} in \code{Data.frame} or \code{xts} if specified. All the data come from socket. It's pretty quickly function to obtain data.
#'
#' Data is loaded silently and will take longer as defined \code{iRow} is.
#'
#' @param sSymbol character; target symbol.
#' @param iTF int; target time frame. See details.
#' @param iRow int; how many rows. It's start from last. (default: \code{5})
#' @param xts bool; if \code{xts = TRUE} the function will return a xts instead. See details.
#'
#' @return
#' Always returns \code{OHCLV} format. Date will be exhibited as it \code{xts} parameter is defined.
#'
#' \describe{
#'   \item{\code{xts = FALSE} (default)}{Returns \eqn{[nx10]} \code{{data.frame} {Year, Month, Day, Hour, Minute, Open, High, Low, Close, Volume}}}
#'   \item{\code{xts = TRUE}}{Returns \eqn{[nx5]}  \code{{xts} {Open, High, Low, Close, Volume}}}
#' }
#'
#' On failure attempts it will return a empty \code{data.frame}.
#'
#' @details
#' It should be used only for tiny table sizes. For big size tables consider to use [mt5R::MT5.GetSymbol()] instead.
#'
#' Supported time frames (\code{iTF}). See references for even more details.
#' \itemize{
#' \item{1: \code{PERIOD_M1}}
#' \item{2: \code{PERIOD_M2}}
#' \item{5: \code{PERIOD_M5}}
#' \item{15: \code{PERIOD_M15}}
#' \item{30: \code{PERIOD_M30}}
#' \item{60: \code{PERIOD_H1}}
#' \item{120: \code{PERIOD_H2}}
#' \item{240: \code{PERIOD_H4}}
#' \item{480: \code{PERIOD_H8}}
#' \item{1440: \code{PERIOD_D1}}
#' \item{7200: \code{PERIOD_W1}}
#' \item{216000: \code{PERIOD_MN1}	}
#' }
#'
#' @seealso
#' [mt5R::MT5.GetSymbol()], [mt5R::MT5.QuickLastRow_GetSymbol()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Open-high-low-close_chart}
#'
#' \url{https://www.mql5.com/en/docs/constants/chartconstants/enum_timeframes}
#' @export
#' @examples
#' \dontrun{
#'
#' MT5.Quick_GetSymbol("EURUSD", iTF = 5, iRows = 3)
#'
#' ## Returns
#' ##   Year Month Day Hour Minute    Open    High     Low   Close Volume
#' ## 1 2020    12  11    1      5 1.21411 1.21414 1.21405 1.21409     39
#' ## 2 2020    12  11    1     10 1.21408 1.21419 1.21404 1.21418     56
#' ## 3 2020    12  11    1     15 1.21421 1.21421 1.21405 1.21408     63
#'
#' MT5.Quick_GetSymbol("EURUSD", iTF = 5, iRows = 3, xts = TRUE)
#'
#' ## Returns
#' ##                        Open    High     Low   Close Volume
#' ## 2020-12-11 01:05:00 1.21411 1.21414 1.21405 1.21409     39
#' ## 2020-12-11 01:10:00 1.21408 1.21419 1.21404 1.21418     56
#' ## 2020-12-11 01:15:00 1.21421 1.21421 1.21405 1.21415     82
#'
#' }
MT5.Quick_GetSymbol <- function(sSymbol, iTF, iRows = 5, xts = FALSE)
{
  # stopifnot(is.character(sSymbol))
  # stopifnot(is.integer(iTF))
  # stopifnot(is.integer(iRows))

  if(base::length(sSymbol) > 1)
  {
    warning("MT5.Quick_GetSymbol only works with one symbol at a time!")
    sSymbol <- sSymbol[1]
  }

  iRows <- ifelse(iRows < 1, 1, iRows)

  sRequest <- utils::read.table(text = MT5.Connect(
                                base::paste0("M4 ", paste(sSymbol, iTF, iRows, sep = "?"))),
                                sep = "?", stringsAsFactors = F)

  if(sRequest[[1]] == "M4ERROR2")
  {
    warning(base::paste0(sSymbol, ": symbol was not found? \nCheck if symbol is in MT5's marketwatch. Check ?MT5.Marketwatch, ?MT5.SymbolInMarketwatch, ?MT5.MarketwatchAdd"))
    return(data.frame())
  }else if(sRequest[[1]] == "M4ERROR3")
  {
    warning(base::paste0(sSymbol, ": time frame not supported: ", iTF," \nCheck supported time frames ?MT5.Quick_GetSymbol"))
    return(data.frame())
  }else if(sRequest[[1]] == "M4ERROR4")
  {
    warning(base::paste0(sSymbol, ": not available data?"))
    return(data.frame())
  }

  Unprocessed_Table <- do.call(rbind, lapply(sRequest, function(x){utils::read.table(text = x, sep = " ", stringsAsFactors = F)}))
  names(Unprocessed_Table) <- c("Year", "Month", "Day", "Hour", "Minute", "Open", "High", "Low", "Close", "Volume")
  row.names(Unprocessed_Table) <- NULL
  if(xts == TRUE)
  {
    Data_string <- base::paste0(Unprocessed_Table[,1],"/", Unprocessed_Table[,2],"/",Unprocessed_Table[,3], " ", Unprocessed_Table[,4],":", Unprocessed_Table[,5])
    xts_return <- xts::xts(Unprocessed_Table[,c(6:10)], order.by = as.POSIXct(Data_string, format = "%Y/%m/%d %H:%M"))
    return(xts_return)
  }

  return(Unprocessed_Table)
}

#' Load last row from MT5
#'
#' @description
#' Function to load the last row of a specific time frame of a vector of symbols. All the data come from socket connection.
#'
#' The usage of this function is preferable than use \code{MT5.Quick_GetSymbol()} several times for each symbol.
#'
#' @param sSymbol character(); vector of target symbols.
#' @param iTF int; target time frame. See details.
#'
#' @return
#' Returns \eqn{[nx7]} \code{{data.frame} {Date, sSymbol, Open, High, Low, Close, Volume}}.
#'
#' @details
#' Supported time frames (\code{iTF}). See references for even more details.
#' \itemize{
#' \item{1: \code{PERIOD_M1}	}
#' \item{2: \code{PERIOD_M2}	}
#' \item{5: \code{PERIOD_M5}	}
#' \item{15: \code{PERIOD_M15}	}
#' \item{30: \code{PERIOD_M30}	}
#' \item{60: \code{PERIOD_H1}	}
#' \item{120: \code{PERIOD_H2}	}
#' \item{240: \code{PERIOD_H4}	}
#' \item{480: \code{PERIOD_H8}	}
#' \item{1440: \code{PERIOD_D1}	}
#' \item{7200: \code{PERIOD_W1}	}
#' \item{216000: \code{PERIOD_MN1}	}
#' }
#'
#' @seealso
#' [mt5R::MT5.GetSymbol()], [mt5R::MT5.Quick_GetSymbol()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Open-high-low-close_chart}
#'
#' \url{https://www.mql5.com/en/docs/constants/chartconstants/enum_timeframes}
#' @export
#' @examples
#' \dontrun{
#' ## Get last row of EURUSD and GBPUSD, 30 minute timeframe both
#' MT5.QuickLastRow_GetSymbol(c("EURUSD", "GBPUSD"), 30)
#'
#' ## Returns
#' ##                  Date sSymbol    Open    High     Low   Close Volume
#' ## 1 2020-12-11 00:30:00  EURUSD 1.21371 1.21401 1.21370 1.21394    184
#' ## 2 2020-12-11 00:30:00  GBPUSD 1.33030 1.33032 1.32947 1.32993    866
#'
#' }
MT5.QuickLastRow_GetSymbol <- function(sSymbols, iTF)
{
  stopifnot(is.character(sSymbols))
  sSymbols <- as.character(sSymbols)
  iTF <- as.integer(iTF)[1]

  sNames <- c("Year", "Month", "Day", "Hour", "Minute", "Open", "High", "Low", "Close", "Volume")

  sRequest <- MT5.Connect(base::paste0("M5 ", paste(iTF, paste(sSymbols, collapse = "?"), sep = "?")))

  if(sRequest[[1]] == "M5ERROR2")
  {
    warning(base::paste0(sSymbol, ": symbol was not found? \nCheck if symbol is in MT5's marketwatch. Check ?MT5.Marketwatch, ?MT5.SymbolInMarketwatch, ?MT5.MarketwatchAdd"))
    return(data.frame())
  }else if(sRequest[[1]] == "M5ERROR3")
  {
    warning(base::paste0(sSymbol, ": time frame not supported: ", iTF," \nCheck supported time frames ?MT5.QuickLastRow_GetSymbol"))
    return(data.frame())
  }

  sStringTable <- read.table(text = sRequest, sep = "?", stringsAsFactors = F)
  sStringTable[which(is.na(sStringTable))] <- paste(rep(NA, length.out = 10), collapse = " ")

  Unprocessed_Table <- do.call(rbind, lapply(sStringTable, function(x){utils::read.table(text = x, sep = " ", stringsAsFactors = F)}))
  names(Unprocessed_Table) <- sNames
  Data_string <- base::paste0(Unprocessed_Table[,1],"/", Unprocessed_Table[,2],"/",Unprocessed_Table[,3], " ", Unprocessed_Table[,4],":", Unprocessed_Table[,5])

  df_ <- data.frame(Date = as.POSIXct(Data_string, format = "%Y/%m/%d %H:%M"), sSymbol = sSymbols, Unprocessed_Table[,c(6:10)], stringsAsFactors = F)
  row.names(df_) <- NULL
  return(df_)
}

#' List options of an underlying asset
#'
#' @description
#' Function to load all available options in MT5 of an underlying asset.
#'
#' Only works in stock brokers.
#'
#' @param sUnderlyingAsset character(); target underlying asset (e.g. for "PETR4" should be used "PETR").
#' @param bUseDescriptionStrike bool; use strike given in symbol description. Some brokers do not adjust the prices following the dividends. Totally optional (experimental). (default \code{FALSE}).
#' @param bAutomaticHelpUnderlying bool; it will try to help the user to only send the underlying asset to mt5R. It will convert "PETR4" to "PETR" automatically (default \code{TRUE}).
#'
#' @return Returns \emph{Data.frame} \eqn{[nx6]}, with follow informations:
#' \itemize{
#'   \item sOpt \code{{character}}: options symbol.
#'   \item PN_ON \code{{int}}: 0 for preferential stocks, 1 otherwise.
#'   \item Type \code{{factor}}: options type. "CALL" for call options, "PUT" for put options.
#'   \item Mod \code{{factor}}: option style. "AMERICAN" or "EUROPEAN". See references.
#'   \item Strike \code{{numeric}}: options strike.
#'   \item Expiration \code{{Date}}: options expiration.
#'   }
#'
#' @seealso
#' [mt5R::MT5.SymbolType()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @author Trader_Patinhas (original MT5 block of code), [MT5 Community Bio](https://www.mql5.com/pt/users/trader_patinhas)
#'
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Option_style}
#' @export
#' @examples
#' \dontrun{
#' ## Get all available options of PETR4 and PETR3 (Petroleo Brasileiro S.A. - Petrobras PBR:NYSE)
#' MT5.ListOptions("PETR")
#'
#' ## Other examples
#' MT5.ListOptions("VALE")
#' MT5.ListOptions("USIM")
#'
#' }
MT5.ListOptions <- function(sUnderlyingAsset, bUseDescriptionStrike = FALSE, bAutomaticHelpUnderlying = TRUE)
{
  if(!is.character(sUnderlyingAsset))
  {
    stop("Need to be a character vector")
  }
  if(base::length(sUnderlyingAsset) > 1)
  {
    sUnderlyingAsset <- sUnderlyingAsset[1]
  }

  if(length(grep("[0-9]$", sUnderlyingAsset)) > 0 & bAutomaticHelpUnderlying)
  {
    sUnderlyingAsset <- gsub('.{0,1}$', '', sUnderlyingAsset)
  }

  sUnderlyingAsset <- toupper(sUnderlyingAsset)

  sTabel1 <- utils::read.table(text = utils::read.table(text = MT5.Connect(base::paste0("M2 ", sUnderlyingAsset)), sep = ";", stringsAsFactors = F)[[1]], sep = "?", stringsAsFactors = F)
  sTabel1 <- sTabel1[!is.na(sTabel1)]

  if(length(sTabel1) == 1)
  {
    if(sTabel1 == "M2ERROR") warning("ERROR: wrong underlying asset name? See examples");
    if(sTabel1 == 0) warning("Not found any options. Wrong underlying asset name? See examples");

    return(data.frame(sOpt = character(),
                      PN_ON = integer(),
                      Type = factor(),
                      Mod = factor(),
                      Strike = numeric(),
                      Expiration = base::as.Date(x = integer(0), origin = "1970-01-01")))
  }

  sTabela1_postrim <- base::gsub("(^[[:space:]]+|[[:space:]]+$)", "", sTabel1)
  # sTabela1_postrim <- str_trim(sTabel1)
  sTabela1_postrim <- sTabela1_postrim[complete.cases(sTabela1_postrim)]
  sAllOptions <- gsub("\\,", "\\.", sTabela1_postrim)
  # sAllOptions <- str_replace_all(sTabela1_postrim, "\\,", "\\.")


  df_final <- data.frame()
  for(i in seq_len(base::length(sAllOptions)))
  {
    sTextoB <- sAllOptions[i]
    sTexto <- utils::read.table(text = sTextoB, sep = " ", stringsAsFactors = F)
    sTexto <- sTexto[!is.na(sTexto)]

    InfosOpt <- sTexto[1:5]
    OtherInfos <- sTexto[-c(1:5)]

    dtExp <- base::as.Date(InfosOpt[3], format = "%Y.%m.%d")
    fStrike <- ifelse(bUseDescriptionStrike, as.numeric(tail(OtherInfos, 1)), as.numeric(InfosOpt[4]))
    bON <- "ON" %in% OtherInfos

    df_temp <- data.frame(sOpt = as.character(InfosOpt[1]),
                          PN_ON = as.integer(bON),
                          Type = factor(InfosOpt[2], levels = c("CALL", "PUT")),
                          Mod = factor(InfosOpt[5], levels = c("EUROPEAN","AMERICAN")),
                          Strike = fStrike,
                          Expiration = dtExp)
    df_final <- rbind(df_final, df_temp)
  }

  return(df_final)
}

#' Check if market is open
#'
#' @description
#'
#' \strong{Warning!} This function is not entirely reliable. It uses the last hour of trading of all marketwatch's symbols to check if the market is open. To actually know when the market is really open, the trader must know the market and rely on his/her knowledge.
#'
#' If trader's computer \emph{lost connection} and all symbols turns outdated this function will return \code{FALSE} by its construction. It's strongly recommended to use support function, as check if internet is working and check for weekends/holidays, to not stuck with False Negatives.
#'
#' Detecting this type of status is quite challenging in MQL. **This function takes \code{1} second to return the result.**
#'
#' @return Returns \code{TRUE} if market is open, \code{FALSE} otherwise.
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
MT5.MarketIsOpen <- function()
{
  sRequest <- utils::read.table(text = MT5.Connect("P0"), sep = ";")

  if (.Platform$OS.type == "windows")
  {
    ipmessage <- system("ipconfig", intern = TRUE)
  }else{
    pmessage <- system("ifconfig", intern = TRUE)
  }

  if(!(any(grep("((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)", ipmessage))))
  {
    warning("Connection to internet failed?")
  }

  return(as.logical(unlist(sRequest) == "P0OK"))
}

#' Symbols in MT5's marketwatch
#'
#' @description
#'
#' List all symbols in MT5's marketwatch.
#'
#' @return Returns \emph{character()} of all symbols in MT5's marketwatch.
#'
#' @seealso
#' [mt5R::MT5.MarketwatchAdd()], [mt5R::MT5.MarketwatchRemove()], [mt5R::MT5.SymbolInMarketwatch()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
MT5.Marketwatch <- function()
{
  sRequest <- utils::read.table(text = MT5.Connect("P1"), sep = "?", colClasses = c("character"))
  return(as.character(sRequest))
}

#' Add symbols in MT5's marketwatch
#'
#' @description
#'
#' Add symbols in MT5's marketwatch.
#'
#' @param sSymbol character(); vector of target symbols to add.
#'
#' @return Returns \emph{logical()}. \code{TRUE} if symbol was successfully added, \code{FALSE} otherwise.
#'
#' @seealso
#' [mt5R::MT5.Marketwatch()], [mt5R::MT5.MarketwatchRemove()], [mt5R::MT5.SymbolInMarketwatch()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' ## Add XAUUSD and XAGUSD
#' MT5.MarketwatchAdd(c("XAUUSD","XAGUSD"))
#'
#' }
MT5.MarketwatchAdd <- function(sSymbols)
{
  sSymbols <- as.character(sSymbols)
  sTextToSent <- mt5R::SUP_MT5_StackRequests(sSymbols, "P3")
  sRequest <- utils::read.table(text = MT5.Connect(sTextToSent), sep = "?", colClasses = c("character"))
  return(as.logical(unlist(sRequest) == "P3OK"))
}

#' Remove symbols in MT5's marketwatch
#'
#' @description
#'
#' Remove symbols in MT5's marketwatch.
#'
#' Any attempt to remove \code{sSymbol} that has any chart opened will fracass automatically by MT5.
#'
#' @param sSymbol character(); vector of target symbols to remove.
#'
#' @return Returns \emph{logical()}. \code{TRUE} if symbol was successfully removed, \code{FALSE} otherwise.
#'
#' @seealso
#' [mt5R::MT5.Marketwatch()], [mt5R::MT5.MarketwatchAdd()], [mt5R::MT5.SymbolInMarketwatch()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' ## Remove XAUUSD and XAGUSD
#' MT5.MarketwatchAdd(c("XAUUSD","XAGUSD"))
#'
#' }
MT5.MarketwatchRemove <- function(sSymbols)
{
  sSymbols <- as.character(sSymbols)
  sTextToSent <- mt5R::SUP_MT5_StackRequests(sSymbols, "P2")
  sRequest <- utils::read.table(text = MT5.Connect(sTextToSent), sep = "?", colClasses = c("character"))
  return(as.logical(unlist(sRequest) == "P2OK"))
}

#' Check if symbol is in MT5's marketwatch
#'
#' @description
#'
#' Function to check if \code{sSymbol} is in MT5's marketwatch.
#'
#' @param sSymbol character; target symbol.
#'
#' @return Returns \code{TRUE} if symbol is in MT5's marketwatch, \code{FALSE} otherwise.
#'
#' @seealso
#' [mt5R::MT5.MarketwatchAdd()], [mt5R::MT5.MarketwatchRemove()], [mt5R::MT5.Marketwatch()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
MT5.SymbolInMarketwatch <- function(sSymbol)
{
  stopifnot(is.character(sSymbol))
  if(length(sSymbol)>1)warning("This function is only for one symbol. Using the first one!");
  sRequest <- utils::read.table(text = MT5.Connect(base::paste0("P5 ", sSymbol[1])), sep = "?", colClasses = c("character"))
  return(as.logical(as.character(sRequest)=="P5OK1"))
}

#' Modify order
#'
#' @description
#'
#' Modification of characteristics of pending orders. To modify an previously opened position use \code{MT5.ModifyPosition()} instead.
#'
#' It can be used targeting only one or several orders at once. See details.
#'
#' Use \code{MT5.ShowOrders()} or look at "Trade" tab, to fetch \code{iTickets}.
#'
#' @param iTicket integer; order's ticket.
#' @param fPrice numeric; target price. To no change use \code{fPrice = 0} or omit.
#' @param fStop numeric; target stop loss. To remove stop loss use \code{fStop = -1}. To no change use \code{fStop = 0} or omit.
#' @param fGain numeric; target stop gain. To remove stop gain use \code{fGain = -1}. To no change use \code{fGain = 0} or omit.
#' @param ... data.frame; using \code{data.frame} for several orders at once. \strong{Columns name should be the same of the arguments above}. See examples.
#'
#' @return
#' Returns \code{TRUE} if order was successful modified, no changes and otherwise will return \code{FALSE}.
#'
#' If used \code{data.frame} it will return \code{logical} vector.
#'
#' @details
#' It need to choose a \code{data.frame} or just one order at time to modify. Not both. An attempt using both a warning will pop up and \code{data.frame} will be used.
#'
#' @seealso
#' [mt5R::MT5.SingleOrder()], [mt5R::MT5.MultipleOrders()], [mt5R::MT5.ShowOrders()], [mt5R::MT5.ShowPositions()], [mt5R::MT5.ClosePosition()], [mt5R::MT5.DeleteOrder()], [mt5R::MT5.ModifyPosition()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' ## Below examples of an existing buy limit order (684701334)
#'
#' MT5.ModifyOrder(iTicket = 684701334, fPrice = 25.00) ## Modifying the trigger price to open a limit order
#' MT5.ModifyOrder(iTicket = 684701334, fStop = 24.00) ## A single modification
#' MT5.ModifyOrder(iTicket = 684701334, fStop = 24.00, fGain = 26.00) ## You can modify both at same time
#' MT5.ModifyOrder(iTicket = 684701334, fPrice = 25.00, fStop = 24.00, fGain = 26.00) ## Or everything at same time
#' MT5.ModifyOrder(684701334, fStop = -1) ## Remove stop, omitting iTicket
#'
#' ## The trader can use data.frame for multiple modifications in different orders. Columns name should be the same
#' ## of the arguments of a single modification (check used arguments above). See example below
#'
#' df_modify <- data.frame(iTicket = 687356562, fPrice = 21, fStop = 20)
#' df_modify <- rbind(df_modify, data.frame(iTicket = 687357112, fPrice = 18, fStop = 17))
#' MT5.ModifyOrder(... = df_modify) ## Modify two orders at same time
#'
#' }
MT5.ModifyOrder <- function(iTicket = 0, fPrice = 0, fStop = 0, fGain = 0, ...)
{
  Argsfunc <- list(...)
  bHasDF <- FALSE

  if(length(Argsfunc) > 0)
  {
    df_request <- Argsfunc[[1]]
    if(is.data.frame(df_request))
    {
      if(base::dim(df_request)[1] < 1)
      {
        stop("Error: no rows?")
      }
      bHasDF <- TRUE;
    }else
    {
      stop("Error: input (...) should be data.frame")
    }
  }

  if(bHasDF & iTicket > 0)
  {
    warning("MT5.ModifyOrder: you should either send a data.frame or just one single order to modify. Not both! Using data.frame!")
  }

  if(!bHasDF)
  {
    iTicket <- as.integer(iTicket)
    fPrice <- as.numeric(fPrice)
    fStop <- as.numeric(fStop)
    fGain <- as.numeric(fGain)

    if(iTicket <= 0)stop("Invalid iTicket!")

    sRequest <- MT5.Connect(base::paste0("O6 ", paste(
      iTicket, fPrice, fStop, fGain,
      sep = "?")))
    return(as.logical(unlist(sRequest) == "O6OK"))
  }

  sTextToSend <- as.character()
  for(i in 1:base::dim(df_request)[1])
  {

    sOrder <- base::paste0("O6 ", as.character(df_request$iTicket[i]), "?",
                     ifelse(is.null(df_request[i,]$fPrice), 0, as.numeric(df_request[i,]$fPrice)), "?",
                     ifelse(is.null(df_request[i,]$fStop), 0, as.numeric(df_request[i,]$fStop)), "?",
                     ifelse(is.null(df_request[i,]$fGain), 0, as.numeric(df_request[i,]$fGain)))
    if(i == 1)
    {
      sTextToSend <- sOrder
    }else
    {
      sTextToSend <- base::paste0(sTextToSend, "@", sOrder)
    }
  }

  sRequest <- utils::read.table(text = MT5.Connect(sTextToSend), sep = ";")

  return(as.logical(unlist(sRequest) == "O6OK"))
}

#' Modify position
#'
#' @description
#'
#' Modification of characteristics of the previously opened position. To modify an existing pending order use \code{MT5.ModifyOrder()} instead.
#'
#' It can be used targeting only one or several positions at once. See details.
#'
#' Use \code{MT5.ShowPositions()} or look at "Trade" tab, to fetch \code{iTickets}.
#'
#' @param iTicket integer; positions's ticket.
#' @param fStop numeric; target stop loss. To remove stop loss use \code{fStop = -1}. To no change use \code{fStop = 0} or omit.
#' @param fGain numeric; target stop gain. To remove stop gain use \code{fGain = -1}. To no change use \code{fGain = 0} or omit.
#' @param ... data.frame; using \code{data.frame} for several positions at once. \strong{Columns name should be the same of the arguments above}. See examples.
#'
#' @return
#' Returns \code{TRUE} if position was successful modified, no changes and otherwise will return \code{FALSE}.
#'
#' If used \code{data.frame} it will return \code{logical} vector.
#'
#' @details
#' It need to choose a \code{data.frame} or just one position at time to modify. Not both. An attempt using both a warning will pop up and \code{data.frame} will be used.
#'
#' @seealso
#' [mt5R::MT5.SingleOrder()], [mt5R::MT5.MultipleOrders()], [mt5R::MT5.ShowOrders()], [mt5R::MT5.ShowPositions()], [mt5R::MT5.ClosePosition()], [mt5R::MT5.DeleteOrder()], [mt5R::MT5.ModifyOrder()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' ## Below examples of an existing buy position (663502899)
#' MT5.ModifyPosition(iTicket = 663502899, fStop = 29)
#'
#' ## Using data.frame for multiple modifications
#' df_modify <- data.frame(iTicket = 663502899, fStop = 29)
#' df_modify <- rbind(df_modify, data.frame(iTicket = 663502211, fStop = 130))
#' MT5.ModifyPosition(... = df_modify)
#'
#' }
MT5.ModifyPosition <- function(iTicket = 0, fStop = 0, fGain = 0, ...)
{
  Argsfunc <- list(...)
  bHasDF <- FALSE

  if(length(Argsfunc) > 0)
  {
    df_request <- Argsfunc[[1]]
    if(!is.data.frame(df_request))
    {
      stop("Error: input (...) should be a data.frame!")
    }
    if(base::dim(df_)[1] < 1)
    {
      stop("Error: no rows?")
    }

    bHasDF <- TRUE
  }

  if(bHasDF & iTicket > 0)
  {
    warning("MT5.ModifyPosition: you should either send a data.frame or just one single position to modify. Not both! Not using data.frame!")
  }

  if(!bHasDF)
  {
    iTicket <- as.integer(iTicket)
    fStop <- as.numeric(fStop)
    fGain <- as.numeric(fGain)

    sRequest <- MT5.Connect(base::paste0("O8 ", paste(
      iTicket, fStop, fGain,
      sep = "?")))
    return(as.logical(unlist(sRequest) == "O8OK"))
  }

  sTextToSend <- as.character()
  for(i in 1:base::dim(df_request)[1])
  {
    sOrder <- base::paste0("O8 ", as.character(df_request$iTicket[i]), "?",
                     ifelse(is.null(df_request[i,]$fStop), 0, as.numeric(df_request[i,]$fStop)), "?",
                     ifelse(is.null(df_request[i,]$fGain), 0, as.numeric(df_request[i,]$fGain)))
    if(i == 1)
    {
      sTextToSend <- sOrder
    }else
    {
      sTextToSend <- base::paste0(sTextToSend, "@", sOrder)
    }
  }

  sRequest <- utils::read.table(text = MT5.Connect(sTextToSend), sep = ";")

  return(as.logical(unlist(sRequest) == "O8OK"))
}


#' Send multiple orders
#'
#' @description
#'
#' Send multiple orders using \code{data.frame}. All columns need to be filled. See details.
#'
#' Some Forex brokers take \code{200ms} to accept orders into market, so take this time on mind. Always check MT5's "Experts" tab.
#'
#' @param df_orders data.frame; see details.
#'
#' @return
#' Returns \code{{logical}} vector. \code{TRUE} if order was successful accepted, otherwise will return \code{FALSE}.
#'
#' @details
#'
#' Take note that buy orders are rejected when order's stop loss are higher than order's price, and vice-versa for sell orders. This is a sanity check made by MT5, not by mt5R. Always check MT5's "Experts" tab for more details when an order is rejected.
#'
#' \code{df_orders {data.frame}} \eqn{[nx7]} columns should be on the same order and type of [mt5R::MT5.SingleOrder] arguments.
#' \itemize{
#'   \item sSymbol \code{{character}}: symbol to send the order.
#'   \item {iCmd \code{{int}}:
#'          \itemize{
#'                  \item{0: ORDER_TYPE_BUY_LIMIT}
#'                  \item{1: ORDER_TYPE_SELL_LIMIT}
#'                  }
#'          }
#'   \item fVol \code{{numeric}}: volume order.
#'   \item fPrice \code{{numeric}}: where the order will be positioned. Use \code{fPrice = 0} for market order.
#'   \item fStop \code{{numeric}}: if applicable use any value > 0.
#'   \item fGain \code{{numeric}}: if applicable use any value > 0.
#'   \item {iFillType \code{{int}}:
#'          \itemize{
#'                  \item{0: ORDER_FILLING_RETURN (default)}
#'                  \item{1: ORDER_FILLING_FOK (market order)}
#'                  \item{2: ORDER_FILLING_IOC}
#'                  }
#'          }
#'    }
#'
#' Using \code{fPrice = 0} for Market Order, automatically \code{iFillType} will be set to \code{1}.
#'
#' @seealso
#' [mt5R::MT5.SingleOrder()], [mt5R::MT5.ShowOrders()], [mt5R::MT5.ShowPositions()], [mt5R::MT5.ClosePosition()], [mt5R::MT5.DeleteOrder()], [mt5R::MT5.ModifyOrder()], [mt5R::MT5.ModifyPosition()]
#'
#' @references
#' \url{https://www.mql5.com/en/docs/constants/tradingconstants/orderproperties}
#'
#' \url{https://www.investopedia.com/terms/m/marketorder.asp}
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' ## Two LONG positions, PETR4 with volumn of 100 and price of 22.0 and VALE5 with volumn of 500 and price of 65.
#' Orders <- data.frame(sSymbol = "PETR4", iCmd = 0, fVol = 100, fPrice = 22, fStop = 0, fGain = 0, iFillType = 0)
#' Orders <- rbind(Orders, data.frame(sSymbol = "ABEV3", iCmd = 0, fVol = 200, fPrice = 14.3, fStop = 0, fGain = 0, iFillType = 0))
#' MT5.MultipleOrders(Orders)
#'
#' ## Forex order. First one is a market order (fPrice = 0)
#' Orders <- data.frame(sSymbol = "EURUSD", iCmd = 0, fVol = 0.01, fPrice = 0, fStop = 0, fGain = 0, iFillType = 1)
#' Orders <- rbind(Orders, data.frame(sSymbol = "GBPUSD", iCmd = 0, fVol = 0.01, fPrice = 1.3276, fStop = 0, fGain = 0, iFillType = 0))
#' MT5.MultipleOrders(Orders)
#'
#' }
MT5.MultipleOrders <- function(df_orders)
{
  if(base::dim(df_orders)[2] != 7)
  {
    stop("Error: wrong dimensions. It should be 7 columns. See details ?MT5.MultipleOrders")
  }
  if(!is.data.frame(df_orders))
  {
    stop("Error: argument is not a dataframe!")
  }

  sTextToSent <- as.character()
  for(i in 1:base::dim(df_orders)[1])
  {
    # browser()
    sSymbol <- as.character(df_orders[i,1])
    iCmd <- as.integer(df_orders[i,2])
    fVol <- as.numeric(df_orders[i,3])
    fPrice <- as.numeric(df_orders[i,4])
    fStop <- as.numeric(df_orders[i,5])
    fGain <- as.numeric(df_orders[i,6])
    iFill <- ifelse(fPrice == 0, 1, as.integer(df_orders[i,7]))

    sOrder <- base::paste0("O1 ", sSymbol, "?", iCmd, "?",
                     fVol, "?", fPrice, "?",
                     fStop, "?", fGain, "?",
                     ifelse(fPrice == 0, 1, iFill))
    if(i == 1)
    {
      sTextToSent <- sOrder
    }else
    {
      sTextToSent <- base::paste0(sTextToSent, "@", sOrder)
    }
  }
  sRequest <- utils::read.table(text = MT5.Connect(sTextToSent), sep = ";")
  return(as.logical(sRequest == "O1OK"))
}

#' Ping MT5 platform
#'
#' @description
#'
#' Signal sent to MT5 that requests a response to check if the mt5R in MT5 is available.
#'
#' It can be used to measure how long the response takes.
#'
#' @param bShowTimeElapsed bool; it will print how long the response takes (default \code{FALSE}). See details.
#'
#' @return
#' Returns \code{TRUE} if mt5R is available in MT5, \code{FALSE} otherwise.
#'
#' @seealso
#' [mt5R::MT5.ChangeDoorSocket()], [mt5R::MT5.Connect()]
#'
#' @details
#' For \code{bShowTime = TRUE}, is pretty common zero results. Check also ?proc.time
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
MT5.Ping <- function(bShowTimeElapsed = F)
{
  if(!bShowTimeElapsed)
  {
    return(!is.null(MT5.Connect("S1", timeout = 1)))
  }

  sTimeElapsed <- system.time({bWorking<-MT5.Connect("S1", timeout = 1)})
  base::print(sTimeElapsed)
  return(!is.null(bWorking))
}

#' Remove all drawn objects
#'
#' @description
#'
#' Remove all drawn  objects in specifics MT5 charts.
#'
#' @param sSymbol character(); you can specify which chart symbol. If not declared it will consider all symbols. See details.
#' @param iTF int; you can specify which charts time frames. If not declared it will consider all time frames. See details.
#'
#' @details
#' \strong{If arguments are not declared} it will remove \strong{all} drawn objects of \strong{all} charts.
#'
#' Supported time frames (\code{iTF}). See references for even more details.
#' \itemize{
#' \item{1: \code{PERIOD_M1}	}
#' \item{2: \code{PERIOD_M2}	}
#' \item{5: \code{PERIOD_M5}	}
#' \item{15: \code{PERIOD_M15}	}
#' \item{30: \code{PERIOD_M30}	}
#' \item{60: \code{PERIOD_H1}	}
#' \item{120: \code{PERIOD_H2}	}
#' \item{240: \code{PERIOD_H4}	}
#' \item{480: \code{PERIOD_H8}	}
#' \item{1440: \code{PERIOD_D1}	}
#' \item{7200: \code{PERIOD_W1}	}
#' \item{216000: \code{PERIOD_MN1}	}
#' }
#'
#' @return
#' Returns \code{TRUE} if succeed, \code{FALSE} otherwise.
#'
#' @seealso
#' [mt5R::MT5.DrawHorizontalLine()]
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#'
#' @references
#' \url{https://www.mql5.com/en/docs/constants/chartconstants/enum_timeframes}
#' @export
#' @examples
#' \dontrun{
#' ## Remove all drawn objects of all GBPUSD charts
#' MT5.RemoveAllChartsObjects("GBPUSD")
#'
#' ## Remove all drawn objects of all 60 minutes (PERIOD_H1) charts
#' MT5.RemoveAllChartsObjects(iTF = 60)
#'
#' }
MT5.RemoveAllChartsObjects <- function(sSymbol = "0", iTF = 0)
{
  iTF <- as.integer(iTF)[1]
  sSymbol <- as.character(sSymbol)[1]

  if(sSymbol == "" & iTF == 0)
  {
    sRequest <- utils::read.table(text = MT5.Connect("C3"), sep = ";")
    return(as.logical(unlist(sRequest) == "C3OK"))
  }

  sRequest <- utils::read.table(text = MT5.Connect(base::paste0("C2 ", paste(
    sSymbol, iTF,
    sep = "?"))), sep = ";")
  return(as.logical(unlist(sRequest) == "C2OK"))
}

#' Show orders
#'
#' @description
#'
#' Show all active orders displayed on the "Trade" tab. For active positions use [mt5R::MT5.ShowPositions()] instead.
#'
#' @return
#' Returns \emph{Data.frame} \eqn{[nx7]}, with follow informations:
#' \itemize{
#'   \item sSymbol \code{{character}}: order's symbol.
#'   \item {iType \code{{int}} order's type, for more see references:
#'          \itemize{
#'                  \item{2: ORDER_TYPE_BUY_LIMIT}
#'                  \item{3: ORDER_TYPE_SELL_LIMIT}
#'                  }
#'          }
#'   \item fVolume \code{{numeric}}: order's volume.
#'   \item fPrice \code{{numeric}}: order's price.
#'   \item fStop \code{{numeric}}: order's stop loss. For stop loss not set is returned \code{0}.
#'   \item fGain \code{{numeric}}: order's stop gain. For stop gain not set is returned \code{0}.
#'   \item iTicket \code{{int}}: order's ticket. Same displayed on the "Trade" tab. Used in other functions in \emph{mt5R} package.
#' }
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#'
#' @seealso
#' [mt5R::MT5.ShowPositions()], [mt5R::MT5.DeleteOrder()], [mt5R::MT5.ModifyOrder()], [mt5R::MT5.MultipleOrders()], [mt5R::MT5.SingleOrder()]
#'
#' @references
#' \url{https://www.mql5.com/en/articles/2513}
#'
#' @export
#' @examples
#' \dontrun{
#'
#' MT5.ShowOrders()
#'
#' ## Returns
#' ##   sSymbol iType fVolume fPrice   fStop fGain iTicket
#' ## 1  NZDUSD     2    0.10  0.682 0.00000     0  763153
#' ## 2  NZDUSD     2    0.10  0.661 0.00000     0  763194
#' ## 3  GBPUSD     2    0.01  1.330 1.32956     0  766489
#'
#' }
MT5.ShowOrders <- function()
{
  sRequest <- MT5.Connect("O3")

  df_final <- data.frame(sSymbol = character(),
                         iType = integer(),
                         fVolume = numeric(),
                         fPrice = numeric(),
                         fStop = numeric(),
                         fGain = numeric(),
                         iTicket = integer())

  for(i in seq_len(base::length(sRequest)))
  {
    Linha <- utils::read.table(text = sRequest[i], sep = "?")
    df <- data.frame(sSymbol = as.character(Linha[2][[1]]), iType  = as.integer(Linha[3][[1]]), fVolume = as.numeric(Linha[4][[1]]),
                     fPrice = as.numeric(Linha[5][[1]]), fStop = as.numeric(Linha[6][[1]]) , fGain = as.numeric(Linha[7][[1]]),
                     iTicket = as.integer(Linha[1][[1]]))
    df_final <- rbind(df_final, df)
  }
  if(base::dim(df_final)[1] > 0)
  {
    df_final$sSymbol <- as.character(df_final$sSymbol)
  }

  return(df_final)
}


#' Show positions
#'
#' @description
#'
#' Show all active positions displayed on the "Trade" tab. For active orders use \code{MT5.ShowOrders()} instead.
#'
#' @return
#' Returns \emph{Data.frame} \eqn{[nx8]}, with follow informations:
#' \itemize{
#'   \item sSymbol \code{{character}}: position's symbol.
#'   \item {iCmd \code{{int}} position's type, for more see references:
#'          \itemize{
#'                  \item{0: POSITION_TYPE_BUY}
#'                  \item{1: POSITION_TYPE_SELL}
#'                  }
#'          }
#'   \item fVolume \code{{numeric}}: position's volume.
#'   \item fPrice \code{{numeric}}: position's price.
#'   \item fStop \code{{numeric}}: position's stop loss. For stop loss not set is returned \code{0}.
#'   \item fGain \code{{numeric}}: position's stop gain. For stop gain not set is returned \code{0}.
#'   \item fProfit \code{{numeric}}: position's profit/loss. Same displayed on the "Trade" tab.
#'   \item iTicket \code{{int}}: position's ticket. Same displayed on the MT5's "Trade" tab. Used in other functions in \emph{mt5R} package.
#' }
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#'
#' @seealso
#' [mt5R::MT5.ShowOrders()], [mt5R::MT5.ClosePosition()], [mt5R::MT5.ModifyPosition()]
#'
#' @references
#' \url{https://www.mql5.com/en/docs/constants/tradingconstants/positionproperties}
#'
#' @export
#' @examples
#' \dontrun{
#'
#' MT5.ShowPositions()
#'
#' ## Returns
#' ##    sSymbol iCmd fVolume   fPrice  fStop fGain fProfit iTicket
#' ## 1  CADJPY    1    0.01 81.09000 0.0000     0    0.47  766491
#' ## 2  CADJPY    0    0.01 81.11700 0.0000     0   -1.00  766490
#' ## 3  EURUSD    0    0.01  1.22592 0.0000     0    0.77  766488
#' ## 4  NZDUSD    0    0.10  0.71160 0.7106     0   29.50  763964
#'
#' }
MT5.ShowPositions <- function()
{
  sRequest <- MT5.Connect("O2")

  df_final <- data.frame(sSymbol = character(),
                         iCmd = integer(),
                         fVolume = numeric(),
                         fPrice = numeric(),
                         fStop = numeric(),
                         fGain = numeric(),
                         fProfit = numeric(),
                         iTicket = integer())

  if(is.null(sRequest))
  {
    ## Nothing
    return(df_final)
  }
  for(i in seq_len(base::length(sRequest)))
  {
    Linha <- utils::read.table(text = sRequest[i], sep = "?")
    df <- data.frame(sSymbol = as.character(Linha[2][[1]]), iCmd = as.integer(Linha[3][[1]]), fVolume = as.numeric(Linha[4][[1]]),
                     fPrice= as.numeric(Linha[5][[1]]), fStop = as.numeric(Linha[6][[1]]), fGain = as.numeric(Linha[7][[1]]),
                     fProfit = as.numeric(Linha[8][[1]]), iTicket = as.integer(Linha[1][[1]]))
    df_final <- rbind(df_final, df)
  }
  if(base::dim(df_final)[1] > 0)
  {
    df_final$sSymbol <- as.character(df_final$sSymbol)
  }

  return(df_final)
}

#' Send single order
#'
#' @description
#'
#' Function used to open market or place a pending order. Only accept only order at time, for multiple orders consider to use [mt5R::MT5.MultipleOrders()] instead.
#'
#' The switch for pending order and market order is using \code{fPrice}. Any value higher than \code{0} is considered a pending order.
#'
#' Some Forex brokers take \code{200ms} to accept orders into market, so take this time on mind. Always check MT5's "Experts" tab for more information.
#'
#' @param sSymbol character; symbol for trading.
#' @param iCmd int; operation type: \itemize{
#'   \item{0: \code{ORDER_TYPE_BUY_LIMIT}}
#'   \item{1: \code{ORDER_TYPE_SELL_LIMIT}}
#'   }
#' @param fVol numeric; number of lots.
#' @param fPrice numeric; order price. \strong{WARNING}: if \code{fPrice = 0} it will be send as market order.
#' @param fStop numeric; if applicable use any value > \code{0}.
#' @param fGain numeric; if applicable use any value > \code{0}.
#' @param iFillType int; order type filling:
#' \itemize{
#'   \item{0: \code{ORDER_TYPE_BUY_LIMIT} (default)}
#'   \item{1: \code{ORDER_TYPE_SELL_LIMIT} (Market Order)}
#'   \item{2: \code{ORDER_FILLING_IOC}}
#'   }
#' @details
#' Not all columns need to be filled as \code{MT5.MultipleOrders()} request. See examples.
#'
#' For \code{fPrice = 0}, a market order will be sent and it will automatically set \code{iFillType} to \code{1}.
#'
#' For more information about \code{ORDER_TYPE_*} see references.
#'
#' \emph{WARNING! INVALID STOPS}: take note that buy orders are rejected when order's stop loss are higher than symbol's ask price, and vice versa for sell orders. This is a sanity check made by MT5, not by mt5R. Always check MT5's "Experts" tab for more details when an order is rejected.
#'
#' @return
#' Returns \code{TRUE} if the order was successfully sent, \code{FALSE} otherwise.
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#'
#' @seealso
#' [mt5R::MT5.DeleteOrder()], [mt5R::MT5.ModifyOrder()], [mt5R::MT5.MultipleOrders()], [mt5R::MT5.ShowOrders()], [mt5R::MT5.ClosePosition()], [mt5R::MT5.ModifyPosition()], [mt5R::MT5.ShowPositions()]
#'
#' @references
#' \url{https://www.mql5.com/en/docs/constants/tradingconstants/orderproperties}
#'
#' \url{https://www.investopedia.com/terms/m/marketorder.asp}
#'
#' @export
#' @examples
#' \dontrun{
#' ## Sell market order. Using arguments
#' MT5.SingleOrder("EURUSD", iCmd = 1, fVol =  0.01)
#'
#' ## Sell market order, same above. Omitting arguments
#' MT5.SingleOrder("EURUSD", 1, 0.01)
#'
#' ## Buy market order, using stop. fStop need to be lower than symbol's ask price, otherwise the order will be rejected.
#' MT5.SingleOrder("EURUSD", 0, 0.01, fStop = 1.200) ## Last price: 1.2242 18-12-20. 1.200 < 1.2242 = OK
#'
#' ## Sell pending order
#' MT5.SingleOrder("EURUSD", iCmd = 1, fVol =  0.01, fPrice = 1.2334)
#'
#' ## Pending Order
#' MT5.SingleOrder("EURUSD", iCmd = 1, fVol =  0.01, fPrice = 1.18320) ## Sell Pending Order
#' MT5.SingleOrder("EURUSD", iCmd = 0, fVol =  0.01, fPrice = 1.18320, fStop = 1.18200) ## Buy Pending Order with stop
#' MT5.SingleOrder("EURUSD", iCmd = 1, fVol =  0.01, fPrice = 1.18720, fStop = 1.18790) ## Sell Pending Order with stop
#'
#' }
MT5.SingleOrder <- function(sSymbol, iCmd, fVol, fPrice = 0, fStop = 0, fGain = 0, iFillType = 0)
{
  sSymbol <- as.character(sSymbol)
  iCmd <- as.integer(iCmd)
  fVol <- as.numeric(fVol)
  fPrice <- as.numeric(fPrice)
  fStop <- as.numeric(fStop)
  fGain <- as.numeric(fGain)
  iFillType <- as.integer(iFillType)

  if(fPrice == 0) iFillType <- 1;

  sRequest <- utils::read.table(text = MT5.Connect(base::paste0("O1 ", paste(
    sSymbol, iCmd, fVol, fPrice, fStop, fGain, iFillType,
    sep = "?"))), sep = ";")

  if(as.character(sRequest[1][[1]]) == "O1ERROR")
  {
    return(FALSE)
  }
  return(TRUE)
}

#' Symbol type
#'
#' @description
#' This function is useful to know what kind of asset target symbol is.
#'
#' It can be used for several symbols at once using a character vector.
#'
#' This function will automatically manage if the \code{sSymbol} is in marketwatch. Not advisable for huge vectorized uses, use a loop instead for safer handling.
#'
#' @param sSymbol character(); target symbols.
#'
#' @return
#' Returns \code{int} vector, with follow information:
#' \itemize{
#'   \item{-1: not found}
#'   \item{0: stock}
#'   \item{1: options}
#'   \item{2: future contracts}
#'   \item{3: Forex}
#'   \item{4: CFD}
#'   }
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#'
#' MT5.SymbolType("EURUSD")
#'
#' ## Returns
#' # 3
#'
#' MT5.SymbolType(c("CADJPY", "UK Oil"))
#'
#' ## Returns
#' # 3 4
#'
#' }
MT5.SymbolType <- function(sSymbol)
{
  if(base::length(sSymbol) < 1)
  {
    stop("Empty vector")
  }
  stopifnot(is.character(sSymbol))

  sSymbolsNotInMW <- as.character()
  for(i in seq_along(sSymbol))
  {
    if(!MT5.SymbolInMarketwatch(sSymbol[i]))
    {
      MT5.MarketwatchAdd(sSymbol[i])
      sSymbolsNotInMW <- append(sSymbolsNotInMW, sSymbol)
    }
  }
  if(length(sSymbolsNotInMW)>0)Sys.sleep(0.1); ##unfortunately need to MT5 process the changes

  sTextToSent <- SUP_MT5_StackRequests(sSymbol, "M3")
  sRecived <- utils::read.table(text = MT5.Connect(sTextToSent), sep = ";", colClasses = c("character"))
  df_temp <- as.data.frame(lapply(sRecived, function(x) utils::read.table(text = x, sep = "?")), stringsAsFactors = F)
  names(df_temp) <- c("Name", "Type")

  if(length(sSymbolsNotInMW)>0)MT5.MarketwatchRemove(sSymbolsNotInMW);

  return(df_temp$Type)
}

#' Check mt5R version in R and MT5
#'
#' @description
#' This function is useful to verify both versions of mt5R. It is advisable to use the same version in R and MT5.
#'
#' In case of \code{FALSE} return consider to update your mt5R package or mt5R EA, check for warning messages in this case.
#'
#' @return
#' Returns \code{TRUE} if MT5 and R have the same version of mt5R, \code{FALSE} otherwise.
#'
#' A warning message will be displayed in case of \code{FALSE} return.
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
MT5.CheckVersion <- function()
{
  sRecived <- utils::read.table(text = MT5.Connect("S3"), sep = ";", colClasses = c("character"))
  sVersion_MT5 <- as.character(sRecived)
  sVersion_R <- utils::packageVersion("mt5R")

  if(sVersion_MT5 == sVersion_R)
  {
    print(paste0("Everything OK! MT5 version: '", sVersion_MT5, "' R version: '",sVersion_R,"'"))
    return(TRUE)
  }else
  {
    warning(paste0("! Different versions of mt5R! MT5 version: '", sVersion_MT5, "' R version: '",sVersion_R,"'"))
    return(FALSE)
  }
}

#' Fetch all symbols
#'
#' @description
#' Fetch all symbols, even those not on Marketwatch.
#'
#' This function may take time.
#'
#' @return
#' Returns \code{character()} of all symbols available in MT5.
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#'
#' MT5.AllSymbols()
#'
#' }
MT5.AllSymbols <- function()
{
  sReq <- MT5.Connect("P4")
  return(as.character(utils::read.table(text = paste0(sReq, collapse = "?"), sep = "?", skipNul = TRUE, quote="", fill = TRUE, comment.char = "")))
}

#' Expiration symbol date
#'
#' @description
#' Obtain expiration date of target \code{sSymbol}.
#'
#' @param sSymbol character; target symbol.
#'
#' @return
#' Returns \code{Date} if symbol has date of expiration, \code{NA} otherwise.
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
MT5.SymbolExpiration <- function(sSymbol)
{
  stopifnot(is.character(sSymbol))
  sRecived <- MT5.Connect(paste("M6", sSymbol))

  if(sRecived[[1]] == "M6ERROR")
  {
    stop(base::paste0(sSymbol, ": symbol was not found? \nCheck if symbol is in MT5's marketwatch. Check ?MT5.Marketwatch, ?MT5.SymbolInMarketwatch, ?MT5.MarketwatchAdd"))
  }

  if(sRecived == "0")return(NA);
  return(base::as.Date(sRecived, format = "%Y.%m.%d"))
}

#' MT5 server time
#'
#' @description
#' Obtain MT5 server time.
#'
#' @return
#' Returns \code{POSIXct} date-time of MT5 server time. It may **not** be local machine time.
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#'
#' dtServerTime <- MT5.ServerTime()
#'
#' ## Extract hour
#' strftime(dtServerTime,format="%H")
#'
#' ## Extract day
#' strftime(dtServerTime,format="%d")
#'
#' }
MT5.ServerTime <- function()
{
  sRecived <- utils::read.table(text = MT5.Connect("M7"), sep = "?")
  return(base::ISOdatetime(sRecived[1], sRecived[2], sRecived[3], sRecived[4], sRecived[5], sRecived[6]))
  if(sRecived == "0")return(NA);
  return(base::as.Date(sRecived, format = "%Y.%m.%d"))
}

#' Load Times & Sales table
#'
#' @description
#' Function to load Times & Sales table from MT5 of target symbol.
#'
#' Times & Sales table provides in-depth trading data, including records on time, direction, price and volume of executed trades.
#'
#' Undefined direction transaction appears as \code{N/A} in MT5's Times & Sales table. Those transactions are automatically removed by default (\code{bIgnoreNAs = TRUE}). See References.
#'
#' @param sSymbol character; target symbol.
#' @param iRows int; how many rows. It's start from last. (default: \code{10})
#' @param bIgnoreNAs bool; ignore \code{NA} type in Times & Series of MT5 table. See references. (default: \code{TRUE})
#'
#' @return
#' Returns \emph{Data.frame} \eqn{[nx4]}, with follow informations:
#' \itemize{
#'   \item Datetime \code{{POSIXct}}: Datetime of trade executed.
#'   \item Type \code{{int}}: \code{0} for Buy trade and \code{1} for Sell trade.
#'   \item Price \code{{numeric}}: Price of trade executed.
#'   \item Volume \code{{int}}: Volume of trade executed.
#'   }
#'
#' @references
#' \url{https://www.metatrader5.com/en/terminal/help/trading/depth_of_market}
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
#' @export
MT5.GetTimesSales <- function(sSymbol, iRows = 10, bIgnoreNAs = TRUE)
{
  if(base::length(sSymbol) > 1)
  {
    warning("MT5.GetTimeSales only works with one symbol at a time!")
    sSymbol <- sSymbol[1]
  }
  sSymbol <- as.character(sSymbol)
  iRows <- as.integer(iRows)
  iRows <- ifelse(iRows < 1, 1, iRows)
  stopifnot(!is.na(iRows))

  sRequest <- MT5.Connect(base::paste0("P6 ", paste(
    sSymbol, iRows,
    sep = "?")))

  if(sRequest[[1]] == "P6ERROR")
  {
    warning(base::paste0(sSymbol, ": error! Check Expert tab in MT5 to more details!"))
    return(data.frame())
  }else if(sRequest[[1]] == "P6ERROR2")
  {
    warning(base::paste0(sSymbol, ": symbol was not found? \nCheck if symbol is in MT5's marketwatch. Check ?MT5.Marketwatch, ?MT5.SymbolInMarketwatch, ?MT5.MarketwatchAdd"))
    return(data.frame())
  }else if(sRequest[[1]] == "P6ERROR3")
  {
    warning(base::paste0(sSymbol, ": there is no Times & Sales table"))
    return(data.frame())
  }

  sStringTable <- utils::read.table(text = sRequest, sep = "?", stringsAsFactors = F)
  sStringTable <- lapply(sStringTable, function(x){utils::read.table(text = x, sep = " ", stringsAsFactors = F)})

  ## Removing NA rows
  NA_Rows <- as.logical(unlist(lapply(sStringTable, length)) > 5)

  if(!bIgnoreNAs)
  {
    sStringTable[which(NA_Rows)] <- lapply(sStringTable[which(NA_Rows)], function(x){x[-c(4,5)]})

    for(i in 1:length(sStringTable))
    {
      colnames(sStringTable[i][[1]]) <- 1:5
    }

    Unprocessed_Table <- do.call(rbind, sStringTable)
    Unprocessed_Table[which(NA_Rows),3] <- NA
  }else
  {
    if(any(NA_Rows == TRUE))
    {
      warning("NA rows from MT5 are been detected and have been removed.")
    }

    Unprocessed_Table <- do.call(rbind, sStringTable[!NA_Rows])
  }

  df <- data.frame(Datetime = as.POSIXct(paste(Unprocessed_Table[,1], Unprocessed_Table[,2]), format = "%Y.%m.%d %H:%M.%OS"),
                   Type = Unprocessed_Table[,3],
                   Price = Unprocessed_Table[,4],
                   Volume = Unprocessed_Table[,5])

  if(any(complete.cases(df))==F)
  {
    warning("NA dates detected and have been removed.")
    df <- df[complete.cases(df),]
  }

  return(df)
}

#' Exemple function
#'
#' @description
#' Example function to help other to create their own functions.
#'
#' Note that we use "?" to subdivide the request.
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
MT5.zExample <- function()
{
  ## Which command we will send
  sTextToSent <- "Z1 Hello?World"

  ## We will send the request above and we will recive back
  sTextRecived <- MT5.Connect(sTextToSent)
  ## [1] "Hello?To?You?Too"

  ## Now we can manipulate the text recived
  sTextRecived_Transformed <- utils::read.table(text = sTextRecived, sep = "?", colClasses = c("character"))
  #      V1 V2  V3  V4
  # 1 Hello To You Too

  ## Hello to you too
  base::print(paste(sTextRecived_Transformed, collapse = " ")) ## (:
}

#' Support function of mt5R package
#'
#' @description
#' This function is used in others mt5R functions to concatenate strings and save space, so they look more elegant! Yay
#'
#' Into further updates this function probably will be retired.
#'
#' @author Guilherme Kinzel, \email{guikinzel@@gmail.com}
SUP_MT5_StackRequests <- function(sVetor, sOrdem)
{
  sTextToSend <- as.character()
  for(i in seq_len(base::length(sVetor)))
  {
    sOrder <- base::paste0(sOrdem," ",sVetor[i])
    if(i == 1)
    {
      sTextToSend <- sOrder
    }else
    {
      sTextToSend <- base::paste0(sTextToSend, "@", sOrder)
    }
  }
  return(sTextToSend)
}
