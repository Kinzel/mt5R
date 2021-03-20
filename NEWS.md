mt5R v0.1.5 (Release date: 2021-03-20)
==============

* Fixed `MT5.GetSymbol()` and `MT5.Quick_GetSymbol()` when the symbol is listed in MT5 but doesn't have data. These are very exceptional cases, though they exists.

mt5R v0.1.4 (Release date: 2021-02-13)
==============

* Added `MT5.GetTimesSales()`.
* Fixed `MT5.AllSymbols()`. Some brokers was returning error message.
* Fixed `MT5.Quick_GetSymbol()`. The function was returning error if rows requested are bigger than the table available in MT5. It was happening more frequently in very recently IPO companies.
* Added EURUSD example in **Articles** tab.

mt5R v0.1.3 (Release date: 2021-01-30)
==============

* Added `MT5.SymbolExpiration()`.
* Added `MT5.ServerTime()`.
* Fixed `MT5.AllSymbols()`.
* `MT5.GetSymbol()` now accepts `iRows = Inf`.
* `MT5.MarketIsOpen()` now checks if internet is working as well.
* `MT5.BidAskBook()` and `MT5.BidAskSpread()` now are adapted to work on auctions.
* Changed `SUB_REQUEST_DIVISOR` from `!` to `#`. Check [forum](https://github.com/Kinzel/mt5R/discussions/6).
* Added layers of sanity checks in data-obtain functions.
* More blocks of texts was translated to english.
* General bugs fixed and documentation updated.

mt5R v0.1.2 (Release date: 2021-01-16)
==============

* Added `MT5.SymbolInMarketwatch()`.
* Added `MT5.CheckVersion()`.
* Fixed `MT5.MarketIsOpen()`. Check ?MT5.MarketIsOpen for more details.

Some functions were updated to newer `unlist()` of latest versions of R. All functions below now are coerced to return logical when is applicable.

* `MT5.MarketwatchAdd()`
* `MT5.MarketwatchRemove()`
* `MT5.MarketwatchRemove()`
* `MT5.ModifyPosition()`
* `MT5.ModifyOrder()`
* `MT5.RemoveAllChartsObjects()`
* `MT5.ClosePosition()`
* `MT5.DeleteOrder()`
* `MT5.DrawHorizontalLine()`


mt5R v0.1.1 (Release date: 2021-01-08)
==============

* Added `MT5.AllSymbols()`.

mt5R v0.1.0 (Release date: 2020-12-24)
==============

Uploaded to GitHub. Experimental use only.
