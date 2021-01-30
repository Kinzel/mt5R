mt5R v0.1.3 (Release date: 2021-01-30)
==============

Changes:

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

Changes:

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

Changes:

* Added `MT5.AllSymbols()`.

mt5R v0.1.0 (Release date: 2020-12-24)
==============

Uploaded to GitHub. Experimental use only.
