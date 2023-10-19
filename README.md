
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pfrscrapr

<!-- badges: start -->
<!-- badges: end -->

The pfrscrapr package is a set of functions that *politely* scrapes Pro
Football Reference IDs and other data from PFR index pages.

A dependency is RSelenium. [You can learn about how to set up RSelenium
here](https://www.youtube.com/watch?v=GnpJujF9dBw&t=10s).

Currently the function expect a Selenium server to be running locally on
your machine on port 4445. In the future I would like to update the code
to accommodate spinning up a Selenium server on demand.

## Installation

You can install the development version of pfrscrapr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("friscojosh/pfrscrapr")
```

## Examples

You can get the PFR ID for a single player by converting their name to
all lowercase and removing punctuation and suffixes:

``` r
library(pfrscrapr)
pfrscrapr::scrape_player_id("joe montana")
#> ℹ Selenium browser loaded
#> ✔ Selenium browser loaded [998ms]
#> 
#> ℹ joe montana scraped with MontJo01 returned
#> ✔ joe montana scraped with MontJo01 returned [13ms]
#> 
#> [1] "MontJo01"
```

Or you can get ID by the first letter of a player’s surname:

``` r
pfrscrapr::scrape_pfr_player_ids_by_letter("A")
#> ℹ Starting PFR scrape of players with surnames beginning with A
#> ✔ Starting PFR scrape of players with surnames beginning with A [15.5s]
#> 
#> ℹ Letter A scrape complete
#> ✔ Letter A scrape complete [16ms]
#> 
#> # A tibble: 878 × 3
#>    name                                    ids        letter
#>    <chr>                                   <chr>      <chr> 
#>  1 Isaako Aaitui (NT) 2013-2013            AaitIs00   A     
#>  2 Joe Abbey (E) 1948-1949                 AbbeJo20   A     
#>  3 Fay Abbott (BB-FB-TB-QB-WB-E) 1921-1929 AbboFa20   A     
#>  4 Vince Abbott (K) 1987-1988              abbotvin01 A     
#>  5 Jared Abbrederis (WR) 2015-2017         AbbrJa00   A     
#>  6 Duke Abbruzzi (HB-DB) 1946-1946         AbbrDu20   A     
#>  7 Mehdi Abdesmad (DE) 2016-2016           AbdeMe00   A     
#>  8 Karim Abdul-Jabbar (RB) 1996-2000       AbduKa00   A     
#>  9 Isa Abdul-Quddus (S) 2011-2016          AbduIs00   A     
#> 10 Ameer Abdullah (RB) 2015-2023           AbduAm00   A     
#> # ℹ 868 more rows
```

Finally you can scrape all IDs and other data. This takes a long time –
at least three minutes. The total number of requests to PFR is low
however: just 26, one for each letter in the alphabet. The rest of the
time is spent running regualr expressions.

``` r
# do not run
# pfrscrapr::get_pfr_ids()
```
