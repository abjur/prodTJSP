[![Travis-CI Build Status](https://travis-ci.org/abjur/prodTJSP.svg?branch=master)](https://travis-ci.org/abjur/prodTJSP)

# prodTJSP

Scraper TJSP productivity data

## Install

```r
if (!require(devtools)) install.packages('devtools')
devtools::install_github('abjur/prodTJSP')
```

### Download court list

```r
d_list <- prod_download_courts()
```

### Prepare list of downloads

```r
d_list <- prod_list()
```

### Download pdf files

(don't run this, it takes hours)

```r
prod_download(d_list, path = 'data-raw/pdf')
```

### Parse pdf files

Not implemented yet.

## TODO

[ ] Parse pdf files
[ ] Better docs
