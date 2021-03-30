# prodTJSP <img src='man/figures/logo.png' align="right" height="138" />

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

```r
# suppose your pdf files are in data-raw/pdf folder
arqs <- dir('data-raw/pdf', full.names = TRUE)
d_prod <- prod_parse_uni(arqs)
d_prod
```

## Citation

To cite `prodTJSP`, write `citation("prodTJSP")`:


```
To cite package ‘prodTJSP’ in publications use:

  Julio Trecenti (2017). prodTJSP: Downloads TJSP productivity data. R package
  version 0.1.9000.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {prodTJSP: Download TJSP Productivity Data},
    author = {Julio Trecenti},
    year = {2017},
    note = {R package version 0.1.9000},
  }
```

## TODO 

- [ ] Parse PDF files for judges
- [ ] Better docs
