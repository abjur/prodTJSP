prod_comarca_npag <- function() {
  j0 <- prod_comarca_pag(1)
  npags <- stringr::str_match(j0$d$Message, 'total de [^0-9]+([0-9]+)')[,2]
  ceiling(as.numeric(npags) / 10)
}

prod_comarca_pag <- function(pag) {
  id <- (pag - 1) * 10
  u_ajax <- 'http://www.tjsp.jus.br/produtividadeweb/WebServices/Foro.asmx/ListarForosParaRadComboBox'
  json <- sprintf('{"context":{"Text":"","NumberOfItems":%d}}', id) %>%
    jsonlite::fromJSON()
  j <- httr::POST(u_ajax, body = json, encode = 'json') %>%
    httr::content('parsed')
}


prod_download_comarca <- function() {
  pags <- seq_len(prod_comarca_npag())
  pags %>%
    purrr::map_df(function(pag) {
      pag %>%
        prod_comarca_pag() %>%
        with(d$Items) %>%
        purrr::map_df(~tibble::as_tibble(.x[sapply(.x, length) > 0]))
    }) %>%
    janitor::clean_names() %>%
    dplyr::select(nm_comarca = text, id_comarca = value)
}
