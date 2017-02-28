prod_scrape_courts <- function(h) {
  css <- '#ctl00_ctl00_cphConteudoGeral_ContentPlaceHolder1_rcbUnidade_DropDown'
  nms <- h %>%
    rvest::html_node(css) %>%
    rvest::html_nodes('li') %>%
    rvest::html_text() %>%
    purrr::discard(stringr::str_detect, pattern = 'Selecione a un')
  ids <- h %>%
    rvest::html_nodes('script') %>%
    rvest::html_text() %>%
    purrr::keep(stringr::str_detect, pattern = 'itemData') %>%
    stringr::str_match('"itemData":\\[([^\\]]+)]') %>%
    .subset2(2) %>%
    stringr::str_split(',') %>%
    unlist() %>%
    purrr::discard(stringr::str_detect, pattern = '"text"') %>%
    stringr::str_replace_all('[^0-9]', '') %>%
    purrr::discard(~.x=='')
  tibble::tibble(nm_court = nms, id_court = ids)
}
