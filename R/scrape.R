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

# parsers de pdf --------------------------------------------------------------

prod_parse_uni_one <- function(arq, save, path, ow) {
  if (save) {
    arq_save <- sprintf('%s/%s.rds', path, tools::file_path_sans_ext(basename(arq)))
    if (file.exists(arq_save) & !ow) return(tibble::tibble(result = 'exists'))
  }
  txt <- pdftools::pdf_text(arq)
  re_mag <- 'Dados +do\\(s\\) +Magistrados *\\(s\\)'
  re_unidade <- 'Dados da Unidade'
  re_banned <- c("^TJ/SP", "^Movimento Judici\\u00e1rio", "^Refer\\u00eancia:",
                 "^Foro:", "^Unidade:", "^Planilha:", "^Foro:", "^$",
                 "^\\u00c1REA:", "^FASE PR", "^J\\u00c1 AJUIZ") %>%
    stringr::str_c(collapse = '|')
  re_id <- '^[0-9]+\\.([A-Z0-9]+)?(\\.[A-Z0-9]+)*'
  re_num <- "[0-9]*$|At[e\\u00e9] +[0-9]+ +m[\\u00eae]s(es)?$|N\\u00e3o se aplica$"
  re_num_notnull <- "[0-9]+$|At[e\\u00e9] +[0-9]+ +m[\\u00eae]s(es)?$|N\\u00e3o se aplica$"
  re_txt_split <- '(  |$| [0-9]$)'
  re_txt <- '^[^ ]+ ([A-Z].+)'
  txt_clean <- txt %>%
    paste(collapse = '@@@') %>%
    stringr::str_sub(stringr::str_locate(., re_unidade)[1, 2] + 1,
                     stringr::str_locate(., re_mag)[1, 1] - 1) %>%
    stringr::str_replace_all('[0-9]+\n@@@', '') %>%
    stringr::str_split('\n') %>%
    unlist() %>%
    purrr::discard(~toupper(.x) == .x &
                     stringr::str_detect(.x, '[A-Za-z]')) %>%
    stringr::str_trim() %>%
    purrr::discard(stringr::str_detect, pattern = re_banned)
  if (all(is.na(txt_clean))) return(tibble::tibble(result = 'vazio'))
  ids <- txt_clean %>% stringr::str_extract(re_id)
  nas_num <- which(is.na(ids) & stringr::str_detect(txt_clean, '^[0-9]+$'))
  nas_txt <- which(is.na(ids) &
                     !stringr::str_detect(txt_clean, '^[0-9]+$') &
                     stringr::str_length(txt_clean) < 30) # ver vara_2011_12_075_003086
  nas_anterior <- which(is.na(ids)[-1] &
                          !is.na(dplyr::lag(ids[-1])) &
                          !stringr::str_detect(dplyr::lag(txt_clean[-1]),
                                               re_num_notnull))
  txt_clean[nas_anterior] <- paste(
    txt_clean[nas_anterior],
    txt_clean[nas_txt],
    txt_clean[nas_num]
  )
  txt_clean <- txt_clean[!is.na(ids)]
  ids <- ids[!is.na(ids)]
  nums <- txt_clean %>%
    stringr::str_extract(re_num) %>%
    stringr::str_trim()
  txts <- txt_clean %>%
    stringr::str_split_fixed(re_txt_split, 2) %>%
    magrittr::extract(TRUE, 1) %>%
    stringr::str_match(re_txt) %>%
    magrittr::extract(TRUE, 2) %>%
    stringr::str_trim()
  tab <- tibble::tibble(id = ids, txt = txts,
                        num = nums, result = 'OK')
  if (save) saveRDS(tab, arq_save)
  tab
}

#' Check if file was read correctly
#'
#' Does some tests to check if a file was read correctly.
#'
#' @param arq name of the file
#'
#' @export
arq_check <- function(arq) {
  d <- prod_parse_uni_one(arq)
  if (all(d$result %in% c('erro'))) cat('Erro: ', arq, '\n')
  if (all(d$result %in% c('vazio', 'erro'))) return(TRUE)
  titulos <- d %>%
    dplyr::filter(num == '') %>%
    with(id) %>%
    purrr::discard(stringr::str_detect, pattern = '^[0-9]+\\.$')
  length(titulos) == 0
}

#' Court productivity data
#'
#' Parses pdf file to get Court productivity data. Ignores information about the
#' judge.
#'
#' @param arqs character vector contaning the paths of the pdf files.
#' @param save save intermediate files?
#' @param path folder where the files will be saved.
#' @param overwrite overwrite?
#'
#' @export
prod_parse_uni <- function(arqs, save = FALSE,
                           path = 'data-raw/raw-rds',
                           overwrite = FALSE) {
  if (save) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  abjutils::dvec(prod_parse_uni_one, arqs,
                 save = save, path = path, ow = overwrite) %>%
    dplyr::rename(arq = item)
}

