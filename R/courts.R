prod_download_courts_one <- function(year, month, id_comarca, comarcas) {
  suppressWarnings({
    s <- prod_url() %>%
      rvest::html_session() %>%
      prod_form(year = year) %>%
      prod_submit() %>%
      prod_form(month = month) %>%
      prod_submit() %>%
      prod_form(id_comarca = id_comarca,
                nm_comarca = prod_build_parm('nm_comarca', id_comarca, comarcas),
                json_comarca = prod_build_parm('json_comarca', id_comarca, comarcas)) %>%
      prod_submit()
  })
  s$response %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    prod_scrape_courts()
}

#' Download court list
#'
#' Download court list from TJSP website.
#'
#' @param minimal download list of courts based in 2016-01 or consider all
#'   possible year-month combinations?
#' @param save save intermediate files? Recommended if \code{minimal} is
#'   \code{FALSE}.
#' @param path folder where intermediate files will be saved.
#'
#' @return tibble with four columns and variable number of rows depending on
#'   \code{minimal} parameter.
#' @export
prod_download_courts <- function(minimal = TRUE, save = FALSE, path = 'data-raw/courts') {
  if (save) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (minimal) {
    months <- '1'
    years <- '2016'
  } else {
    months <- as.character(1:12)
    css <- '#ctl00_ctl00_cphConteudoGeral_ContentPlaceHolder1_ddlAno'
    years <- prod_url() %>%
      rvest::html_session() %>%
      rvest::html_node(css) %>%
      rvest::html_nodes('option') %>%
      rvest::html_text() %>%
      purrr::discard(stringr::str_detect, pattern = 'Selecione')
  }
  f <- dplyr::failwith(tibble::tibble(result = 'erro'), prod_download_courts_one)
  comarcas <- prod_download_comarca()
  d_courts <- tibble::tibble(year = years, month = list(months)) %>%
    tidyr::unnest(month) %>%
    dplyr::mutate(x = purrr::map(seq_len(nrow(.)), ~comarcas)) %>%
    tidyr::unnest(x) %>%
    dplyr::select(year, month, id_comarca, nm_comarca) %>%
    dplyr::group_by(year, month, id_comarca, nm_comarca) %>%
    dplyr::do({
      d <- f(.$year, .$month, .$id_comarca, comarcas)
      if (save) {
        arq <- sprintf('%s/%s_%02d_%03d.rds', path, .$year,
                       as.numeric(.$month), as.numeric(.$id_comarca))
        if (!file.exists(arq)) saveRDS(d, arq)
      }
      d
    }) %>%
    dplyr::ungroup()
  if (minimal) d_courts <- dplyr::select(d_courts, -year, -month)
  d_courts
}
