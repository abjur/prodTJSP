prod_url <- function() {
  "http://www.tjsp.jus.br/produtividadeweb/"
}

prod_um <- function(year, month, id_comarca, id_court, path, ow) {
  arq <- sprintf('%s/vara_%s_%02d_%03d_%06d.pdf', path, year,
                 as.numeric(month), as.numeric(id_comarca), as.numeric(id_court))
  if (!file.exists(arq) || ow) {
    suppressWarnings({
      s <- prod_url() %>%
        rvest::html_session() %>%
        prod_form(year = year) %>%
        prod_submit() %>%
        prod_check() %>%
        prod_form(month = month) %>%
        prod_submit() %>%
        prod_check() %>%
        prod_form(id_comarca = id_comarca,
                  nm_comarca = prod_build_parm('nm_comarca', id_comarca),
                  json_comarca = prod_build_parm('json_comarca', id_comarca)) %>%
        prod_submit() %>%
        prod_check() %>%
        prod_form(nm_court = prod_build_parm('nm_court', id_court),
                  json_court = prod_build_parm('json_court', id_court)) %>%
        prod_submit() %>%
        prod_check() %>%
        prod_form(nm_court = prod_build_parm('nm_court', id_court)) %>%
        prod_submit(pdf = TRUE, httr::write_disk(arq, overwrite = TRUE))
    })
  }
  tibble::tibble(result = 'OK')
}

prod_check <- function(s, label) {
  if (s$response$status_code != 200)
    stop('There are no productivity data for this combination of parameters.')
  s
}

prod_submit <- function(l, pdf = FALSE, ...) {
  if (pdf) {
    # precisa fazer isso para o rvest nao confundir os prod_element de submit!
    l$form$fields[[prod_element('btn_mag')]] <- NULL
    l$form$fields[[prod_element('btn_uni')]] <- NULL
    l$session
  }
  suppressMessages(
    rvest::submit_form(l$session, l$form, submit = NULL, ...)
  )
}

prod_build_parm <- function(type, id, comarcas = NULL) {
  # esse codigo pode ficar bem mais bonito com um pouco de paciencia
  id <- as.character(id)
  j <- '{"logEntries":[],"value":"%s","text":"%s","enabled":true,"checkedIndices":[],"checkedItemsTextOverflows":false}'
  if (type == 'json_comarca') {
    if (!is.null(comarcas)) {
      txt <- comarcas[comarcas$id_comarca==id,]$nm_comarca[1]
    } else {
      txt <- courts_internal[courts_internal$id_comarca==id,]$nm_comarca[1]
    }
    sprintf(j, id, txt)
  } else if (type == 'nm_comarca') {
    if (!is.null(comarcas)) {
      txt <- comarcas[comarcas$id_comarca==id,]$nm_comarca[1]
    } else {
      txt <- courts_internal[courts_internal$id_comarca==id,]$nm_comarca[1]
    }
    txt
  } else if (type == 'json_court' ) {
    txt <- courts_internal[courts_internal$id_court==id,]$nm_court[1]
    sprintf(j, id, txt)
  } else if (type == 'nm_court') {
    courts_internal[courts_internal$id_court==id,]$nm_court[1]
  }
}

prod_element <- function(tipo) {
  # talvez precise otimizar para funcionar com magistrados
  switch(tipo,
         year = 'ctl00$ctl00$cphConteudoGeral$ContentPlaceHolder1$ddlAno',
         month = 'ctl00$ctl00$cphConteudoGeral$ContentPlaceHolder1$ddlMes',
         id_mag = 'ctl00_ctl00_cphConteudoGeral_ContentPlaceHolder1_btnMagistrados_ClientState',
         id_comarca = 'ctl00_ctl00_cphConteudoGeral_ContentPlaceHolder1_btnUnidades_ClientState',
         nm_comarca = 'ctl00$ctl00$cphConteudoGeral$ContentPlaceHolder1$rcbForoOuMagistrado',
         json_comarca = 'ctl00_ctl00_cphConteudoGeral_ContentPlaceHolder1_rcbForoOuMagistrado_ClientState',
         nm_court = 'ctl00$ctl00$cphConteudoGeral$ContentPlaceHolder1$rcbUnidade',
         json_court = 'ctl00_ctl00_cphConteudoGeral_ContentPlaceHolder1_rcbUnidade_ClientState',
         btn_mag = 'ctl00$ctl00$cphConteudoGeral$ContentPlaceHolder1$btnMagistrados_input',
         btn_uni = 'ctl00$ctl00$cphConteudoGeral$ContentPlaceHolder1$btnUnidades_input'
  )
}

prod_form <- function(s, ...) {
  # Essa funcao atualiza os parametros do formulario
  # fazendo uso da funcao prod_element para os titulos
  # ficarem em local organizado.
  l <- list(...)
  form <- s %>%
    rvest::html_form() %>%
    dplyr::first() %>% {
      f <- function(...) rvest::set_values(., ...)
      nm <- sapply(names(l), prod_element)
      do.call(f, stats::setNames(l, nm))
    }
  list(session = s, form = form)
}



#' Build pdf list
#'
#' Builds all valid combinations of pdf available.
#'
#' @return tibble with 6 columns (year, month, id_comarca, id_court, nm_comarca,
#'   nm_court)
#' @export
prod_list <- function() {
  css <- '#ctl00_ctl00_cphConteudoGeral_ContentPlaceHolder1_ddlAno'
  years <- prod_url() %>%
    rvest::html_session() %>%
    rvest::html_node(css) %>%
    rvest::html_nodes('option') %>%
    rvest::html_text() %>%
    purrr::discard(stringr::str_detect, pattern = 'Selecione')
  months <- as.character(1:12)
  comarcas <- prod_download_comarca()
  d_list <- tibble::tibble(year = years, month = list(months)) %>%
    tidyr::unnest(month) %>%
    dplyr::mutate(x = purrr::map(seq_len(nrow(.)), ~courts_internal)) %>%
    tidyr::unnest(x) %>%
    dplyr::select(year, month, dplyr::starts_with('id'),
                  dplyr::starts_with('nm')) %>%
    dplyr::mutate(year = as.numeric(year), month = as.numeric(month))
  d_list
}

#' Downloads productivity pdf files
#'
#' Downloads TJSP productivity pdf files based on database returned by \code{prod_list}.
#'
#' @param .data database of id's returned by \code{prod_list}
#' @param path folder where the files will be saved.
#' @param overwrite overwrite?
#'
#' @return tibble informing if each download was OK.
#' @export
prod_download <- function(.data, path = 'data-raw/pdf', overwrite = FALSE) {
  if (missing(.data)) .data <- prod_list()
  f <- dplyr::failwith(tibble::tibble(result = 'erro'), prod_um)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  .data %>%
    dplyr::group_by(year, month, id_comarca, id_court) %>%
    dplyr::do(f(.$year, .$month, .$id_comarca,
                .$id_court, path, overwrite)) %>%
    dplyr::ungroup()
}

prod_download_list <- function(l) {
  prod_um(l$year, l$month, l$id_comarca, l$id_court, l$path, l$ow)
}

build_cmd <- function(l, path, overwrite) {
  sprintf("R -e 'prodTJSP:::prod_um(%s, %s, %s, %s, \"%s\", %s)'",
          l$year, l$month, l$id_comarca, l$id_court,
          normalizePath(path), overwrite)
}

#' Downloads productivity pdf files, async
#'
#' Downloads TJSP productivity pdf files based on database returned by \code{prod_list}.
#'
#' @param .data database of id's returned by \code{prod_list}
#' @param max_nproc maximum number of processes. Default 10 processes.
#' @param sleep_secs seconds to sleep between process checks. Default 1 second.
#' @param path folder where the files will be saved.
#' @param overwrite overwrite?
#'
#' @return nothing
#' @export
prod_download_async <- function(.data, max_nproc = 10, sleep_secs = 1,
                                path = 'data-raw/pdf', overwrite = FALSE) {
  if (missing(.data)) .data <- prod_list()
  f <- dplyr::failwith(tibble::tibble(result = 'erro'), prod_um)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  i <- 1
  cmd <- build_cmd(.data[1,], path, overwrite)
  ps <- lapply(seq_len(max_nproc), function(x) processx::process$new('ls'))
  ticks <- dplyr::progress_estimated(nrow(.data))
  while (i <= nrow(.data)) {
    for (j in seq_len(max_nproc)) {
      if (!ps[[j]]$is_alive()) {
        ps[[j]]$kill()
        ps[[j]] <- processx::process$new(commandline = cmd)
        i <- i + 1
        print(ticks$tick())
        cmd <- build_cmd(.data[i,], path, overwrite)
        if (i > nrow(.data)) break
      }
    }
    Sys.sleep(sleep_secs)
  }
  invisible(NULL)
}
