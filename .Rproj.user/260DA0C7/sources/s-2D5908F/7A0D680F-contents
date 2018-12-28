#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#' @importFrom rlang enquo
#' @title Print Brazilian State Abbreviations to the Console
#' @export
UF <- function() {
  uf <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES",
          "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR",
          "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC",
          "SP", "SE", "TO")
  print(uf)
}

#' @title Load Simple Features DataFrame of a Brazilian State
#' @param state Two-letter abbreviation of Brazilian state name. Use \code{UF()} for
#' a list.
#' @export
select_state <- function(state) {

  uf <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES",
          "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR",
          "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC",
          "SP", "SE", "TO")

  if(!state %in% uf) stop("State not found. Try `UF()` for a list of possibilities.")

  data("br_municipalities")

  st <- rlang::enquo(state)

  br_state <- br_municipalities %>%
    dplyr::filter(state == !! st)

  return(br_state)

}

#' @title Load Simple Features DataFrame of a Brazilian Municipality
#' @param municipality Name of municipality. For a dataframe of these, plus their
#' state and ID, use \code{data("ibge")}.
#' @export
select_municipality <- function(municipality){
  mun <- enquo(municipality)

  br_mun <- br_municipalities %>%
    dplyr::filter(name == !! mun)

  return(br_mun)

}
