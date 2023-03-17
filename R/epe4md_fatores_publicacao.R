#' Altera fatores para facilitar entendimento em publicações
#'
#' @param dados data.frame. Base de dados para ser alterada.
#'
#' @return data.frame com nomes dos segmentos alterados.
#' @export
#'
#'@encoding UTF-8
#'
#'@import dplyr
#'
#'@examples
#'epe4md_fatores_publicacao(dados)



epe4md_fatores_publicacao <- function(dados) {

  dados <- dados %>%
    mutate(
      across(
        where(is.character),
        ~stringr::str_replace(., "comercial_bt", "Comercial (BT)"))) %>%
    mutate(
      across(
        where(is.character),
        ~stringr::str_replace(., "comercial_at_remoto",
                              "Comercial Remoto (AT/BT)"))) %>%
    mutate(
      across(
        where(is.character),
        ~stringr::str_replace(., "comercial_at", "Comercial (AT)"))) %>%
    mutate(
      across(
        where(is.character),
        ~stringr::str_replace(., "residencial_remoto",
                              "Residencial Remoto"))) %>%
    mutate(
      across(
        where(is.character),
        ~stringr::str_replace(., "residencial", "Residencial")))

}
