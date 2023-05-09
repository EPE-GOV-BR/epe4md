#' Realiza a projecao da capacidade instalada de micro e minigeracao distribuida
#'
#' [epe4md::epe4md_proj_adotantes].
#' @param proj_adotantes data.frame. Resultado da função
#' @param lista_adotantes data.frame. Data.frame que contém dados relativos aos
#' adotantes.
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param dir_dados_premissas Diretório onde se encontram as premissas.
#' Se esse parâmetro não for passado, a função usa os dados default que são
#' instalados com o pacote. É importante que os nomes dos arquivos sejam os
#' mesmos da pasta default.
#'
#' @return list com dois data.frames. "proj_potencia" possui os resultados da
#' projeção de capacidade instalada de micro e minigeração distribuída.
#' "part_adotantes" possui o resultado em termos de participação do
#' número de adotantes frente ao total de unidades consumidoras.
#' @export
#'
#' @import tidyr
#' @import dplyr
#'
#' @encoding UTF-8
#'
#' @examples
#'
#' lista_adotantes <- list(
#'   structure(
#'     list(nome_4md = c("CEB", "EQUATORIAL MA", "LIGHT"),
#'          segmento = c("comercial_at_remoto", "comercial_bt", "comercial_at_remoto"),
#'          p = c(0.000314088483901221, 0.000326288160094334, 0.01),
#'          q = c(0.860602105239079, 0.796618861208451, 0.0771175699573262),
#'          spb = c(0.3, 0.3, 0.3),
#'          ano = c(2024, 2028, 2022),
#'          Ft = c(0.917955265426745, 0.992968921614641, 0.137575817915978),
#'          consumidores = c(12146, 47955, 22878),
#'          payback = c(5.00924084667372, 4.40202323859729, 8.53878219211533),
#'          mercado_potencial = c(675.75, 3200.75, 441.5),
#'          adotantes_ano = c(309, 0, 4),
#'          adotantes_acum = c(2361, 0, 42),
#'          fonte_resumo = c("Fotovoltaica", "Eólica", "Fotovoltaica"),
#'          part_fonte = c(1, 0, 0.186666666666667),
#'          adotantes_hist = c(NA_real_, NA_real_, NA_real_)),
#'     row.names = c(NA, -3L),
#'     class = c("tbl_df", "tbl", "data.frame")),
#'   structure(
#'     list(ano = c(2030, 2016, 2020),
#'          segmento = c("comercial_bt", "residencial", "residencial"),
#'          adotantes = c(694432, 7214, 357386),
#'          mercado_potencial = c(620539, 857786, 2931122),
#'          total_ucs = c(14858570, 66148593.0147109, 71001451.6735913),
#'          penetracao_total = c(0.0467361260202025, 0.000109057497238009, 0.00503350271826806),
#'          mercado_nicho = c(2541888, 10668970, 11014026),
#'          penetracao_nicho = c(0.273195357151849, 0.000676166490298501, 0.0324482618798975),
#'          penetracao_potencial = c(1.11907873638885, 0.00841002301273278, 0.121928053489415)),
#'  row.names = c(NA, -3L),
#'     class = c("tbl_df", "tbl", "data.frame"))
#' )
#'
#' names(lista_adotantes) <- c("proj_adotantes", "part_adotantes")
#'
#' proj_potencia <- epe4md_proj_potencia(
#'   lista_adotantes = lista_adotantes,
#'   ano_base = 2021,
#'   dir_dados_premissas = NA_character_
#' )

utils::globalVariables(c("pot_media", "pot_hist", "geracao_gwh"))

epe4md_proj_potencia <- function(lista_adotantes,
                                 ano_base,
                                 dir_dados_premissas = "inst/dados_premissas") {

  dir_dados_premissas <- if_else(
    dir_dados_premissas == "inst/dados_premissas",
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )


  dados_gd <-
    readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx"))

  proj_adotantes <- lista_adotantes$proj_adotantes

  potencia_tipica <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/potencia_tipica.xlsx"))

  potencia_media <- dados_gd %>%
    group_by(nome_4md, segmento, fonte_resumo) %>%
    summarise(pot_total = sum(potencia_instalada_k_w),
              adotantes_total = sum(qtde_u_csrecebem_os_creditos),
              pot_media = pot_total / adotantes_total) %>%
    ungroup() %>%
    complete(nome_4md, segmento, fonte_resumo) %>%
    group_by(segmento, fonte_resumo) %>%
    mutate(pot_media = ifelse(is.na(pot_media),
                              mean(pot_media, na.rm = TRUE),
                              pot_media)) %>%
    ungroup() %>%
    select(nome_4md, segmento, fonte_resumo, pot_media) %>%
    left_join(potencia_tipica, by = "segmento",
              multiple = "all") %>%
    mutate(pot_media = ifelse(is.na(pot_media), pot_sistemas, pot_media)) %>%
    select(-pot_sistemas)

  proj_potencia <- left_join(proj_adotantes, potencia_media,
                             by = c("nome_4md", "segmento", "fonte_resumo"),
                             multiple = "all")


  # historico de adotantes para substituir anos iniciais da projeção

  historico_pot_fontes <- dados_gd %>%
    filter(ano <= ano_base) %>%
    group_by(ano, nome_4md, segmento, fonte_resumo) %>%
    summarise(pot_hist = sum(potencia_instalada_k_w)) %>%
    ungroup() %>%
    complete(ano, nome_4md, segmento, fonte_resumo) %>%
    mutate(pot_hist = ifelse(is.na(pot_hist), 0, pot_hist))


  proj_potencia <- proj_potencia %>%
    mutate(pot_ano = adotantes_ano * pot_media)

  proj_potencia <- left_join(proj_potencia, historico_pot_fontes,
                             by = c("nome_4md", "segmento",
                                    "ano", "fonte_resumo"),
                             multiple = "all")

  proj_potencia <- proj_potencia %>%
    mutate(pot_ano = ifelse(ano <= ano_base, pot_hist, pot_ano)) %>%
    ungroup()

  proj_potencia <- proj_potencia %>%
    mutate(pot_ano_mw = pot_ano / 1000) %>%
    group_by(nome_4md, segmento, fonte_resumo) %>%
    arrange(ano) %>%
    mutate(pot_acum_mw = cumsum(pot_ano_mw)) %>%
    ungroup()


  #lista resultados

  lista_potencia <- list(proj_potencia, lista_adotantes$part_adotantes)

  names(lista_potencia) <- c("proj_potencia", "part_adotantes")

  lista_potencia

}

