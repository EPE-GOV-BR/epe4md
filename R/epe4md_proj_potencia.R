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
#'@import tidyr
#'@import dplyr
#'
#'@encoding UTF-8
#' @examples
#' epe4md_proj_potencia(
#'    lista_adotantes,
#'    ano_base,
#'    dir_dados_premissas = NA_character_
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

