#' Calcula o montante investido
#'
#' @param resultados_mensais Resultado da função [epe4md::epe4md_calcula].
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param ano_max_resultado numeric. Ano final para apresentação dos resultados.
#' Máximo igual a 2050. Default igual a 2050.
#' @param dir_dados_premissas Diretório onde se encontram as premissas.
#' Se esse parâmetro não for passado, a função usa os dados default que são
#' instalados com o pacote. É importante que os nomes dos arquivos sejam os
#' mesmos da pasta default.
#'
#'
#' @return data.frame com o montante estimado de investimentos relativos à
#' expansão da capacidade instalada de micro e minigeração distribuída.
#' @export
#'
#'@import tidyr
#'@import readxl
#'@import dplyr
#'
#'@encoding UTF-8
#'
#' @examples
#'
#' resultados_mensais <- tibble(
#'    data = c('2021-01-01', '2021-01-01', '2021-01-01'),
#'    ano = 2021,
#'    nome_4md = c("ENEL RJ", "ENF", "LIGHT"),
#'    subsistema = "SE",
#'    uf = "RJ",
#'    segmento = "comercial_at",
#'    fonte_resumo = "Fotovoltaica",
#'    energica_mwh = c(2019.6225, 69.06433, 2303.18754),
#'    energica_autoc_mwh = c(1615.698, 55.25146, 1842.55003),
#'    energia_inj_mwh = c(403.92450, 13.81287, 460.63751),
#'    energia_mwmed = c(2.714546380, 0.092828390, 3.095682180),
#'    pot_mes_mw = c(1.5275, 0.0000, 3.09568218),
#'    adotantes_mes = c(21, 0, 4),
#'    p = c(0.00027826110, 0.00060785180, 0.00137570060),
#'    q = c(1.0000000, 1.0000000, 0.5282319)
#' )
#'
#' epe4md_investimentos(
#'    resultados_mensais = resultados_mensais,
#'    ano_base = 2021,
#'    ano_max_resultado = 2050,
#'    dir_dados_premissas = NA_character_
#' )

utils::globalVariables(c("custo_unitario", "taxa_crescimento_mercado"))

epe4md_investimentos <- function(resultados_mensais,
                                 ano_base,
                                 ano_max_resultado = 2050,
                                 dir_dados_premissas = NA_character_ ) {


  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )


  proj_potencia <- resultados_mensais %>%
    group_by(ano, segmento, fonte_resumo) %>%
    summarise(pot_ano_mw = sum(pot_mes_mw)) %>%
    ungroup()


# Investimentos ------------------------------------------------------------

  custo_outras <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/capex_historico_outras.xlsx")) %>%
    rename(custo_outras = custo_unitario)

  custo_fv <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/custos.xlsx")) %>%
    mutate(fonte_resumo = "Fotovoltaica")

  potencia_custos <- left_join(proj_potencia, custo_fv,
                               by = c("ano", "fonte_resumo", "segmento"),
                               multiple = "all")

  potencia_custos <- left_join(potencia_custos, custo_outras,
                               by = "fonte_resumo",
                               multiple = "all")

  potencia_custos <- potencia_custos %>%
    mutate(custo_unitario = ifelse(is.na(custo_unitario),
                                   custo_outras,
                                   custo_unitario)) %>%
    select(- custo_outras)

  potencia_custos <- potencia_custos %>%
    mutate(investimento_ano_milhoes = pot_ano_mw * custo_unitario)

  potencia_custos

}
