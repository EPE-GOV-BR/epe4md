#' Calcula o montante investido
#'
#' @param resultados_mensais. Resultado da função [epe4md::epe4md_calcula].
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param ano_max_resultado numeric. Ano final para apresentação dos resultados.
#' Máximo igual a 2060. Default igual a 2060.
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
#'@import deflateBR
#'
#'@encoding UTF-8
#'
#' @examples
#' \dontrun{
#' # Executa o cálculo para obter os resultados mensais
#' resultados <- epe4md_calcula(
#'   premissas_reg = readxl::read_xlsx("dados_premissas/2024/premissas_reg.xlsx"),
#'   ano_base = 2024,
#'   dir_dados_premissas = "dados_premissas/2024"
#' )
#'
#' # Calcula os investimentos a partir dos resultados
#' investimentos <- epe4md_investimentos(
#'   resultados_mensais = resultados,
#'   ano_base = 2024,
#'   ano_max_resultado = 2060,
#'   dir_dados_premissas = "dados_premissas/2024"
#' )
#' head(investimentos)
#' }

epe4md_investimentos <- function(resultados_mensais,
                                 ano_base,
                                 ano_max_resultado = 2060,
                                 dir_dados_premissas = NA_character_ ) {


  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )

  # Verifica se a simula_bateria == TRUE (variáveis de baterias só estão contidas em resultados_mensais se sim)
  if("cap_bateria_mwh" %in% colnames(resultados_mensais)){
    proj_potencia <- resultados_mensais %>%
      group_by(ano, segmento, fonte_resumo) %>%
      summarise(pot_ano_mw = sum(pot_mes_mw),
                cap_bateria_mwh = sum(cap_bateria_mwh)) %>%
      ungroup()
  } else {

  proj_potencia <- resultados_mensais %>%
    group_by(ano, segmento, fonte_resumo) %>%
    summarise(pot_ano_mw = sum(pot_mes_mw)) %>%
    ungroup()
  }


# Investimentos ------------------------------------------------------------

  # Cria dataframe com dados de CAPEX unitário para cada tecnologia, com exceção da fotovoltaica.
  custo_outras <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/capex_historico_outras.xlsx")) %>%
    rename(custo_outras = custo_unitario)

  combinacoes <- custo_outras %>%
    select(fonte_resumo) %>%
    expand(fonte_resumo, ano = 2013:2060)

  seq_anos <- tibble(ano = seq(2013, ano_base, 1))

  custo_outras <- crossing(custo_outras, seq_anos)

  custo_outras <- custo_outras %>%
    mutate(mes_final = str_glue("12/{ano}"))

  # Calcula o valor atualizado no tempo com base no inpc para o 12/2018.
  calcula_inflacao <- function(custo, data_final) {

    deflateBR::deflate(custo, as.Date("2018-12-01"), data_final, "inpc")

  }

  custo_outras <- custo_outras %>%
    mutate(custo_deflacionado = map2(.x = custo_outras, .y = mes_final,
                                     .f = calcula_inflacao))

  custo_outras$custo_deflacionado <- as.numeric(custo_outras$custo_deflacionado)

  custo_outras <- custo_outras %>%
    mutate(custo = round(custo_deflacionado, 2)) %>%
    select(ano, fonte_resumo, custo)

  # Cria dataframe com os custos atualizado no tempo das outras tecnologias
  # para cada tecnologia e ano de projeção
  custo_outras <- left_join(combinacoes, custo_outras,
                            by = c("fonte_resumo", "ano")) %>%
    fill(custo)


  custo_fv <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/custos.xlsx")) %>%
    mutate(fonte_resumo = "Fotovoltaica")


  potencia_custos <- left_join(proj_potencia, custo_fv,
                               by = c("ano", "fonte_resumo", "segmento"))

  potencia_custos <- left_join(potencia_custos, custo_outras,
                               by = c("ano", "fonte_resumo"))

  # Unifica os custos por tecnologia e ano
  potencia_custos <- potencia_custos %>%
    mutate(custo_unitario = ifelse(is.na(custo_unitario),
                                   custo,
                                   custo_unitario)) %>%
    select(- custo)

  # Calcula o montante de investimento
  if("cap_bateria_mwh" %in% colnames(resultados_mensais)){
  precos_bateria <- readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/precos_baterias.xlsx"))

  potencia_custos <- potencia_custos %>%
    left_join(precos_bateria, by = c("ano", "segmento"))

  potencia_custos <- potencia_custos %>%
    fill(bateria_custo, .direction = "up") %>%
    select(-bateria_oem)

  potencia_custos <- potencia_custos %>%
    mutate(investimento_ano_milhoes = pot_ano_mw * custo_unitario,
           investimento_ano_bat_milhoes = cap_bateria_mwh * bateria_custo / 10^3)
  } else {

  potencia_custos <- potencia_custos %>%
    mutate(investimento_ano_milhoes = pot_ano_mw * custo_unitario)
  }

  potencia_custos

}
