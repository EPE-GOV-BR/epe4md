#' Calcula o montante investido
#'
#' @param resultados_mensais. Resultado da função [epe4md::epe4md_calcula].
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
#'@import deflateBR
#'
#'@encoding UTF-8
#'
#' @examples


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
  
  combinacoes <- custo_outras %>% 
    select(fonte_resumo) %>% 
    expand(fonte_resumo, ano = 2013:2050)
  
  seq_anos <- tibble(ano = seq(2013, ano_base, 1))
  
  custo_outras <- crossing(custo_outras, seq_anos)
  
  custo_outras <- custo_outras %>% 
    mutate(mes_final = str_glue("12/{ano}"))
  
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

  potencia_custos <- potencia_custos %>%
    mutate(custo_unitario = ifelse(is.na(custo_unitario),
                                   custo,
                                   custo_unitario)) %>%
    select(- custo)

  potencia_custos <- potencia_custos %>%
    mutate(investimento_ano_milhoes = pot_ano_mw * custo_unitario)

  potencia_custos

}
