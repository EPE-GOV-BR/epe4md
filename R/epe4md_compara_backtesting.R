#' Calcula o erro da projeção em relação aos dados verificados.
#'
#' @param proj_mensal dataframe. Resultado da função
#' [epe4md::epe4md_proj_mensal].
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param dir_dados_premissas Diretório onde se encontram as premissas.
#' Se esse parâmetro não for passado, a função usa os dados default que são
#' instalados com o pacote. É importante que os nomes dos arquivos sejam os
#' mesmos da pasta default.
#'
#' @return data.frame com os resultados da projeção de capacidade instalada
#' de micro e minigeração distribuída, número de adotantes e geração
#' mensal de energia.
#'
#' @export
#'
#'@import lubridate
#'@import tidyr
#'@import readxl
#'@import dplyr
#'
#'@encoding UTF-8
#'
#' @examples
#' 

epe4md_compara_backtesting <- function(resultados_mensais,
                                    ano_base,
                                    ano_backtesting,
                                    dir_dados_premissas = NA_character_) {
  
  
  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )
  
  dados_gd <-
    readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx")) %>% 
    filter(ano <= ano_base)
  
  dados_historicos <- dados_gd %>%
    group_by(ano, segmento, nome_4md) %>%
    summarise(adotantes_ano = sum(qtde_u_csrecebem_os_creditos),
              pot_ano_mw = sum(potencia_mw)) %>%
    ungroup() %>%
    group_by(segmento, nome_4md) %>% 
    mutate(adotantes_acum_hist = cumsum(adotantes_ano),
           pot_acum_mw_hist = cumsum(pot_ano_mw)) %>% 
    select(ano, segmento, nome_4md, adotantes_acum_hist, pot_acum_mw_hist) %>% 
    ungroup()
  
  projecao <- resultados_mensais %>%
    group_by(ano, segmento, nome_4md) %>%
    summarise(adotantes_ano = sum(adotantes_mes),
              pot_ano_mw = sum(pot_mes_mw)) %>%
    ungroup() %>%
    group_by(segmento, nome_4md) %>%
    mutate(adotantes_acum_proj = cumsum(adotantes_ano),
           pot_acum_mw_proj = cumsum(pot_ano_mw)) %>% 
    select(ano, segmento, nome_4md, adotantes_acum_proj, pot_acum_mw_proj) %>% 
    ungroup()
  
  comparacao <- dados_historicos %>% 
    left_join(projecao, by = c("ano", "segmento", "nome_4md")) %>%
    mutate(desvio_adotantes = abs((adotantes_acum_hist - adotantes_acum_proj) /
                                    adotantes_acum_hist),
           desvio_potencia = abs((pot_acum_mw_hist - pot_acum_mw_proj) /
                                   pot_acum_mw_hist),
           faltantes = sum(is.na(adotantes_acum_proj))) 
  
  
}