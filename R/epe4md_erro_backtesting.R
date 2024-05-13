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

epe4md_erro_backtesting <- function(resultados_comparados,
                                       ano_backtesting) {
  
  
  resultados_comparados <- resultados_comparados %>% 
    filter(ano > ano_backtesting) %>%
    drop_na(adotantes_acum_proj) %>%
    mutate(ano_proj = ano - ano_backtesting) %>% 
    group_by(ano_proj, segmento) %>%
    summarise(mape_adotantes = mean(desvio_adotantes),
              mape_potencia = mean(desvio_potencia),
              faltantes = mean(faltantes)) %>%
    ungroup()
  
}