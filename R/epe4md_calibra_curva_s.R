#' Calibra o modelo de Bass com dados históricos e gera curvas S de adoção.
#'
#' @param resultado_payback data.frame. Resultado da função
#' [epe4md::epe4md_payback].
#' @param consumidores list. Resultado da função
#' [epe4md::epe4md_mercado_potencial].
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param ano_max_resultado numeric. Ano final para apresentação dos resultados.
#' Máximo igual a 2050. Default igual a 2050.
#' @param p_max numeric. Fator de inovação (p) máximo. Default igual a 0.01.
#' @param q_max numeric. Fator de imitação (q) máximo. DEfault igual a 1.
#' @param dir_dados_premissas Diretório onde se encontram as premissas. Se esse
#' parâmetro não for passado, a função usa os dados default que são instalados
#' com o pacote. É importante que os nomes dos arquivos sejam os mesmos da
#' pasta default.
#'
#' @return data.frame com curvas de difusão e mercado potencial
#' @export
#'
#'@import tidyr
#'@import purrr
#'@import readxl
#'@import dplyr
#'
#'@encoding UTF-8
#'
#' @examples

epe4md_calibra_curva_s <- function(resultado_payback,
                                   consumidores,
                                   ano_base,
                                   ano_max_resultado = 2050,
                                   p_max = 0.01,
                                   q_max = 1,
                                   dir_dados_premissas = NA_character_) {

  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )


  dados_gd <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx"))

  dados_gd <- dados_gd %>%
    filter(ano <= ano_base)

  #selecionando data.frame da lista
  consumidores <- consumidores$consumidores

  casos_otimizacao <- resultado_payback %>%
    group_by(nome_4md, segmento) %>%
    tally() %>%
    select(-n) %>%
    ungroup()

  historico <- dados_gd %>%
    group_by(nome_4md, segmento, ano) %>%
    summarise(adotantes_hist = sum(qtde_u_csrecebem_os_creditos)) %>%
    ungroup()

  resultado_payback_historico <- resultado_payback %>%
    filter(ano <= ano_base) %>%
    select(nome_4md, segmento, ano, payback,
           payback_desc, tir_nominal, tir_real)

  base_otimizacao <- left_join(resultado_payback_historico, consumidores,
                               by = c("nome_4md", "ano", "segmento"))

  base_otimizacao <- left_join(base_otimizacao, historico,
                               by = c("nome_4md", "ano", "segmento"))

  tipo_payback <- read_xlsx(
    stringr::str_glue("{dir_dados_premissas}/tipo_payback.xlsx"))
  
  base_otimizacao <- left_join(base_otimizacao, tipo_payback,
                               by = "segmento")
  
  base_otimizacao <- base_otimizacao %>%
    mutate(payback = ifelse(tipo_payback == "simples",
                            payback,
                            payback_desc))

  base_otimizacao$adotantes_hist <- base_otimizacao$adotantes_hist %>%
    replace_na(0)

  base_otimizacao <- base_otimizacao %>%
    arrange(ano) %>%
    group_by(nome_4md, segmento) %>%
    mutate(adotantes_acum = cumsum(adotantes_hist)) %>%
    ungroup() %>%
    select(nome_4md, consumidores, ano, payback, adotantes_acum, segmento) %>%
    mutate(ano = ano - 2012)
  
  fator_sbp <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/spb.xlsx"))
  
  base_otimizacao <- left_join(base_otimizacao, fator_sbp, by = "segmento")


  base_otimizacao <- base_otimizacao %>%
    rename(
      "nome_dist" = nome_4md,
      "nome_seg" = segmento
    )


  otimiza_casos <- function(nome_4md, segmento) {

    caso <- base_otimizacao %>%
      filter(nome_dist == nome_4md,
             nome_seg == segmento)


    otimiza_curva_s <- function(params, y) {
      p <- params[1]
      q <- params[2]
      spb <- y$spb
      consumidores <- y$consumidores
      t <- y$ano
      payback <- y$payback
      historico_adotantes <- y$adotantes_acum

      taxa_difusao <- (1 - exp(- (p + q) * t)) /
        (1 + (q / p) * exp(- (p + q) * t))

      mercado_potencial <- exp(-spb * payback) * consumidores

      projecao <- taxa_difusao * mercado_potencial

      erro <- sum((historico_adotantes - projecao)^2)
      return(erro)
    }


    otimizador <- stats::optim(c(0.005, 0.3), otimiza_curva_s, y = caso,
                        method = "L-BFGS-B",
                        lower = c(0.0001, 0.01),
                        upper = c(p_max, q_max),
                        control = list(parscale = c(0.005, 0.3)))

    parametros <- tibble::enframe(otimizador$par)


    parametros <- pivot_wider(parametros, names_from = name,
                              values_from = value) %>%
      rename("p" = 1,
             "q" = 2)

  }

  casos_otimizados <- casos_otimizacao %>%
    mutate(saida = pmap(.l = list(nome_4md, segmento), .f = otimiza_casos)) %>%
    unnest(saida)

  casos_otimizados <- casos_otimizados %>%
    left_join(fator_sbp, by = "segmento")

  anos_simulacao <- tibble::tibble("ano" = seq(1, 38))

  casos_otimizados <- crossing(casos_otimizados, anos_simulacao)

#equação do modelo de Bass
  casos_otimizados <- casos_otimizados %>%
    mutate(Ft = (1 - exp(-(p + q) * ano)) /
             (1 + (q / p) * exp(-(p + q) * ano)))


  casos_otimizados <- casos_otimizados %>%
    mutate(ano = ano + 2012) %>%
    left_join(consumidores, by = c("nome_4md", "segmento", "ano"))

  resultado_payback <- resultado_payback %>%
    left_join(tipo_payback, by = "segmento") %>% 
    select(nome_4md, segmento, ano, payback, payback_desc, tipo_payback) %>%
    mutate(payback = ifelse(tipo_payback == "simples",
                            payback,
                            payback_desc)) %>%
    select(-payback_desc)

  casos_otimizados <- left_join(casos_otimizados, resultado_payback,
                                by = c("nome_4md", "segmento", "ano"))

  casos_otimizados <- casos_otimizados %>%
    mutate(mercado_potencial = round(exp(-spb * payback) * consumidores, 0),
           mercado_potencial = ifelse(mercado_potencial == 0,
                                      1,
                                      mercado_potencial))

  casos_otimizados <- casos_otimizados %>%
    filter(ano <= ano_max_resultado) %>% 
    select(-tipo_payback)

}
