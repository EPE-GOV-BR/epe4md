#' Calibra o modelo de Bass com dados históricos e gera curvas S de adoção.
#'
#' @param resultado_payback data.frame. Resultado da função
#' [epe4md::epe4md_payback].
#' @param consumidores list. Resultado da função
#' [epe4md::epe4md_mercado_potencial].
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param ano_max_resultado numeric. Ano final para apresentação dos resultados.
#' Máximo igual a 2060. Default igual a 2060.
#' @param p_max numeric. Fator de inovação (p) máximo. Default igual a 0.01.
#' @param q_max numeric. Fator de imitação (q) máximo. Default igual a 1.
#' @param curva_bateria string. Pode ser "propria" (Default) ou "lag". A primeira
#' opção utiliza os fatores p_bat e q_bat. A opção "lag" utiliza a curva de
#' difusão de MMGD, atrasada no tempo, conforme o parâmetro inicio_curva_bateria.
#' @param p_bat numeric. Fator de inovação (p) para baterias. Default igual a 0.0015.
#' @param q_bat numeric. Fator de imitação (q) para_baterias. Default igual a 0.3.
#' @param inicio_curva_bateria numeric. Indica o ano que começa a difusão
#' das baterias. Default igual a 2023.
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
                                   ano_max_resultado = 2060,
                                   p_max = 0.01,
                                   q_max = 1,
                                   curva_bateria = "propria",
                                   p_bat = 0.0015,
                                   q_bat = 0.3,
                                   inicio_curva_bateria = 2023,
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
  consumidores_bateria <- consumidores$consumidores_bateria

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
           payback_desc, payback_bateria, payback_desc_bateria)

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
                            payback_desc),
           payback_bateria = ifelse(tipo_payback == "simples",
                                    payback_bateria,
                                    payback_desc_bateria))

  base_otimizacao$adotantes_hist <- base_otimizacao$adotantes_hist %>%
    replace_na(0)

  base_otimizacao <- base_otimizacao %>%
    arrange(ano) %>%
    group_by(nome_4md, segmento) %>%
    mutate(adotantes_acum = cumsum(adotantes_hist)) %>%
    ungroup() %>%
    select(nome_4md, consumidores, consumidores_bateria, ano, payback, payback_bateria, adotantes_acum, segmento) %>%
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
      consumidores_bateria <- y$consumidores_bateria
      t <- y$ano
      payback <- y$payback
      payback_bateria <- y$payback_bateria
      historico_adotantes <- y$adotantes_acum

      taxa_difusao <- (1 - exp(- (p + q) * t)) /
        (1 + (q / p) * exp(- (p + q) * t))

      mercado_potencial <- exp(-spb * payback) * consumidores
      mercado_potencial_bateria <- exp(-spb * payback_bateria) * consumidores_bateria

      projecao <- taxa_difusao * mercado_potencial
      projecao_bateria <- taxa_difusao * mercado_potencial_bateria

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

  anos_simulacao <- tibble::tibble("ano" = seq(1, 48))

  casos_otimizados <- crossing(casos_otimizados, anos_simulacao)

#equação do modelo de Bass
  casos_otimizados <- casos_otimizados %>%
    mutate(Ft = (1 - exp(-(p + q) * ano)) /
             (1 + (q / p) * exp(-(p + q) * ano)))

  if(curva_bateria == "lag") {
    casos_otimizados <- casos_otimizados %>%
    group_by(nome_4md, segmento) %>%
    mutate(Ft_bateria = lag(Ft, n = (inicio_curva_bateria - 2013), default = 0)) %>%
    ungroup()
  }

  if(curva_bateria == "propria") {
    casos_otimizados <- casos_otimizados %>%
      mutate(Ft_bateria_0 = (1 - exp(-(p_bat + q_bat) * ano)) /
               (1 + (q_bat / p_bat) * exp(-(p_bat + q_bat) * ano))) %>%
      group_by(nome_4md, segmento) %>%
      mutate(Ft_bateria = lag(Ft_bateria_0, n = (inicio_curva_bateria - 2013), default = 0)) %>%
      select(-Ft_bateria_0)
  }

  casos_otimizados <- casos_otimizados %>%
    mutate(ano = ano + 2012) %>%
    left_join(consumidores, by = c("nome_4md", "segmento", "ano"))

  resultado_payback <- resultado_payback %>%
    left_join(tipo_payback, by = "segmento") %>%
    select(nome_4md, segmento, ano, payback, payback_desc, payback_bateria, payback_desc_bateria, tipo_payback) %>%
    mutate(payback = ifelse(tipo_payback == "simples",
                            payback,
                            payback_desc),
           payback_bateria = ifelse(tipo_payback == "simples",
                                    payback_bateria,
                                    payback_desc_bateria)) %>%
    select(-payback_desc, -payback_desc_bateria)

  casos_otimizados <- left_join(casos_otimizados, resultado_payback,
                                by = c("nome_4md", "segmento", "ano"))

  casos_otimizados <- casos_otimizados %>%
    mutate(mercado_potencial = round(exp(-spb * payback) * consumidores, 0),
           mercado_potencial = ifelse(mercado_potencial == 0, 1, mercado_potencial),
           mercado_potencial_bateria = round(exp(-spb * payback_bateria) * consumidores_bateria, 0),
           mercado_potencial_bateria = ifelse(mercado_potencial_bateria == 0,
                                              1,
                                              mercado_potencial_bateria))

  casos_otimizados <- casos_otimizados %>%
    filter(ano <= ano_max_resultado) %>%
    select(-tipo_payback)

}
