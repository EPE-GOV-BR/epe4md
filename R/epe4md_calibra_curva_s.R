#' Calibra o modelo de Bass com dados históricos e gera curvas S de adoção.
#'
#' @param resultado_payback data.frame. Resultado da função [epe4md::epe4md_payback].
#' @param consumidores list. Resultado da função [epe4md::epe4md_mercado_potencial].
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param ano_max_resultado numeric. Ano final para apresentação dos resultados.
#' Máximo igual a 2050. Default igual a 2050.
#' @param spb numeric. Fator de Sensibilidade ao Payback (SPB). Default igual a
#' 0.3.
#' @param p_max numeric. Fator de inovação (p) máximo. Default igual a 0.01.
#' @param q_max numeric. Fator de imitação (q) máximo. Default igual a 1.
#' @param dir_dados_premissas Diretório onde se encontram as premissas. Se esse
#' parâmetro não for passado, a função usa os dados default que são instalados
#' com o pacote. É importante que os nomes dos arquivos sejam os mesmos da
#' pasta default.
#'
#' @return data.frame com curvas de difusão e mercado potencial
#' @export
#'
#'@import tidyr
#'@rawNamespace import(purrr, except=c(discard))
#'@import readxl
#'@import dplyr
#'@import tibble
#'
#'@encoding UTF-8
#'
#' @examples
#'
#' resultado_payback <- tibble(
#'   segmento = "comercial_at",
#'   fonte_resumo = "Fotovoltaica",
#'   fator_autoconsumo = 0.8,
#'   oem_anual = 0.01,
#'   nome_4md = c("AME", "CEB", "CEEE", "ENEL GO", "LIGHT"),
#'   fc = c(0.1405358, 0.1723692, 0.1723692, 0.1710513, 0.1516523),
#'   vida_util = 25,
#'   degradacao = 0.005,
#'   pot_sistemas = 70,
#'   geracao_1_kwh = c(86176.54, 105696.78, 90105.27, 104888.68, 92993.17),
#'   ano = 2021,
#'   custo_unitario = 3.81,
#'   custo_inversor = 0.5715,
#'   capex_inicial = 266700,
#'   capex_inversor = 57808.98,
#'   payback = c(4.921500, 4.967306, 4.940258, 4.940258, 4.415278),
#'   payback_desc = c(6.683981, 6.773162, 6.720501, 5.894958, 5.723854),
#'   tir_nominal = c(0.2691598, 0.2660881, 0.2678933, 0.3002609, 0.3086373),
#'   tir_real = c(0.2232865, 0.2203258, 0.2220658, 0.2532635, 0.2613372)
#' )
#'
#' consumidores <- list(
#'   consumidores <- tibble(
#'     nome_4md = c("CELESC", "CEMIG", "COELBA", "COPEL", "CPFL PAULISTA", "ENEL GO", "ENEL RJ", "ENEL SP", "LIGHT", "RGE"),
#'     ano = 2021,
#'     segmento = "residencial",
#'     consumidores = c(294853, 705996, 328907, 488333, 531478, 225929, 263874, 859724, 523613, 315290)
#'   ),
#'  consumidores_totais <- tibble(
#'   ano = 2021,
#'   total_ucs = c(72212363, 12098612, 181394),
#'   segmento = c("residencial", "comercial_bt", "comercial_at")
#'   )
#' )
#'
#'epe4md_calibra_curva_s(
#'  resultado_payback,
#'  consumidores,
#'  ano_base = 2021,
#'  ano_max_resultado = 2050,
#'  spb = 0.3,
#'  p_max = 0.01,
#'  q_max = 1,
#'  dir_dados_premissas = NA_character_
#' )

utils::globalVariables(c("ano", "nome_4md", "segmento", "qtde_u_csrecebem_os_creditos", "payback", "payback_desc",
                         "tir_nominal", "tir_real", "adotantes_hist", "adotantes_acum", "nome_dist", "nome_seg",
                         "name", "saida", "p", "mercado_potencial", "fonte_resumo"))

epe4md_calibra_curva_s <- function(resultado_payback,
                                   consumidores,
                                   ano_base,
                                   ano_max_resultado = 2050,
                                   spb = 0.3,
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
                               by = c("nome_4md", "ano", "segmento"),
                               multiple = "all")

  base_otimizacao <- left_join(base_otimizacao, historico,
                               by = c("nome_4md", "ano", "segmento"),
                               multiple = "all")

  base_otimizacao <- base_otimizacao %>%
    mutate(payback = ifelse(segmento %in% c("residencial",
                                            "residencial_remoto"),
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
      spb <- spb
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
    mutate(spb = spb)

  anos_simulacao <- tibble::tibble("ano" = seq(1, 38))

  casos_otimizados <- crossing(casos_otimizados, anos_simulacao)

#equação do modelo de Bass
  casos_otimizados <- casos_otimizados %>%
    mutate(Ft = (1 - exp(-(p + q) * ano)) /
             (1 + (q / p) * exp(-(p + q) * ano)))


  casos_otimizados <- casos_otimizados %>%
    mutate(ano = ano + 2012) %>%
    left_join(consumidores, by = c("nome_4md", "segmento", "ano"),
              multiple = "all")

  resultado_payback <- resultado_payback %>%
    select(nome_4md, segmento, ano, payback, payback_desc) %>%
    mutate(payback = ifelse(segmento %in%
                              c("residencial", "residencial_remoto"),
                            payback,
                            payback_desc)) %>%
    select(-payback_desc)

  casos_otimizados <- left_join(casos_otimizados, resultado_payback,
                                by = c("nome_4md", "segmento", "ano"),
                                multiple = "all")

  casos_otimizados <- casos_otimizados %>%
    mutate(mercado_potencial = round(exp(-spb * payback) * consumidores, 0),
           mercado_potencial = ifelse(mercado_potencial == 0,
                                      1,
                                      mercado_potencial))

  casos_otimizados <- casos_otimizados %>%
    filter(ano <= ano_max_resultado)

}
