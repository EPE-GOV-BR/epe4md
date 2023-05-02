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
#' resultado_payback <- structure(
#'   list(segmento = c("comercial_at", "comercial_at", "comercial_at", "comercial_at", "comercial_at"),
#'        fonte_resumo = c("Fotovoltaica", "Fotovoltaica", "Fotovoltaica", "Fotovoltaica", "Fotovoltaica"),
#'        fator_autoconsumo = c(0.8, 0.8, 0.8, 0.8, 0.8),
#'        oem_anual = c(0.01, 0.01, 0.01, 0.01, 0.01),
#'        nome_4md = c("RORAIMA", "RORAIMA", "RORAIMA", "RORAIMA", "RORAIMA"),
#'        fc = c(0.150559213009562, 0.150559213009562, 0.150559213009562, 0.150559213009562, 0.150559213009562),
#'        vida_util = c(25, 25, 25, 25, 25),
#'        degradacao = c(0.005, 0.005, 0.005, 0.005, 0.005),
#'        pot_sistemas = c(70, 70, 70, 70, 70),
#'        geracao_1_kwh = c(92322.9094174635, 92322.9094174635, 92322.9094174635, 92322.9094174635, 92322.9094174635),
#'        ano = c(2017, 2018, 2019, 2020, 2021),
#'        custo_unitario = c(4.6, 4.23, 3.4, 3.62, 3.81),
#'        custo_inversor = c(0.69, 0.6345, 0.51, 0.543, 0.5715),
#'        capex_inicial = c(322000, 296100, 238000, 253400, 266700),
#'        capex_inversor = c(69795.6224290695, 64181.6267119487, 51588.0687519209, 54926.1202593981, 57808.9829249466),
#'        uf = c("RR", "RR", "RR", "RR", "RR"),
#'        subsistema = c("MAN", "MAN", "MAN", "MAN", "MAN"),
#'        regiao = c("N", "N", "N", "N", "N"),
#'        payback = c(9.8778051007831, 6.84888025046529, 5.42743545180765, 6.58428138751385, 6.57787077623868),
#'        payback_desc = c(25, 12.9151902647661, 7.76175962915069, 11.9207871041042, 11.8969973448089),
#'        tir_nominal = c(0.117295199807788, 0.177811894010469, 0.237962995155112, 0.186855919654218, 0.1870853811094),
#'        tir_real = c(0.0769110359593141, 0.135240379769127, 0.193217344727818, 0.143957512919728, 0.144178680587373)),
#'   class = c("tbl_df", "tbl", "data.frame"),
#'   row.names = c(NA, -5L)
#' )
#'
#' consumidores <- readRDS("C:/Arquivos Locais/R/epe4md/inst/dados_exemplos/consumidores.Rds")
#'
#' casos_otimizados <- epe4md_calibra_curva_s(
#'   resultado_payback = resultado_payback,
#'   consumidores = consumidores,
#'   ano_base = 2021,
#'   ano_max_resultado = 2050,
#'   spb = 0.3,
#'   p_max = 0.01,
#'   q_max = 1,
#'   dir_dados_premissas = NA_character_
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

    parametros <- enframe(otimizador$par)


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
