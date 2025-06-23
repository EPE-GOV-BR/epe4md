#' Estima a geração de eletricidade a partir da projeção de potência
#'
#' @param proj_mensal dataframe. Resultado da função
#' [epe4md::epe4md_proj_mensal].
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param bateria_eficiencia numeric. Eficiência da bateria. Default igual a 0.90.
#' @param degradacao_bateria_mil_ciclos numeric. Degradação linear da bateria,
#' em percentual a cada 1000 ciclos. Default igual a 10%.
#' @param simula_bateria. Define se o modelo irá considerar a projeção de
#' baterias no resultado final. Default igual a FALSE.
#' @param ano_recapex_bat Ano em que será feito um investimento adicional em baterias
#' para compensar a degradação. Default igual a 11.
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

epe4md_proj_geracao <- function(proj_mensal,
                                ano_base,
                                bateria_eficiencia = 0.9,
                                degradacao_bateria_mil_ciclos = 0.1,
                                simula_bateria = FALSE,
                                ano_recapex_bat = 11,
                                dir_dados_premissas = NA_character_) {

  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )

  # Bases de fatores e parâmetros

  fc_fv_mensal <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/fc_distribuidoras_mensal.xlsx"))

  fc_outras_mensal <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/fc_outras_fontes.xlsx")) %>%
    pivot_longer(cols = 3:14, names_to = "mes", values_to = "fc") %>%
    mutate(mes = as.numeric(mes))

  injecao <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/injecao.xlsx"))

  tabela_regiao <-
    readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/tabela_dist_subs.xlsx"))

  proj_mensal <- proj_mensal %>%
    left_join(tabela_regiao, by = c("nome_4md"))

  # Considera-se que os sistemas são instalados no dia 15 de cada mês
  dia_instalacao <- 15

  meses <- tibble(mes = seq(1, 12))

  anos_operacao <- tibble(ano_operacao = seq(2013, max(proj_mensal$ano)))

  meses_operacao <- crossing(anos_operacao, meses) %>%
    mutate(mes_operacao = make_date(ano_operacao, mes),
           mes_operacao = ceiling_date(mes_operacao, "month") - 1) %>%
    select(mes_operacao)

  proj_mensal <- proj_mensal %>%
    mutate(mes_instalacao = make_date(year = ano, month = month(mes_ano),
                                      day = dia_instalacao)) %>%
    filter(pot_mes_mw != 0)

  # Crossing das instalações com os meses de operação

  projecao_energia <- crossing(proj_mensal, meses_operacao)

  projecao_energia <- projecao_energia %>%
    mutate(dias_operando = as.numeric(mes_operacao - mes_instalacao)) %>%
    filter(dias_operando > 0)

  projecao_energia <- projecao_energia %>%
    mutate(mes = month(mes_operacao)) %>%
    left_join(fc_outras_mensal, by = c("subsistema", "mes", "fonte_resumo")) %>%
    left_join(fc_fv_mensal, by = c("nome_4md", "mes")) %>%
    mutate(fc = case_when(
      fonte_resumo == "Fotovoltaica" &
        segmento == "comercial_at_remoto" ~ fc_remoto,
      fonte_resumo == "Fotovoltaica" &
        segmento != "comercial_at_remoto" ~ fc_local,
      TRUE ~ fc)) %>%
    select(-fc_local, -fc_remoto, -mes)

  degradacao_diaria <- (1 + 0.005)^(1 / 365) - 1
  degradacao_ciclo_bat <- degradacao_bateria_mil_ciclos / 1000
  vida_util_fv_dias <- 25 * 365
  vida_util_bateria_dias <- (ano_recapex_bat - 1) * 365

  projecao_energia <- projecao_energia %>%
    mutate(dias_operando_fv = ifelse(dias_operando > vida_util_fv_dias,
                                  dias_operando - vida_util_fv_dias,
                                  dias_operando),
           troca_bateria = floor(dias_operando / vida_util_bateria_dias),
           dias_operando_bat = case_when(
             troca_bateria == 0 ~ dias_operando,
             troca_bateria >= 1 ~ dias_operando - vida_util_bateria_dias),
           dias_operando_mes = days_in_month(mes_operacao),
           dias_operando_mes = ifelse(mes_operacao - mes_instalacao < 28,
                                      dias_operando_mes - dia_instalacao,
                                      dias_operando_mes))

  projecao_energia <- projecao_energia %>%
    mutate(energia_mwh = ifelse(fonte_resumo == "Fotovoltaica",
                                pot_mes_mw * fc * dias_operando_mes * 24 *
                                  (1 - degradacao_diaria)^dias_operando_fv,
                                pot_mes_mw * fc * dias_operando_mes * 24),
           consumo_bateria_mwh = cap_bateria_mes_mwh * dias_operando_mes *
             (1 - degradacao_ciclo_bat * dias_operando_bat) * (1 - bateria_eficiencia),
           armazenamento_bateria_mwh = cap_bateria_mes_mwh * dias_operando_mes *
             (1 - degradacao_ciclo_bat * dias_operando_bat) * bateria_eficiencia)

  if(simula_bateria == TRUE) {
    projecao_energia <- projecao_energia %>%
           mutate(energia_mwh = energia_mwh - consumo_bateria_mwh) %>%
      left_join(injecao, by = c("segmento", "fonte_resumo")) %>%
      mutate(energia_autoc_mwh = energia_mwh * fator_autoconsumo +
               armazenamento_bateria_mwh,
             energia_inj_mwh = energia_mwh * (1 - fator_autoconsumo) -
               armazenamento_bateria_mwh)
  } else {
  projecao_energia <- projecao_energia %>%
    left_join(injecao, by = c("segmento", "fonte_resumo")) %>%
    mutate(energia_autoc_mwh = energia_mwh * fator_autoconsumo,
           energia_inj_mwh = energia_mwh * (1 - fator_autoconsumo))}

  #abertura GD1
  projecao_energia <- projecao_energia %>%
    mutate(enquadramento_gd = case_when(
      ano <= 2023 & segmento %in%
        c("comercial_at", "comercial_at_remoto") ~ "GD 1",
      mes_instalacao < make_date(2023, 5, 1) ~ "GD 1",
      mes_operacao > make_date(2045, 12, 31) ~ "GD 2",
      TRUE ~ "GD 2"))

  resumo_projecao_energia <- projecao_energia %>%
    group_by(mes_operacao, nome_4md, subsistema, uf, segmento, fonte_resumo,
             enquadramento_gd) %>%
    summarise(energia_mwh = sum(energia_mwh),
              consumo_bateria_mwh = sum(consumo_bateria_mwh),
              armazenamento_bateria_mwh = sum(armazenamento_bateria_mwh),
              energia_autoc_mwh = sum(energia_autoc_mwh),
              energia_inj_mwh = sum(energia_inj_mwh)) %>%
    ungroup()

  resumo_projecao_energia <- resumo_projecao_energia %>%
    mutate(mes = tsibble::yearmonth(mes_operacao),
           ano = year(mes),
           dias_mes = days_in_month(mes),
           data = lubridate::date(mes),
           mes = month(mes),
           energia_mwmed = energia_mwh / (24 * dias_mes)) %>%
    select(data, ano, mes, everything(), -dias_mes, -mes_operacao)

  resumo_projecao_potencia <- proj_mensal %>%
    mutate(enquadramento_gd = case_when(
      ano <= 2023 & segmento %in%
        c("comercial_at", "comercial_at_remoto") ~ "GD 1",
      mes_instalacao < make_date(2023, 5, 1) ~ "GD 1",
      TRUE ~ "GD 2")) %>%
    group_by(mes_ano, nome_4md, subsistema, uf, segmento, fonte_resumo,
             enquadramento_gd) %>%
    summarise(pot_mes_mw = sum(pot_mes_mw),
              adotantes_mes = sum(adotantes_mes),
              pot_mes_bateria_mw = sum(pot_mes_bateria_mw),
              adotantes_bateria_mes = sum(adotantes_bateria_mes),
              cap_bateria_mwh = sum(cap_bateria_mes_mwh)) %>%
    ungroup() %>%
    mutate(data = lubridate::date(mes_ano),
           ano = year(data), mes = month(data)) %>%
    select(data, ano, mes, everything(), -mes_ano)

  # Junção de potência e energia

  juncao <- left_join(resumo_projecao_energia, resumo_projecao_potencia,
                      by = c("data", "ano", "mes", "nome_4md", "subsistema",
                             "uf", "segmento", "fonte_resumo", "enquadramento_gd"))

  juncao <- juncao %>%
    replace_na(list(pot_mes_mw = 0, adotantes_mes = 0, pot_mes_bateria_mw = 0,
                    adotantes_bateria_mes = 0, cap_bateria_mwh = 0))

  fatores_pq <- projecao_energia %>%
    select(segmento, nome_4md, p, q) %>%
    distinct()

  resumo_resultados <- juncao %>%
    left_join(fatores_pq, by = c("segmento", "nome_4md")) %>%
    left_join(tabela_regiao, by = c("nome_4md", "subsistema", "uf"))

  if(simula_bateria == FALSE){
    resumo_resultados <- resumo_resultados %>%
      select(-consumo_bateria_mwh, -armazenamento_bateria_mwh, -pot_mes_bateria_mw,
             -adotantes_bateria_mes, -cap_bateria_mwh)
  }

  resumo_resultados
}
