#' Estima a geração de eletricidade a partir da projeção de potência
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

epe4md_proj_geracao <- function(proj_mensal,
                                ano_base,
                                dir_dados_premissas = NA_character_) {

  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )

  #Bases


  fc_fv_mensal <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/fc_distribuidoras_mensal.xlsx"))

  fc_outras_mensal <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/fc_outras_fontes.xlsx")) %>%
    pivot_longer(cols = 3:14, names_to = "mes", values_to = "fc")

  fc_outras_mensal$mes <- as.numeric(fc_outras_mensal$mes)

  injecao <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/injecao.xlsx"))

  tabela_regiao <-
    readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/tabela_dist_subs.xlsx"))

  proj_mensal <- proj_mensal %>%
    left_join(tabela_regiao, by = c("nome_4md"))


  #considera-se que os sitemas são instalados no dia 15 de cada mês
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


  #crossing das instalacoes com os meses de operacao
  projecao_energia <- crossing(proj_mensal, meses_operacao)

  projecao_energia <- projecao_energia %>%
    mutate(dias_operando = mes_operacao - mes_instalacao) %>%
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
      TRUE ~ fc)
    ) %>%
    select(-fc_local, -fc_remoto, -mes)

  degradacao_diaria <- (1 + 0.005)^(1 / 365) - 1


  #após 25 anos, assume-se que o sistema FV é renovado, zerando a degradação
  vida_util_fv_dias <- 25 * 365

  projecao_energia <- projecao_energia %>%
    mutate(dias_operando = ifelse(dias_operando > vida_util_fv_dias,
                                  dias_operando - vida_util_fv_dias,
                                  dias_operando),
           dias_operando_mes = days_in_month(mes_operacao),
           dias_operando_mes = ifelse(mes_operacao - mes_instalacao < 28,
                                      dias_operando_mes - dia_instalacao,
                                      dias_operando_mes))

  projecao_energia <- projecao_energia %>%
    mutate(energia_mwh = ifelse(fonte_resumo == "Fotovoltaica",
                                pot_mes_mw * fc * dias_operando_mes * 24 *
                                  (1 - degradacao_diaria)^dias_operando,
                                pot_mes_mw * fc * dias_operando_mes * 24))

  projecao_energia <- projecao_energia %>%
    left_join(injecao, by = c("segmento", "fonte_resumo")) %>%
    mutate(energia_autoc_mwh = energia_mwh * fator_autoconsumo,
           energia_inj_mwh = energia_mwh * (1 - fator_autoconsumo))

  resumo_projecao_energia <- projecao_energia %>%
    group_by(mes_operacao, nome_4md, subsistema, uf, segmento, fonte_resumo) %>%
    summarise(energia_mwh = sum(energia_mwh),
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
    group_by(mes_ano, nome_4md, subsistema, uf, segmento, fonte_resumo) %>%
    summarise(pot_mes_mw = sum(pot_mes_mw),
              adotantes_mes = sum(adotantes_mes)) %>%
    ungroup() %>%
    mutate(data = lubridate::date(mes_ano),
           ano = year(data),
           mes = month(data)) %>%
    select(data, ano, mes, everything(), -mes_ano)


  #juncao potencia e energia

  juncao <- left_join(resumo_projecao_energia, resumo_projecao_potencia,
                      by = c("data", "ano", "mes", "nome_4md",
                             "subsistema", "uf", "segmento", "fonte_resumo"))

  juncao <- juncao %>%
    replace_na(list(pot_mes_mw = 0, adotantes_mes = 0))

  fatores_pq <- projecao_energia %>%
    select(segmento, nome_4md, p, q) %>%
    distinct()

  resumo_resultados <- juncao %>%
    left_join(fatores_pq, by = c("segmento", "nome_4md"))
  
  resumo_resultados <- resumo_resultados %>%
    left_join(tabela_regiao, by = c("nome_4md", "subsistema", "uf"))

  resumo_resultados

}
