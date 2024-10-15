
library(lubridate)
library(dplyr)




proj_mensal <- function (lista_potencia, ano_base, ano_max_resultado = 2060,
          ajuste_ano_corrente = FALSE, ultimo_mes_ajuste = NA, metodo_ajuste = NA,
          dir_dados_premissas = NA_character_)
{
  dir_dados_premissas <- if_else(is.na(dir_dados_premissas),
                                 system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                                             package = "epe4md"), dir_dados_premissas)
  proj_potencia <- lista_potencia$proj_potencia
  if (ajuste_ano_corrente == TRUE) {
    dados_gd_historico <- readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx")) %>%
      filter(data_conexao < make_date(ano_base + 1, ultimo_mes_ajuste +
                                        1, 1))
    if (metodo_ajuste == "extrapola") {
      dados_gd_ano <- dados_gd_historico %>% group_by(ano, nome_4md, segmento, fonte_resumo) %>%
        summarise(adotantes_ano = sum(qtde_u_csrecebem_os_creditos),
                  pot_ano_mw = sum(potencia_mw)) %>%


        ungroup() %>%

        mutate(adotantes_ano = ifelse(ano == ano_base +  1, round(adotantes_ano * (12/ultimo_mes_ajuste),0), adotantes_ano),
               pot_ano_mw = ifelse(ano == ano_base + 1, pot_ano_mw * (12/ultimo_mes_ajuste), pot_ano_mw))
    }
  }
  else {
    dados_gd_historico <- readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx")) %>%
      filter(ano <= ano_base)
  }

  dados_gd_historico <- dados_gd_historico %>% mutate(mes_ano = tsibble::yearmonth(data_conexao)) %>%
    rename(pot_mes_mw = potencia_mw, adotantes_mes = qtde_u_csrecebem_os_creditos) %>%
    mutate(mes = month(mes_ano)) %>% select(-data_conexao,
                                            -num_geradores, -potencia_instalada_k_w)
  meses <- tibble(mes = seq(1, 12))
  projecao_mensal <- proj_potencia %>%
    select(ano, nome_4md,segmento, fonte_resumo, pot_ano_mw,pot_ano_bateria_mw, adotantes_ano,adotantes_ano_bateria) %>%
    crossing(meses) %>%
    mutate(mes_ano = make_date(ano, mes,1), mes_ano = tsibble::yearmonth(mes_ano))

  historico_fatores <- dados_gd_historico %>% filter(ano <=
                                                       ano_base) %>% group_by(mes_ano, ano) %>%
    summarise(pot_mes_mw = sum(pot_mes_mw)) %>%
    ungroup() %>%
    filter(between(ano, 2014, ano_base))

  fatores_mensais <- historico_fatores %>% tsibble::as_tsibble(index = mes_ano) %>%
    fabletools::model(feasts::classical_decomposition(pot_mes_mw ~  season(12), type = "mult")) %>%
    fabletools::components()

  fatores_mensais <- fatores_mensais %>% as_tibble() %>% mutate(mes = lubridate::month(mes_ano)) %>%
    group_by(mes) %>% summarise(fator_mensal = mean(seasonal)) %>%
    ungroup()

  projecao_mensal <- projecao_mensal %>% left_join(fatores_mensais,by = "mes") %>%
    mutate(pot_mes_mw = fator_mensal * pot_ano_mw/12,
           adotantes_mes = round(fator_mensal * adotantes_ano/12),
           pot_mes_bateria_mw = fator_mensal * pot_ano_bateria_mw/12, #bateria
           adotantes_bateria_mes = round(fator_mensal * adotantes_ano_bateria/12)) %>% #bateria

    select(-fator_mensal, -adotantes_ano, -pot_ano_mw,-adotantes_ano_bateria, -pot_ano_bateria_mw)

  if (ajuste_ano_corrente == TRUE & metodo_ajuste == "extrapola") {
    extrapola_mes <- dados_gd_ano %>% crossing(meses) %>%
      mutate(mes_ano = make_date(ano, mes, 1), mes_ano = tsibble::yearmonth(mes_ano)) %>%
      left_join(fatores_mensais, by = "mes") %>%
      mutate(pot_mes_mw = fator_mensal * pot_ano_mw/12, adotantes_mes = round(fator_mensal *adotantes_ano/12)) %>%
      select(-fator_mensal, -adotantes_ano,-pot_ano_mw) %>%
      filter(mes_ano > tsibble::yearmonth(make_date(ano_base +1, ultimo_mes_ajuste, 1)))

    historico_mensal <- bind_rows(dados_gd_historico, extrapola_mes)
  }

  if (ajuste_ano_corrente == FALSE) {
    projecao_mensal <- projecao_mensal %>% filter(ano > ano_base)
    historico_mensal <- dados_gd_historico %>% filter(mes_ano <
                                                        tsibble::yearmonth(make_date(ano_base + 1, 1, 1)))
  }
  else {
    if (metodo_ajuste == "extrapola") {
      projecao_mensal <- projecao_mensal %>% filter(ano >
                                                      ano_base + 1)
    }
    else {
      projecao_mensal <- projecao_mensal %>% filter(mes_ano >
                                                      tsibble::yearmonth(make_date(ano_base + 1, ultimo_mes_ajuste,
                                                                                   1)))
      historico_mensal <- dados_gd_historico %>% filter(mes_ano <=
                                                          tsibble::yearmonth(make_date(ano_base + 1, ultimo_mes_ajuste,
                                                                                       1)))
    }
  }
  projecao_mensal <- bind_rows(historico_mensal, projecao_mensal)
  fatores_pq <- lista_potencia$proj_potencia %>% select(ano,
                                                        segmento, fonte_resumo, nome_4md, p, q, Ft)
  projecao_mensal <- projecao_mensal %>%
    left_join(fatores_pq,  by = c("ano", "segmento", "nome_4md", "fonte_resumo"))

  projecao_mensal <- projecao_mensal %>%
    replace_na(list(pot_mes_bateria_mw = 0, adotantes_bateria_mes = 0))


  projecao_mensal
}




mensal <- proj_mensal(
  lista_potencia = potencia,
  ano_base = 2022,
  ano_max_resultado = 2060,
  ajuste_ano_corrente = TRUE,
  ultimo_mes_ajuste = 2,
  metodo_ajuste = "extrapola"
)
