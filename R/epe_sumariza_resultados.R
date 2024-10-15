
sumariza_resultados <- function (resultados_mensais)
{
  result <- resultados_mensais %>% group_by(ano) %>%
    summarise(pot_ano = sum(pot_mes_mw/1000),
              pot_ano_bateria = sum(pot_mes_bateria_mw/1000),
              pot_ano_fv_bateria = sum(pot_mes_mw/1000) + sum(pot_mes_bateria_mw/1000),
              geracao_gwh = sum(energia_mwh/1000),
              energia_armazenada_gwh = sum(energia_bateria_mwh/1000)) %>%
    ungroup() %>%
    mutate(pot_acum = cumsum(pot_ano),
           pot_acum_bateria = cumsum(pot_ano_bateria),
           geracao_mwmed = geracao_gwh/8.76,
           energia_bateria_mwmed = energia_armazenada_gwh/8.76)
}

resumo_partes_br <- sumariza_resultados(geracao)
