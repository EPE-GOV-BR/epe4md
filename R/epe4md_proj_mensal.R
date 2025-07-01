#' Faz a abertura mensal da projeção de potência
#'
#' @param lista_potencia list. Resultado da função
#' [epe4md::epe4md_proj_potencia].
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param ano_max_resultado numeric. Ano final para apresentação dos resultados.
#' Máximo igual a 2060. Default igual a 2060.
#' @param ajuste_ano_corrente logic. Se TRUE indica que a projeção deverá
#' incorporar o histórico mensal recente, verificado em parte do primeiro ano
#' após o ano base. Default igual a FALSE. O arquivo base_mmgd.xlsx deve
#' incorporar esse histórico.
#' @param ultimo_mes_ajuste numeric. Último mês com dados completos na
#' base_ano_corrente. Default igual a NA. Só tem efeito caso ajuste_ano_corrente
#' seja igual a TRUE.
#' @param metodo_ajuste string. Se igual a "extrapola" o modelo irá extrapolar a
#' potência e o número de adotantes até o final do ano base + 1 com base no
#' verificado até o ultimo_mes_ajuste. Se igual a "substitui", o modelo
#' substitui a projeção até o ultimo_mes_ajuste e mantém o restante do ano com a
#' projeção normal. Só tem efeito caso ajuste_ano_corrente seja igual a TRUE.
#' @param dir_dados_premissas Diretório onde se encontram as premissas.
#' Se esse parâmetro não for passado, a função usa os dados default que são
#' instalados com o pacote. É importante que os nomes dos arquivos sejam os
#' mesmos da pasta default.
#'
#' @return data.frame com os resultados da projeção de capacidade instalada
#' de micro e minigeração distribuída e número de adotantes mensal
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

epe4md_proj_mensal <- function(lista_potencia,
                               ano_base,
                               ano_max_resultado = 2060,
                               ajuste_ano_corrente = FALSE,
                               ultimo_mes_ajuste = NA,
                               metodo_ajuste = NA,
                               dir_dados_premissas = NA_character_
) {

  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )

  # Extrair proj_potencia da lista
  proj_potencia <- lista_potencia$proj_potencia

  # Se ajuste_ano_corrente == TRUE, a projeção deverá incorporar o histórico mensal recente,
  # verificado em parte do primeiro ano após o ano base.
  if (ajuste_ano_corrente == TRUE) {

    dados_gd_historico <- readxl::read_xlsx(
      stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx")) %>%
      filter(data_conexao < make_date(ano_base + 1, ultimo_mes_ajuste + 1, 1)) %>%
      select(-local_remoto)

    # Se metodo_ajuste igual a "extrapola" o modelo irá extrapolar a potência e o número de adotantes
    # até o final do ano base + 1 com base no verificado até o ultimo_mes_ajuste.
    # Se igual a "substitui", o modelo substitui a projeção até o ultimo_mes_ajuste e mantém o restante do ano
    # com a projeção normal. Só tem efeito caso ajuste_ano_corrente seja igual a TRUE.
    if(metodo_ajuste == "extrapola") {

      dados_gd_ano <- dados_gd_historico %>%
        group_by(ano, nome_4md, segmento, fonte_resumo) %>%
        summarise(adotantes_ano = sum(qtde_u_csrecebem_os_creditos),
                  pot_ano_mw = sum(potencia_mw)) %>%
        ungroup() %>%
        mutate(adotantes_ano = ifelse(ano == ano_base + 1,
                                      round(adotantes_ano *
                                              (12 / ultimo_mes_ajuste), 0),
                                      adotantes_ano),
               pot_ano_mw = ifelse(ano == ano_base + 1,
                                   pot_ano_mw * (12 / ultimo_mes_ajuste),
                                   pot_ano_mw))

    }

  } else {

    dados_gd_historico <- readxl::read_xlsx(
      stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx")) %>%
      filter(ano <= ano_base) %>%
      select(-local_remoto)
  }

  dados_gd_historico <- dados_gd_historico %>%
    mutate(mes_ano = tsibble::yearmonth(data_conexao)) %>%
    rename(pot_mes_mw = potencia_mw,
           adotantes_mes = qtde_u_csrecebem_os_creditos) %>%
    mutate(mes = month(mes_ano)) %>%
    select(- data_conexao, - num_geradores,- potencia_instalada_k_w)


  meses <- tibble(mes = seq(1, 12))

  projecao_mensal <- proj_potencia %>%
    select(ano, nome_4md, segmento, fonte_resumo,
           pot_ano_mw, pot_ano_bateria_mw, adotantes_ano, adotantes_ano_bateria,
           cap_ano_bateria_mwh) %>%
    crossing(meses) %>%
    mutate(mes_ano = make_date(ano, mes, 1),
           mes_ano = tsibble::yearmonth(mes_ano))

  # Cria dataframe de série temporal com a potência mensal para definir a sazonalização
  historico_fatores <- dados_gd_historico %>%
    filter(ano <= ano_base) %>%
    group_by(mes_ano, ano) %>%
    summarise(pot_mes_mw = sum(pot_mes_mw)) %>%
    ungroup() %>%
    filter(between(ano, 2014, ano_base)) # Retira-se o ano de 2013 por ter dados incompletos.

  # Cria um dataframe com a sasonalidade, tendência e resíduos da série temporal
  fatores_mensais <- historico_fatores %>%
    tsibble::as_tsibble(index = mes_ano) %>%
    fabletools::model(feasts::classical_decomposition(pot_mes_mw ~ season(12),
                                                      type = "mult")) %>%
    fabletools::components()

  # Define a sasonalidade média de cada mês do ano como "fator_mensal"
  fatores_mensais <- fatores_mensais %>%
    as_tibble() %>%
    mutate(mes = lubridate::month(mes_ano)) %>%
    group_by(mes) %>%
    summarise(fator_mensal = mean(seasonal)) %>%
    ungroup()

  # Transforma as projeções anuais em mensais com base no fator_mensal
  projecao_mensal <- projecao_mensal %>%
    left_join(fatores_mensais, by = "mes") %>%
    mutate(pot_mes_mw = fator_mensal * pot_ano_mw / 12,
           pot_mes_bateria_mw = fator_mensal * pot_ano_bateria_mw / 12,
           adotantes_mes = round(fator_mensal * adotantes_ano / 12),
           adotantes_bateria_mes = round(fator_mensal * adotantes_ano_bateria / 12),
           cap_bateria_mes_mwh = fator_mensal * cap_ano_bateria_mwh / 12) %>%
    select(- fator_mensal, - adotantes_ano, - pot_ano_mw, - adotantes_ano_bateria,
           - pot_ano_bateria_mw, -cap_ano_bateria_mwh)

  # Cria dataframe com as projeções relacionadas a baterias
  proj_mensal_bat <- projecao_mensal %>%
    select(ano, mes, mes_ano, nome_4md, segmento, fonte_resumo, adotantes_bateria_mes, pot_mes_bateria_mw,
           cap_bateria_mes_mwh)

  # Cria dataframe com as projeções relacionadas a MMGD
  projecao_mensal <- projecao_mensal %>%
    select(-adotantes_bateria_mes, -pot_mes_bateria_mw,
           -cap_bateria_mes_mwh)


  if (ajuste_ano_corrente == TRUE & metodo_ajuste == "extrapola") {

    extrapola_mes <- dados_gd_ano %>%
      crossing(meses) %>%
      mutate(mes_ano = make_date(ano, mes, 1),
             mes_ano = tsibble::yearmonth(mes_ano)) %>%
      left_join(fatores_mensais, by = "mes") %>%
      mutate(pot_mes_mw = fator_mensal * pot_ano_mw / 12,
             adotantes_mes = round(fator_mensal * adotantes_ano / 12)) %>%
      select(- fator_mensal, - adotantes_ano, - pot_ano_mw) %>%
      filter(mes_ano > tsibble::yearmonth(make_date(ano_base + 1, ultimo_mes_ajuste, 1)))

    historico_mensal <- bind_rows(dados_gd_historico, extrapola_mes)

  }

  # Substituicao dos dados de projeção pelos dados históricos disponíveis
  if (ajuste_ano_corrente == FALSE) {

    projecao_mensal <- projecao_mensal %>%
      filter(ano > ano_base)

    historico_mensal <- dados_gd_historico %>%
      filter(mes_ano < tsibble::yearmonth(make_date(ano_base + 1, 1, 1)))

  } else {

    if(metodo_ajuste == "extrapola") {

      projecao_mensal <- projecao_mensal %>%
        filter(ano > ano_base + 1)

    } else {

      projecao_mensal <- projecao_mensal %>%
        filter(mes_ano > tsibble::yearmonth(make_date(ano_base + 1, ultimo_mes_ajuste, 1)))

      historico_mensal <- dados_gd_historico %>%
        filter(mes_ano <= tsibble::yearmonth(make_date(ano_base + 1, ultimo_mes_ajuste, 1)))

    }

  }


  # Junta histórico e projeção
  projecao_mensal <- bind_rows(historico_mensal, projecao_mensal)

  # join com dados de baterias
  projecao_mensal <- projecao_mensal %>%
    left_join(proj_mensal_bat, by = c("ano", "mes", "mes_ano", "segmento", "fonte_resumo",
                                      "nome_4md"))

  # Inclui fatores de imitação e inovação
  fatores_pq <- lista_potencia$proj_potencia %>%
    select(ano, segmento, fonte_resumo, nome_4md, p, q, Ft, Ft_bateria)

  projecao_mensal <- projecao_mensal %>%
    left_join(fatores_pq, by = c("ano", "segmento", "nome_4md", "fonte_resumo"))

  projecao_mensal
}
