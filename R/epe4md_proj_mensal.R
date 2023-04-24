#' Faz a abertura mensal da projeção de potência
#'
#' @param lista_potencia list. Resultado da função
#' [epe4md::epe4md_proj_potencia].
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param ano_max_resultado numeric. Ano final para apresentação dos resultados.
#' Máximo igual a 2050. Default igual a 2050.
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
#' @import lubridate
#' @import tidyr
#' @import readxl
#' @import dplyr
#' @import tibble
#'
#' @encoding UTF-8
#'
#' @examples
#'
#' lista_adotantes <- list(
#'   structure(
#'     list(nome_4md = c("CEB", "EQUATORIAL MA", "LIGHT"),
#'          segmento = c("comercial_at_remoto", "comercial_bt", "comercial_at_remoto"),
#'          p = c(0.000314088483901221, 0.000326288160094334, 0.01),
#'          q = c(0.860602105239079, 0.796618861208451, 0.0771175699573262),
#'          spb = c(0.3, 0.3, 0.3),
#'          ano = c(2024, 2028, 2022),
#'          Ft = c(0.917955265426745, 0.992968921614641, 0.137575817915978),
#'          consumidores = c(12146, 47955, 22878),
#'          payback = c(5.00924084667372, 4.40202323859729, 8.53878219211533),
#'          mercado_potencial = c(675.75, 3200.75, 441.5),
#'          adotantes_ano = c(309, 0, 4),
#'          adotantes_acum = c(2361, 0, 42),
#'          fonte_resumo = c("Fotovoltaica", "Eólica", "Fotovoltaica"),
#'          part_fonte = c(1, 0, 0.186666666666667),
#'          adotantes_hist = c(NA_real_, NA_real_, NA_real_)),
#'     row.names = c(NA, -3L),
#'     class = c("tbl_df", "tbl", "data.frame")),
#'   structure(
#'     list(ano = c(2030, 2016, 2020),
#'          segmento = c("comercial_bt", "residencial", "residencial"),
#'          adotantes = c(694432, 7214, 357386),
#'          mercado_potencial = c(620539, 857786, 2931122),
#'          total_ucs = c(14858570, 66148593.0147109, 71001451.6735913),
#'          penetracao_total = c(0.0467361260202025, 0.000109057497238009, 0.00503350271826806),
#'          mercado_nicho = c(2541888, 10668970, 11014026),
#'          penetracao_nicho = c(0.273195357151849, 0.000676166490298501, 0.0324482618798975),
#'          penetracao_potencial = c(1.11907873638885, 0.00841002301273278, 0.121928053489415)),
#'     row.names = c(NA, -3L),
#'     class = c("tbl_df", "tbl", "data.frame"))
#' )
#'
#' names(lista_adotantes) <- c("proj_adotantes", "part_adotantes")
#'
#' epe4md_proj_potencia(
#'   lista_adotantes = lista_adotantes,
#'   ano_base = 2021
#' )

utils::globalVariables(c("seasonal", "fator_mensal", "adotantes_total", "pot_media", "pot_hist"))

epe4md_proj_mensal <- function(lista_potencia,
                                  ano_base,
                                  ano_max_resultado = 2050,
                                  ajuste_ano_corrente = FALSE,
                                  ultimo_mes_ajuste = NA,
                                  metodo_ajuste = NA,
                                  dir_dados_premissas = "inst/dados_premissas") {

  dir_dados_premissas <- if_else(
    dir_dados_premissas == "inst/dados_premissas",
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )

  #extrair proj potencia da lista

  proj_potencia <- lista_potencia$proj_potencia

  if (ajuste_ano_corrente == TRUE) {

    dados_gd_historico <- readxl::read_xlsx(
      stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx")) %>%
      filter(data_conexao < make_date(ano_base + 1, ultimo_mes_ajuste + 1, 1))

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
      filter(ano <= ano_base)
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
           pot_ano_mw, adotantes_ano) %>%
    crossing(meses) %>%
    mutate(mes_ano = make_date(ano, mes, 1),
           mes_ano = tsibble::yearmonth(mes_ano))

  # calculo dos fatores de sazonalizacao
  historico_fatores <- dados_gd_historico %>%
    filter(ano <= ano_base) %>%
    group_by(mes_ano, ano) %>%
    summarise(pot_mes_mw = sum(pot_mes_mw)) %>%
    ungroup() %>%
    filter(between(ano, 2014, ano_base))

  fatores_mensais <- historico_fatores %>%
    tsibble::as_tsibble(index = mes_ano) %>%
    fabletools::model(feasts::classical_decomposition(pot_mes_mw ~ season(12),
                                                      type = "mult")) %>%
    fabletools::components()

  fatores_mensais <- fatores_mensais %>%
    as_tibble() %>%
    mutate(mes = lubridate::month(mes_ano)) %>%
    group_by(mes) %>%
    summarise(fator_mensal = mean(seasonal)) %>%
    ungroup()

  # transformação da potencia anual em mensal com base nos fatores

  projecao_mensal <- projecao_mensal %>%
    left_join(fatores_mensais,
              by = "mes",
              multiple = "all") %>%
    mutate(pot_mes_mw = fator_mensal * pot_ano_mw / 12,
           adotantes_mes = round(fator_mensal * adotantes_ano / 12)) %>%
    select(- fator_mensal, - adotantes_ano, - pot_ano_mw)

  if (ajuste_ano_corrente == TRUE & metodo_ajuste == "extrapola") {

    extrapola_mes <- dados_gd_ano %>%
      crossing(meses) %>%
      mutate(mes_ano = make_date(ano, mes, 1),
             mes_ano = tsibble::yearmonth(mes_ano)) %>%
      left_join(fatores_mensais,
                by = "mes",
                multiple = "all") %>%
      mutate(pot_mes_mw = fator_mensal * pot_ano_mw / 12,
             adotantes_mes = round(fator_mensal * adotantes_ano / 12)) %>%
      select(- fator_mensal, - adotantes_ano, - pot_ano_mw) %>%
      filter(mes_ano > tsibble::yearmonth(make_date(ano_base + 1, ultimo_mes_ajuste, 1)))

    historico_mensal <- bind_rows(dados_gd_historico, extrapola_mes)

  }

  # substituicao do inicio com dados realizados

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

  #junção historico e projecao
  projecao_mensal <- bind_rows(historico_mensal, projecao_mensal)

  #inclusao fatores imitacao e inovacao para registro
  fatores_pq <- lista_potencia$proj_potencia %>%
    select(ano, segmento, fonte_resumo, nome_4md, p, q, Ft)

  projecao_mensal <- projecao_mensal %>%
    left_join(fatores_pq,
              by = c("ano", "segmento", "nome_4md", "fonte_resumo"),
              multiple = "all")

  projecao_mensal

}
