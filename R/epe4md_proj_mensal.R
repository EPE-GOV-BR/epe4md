#' Faz a abertura mensal da projeção de potência
#'
#' @param lista_potencia list. Resultado da função
#' [epe4md::epe4md_proj_potencia].
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param filtro_nome4md string. Parâmetro que define o nome de uma
#' concessionária de energia a ser filtrado. Caso não seja #' informado um valor
#' ou seja informado um valor inválido o resultado será apresentado sem filtro.
#' @param filtro_de_segmento string. Parâmetro que define um segmento a ser
#' filtrado. Pode se escolher entre "comercial_at", "comercial_at_remoto",
#' "comercial_bt", "residencial" e "residencial_remoto".  Caso não seja
#' informado um valor ou seja informado um valor inválido o resultado será
#' apresentado sem filtro.
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
#' lista_potencia <- list(
#'   proj_potencia = structure(
#'     list(nome_4md = c("RORAIMA", "RORAIMA", "RORAIMA", "RORAIMA", "RORAIMA"),
#'          segmento = c("comercial_at", "comercial_at", "comercial_at",
#'                       "comercial_at", "comercial_at"),
#'          p = c(0.000122298434419402, 0.000122298434419402,
#'                0.000122298434419402, 0.000122298434419402,
#'                0.000122298434419402),
#'          q = c(1, 1, 1, 1, 1),
#'          spb = c(0.3, 0.3, 0.3, 0.3, 0.3),
#'          ano = c(2017, 2018, 2019, 2020, 2021),
#'          Ft = c(0.0177177153816275, 0.0469352065606947, 0.118237854106428,
#'                 0.267268529395706, 0.497952210651889),
#'          consumidores = c(656, 678, 751, 764, 791),
#'          payback = c(25, 12.9151902647661, 7.76175962915069,
#'                      11.9207871041042, 11.8969973448089),
#'          mercado_potencial = c(0.25, 3.5, 18.25, 5.25, 5.5),
#'          adotantes_ano = c(3, 1, 1, 3, 6),
#'          adotantes_acum = c(3, 4, 5, 8, 14),
#'          fonte_resumo = c("Fotovoltaica", "Fotovoltaica", "Fotovoltaica",
#'                           "Fotovoltaica", "Fotovoltaica"),
#'          part_fonte = c(1, 1, 1, 1, 1),
#'          adotantes_hist = c(3, 1, 1, 3, 6),
#'          pot_media = c(353.694375, 353.694375, 353.694375, 353.694375,
#'                        353.694375),
#'          pot_ano = c(199, 80, 94, 4602, 529.11),
#'          pot_hist = c(199, 80, 94, 4602, 529.11),
#'          pot_ano_mw = c(0.199, 0.08, 0.094, 4.602, 0.52911),
#'          pot_acum_mw = c(0.199, 0.279, 0.373, 4.975, 5.50411)),
#'     class = c("tbl_df", "tbl", "data.frame"),
#'     row.names = c(NA, -5L)
#'   ),
#'   part_adotantes = structure(
#'     list(ano = c(2017, 2018, 2019, 2020, 2021),
#'          segmento = c("comercial_at", "comercial_at", "comercial_at",
#'                       "comercial_at", "comercial_at"),
#'          adotantes = c(3, 4, 5, 8, 14),
#'          mercado_potencial = c(1, 14, 73, 21, 22),
#'          total_ucs = c(187922, 187853, 184276, 182048, 181394),
#'          penetracao_total = c(1.5964070199338e-05, 2.12932452502755e-05,
#'                               2.71332132236428e-05, 4.39444542098787e-05,
#'                               7.71800610825055e-05),
#'          mercado_nicho = c(187922, 187853, 184276, 182048, 181394),
#'          penetracao_nicho = c(1.5964070199338e-05, 2.12932452502755e-05,
#'                               2.71332132236428e-05, 4.39444542098787e-05,
#'                               7.71800610825055e-05),
#'          penetracao_potencial = c(3, 0.285714285714286, 0.0684931506849315,
#'                                   0.380952380952381, 0.636363636363636)),
#'     row.names = c(NA, -5L),
#'     class = c("tbl_df", "tbl", "data.frame")
#'   )
#' )
#'
#' proj_mensal <- epe4md_proj_mensal(
#'   lista_potencia = lista_potencia,
#'   ano_base = 2021,
#'   filtro_nome4md = "RORAIMA",
#'   filtro_de_segmento = "comercial_at",
#'   ano_max_resultado = 2021
#' )

utils::globalVariables(c("seasonal", "fator_mensal", "adotantes_total", "pot_media", "pot_hist"))

epe4md_proj_mensal <- function(lista_potencia,
                               ano_base,
                               filtro_nome4md = "N",
                               filtro_de_segmento = "N",
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

  # Filtro de nome
  lista_nome4md <- projecao_mensal$nome_4md %>% unique()
  if(filtro_nome4md %in% lista_nome4md){
      projecao_mensal <- projecao_mensal %>%
        filter(nome_4md == filtro_nome4md)
  }
  else if(filtro_nome4md != "N"){
    warning("\nNome incorreto! Resultado apresentado sem filtro!")
  }

  # Aplicando filtro de segmento
  lista_segmentos <- projecao_mensal$segmento %>% unique()
  if(filtro_de_segmento %in% lista_segmentos){
    projecao_mensal <- projecao_mensal %>%
      filter(segmento == filtro_de_segmento)
  }
  else if (filtro_de_segmento != "N"){
    message("\nSegmento incorreto! Resultado apresentado sem filtro!")
  }

  projecao_mensal

}
