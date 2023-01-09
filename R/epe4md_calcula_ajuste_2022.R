#' Realiza projeção assim como a função [epe4md::epe4md_calcula], porém com
#' ajuste em 2022 em função do impacto da Lei n° 14.300
#'
#' @param premissas_reg data.frame. Input de premissas regulatórias para serem
#' consideradas nos cálculos.
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param ano_max_resultado numeric. Ano final para apresentação dos resultados.
#' Máximo igual a 2050. Default igual a 2050.
#' @param altera_sistemas_existentes logic. TRUE se alterações regulatórias
#' afetam investimentos realizados em anos anteriores à revisão da regulação.
#' Default igual a FALSE.
#' @param ano_decisao_alteracao numeric. Ano em que são definidas novas regras
#' e se tornam de conhecimento público. Esse parâmetro só tem efeito caso o
#' anterior seja igual a TRUE. Default igual a 2023.
#' @param inflacao mumeric. Taxa anual de inflacao considerada no reajuste das
#' tarifas e para calcular o retorno real de projetos. Default igual a 0.0375.
#' @param taxa_desconto_nominal numeric. Taxa de desconto nominal considerada
#' nos cálculos de payback descontado. Default igual a 0.13.
#' @param custo_reforco_rede numeric. Custo em R$/kW aplicado a projetos de
#' geracao remota em Alta Tensão. Representa um custo pago pelo empreendedor
#' para reforços na rede. Default igual a 200.
#' @param ano_troca_inversor numeric. Ano, a partir do ano de instalação, em
#' que é realizada a troca do inversor fotovoltaico. Default igual a 11.
#' @param pagamento_disponibilidade. numeric. Percentual de meses em que o
#' consumidor residencial paga custo de disponbilidade em função da
#' variabilidade da geração FV. Default igual a 0.3.
#' @param disponibilidade_kwh_mes numeric. Consumo de disponbilidade do
#' consumidor em kWh/mês. Default igual a 100, equivalente a um consumidor
#' trifásico.
#' @param filtro_renda_domicilio string. Define o filtro aplicado a consumidores
#' residenciais, de acordo com a renda mensal do responsável, em salários
#' mínimos. Permite: "maior_2sm", "maior_3sm" ou "maior_5sm". Default igual a
#' "maior_3sm".
#' @param desconto_capex_local numeric. Percentual de desconto a ser aplicado no
#' CAPEX de sistemas de geração local(ex: 0.1) para simulação de incentivos.
#' Default igual a 0.
#' @param anos_desconto vector. Anos em que há a incidência do desconto no
#' CAPEX. Default igual a 0.
#' @param ajuste_ano_corrente logic. Se TRUE indica que a projeção deverá
#' incorporar o histórico mensal recente, verificado em parte do primeiro ano
#' após o ano base. Default igual a FALSE.
#' @param base_ano_corrente data.frame Resultado da função
#' [epe4md::epe4md_prepara_base] com histórico dos meses recentes.
#' @param ultimo_mes_ajuste numeric. Último mês com dados completos na
#' base_ano_corrente. @param dir_dados_premissas Diretório onde se encontram as
#' premissas. Se esse parâmetro não for passado, a função usa os dados default
#' que são instalados com o pacote. É importante que os nomes dos arquivos sejam
#' os mesmos da pasta default.
#'
#'
#' @return data.frame com os resultados da projeção de capacidade instalada
#' de micro e minigeração distribuída, número de adotantes e geração
#' mensal de energia.
#'
#' @export
#'
#'
#'@import tidyr
#'@import dplyr
#'
#'@encoding UTF-8
#'
#' @examples

epe4md_calcula_ajuste_2022 <- function(
  premissas_reg,
  ano_base,
  ano_max_resultado = 2050,
  altera_sistemas_existentes = FALSE,
  ano_decisao_alteracao = 2023,
  inflacao = 0.0375,
  taxa_desconto_nominal = 0.13,
  custo_reforco_rede = 200, #R$/kW para sistemas comercial_at_remoto
  ano_troca_inversor = 11,
  pagamento_disponibilidade = 0.3, #devido à variabilidade da FV, em alguns
  #meses o cliente pode pagar disponibilidade
  disponibilidade_kwh_mes = 100, #equivalente a um consumidor trifásico
  filtro_renda_domicilio = "maior_3sm",
  desconto_capex_local = 0,
  anos_desconto = 0,
  tx_cresc_grupo_a = 0.016,
  spb = 0.3,
  p_max = 0.01,
  q_max = 1,
  filtro_comercial = NA,
  ajuste_ano_corrente = FALSE,
  base_ano_corrente = NA,
  ultimo_mes_ajuste = NA,
  dir_dados_premissas = NA_character_) {

  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
    )

  dados_2022 <-
    readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd_ajuste2022.xlsx"))


  casos_payback <- epe4md_casos_payback(
    ano_base = ano_base,
    ano_max_resultado = ano_max_resultado,
    inflacao = inflacao,
    dir_dados_premissas = dir_dados_premissas
  )



  resultado_payback <- epe4md_payback(
    ano_base = ano_base,
    casos_payback = casos_payback,
    premissas_reg = premissas_reg,
    altera_sistemas_existentes = altera_sistemas_existentes,
    ano_decisao_alteracao = ano_decisao_alteracao,
    inflacao = inflacao,
    taxa_desconto_nominal = taxa_desconto_nominal,
    custo_reforco_rede = custo_reforco_rede,
    ano_troca_inversor = ano_troca_inversor,
    pagamento_disponibilidade = pagamento_disponibilidade,
    disponibilidade_kwh_mes = disponibilidade_kwh_mes,
    desconto_capex_local = desconto_capex_local,
    anos_desconto = anos_desconto,
    dir_dados_premissas = dir_dados_premissas

  )

  consumidores <- epe4md_mercado_potencial(
    ano_base = ano_base,
    tx_cresc_grupo_a = tx_cresc_grupo_a,
    filtro_renda_domicilio = filtro_renda_domicilio,
    filtro_comercial = filtro_comercial,
    dir_dados_premissas = dir_dados_premissas
  )

  casos_otimizados <- epe4md_calibra_curva_s(
    resultado_payback = resultado_payback,
    consumidores = consumidores,
    ano_base = ano_base,
    ano_max_resultado = ano_max_resultado,
    spb = spb,
    p_max = p_max,
    q_max = q_max,
    dir_dados_premissas = dir_dados_premissas)

  dados_gd <-
    readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx"))

  projecao <- casos_otimizados %>%
    mutate(mercado_potencial = round(exp(-spb * payback) * consumidores, 0),
           mercado_potencial = ifelse(mercado_potencial == 0,
                                      1,
                                      mercado_potencial)) %>%
    group_by(nome_4md, segmento) %>%
    arrange(ano) %>%
    mutate(adotantes_acum = mercado_potencial * Ft,
           adotantes_ano = ifelse(ano == 2013,
                                  adotantes_acum,
                                  adotantes_acum - lag(adotantes_acum)),
           adotantes_ano = ifelse(adotantes_ano < 0,
                                  0,
                                  round(adotantes_ano, 0)),
           adotantes_acum = cumsum(adotantes_ano)) %>%
    ungroup()

  #suavização em caso de adotantes = 0

  projecao <- projecao %>%
    group_by(nome_4md, segmento) %>%
    arrange(ano) %>%
    mutate(adotantes_ano_media = zoo::rollmean(adotantes_ano, 2,
                                               fill = NA, align = "left"),
           adotantes_ano_media = round(ifelse(is.na(adotantes_ano_media),
                                              adotantes_ano,
                                              adotantes_ano_media), 0))

  projecao <- projecao %>%
    mutate(adotantes_ano_c = case_when(
      adotantes_ano == 0 & ano > 2019 ~ adotantes_ano_media,
      lag(adotantes_ano) == 0 & ano > 2019 ~ lag(adotantes_ano_media),
      TRUE ~ adotantes_ano),
      adotantes_acum_c = cumsum(adotantes_ano_c)) %>%
    ungroup() %>%
    select(-adotantes_ano, -adotantes_acum, -adotantes_ano_media) %>%
    rename(
      adotantes_ano = adotantes_ano_c,
      adotantes_acum = adotantes_acum_c
    )

  # Abertura dos adotantes por fonte ----------------------------------------

  part_adot_fontes <- dados_gd %>%
    group_by(nome_4md, segmento, fonte_resumo) %>%
    summarise(adotantes_hist = sum(qtde_u_csrecebem_os_creditos)) %>%
    ungroup() %>%
    group_by(nome_4md, segmento) %>%
    mutate(adotantes_hist_total = sum(adotantes_hist),
           part_fonte = adotantes_hist / adotantes_hist_total) %>%
    ungroup()

  # completar faltantes para serem encontrados no join
  part_adot_fontes <- part_adot_fontes %>%
    complete(nome_4md, segmento, fonte_resumo) %>%
    group_by(nome_4md, segmento) %>%
    mutate(faltantes = sum(is.na(part_fonte))) %>%
    ungroup() %>%
    mutate(part_fonte = ifelse(faltantes == 4 & fonte_resumo == "Fotovoltaica",
                               1,
                               part_fonte),
           part_fonte = ifelse(is.na(part_fonte), 0, part_fonte)) %>%
    select(nome_4md, segmento, fonte_resumo, part_fonte)

  # historico de adotantes para substituir anos iniciais da projeção

  historico_adot_fontes <- dados_gd %>%
    group_by(ano, nome_4md, segmento, fonte_resumo) %>%
    summarise(adotantes_hist = sum(qtde_u_csrecebem_os_creditos)) %>%
    ungroup()


  #inclusao 2022

  historico_adot_fontes <- bind_rows(historico_adot_fontes, dados_2022)

  historico_adot_fontes <- historico_adot_fontes %>%
    complete(ano, nome_4md, segmento, fonte_resumo) %>%
    mutate(adotantes_hist = ifelse(is.na(adotantes_hist),
                                   0,
                                   adotantes_hist)) %>%
    select(-pot_hist)

  projecao <- left_join(projecao, part_adot_fontes,
                        by = c("nome_4md", "segmento")) %>%
    mutate(adotantes_ano = round(adotantes_ano * part_fonte, 0))

  projecao <- left_join(projecao, historico_adot_fontes,
                        by = c("nome_4md", "segmento", "ano", "fonte_resumo"))

  projecao <- projecao %>%
    mutate(adotantes_ano = ifelse(ano <= 2022,
                                  adotantes_hist,
                                  adotantes_ano)) %>%
    group_by(nome_4md, segmento, fonte_resumo) %>%
    arrange(ano) %>%
    mutate(adotantes_acum = cumsum(adotantes_ano)) %>%
    ungroup()

  #calculo percentual de adocao frente ao numero de consumidores totais


  consumidores_totais <- consumidores$consumidores_totais

  adotantes_segmento <- projecao %>%
    mutate(segmento = ifelse(segmento == "residencial_remoto",
                             "residencial",
                             segmento),
           segmento = ifelse(segmento == "comercial_at_remoto",
                             "comercial_bt",
                             segmento)) %>%
    group_by(ano, segmento) %>%
    summarise(adotantes = sum(adotantes_acum)) %>%
    ungroup()

  mercado_nicho <- consumidores$consumidores %>%
    mutate(segmento = ifelse(segmento == "residencial_remoto",
                             "residencial",
                             segmento),
           segmento = ifelse(segmento == "comercial_at_remoto",
                             "comercial_bt",
                             segmento)) %>%
    group_by(ano, segmento) %>%
    summarise(mercado_nicho = sum(consumidores)) %>%
    ungroup()

  part_adotantes <- left_join(adotantes_segmento, consumidores_totais,
                              by = c("ano", "segmento")) %>%
    mutate(penetracao_total = adotantes / total_ucs) %>%
    left_join(mercado_nicho, by = c("ano", "segmento")) %>%
    mutate(penetracao_nicho = adotantes / mercado_nicho)

  #lista com resultados

  proj_adotantes <- projecao

  lista_adotantes <- list(proj_adotantes, part_adotantes)
  names(lista_adotantes) <- c("proj_adotantes", "part_adotantes")

  #projecao potencia

  tabela_dist_subs <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/tabela_dist_subs.xlsx"))

  dados_gd <-
    readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx"))

  proj_adotantes <- lista_adotantes$proj_adotantes

  potencia_tipica <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/potencia_tipica.xlsx"))

  potencia_media <- dados_gd %>%
    group_by(nome_4md, segmento, fonte_resumo) %>%
    summarise(pot_total = sum(potencia_instalada_k_w),
              adotantes_total = sum(qtde_u_csrecebem_os_creditos),
              pot_media = pot_total / adotantes_total) %>%
    ungroup() %>%
    complete(nome_4md, segmento, fonte_resumo) %>%
    group_by(segmento, fonte_resumo) %>%
    mutate(pot_media = ifelse(is.na(pot_media), mean(pot_media, na.rm = TRUE),
                              pot_media)) %>%
    ungroup() %>%
    select(nome_4md, segmento, fonte_resumo, pot_media) %>%
    left_join(potencia_tipica, by = "segmento") %>%
    mutate(pot_media = ifelse(is.na(pot_media), pot_sistemas, pot_media)) %>%
    select(-pot_sistemas)

  proj_potencia <- left_join(proj_adotantes, potencia_media,
                             by = c("nome_4md", "segmento", "fonte_resumo"))


  # historico de adotantes para substituir anos iniciais da projeção

  historico_pot_fontes <- dados_gd %>%
    group_by(ano, nome_4md, segmento, fonte_resumo) %>%
    summarise(pot_hist = sum(potencia_instalada_k_w)) %>%
    ungroup()

  historico_pot_fontes <- bind_rows(historico_pot_fontes, dados_2022) %>%
    complete(ano, nome_4md, segmento, fonte_resumo) %>%
    mutate(pot_hist = ifelse(is.na(pot_hist), 0, pot_hist)) %>%
    select(-adotantes_hist)


  proj_potencia <- proj_potencia %>%
    mutate(pot_ano = adotantes_ano * pot_media)

  proj_potencia <- left_join(proj_potencia, historico_pot_fontes,
                             by = c("nome_4md", "segmento",
                                    "ano", "fonte_resumo"))

  proj_potencia <- proj_potencia %>%
    mutate(pot_ano = ifelse(ano <= 2022, pot_hist, pot_ano)) %>%
    ungroup()

  proj_potencia <- proj_potencia %>%
    mutate(pot_ano_mw = pot_ano / 1000) %>%
    group_by(nome_4md, segmento, fonte_resumo) %>%
    arrange(ano) %>%
    mutate(pot_acum_mw = cumsum(pot_ano_mw)) %>%
    ungroup()


  proj_potencia <- left_join(proj_potencia, tabela_dist_subs, by = "nome_4md")


  #lista resultados

  lista_potencia <- list(proj_potencia, lista_adotantes$part_adotantes)

  names(lista_potencia) <- c("proj_potencia", "part_adotantes")

  #extrair proj potencia da lista
  proj_potencia <- lista_potencia$proj_potencia

  if (ajuste_ano_corrente == TRUE) {

    dados_gd_historico <- base_ano_corrente %>%
      filter(data_conexao < make_date(ano_base + 1, ultimo_mes_ajuste + 1, 1))

  } else {

    dados_gd_historico <- readxl::read_xlsx(
      stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx"))
  }


  dados_gd_otimizacao <- readxl::read_xlsx(
    stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx"))

  meses <- tibble(mes = seq(1, 12))

  projecao_potencia <- proj_potencia %>%
    select(ano, nome_4md, segmento, fonte_resumo,
           pot_ano_mw, adotantes_ano) %>%
    crossing(meses) %>%
    mutate(mes_ano = make_date(ano, mes, 1),
           mes_ano = tsibble::yearmonth(mes_ano))

  #historico com dados reais

  historico <- dados_gd_historico %>%
    mutate(mes_ano = tsibble::yearmonth(data_conexao)) %>%
    group_by(mes_ano, fonte_resumo, segmento, nome_4md) %>%
    summarise(pot_mes_mw = sum(potencia_mw),
              adotantes_mes = sum(qtde_u_csrecebem_os_creditos)) %>%
    ungroup()

  if (ajuste_ano_corrente == TRUE) {

    historico_mensal <- projecao_potencia %>%
      filter(mes_ano < tsibble::yearmonth(make_date(ano_base + 1,
                                                    ultimo_mes_ajuste + 1, 1)))

  } else {

    historico_mensal <- projecao_potencia %>%
      filter(mes_ano < tsibble::yearmonth(make_date(ano_base + 1, 1, 1)))
  }

  historico_mensal <- historico_mensal %>%
    left_join(historico,
              by = c("mes_ano", "fonte_resumo", "segmento", "nome_4md")) %>%
    mutate(pot_mes_mw = ifelse(is.na(pot_mes_mw), 0, pot_mes_mw),
           adotantes_mes = ifelse(is.na(adotantes_mes), 0, adotantes_mes))

  # fatores historicos mensal retirando a sazonalidade

  historico_fatores <- dados_gd_otimizacao %>%
    mutate(mes_ano = tsibble::yearmonth(data_conexao)) %>%
    group_by(mes_ano, ano) %>%
    summarise(pot_mes_mw = sum(potencia_mw)) %>%
    ungroup() %>%
    filter(between(ano, 2014, 2019))

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

  if (ajuste_ano_corrente == TRUE) {

    projecao_potencia <- projecao_potencia %>%
      filter(mes_ano > tsibble::yearmonth(make_date(ano_base + 1,
                                                    ultimo_mes_ajuste, 1)))

  } else {

    projecao_potencia <- projecao_potencia %>%
      filter(mes_ano > tsibble::yearmonth(make_date(ano_base, 12, 1)))
  }

  projecao_potencia <- projecao_potencia %>%
    left_join(fatores_mensais, by = "mes") %>%
    mutate(pot_mes_mw = fator_mensal * pot_ano_mw / 12,
           adotantes_mes = round(fator_mensal * adotantes_ano / 12)) %>%
    select(- fator_mensal)


  #junção historico e projecao
  projecao_potencia <- bind_rows(historico_mensal, projecao_potencia)

  #inclusao fatores imitacao e inovacao para registro
  fatores_pq <- lista_potencia$proj_potencia %>%
    select(ano, segmento, fonte_resumo, nome_4md, p, q, Ft)

  projecao_mensal <- projecao_potencia %>%
    left_join(fatores_pq, by = c("ano", "segmento", "nome_4md", "fonte_resumo"))

  resultados_mensais <- epe4md_proj_geracao(
    proj_mensal = projecao_mensal,
    ano_base = ano_base,
    dir_dados_premissas = dir_dados_premissas
  )

}
