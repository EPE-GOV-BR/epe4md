

#' Resume os resultados de capacidade instalada
#'
#' @param resultados_mensais data.frame. Saída da função
#' `epe4md::epe4md_calcula`
#'
#' @return data.frame com projeção de capacidade instalada nacional, em GW e
#' geração de energia, em GWh e MWméd.
#' @export
#'
#'@import dplyr
#'
#' @examples
#' \dontrun{
#' resultados <- epe4md_calcula(premissas_reg, ano_base = 2023)
#' resumo <- epe4md_sumariza_resultados(resultados)
#' }
#'

epe4md_sumariza_resultados <- function(resultados_mensais) {

  if("cap_bateria_mwh" %in% colnames(resultados_mensais)){
    result <- resultados_mensais %>%
      group_by(ano) %>%
      summarise(pot_ano_gw = sum(pot_mes_mw / 1000),
                pot_ano_bat_gw = sum(pot_mes_bateria_mw / 1000),
                cap_bat_mwh = sum(cap_bateria_mwh),
                geracao_gwh = sum(energia_mwh / 1000)
      ) %>%
      ungroup() %>%
      mutate(pot_acum_gw = cumsum(pot_ano_gw),
             pot_acum_bat_gw = cumsum(pot_ano_bat_gw),
             cap_acum_bat_mwh = cumsum(cap_bat_mwh),
             geracao_mwmed = geracao_gwh / 8.76)
  } else {
    result <- resultados_mensais %>%
      group_by(ano) %>%
      summarise(pot_ano_gw = sum(pot_mes_mw / 1000),
                geracao_gwh = sum(energia_mwh / 1000)) %>%
      ungroup() %>%
      mutate(pot_acum_gw = cumsum(pot_ano_gw),
             geracao_mwmed = geracao_gwh / 8.76)
  }

}


#' Roda o modelo 4MD
#'
#'
#' O resultado do modelo 4MD são projeções de capacidade instalada, número de
#' adotantes e geração de energia em base mensal.
#'
#' @param premissas_reg data.frame. Input de premissas regulatórias para serem
#' consideradas nos cálculos. O dataframe deve ter as seguintes colunas
#' * ano, numérico
#' * alternativa, numérico. Uma das seguintes opções:
#'     + 0: Consumidor compensa todas as componentes tarifárias;
#'     + 1: Paga TUSD Distribuição;
#'     + 2: Anterior + TUSD Transmissão.
#'     + 3: Anterior + TUSD Encargos.
#'     + 4: Anterior + TUSD Perdas.
#'     + 5: Anterior + TE Encargos. Ou seja, compensa somente a TE Energia.
#' * p_transicao, numérico. Parcela do custo da alternativa escolhida no parâmetro alternativa
#' a ser pago pelo consumidor
#' * binomia e, binário. Define se há cobrança de uma
#' tarifa binômia na baixa tensão, em que as
#' componentes TUSD Distribuição e TUSD Transmissão passariam a ser cobradas de forma fixa, não sendo passíveis de compensação
#' * demanda_g, binário. Define se há cobrança de TUSDg para a demanda de consumidores do
#' grupo A. Caso seja `FALSE`, é considerada a cobrança da TUSD consumo.
#'
#' Um arquivo excel
#' instalado com este pacote, acessível via
#' `system.file("dados_premissas/{ano_base}/premissas_reg.xlsx", package = "epe4md")`,
#' contém um exemplo de premissas de entrada.
#'
#'
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param ano_max_resultado numeric. Ano final para apresentação dos resultados.
#' Máximo igual a 2060. Default igual a 2060.
#' @param altera_sistemas_existentes logic. TRUE se alterações regulatórias
#' afetam investimentos realizados em anos anteriores à revisão da regulação.
#' Default igual a FALSE.
#' @param ano_decisao_alteracao numeric. Ano em que são definidas novas regras e
#' se tornam de conhecimento público. Esse parâmetro só tem efeito caso o
#' anterior seja igual a TRUE. Default igual a 2023.
#' @param ano_final_alteracao numeric. Até qual ano as alterações regulatórias
#' previstas afetam investimentos presentes. Só válido quando
#' altera_sistemas_existentes igual a TRUE. Default igual a 2060.
#' @param inflacao mumeric. Taxa anual de inflacao considerada no reajuste das
#' tarifas e para calcular o retorno real de projetos. Default igual a 0.0375.
#' @param taxa_desconto_nominal numeric. Taxa de desconto nominal considerada
#' nos cálculos de payback descontado. Default igual a 0.13.
#' @param custo_reforco_rede numeric. Custo em R$/kW aplicado a projetos de
#' geracao remota em Alta Tensão. Representa um custo pago pelo empreendedor
#' para reforços na rede. Default igual a 200.
#' @param ano_troca_inversor numeric. Ano, a partir do ano de instalação, em que
#' é realizada a troca do inversor fotovoltaico. Default igual a 11.
#' @param ano_recapex_bat numeric. Ano em que será feito um investimento adicional em baterias
#' para compensar a degradação. Default igual a 11.
#' @param fator_custo_inversor numeric. Custo do inversor para a substituição em
#' percentual do CAPEX total do sistema de geração. Default igual a 0.15.
#' @param pagamento_disponibilidade. numeric. Percentual de meses em que o
#' consumidor residencial paga custo de disponbilidade em função da
#' variabilidade da geração FV. Default igual a 0.3. Tem efeito somente até o
#' ano de 2022.
#' @param disponibilidade_kwh_mes numeric. Consumo de disponbilidade do
#' consumidor em kWh/mês. Default igual a 100, equivalente a um consumidor
#' trifásico. Tem efeito somente até o ano de 2022.
#' @param filtro_renda_domicilio string. Define o filtro aplicado a consumidores
#' residenciais, de acordo com a renda mensal do responsável, em salários
#' mínimos. Permite: "total", "maior_1sm", maior_2sm", "maior_3sm" ou
#' "maior_5sm". Default igual a "maior_3sm".
#' @param filtro_renda_bateria string. Define o filtro aplicado a consumidores
#' residenciais para investimento em baterias, de acordo com a renda mensal do
#' responsável, em salários mínimos. Permite: "total", "maior_1sm, maior_2sm",
#' "maior_3sm", "maior_5sm" ou "maior_10sm". Default igual a "maior_10sm".
#' @param fator_local_comercial string. Define a origem dos dados do Fator de
#' Aptidão Local "FAL" para os consumidores não residenciais atendidos em baixa
#' tensão. Como default, são utilizados os mesmos valores dos consumidores
#' residenciais. Caso selecionado "historico", utiliza o histórico do percentual
#' de adotantes locais por distribuidora até o ano base.
#' @param desconto_capex_local numeric. Percentual de desconto a ser aplicado no
#' CAPEX de sistemas de geração local(ex: 0.1) para simulação de incentivos.
#' Default igual a 0.
#' @param anos_desconto vector. Anos em que há a incidência do desconto no
#' CAPEX.Ex: c(2024, 2025). Default igual a 0.
#' @param tarifa_bonus integer. Tarifa que representa benefícios adicionais
#' da MMGD. Valor em R$/kWh. A tarifa é multiplicada pela energia injetada na
#' rede para formar uma receita adicional ao empreendimento. Default igual a 0.
#' @param ano_inicio_bonus integer. Ano em que o bônus começa a ser incorporado
#' na receita.
#' @param custo_deficit numeric. Custo atribuído pelo consumidor à interrupção
#' de energia. Em R$/MWh. Default = 0.
#' @param filtro_comercial numeric. Fator percentual para definir o nicho do
#' segmento comercial. Default é calculado pelo modelo com base no nicho
#' residencial.
#' @param p_max numeric. Fator de inovação (p) máximo. Default igual a 0.01.
#' @param q_max numeric. Fator de imitação (q) máximo. DEfault igual a 1.
#' @param curva_bateria string. Pode ser "propria" (Default) ou "lag". A primeira
#' opção utiliza os fatores p_bat e q_bat. A opção "lag" utiliza a curva de
#' difusão de MMGD, atrasada no tempo, conforme o parâmetro inicio_curva_bateria.
#' @param p_bat numeric. Fator de inovação (p) para baterias. Default igual a 0.0015.
#' @param q_bat numeric. Fator de imitação (q) para_baterias. Default igual a 0.3.
#' @param inicio_curva_bateria numeric. Indica o ano que começa a difusão
#' das baterias. Default igual a 2023.
#' @param tx_cresc_grupo_a numeric. Taxa de crescimento anual dos consumuidores
#' cativos do Grupo A. Default igual a 0.
#' @param cresc_fv numeric. Taxa de crescimento da participação da fonte
#' fotovoltaica. Default igual a 0 (mantém participação histórica).
#' @param cresc_eol numeric. Taxa de crescimento da participação da fonte
#' eólica. Default igual a 0 (mantém participação histórica).
#' @param cresc_cgh numeric. Taxa de crescimento da participação da fonte
#' hidráulica. Default igual a 0 (mantém participação histórica).
#' @param cresc_ute numeric. Taxa de crescimento da participação da fonte
#' termelétrica. Default igual a 0 (mantém participação histórica).
#' @param ajuste_ano_corrente logic. Se TRUE indica que a projeção deverá
#' incorporar o histórico mensal recente, verificado em parte do primeiro ano
#' após o ano base. Default igual a FALSE. O arquivo base_mmgd.xlsx deve
#' incorporar esse histórico.
#' @param ultimo_mes_ajuste numeric. Último mês com dados completos na
#' base_ano_corrente. Default igual a NA. Só tem efeito caso ajuste_ano_corrente
#' seja igual a TRUE.
#' @param metodo_ajuste string. Se igual a "extrapola" o modelo irá extrapolar a
#' potência e o número de adotantes até o final do ano base + 1 com base no
#' verificado até o ultimo_mes_ajuste. Default igual a NA. Só tem efeito caso
#' ajuste_ano_corrente seja igual a TRUE.
#' @param bateria_eficiencia numeric. Eficiência da bateria. Default igual a 0.90.
#' @param degradacao_bateria_mil_ciclos numeric. Degradação linear da bateria,
#' em percentual a cada 1000 ciclos. Default igual a 10%.
#' @param simula_bateria. Define se o modelo irá considerar a projeção de
#' baterias no resultado final. Default igual a FALSE.
#' @param dir_dados_premissas Diretório onde se encontram as premissas. Se esse
#' parâmetro não for passado, a função usa os dados default que são instalados
#' com o pacote. É importante que os nomes dos arquivos sejam os mesmos da
#' pasta default.
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
#'@import assertthat
#'
#'@encoding UTF-8
#'
#' @examples
#' \dontrun{
#' resultados <- epe4md_calcula(premissas_reg, ano_base = 2023)
#' resumo <- epe4md_sumariza_resultados(resultados)
#' }
#'

epe4md_calcula <- function(
  premissas_reg,
  ano_base,
  ano_max_resultado = 2060,
  altera_sistemas_existentes = FALSE,
  ano_decisao_alteracao = 2023,
  ano_final_alteracao = 2060,
  inflacao = 0.0375,
  taxa_desconto_nominal = 0.13,
  custo_reforco_rede = 200,
  ano_troca_inversor = 11,
  fator_custo_inversor = 0.15,
  pagamento_disponibilidade = 0.3,
  disponibilidade_kwh_mes = 100,
  filtro_renda_domicilio = "maior_3sm",
  fator_local_comercial = "residencial",
  desconto_capex_local = 0,
  anos_desconto = 0,
  tarifa_bonus = 0,
  ano_inicio_bonus = 2099,
  tx_cresc_grupo_a = 0,
  p_max = 0.01,
  q_max = 1,
  filtro_comercial = NA_real_,
  cresc_fv = 0,
  cresc_eol = 0,
  cresc_cgh = 0,
  cresc_ute = 0,
  ajuste_ano_corrente = FALSE,
  ultimo_mes_ajuste = NA_integer_,
  metodo_ajuste = NA_character_,
  simula_bateria = FALSE,
  bateria_eficiencia = 0.9,
  degradacao_bateria_mil_ciclos = 0.1,
  ano_recapex_bat = 11,
  filtro_renda_bateria = "maior_10sm",
  custo_deficit = 0,
  inicio_curva_bateria = 2023,
  curva_bateria = "propria",
  p_bat = 0.0015,
  q_bat = 0.3,
  dir_dados_premissas = NA_character_
  )
{


  assert_that(premissas_reg %has_name% c(
      "ano",
      "alternativa",
      "p_transicao",
      "binomia",
      "demanda_g"
    )
  )


  assert_that(is.numeric(premissas_reg$ano))
  assert_that(is.numeric(premissas_reg$alternativa))
  assert_that(is.numeric(premissas_reg$p_transicao))
  assert_that(is.logical(premissas_reg$binomia))
  assert_that(is.logical(premissas_reg$demanda_g))

  assert_that(is.number(ano_base))
  assert_that(is.flag(altera_sistemas_existentes))
  assert_that(is.flag(ajuste_ano_corrente))
  assert_that(is.number(ano_decisao_alteracao))
  assert_that(is.number(ano_final_alteracao))
  assert_that(is.number(inflacao))
  assert_that(is.number(taxa_desconto_nominal))
  assert_that(is.number(tarifa_bonus))
  assert_that(is.number(cresc_fv))
  assert_that(is.number(cresc_eol))
  assert_that(is.number(cresc_cgh))
  assert_that(is.number(cresc_ute))
  assert_that(is.number(ano_inicio_bonus))
  assert_that(is.number(inicio_curva_bateria))
  assert_that(is.number(bateria_eficiencia))
  assert_that(is.number(degradacao_bateria_mil_ciclos))
  assert_that(is.number(custo_deficit))
  assert_that(is.number(custo_reforco_rede))
  assert_that(is.number(ano_troca_inversor))
  assert_that(is.number(ano_recapex_bat))
  assert_that(is.number(fator_custo_inversor))
  assert_that(is.number(pagamento_disponibilidade))
  assert_that(is.number(disponibilidade_kwh_mes))
  assert_that(
    assert_in(
      filtro_renda_domicilio,
      categorias = c(
        "maior_1sm",
        "maior_2sm",
        "maior_3sm",
        "maior_5sm",
        "maior_10sm"
      )
    )
  )

  assert_that(
    assert_in(
      filtro_renda_bateria,
      categorias = c(
        "maior_1sm",
        "maior_2sm",
        "maior_3sm",
        "maior_5sm",
        "maior_10sm"
      )
    )
  )

  assert_that(
    assert_in(
      fator_local_comercial,
      categorias = c(

        "residencial",
        "historico"

      )
    )
  )

  assert_that(
    assert_in(
      curva_bateria,
      categorias = c(

        "lag",
        "propria"

      )
    )
  )

  assert_that(is.number(filtro_comercial) || is.na(filtro_comercial))
  assert_that(is.number(desconto_capex_local))
  assert_that(is.number(anos_desconto))
  assert_that(is.number(tx_cresc_grupo_a))
  assert_that(is.number(p_max))
  assert_that(is.number(q_max))
  assert_that(is.number(p_bat))
  assert_that(is.number(q_bat))
  assert_that(is.flag(simula_bateria))


  assertthat::assert_that(ano_max_resultado <= 2060)
  assertthat::assert_that(inicio_curva_bateria >= 2013)
  assert_that(bateria_eficiencia >= 0 && bateria_eficiencia <= 1)
  assert_that(degradacao_bateria_mil_ciclos >= 0 && degradacao_bateria_mil_ciclos <= 1)


  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
            system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                        package = "epe4md"),
            dir_dados_premissas
    )


  casos_payback <- epe4md_casos_payback(
    ano_base = ano_base,
    ano_max_resultado = ano_max_resultado,
    inflacao = inflacao,
    ano_troca_inversor = ano_troca_inversor,
    ano_recapex_bat = ano_recapex_bat,
    fator_custo_inversor = fator_custo_inversor,
    dir_dados_premissas = dir_dados_premissas
  )


  resultado_payback <- epe4md_payback(
    ano_base = ano_base,
    casos_payback = casos_payback,
    premissas_reg = premissas_reg,
    altera_sistemas_existentes = altera_sistemas_existentes,
    ano_decisao_alteracao = ano_decisao_alteracao,
    ano_final_alteracao = ano_final_alteracao,
    inflacao = inflacao,
    taxa_desconto_nominal = taxa_desconto_nominal,
    custo_reforco_rede = custo_reforco_rede,
    ano_troca_inversor = ano_troca_inversor,
    ano_recapex_bat = ano_recapex_bat,
    degradacao_bateria_mil_ciclos = degradacao_bateria_mil_ciclos,
    pagamento_disponibilidade = pagamento_disponibilidade,
    disponibilidade_kwh_mes = disponibilidade_kwh_mes,
    desconto_capex_local = desconto_capex_local,
    anos_desconto = anos_desconto,
    tarifa_bonus = tarifa_bonus,
    ano_inicio_bonus = ano_inicio_bonus,
    custo_deficit = custo_deficit,
    bateria_eficiencia = bateria_eficiencia,
    dir_dados_premissas = dir_dados_premissas
  )


  consumidores <- epe4md_mercado_potencial(
    ano_base = ano_base,
    tx_cresc_grupo_a = tx_cresc_grupo_a,
    filtro_renda_domicilio = filtro_renda_domicilio,
    filtro_renda_bateria = filtro_renda_bateria,
    filtro_comercial = filtro_comercial,
    fator_local_comercial = fator_local_comercial,
    dir_dados_premissas = dir_dados_premissas
  )

  casos_otimizados <- epe4md_calibra_curva_s(
    resultado_payback = resultado_payback,
    consumidores = consumidores,
    ano_base = ano_base,
    ano_max_resultado = ano_max_resultado,
    p_max = p_max,
    q_max = q_max,
    inicio_curva_bateria = inicio_curva_bateria,
    curva_bateria = curva_bateria,
    p_bat = p_bat,
    q_bat = q_bat,
    dir_dados_premissas = dir_dados_premissas)

  lista_adotantes <- epe4md_proj_adotantes(
    casos_otimizados = casos_otimizados,
    consumidores = consumidores,
    ano_base = ano_base,
    cresc_fv = cresc_fv,
    cresc_eol = cresc_eol,
    cresc_cgh = cresc_cgh,
    cresc_ute = cresc_ute,
    dir_dados_premissas = dir_dados_premissas)

  lista_potencia <- epe4md_proj_potencia(
    lista_adotantes = lista_adotantes,
    ano_base = ano_base,
    dir_dados_premissas = dir_dados_premissas)

  proj_mensal <- epe4md_proj_mensal(
    lista_potencia = lista_potencia,
    ano_base = ano_base,
    dir_dados_premissas = dir_dados_premissas,
    ano_max_resultado = ano_max_resultado,
    ajuste_ano_corrente = ajuste_ano_corrente,
    metodo_ajuste = metodo_ajuste,
    ultimo_mes_ajuste = ultimo_mes_ajuste
  )

  resultados_mensais <- epe4md_proj_geracao(
    proj_mensal = proj_mensal,
    ano_base = ano_base,
    bateria_eficiencia = bateria_eficiencia,
    degradacao_bateria_mil_ciclos = degradacao_bateria_mil_ciclos,
    simula_bateria = simula_bateria,
    ano_recapex_bat = ano_recapex_bat,
    dir_dados_premissas = dir_dados_premissas
  )

  resultados_mensais
}
