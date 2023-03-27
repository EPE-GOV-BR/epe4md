

#' Resume os resultados de capacidade instalada
#'
#' @param resultados_mensais data.frame. Saída da função
#' `epe4md::epe4md_calcula`
#'
#' @return data.frame com projeção de capacidade instalada nacional, em GW e
#' geração de energia, em GWh e MWméd.
#' @export
#'
#' @import dplyr
#' @import tibble
#'
#' @examples
#'
#' resultados_mensais <- tibble(
#'   data = c('2021-01-01', '2021-01-01', '2021-01-01'),
#'   ano = 2021,
#'   nome_4md = c("ENEL RJ", "ENF", "LIGHT"),
#'   subsistema = "SE",
#'   uf = "RJ",
#'   segmento = "comercial_at",
#'   fonte_resumo = "Fotovoltaica",
#'   energica_mwh = c(2019.6225, 69.06433, 2303.18754),
#'   energica_autoc_mwh = c(1615.698, 55.25146, 1842.55003),
#'   energia_inj_mwh = c(403.92450, 13.81287, 460.63751),
#'   energia_mwmed = c(2.714546380, 0.092828390, 3.095682180),
#'   pot_mes_mw = c(1.5275, 0.0000, 3.09568218),
#'   adotantes_mes = c(21, 0, 4),
#'   p = c(0.00027826110, 0.00060785180, 0.00137570060),
#'   q = c(1.0000000, 1.0000000, 0.5282319)
#' )
#'
#' epe4md_sumariza_resultados(resultados_mensais)
#'
epe4md_sumariza_resultados <- function(resultados_mensais) {

  result <- resultados_mensais %>%
    group_by(ano) %>%
    summarise(pot_ano = sum(pot_mes_mw / 1000),
              geracao_gwh = sum(energia_mwh / 1000)) %>%
    ungroup() %>%
    mutate(pot_acum = cumsum(pot_ano),
           geracao_mwmed = geracao_gwh / 8.76)


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
#' `system.file("dados_premissas/2021/premissas_reg.xlsx", package = "epe4md")`,
#' contém um exemplo de premissas de entrada.
#'
#'
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param ano_max_resultado numeric. Ano final para apresentação dos resultados.
#' Máximo igual a 2050. Default igual a 2050.
#' @param altera_sistemas_existentes logic. TRUE se alterações regulatórias
#' afetam investimentos realizados em anos anteriores à revisão da regulação.
#' Default igual a FALSE.
#' @param ano_decisao_alteracao numeric. Ano em que são definidas novas regras e
#' se tornam de conhecimento público. Esse parâmetro só tem efeito caso o
#' anterior seja igual a TRUE. Default igual a 2023.
#' @param inflacao mumeric. Taxa anual de inflacao considerada no reajuste das
#' tarifas e para calcular o retorno real de projetos. Default igual a 0.0375.
#' @param taxa_desconto_nominal numeric. Taxa de desconto nominal considerada
#' nos cálculos de payback descontado. Default igual a 0.13.
#' @param custo_reforco_rede numeric. Custo em R$/kW aplicado a projetos de
#' geracao remota em Alta Tensão. Representa um custo pago pelo empreendedor
#' para reforços na rede. Default igual a 200.
#' @param ano_troca_inversor numeric. Ano, a partir do ano de instalação, em que
#' é realizada a troca do inversor fotovoltaico. Default igual a 11.
#' @param pagamento_disponibilidade numeric. Percentual de meses em que o
#' consumidor residencial paga custo de disponbilidade em função da
#' variabilidade da geração FV. Default igual a 0.3.
#' @param disponibilidade_kwh_mes numeric. Consumo de disponbilidade do
#' consumidor em kWh/mês. Default igual a 100, equivalente a um consumidor
#' trifásico.
#' @param filtro_renda_domicilio string. Define o filtro aplicado a consumidores
#' residenciais, de acordo com a renda mensal do responsável, em salários
#' mínimos. Permite: "total", "maior_1sm", maior_2sm", "maior_3sm" ou
#' "maior_5sm". Default igual a "maior_3sm".
#' @param desconto_capex_local numeric. Percentual de desconto a ser aplicado no
#' CAPEX de sistemas de geração local(ex: 0.1) para simulação de incentivos.
#' Default igual a 0.
#' @param anos_desconto vector. Anos em que há a incidência do desconto no
#' CAPEX. Default igual a 0.
#' @param filtro_comercial numeric. Fator percentual para definir o nicho do
#' segmento comercial. Default é calculado pelo modelo com base no nicho
#' residencial.
#' @param spb numeric. Fator de Sensibilidade ao Payback (SPB).
#' Default igual a 0.3.
#' @param p_max numeric. Fator de inovação (p) máximo. Default igual a 0.01.
#' @param q_max numeric. Fator de imitação (q) máximo. Default igual a 1.
#' @param tx_cresc_grupo_a numeric. Taxa de crescimento anual dos consumidores
#' cativos do Grupo A. Default igual a 0.016 representa crescimento entre
#' 2006 e 2019.
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
#' @param dir_dados_premissas Diretório onde se encontram as premissas. Se esse
#' parâmetro não for passado, a função usa os dados default que são instalados
#' com o pacote. É importante que os nomes dos arquivos sejam os mesmos da
#' pasta default.
#' @param sequencial logic. Parâmetro que define se a projeção deve ser
#' realizada de forma sequencial ou paralela. Para executar a projeção de forma
#' sequencial defina o parâmetro como TRUE. Para executar a projeção de forma
#' paralela, defina o parâmetro como FALSE.
#' @param filter_uf string. Parâmetro que define uma unidade federativa (UF) a
#' ser filtrada. Para um resultado com todas as unidades federativas, defina o
#' parâmetro como "N".
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
#'@rawNamespace import(assertthat, except=c(has_name))
#'
#'@encoding UTF-8
#'
#' @examples
#'
#' premissas_regulatorias <- tibble(
#'   ano = 2021,
#'   alternativa = 0,
#'   p_transicao = 1.00,
#'   binomia = FALSE,
#'   demanda_g = FALSE
#' )
#'
#' resultado <- epe4md_calcula(
#'   ano_base = 2021,
#'   premissas_reg = premissas_regulatorias,
#'   ano_max_resultado = 2027)
#'
epe4md_calcula <- function(
  premissas_reg,
  ano_base,
  sequencial,
  filter_uf,
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
  filtro_comercial = NA_real_,
  ajuste_ano_corrente = FALSE,
  ultimo_mes_ajuste = NA_integer_,
  metodo_ajuste = NA_character_,
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
  assert_that(is.logical(sequencial))
  assert_that(is.flag(altera_sistemas_existentes))
  assert_that(is.number(ano_decisao_alteracao))
  assert_that(is.number(inflacao))
  assert_that(is.number(taxa_desconto_nominal))
  assert_that(is.number(custo_reforco_rede))
  assert_that(is.number(ano_troca_inversor))
  assert_that(is.number(pagamento_disponibilidade))
  assert_that(is.number(disponibilidade_kwh_mes))
  assert_that(
    assert_in(
      filtro_renda_domicilio,
      categorias = c(

        "total",
        "maior_1sm",
        "maior_2sm",
        "maior_3sm",
        "maior_5sm"

      )
    )
  )


  assert_that(is.number(desconto_capex_local))
  assert_that(is.number(anos_desconto))
  assert_that(is.number(tx_cresc_grupo_a))
  assert_that(is.number(spb))
  assert_that(is.number(p_max))
  assert_that(is.number(q_max))


  assertthat::assert_that(ano_max_resultado <= 2050)


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
    dir_dados_premissas = dir_dados_premissas
  )

  resultado_payback <- epe4md_payback(
    ano_base = ano_base,
    casos_payback = casos_payback,
    premissas_reg = premissas_reg,
    sequencial = sequencial,
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

  rm(casos_payback)

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

  lista_adotantes <- epe4md_proj_adotantes(
    casos_otimizados = casos_otimizados,
    consumidores = consumidores,
    ano_base = ano_base,
    dir_dados_premissas = dir_dados_premissas)

  rm(casos_otimizados)
  rm(consumidores)

  lista_potencia <- epe4md_proj_potencia(
    lista_adotantes = lista_adotantes,
    ano_base = ano_base,
    dir_dados_premissas = dir_dados_premissas)

  rm(lista_adotantes)

  proj_mensal <- epe4md_proj_mensal(
    lista_potencia = lista_potencia,
    ano_base = ano_base,
    dir_dados_premissas = dir_dados_premissas,
    ano_max_resultado = ano_max_resultado,
    ajuste_ano_corrente = ajuste_ano_corrente,
    metodo_ajuste = metodo_ajuste,
    ultimo_mes_ajuste = ultimo_mes_ajuste
  )

  rm(lista_potencia)

  resultados_mensais <- epe4md_proj_geracao(
    proj_mensal = proj_mensal,
    ano_base = ano_base,
    filter_uf = filter_uf,
    dir_dados_premissas = dir_dados_premissas
  )

  resultados_mensais
}
