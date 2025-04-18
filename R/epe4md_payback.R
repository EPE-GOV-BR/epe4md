#' Roda um fluxo de caixa para cada caso e retorna métricas financeiras.
#'
#' @param casos_payback data.frame. Base gerada pela função
#' [epe4md::epe4md_casos_payback]
#' @param premissas_reg data.frame. Input de premissas regulatórias para serem
#' consideradas nos cálculos.
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
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
#' @param degradacao_bateria_mil_ciclos numeric. Degradação linear da bateria,
#' em percentual a cada 1000 ciclos. Default igual a 10%.
#' @param pagamento_disponibilidade. numeric. Percentual de meses em que o
#' consumidor residencial paga custo de disponbilidade em função da
#' variabilidade da geração FV. Default igual a 0.3. Tem efeito somente até o
#' ano de 2022.
#' @param disponibilidade_kwh_mes numeric. Consumo de disponbilidade do
#' consumidor em kWh/mês. Default igual a 100, equivalente a um consumidor
#' trifásico. Tem efeito somente até o ano de 2022.
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
#' @param bateria_eficiencia numeric. Eficiência das baterias. Default igual a 0.95.
#' @param custo_deficit numeric. Custo atribuído pelo consumidor à interrupção
#' de energia. Em R$/MWh. Default = 0.
#' @param dir_dados_premissas Diretório onde se encontram as premissas. Se esse
#' parâmetro não for passado, a função usa os dados default que são instalados
#' com o pacote. É importante que os nomes dos arquivos sejam os mesmos da
#' pasta default.
#'
#' @return data.frame. Métricas financeiras para cada caso.
#' @export
#'
#'@import tidyr
#'@import purrr
#'@import readxl
#'@import dplyr
#'
#'@encoding UTF-8
#'
#' @examples

epe4md_payback <- function(
    casos_payback,
    premissas_reg,
    ano_base,
    altera_sistemas_existentes = TRUE,
    ano_decisao_alteracao = 2023,
    ano_final_alteracao = 2060,
    inflacao = 0.0375,
    taxa_desconto_nominal = 0.13,
    custo_reforco_rede = 200,
    ano_troca_inversor = 11,
    ano_recapex_bat = 11,
    degradacao_bateria_mil_ciclos = 0.1,
    pagamento_disponibilidade = 0.3,
    disponibilidade_kwh_mes = 100,
    desconto_capex_local = 0,
    anos_desconto = 0,
    tarifa_bonus = 0,
    ano_inicio_bonus = 2099,
    custo_deficit = 0,
    bateria_eficiencia = 0.9,
    dir_dados_premissas = NA_character_
) {


  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )


  fator_construcao <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/tempo_construcao.xlsx"),
                                sheet = "fator")

  tarifas <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/tarifas_4md.xlsx"))

  tarifas_comercial_at <- tarifas %>%
    filter(subgrupo == "A4") %>%
    mutate(tarifa_demanda_g = 0,
           tarifa_demanda_c = 0,
           segmento = "comercial_at")

  tarifas_demanda_a <- tarifas %>%
    filter(subgrupo == "A4") %>%
    select(nome_4md, ano, tarifa_demanda_c, tarifa_demanda_g) %>%
    unique()

  tarifas_comercial_at_remoto <- tarifas %>%
    filter(subgrupo == "B3") %>%
    mutate(segmento = "comercial_at_remoto") %>%
    select(-tarifa_demanda_c, - tarifa_demanda_g) %>%
    left_join(tarifas_demanda_a, by = c("ano", "nome_4md"))

  tarifas_residencial <- tarifas %>%
    filter(subgrupo == "B1") %>%
    mutate(segmento = "residencial")

  tarifas_residencial_remoto <- tarifas %>%
    filter(subgrupo == "B1") %>%
    mutate(segmento = "residencial_remoto")

  tarifas_comercial_bt <- tarifas %>%
    filter(subgrupo == "B3") %>%
    mutate(segmento = "comercial_bt")

  tarifas <- bind_rows(tarifas_comercial_at, tarifas_comercial_at_remoto,
                       tarifas_residencial,
                       tarifas_residencial_remoto, tarifas_comercial_bt)

# premissas regulatorias

  anos <- tibble(ano = seq(2013, 2060, 1))

  premissas_regulatorias <- left_join(anos, premissas_reg, by = "ano") %>%
    mutate(across(
      .cols = c(binomia, demanda_g),
      .fns = ~as.logical(.x))) %>%
    fill(2:5) %>%
    mutate(p_transicao = ifelse(is.na(p_transicao), 1, p_transicao),
           alternativa = ifelse(is.na(alternativa), 0, alternativa),
           across(
             .cols = c(binomia, demanda_g),
             .fns = ~ifelse(is.na(.x), FALSE, .x)
           )
  )

  premissas_regulatorias <- premissas_regulatorias %>%
    mutate(alternativa = ifelse(binomia == TRUE & alternativa %in% c(0, 1),
                                2,
                                alternativa))


  fluxo_de_caixa <- function(nome_4md, ano, segmento, vida_util,
                             fator_autoconsumo, geracao_1_kwh, degradacao,
                             capex_inicial, capex_inversor, oem_anual,
                             pot_sistemas, bateria_eficiencia, bateria_recapex,
                             bateria_capex_inicial, bateria_oem_anual, dec) {

    # # auxilio debug
    # ano_base = 2023
    # nome_4md="FORCEL"
    # ano=2038
    # inflacao = 0.0375
    # segmento="comercial_at"
    # vida_util=25
    # fator_autoconsumo=0.8
    # geracao_1_kwh=99195.33825
    # degradacao=0.005
    # capex_inicial=488370
    # capex_inversor=98449.2663270433
    # oem_anual=0.01
    # pot_sistemas=73
    # ano_troca_inversor = 11
    # altera_sistemas_existentes = TRUE
    # ano_decisao_alteracao = 2023
    # ano_final_alteracao = 2034
    # tarifa_bonus = 0.2
    # ano_inicio_bonus = 2027
    # desconto_capex_local = 0
    # anos_desconto = 0
    # pagamento_disponibilidade = 0.3
    # disponibilidade_kwh_mes = 100


    fluxo_caixa <- data.frame("ano_simulacao" = 1:vida_util,
                              "segmento" = segmento,
                              "nome_4md" = nome_4md,
                              "ano" = ano)

    fluxo_caixa <- fluxo_caixa %>%
      left_join(fator_construcao, by = "segmento") %>%
      mutate(fator_construcao = ifelse(ano_simulacao == 1, fator_construcao, 1),
             ano_inicial = ano)


    if (altera_sistemas_existentes == TRUE && ano >= ano_decisao_alteracao) {
      fluxo_caixa <- fluxo_caixa %>%
        mutate(ano = row_number() + ano - 1,
               ano = ifelse(ano > 2060, 2060, ano),
               ano = ifelse(ano >= ano_final_alteracao, ano_final_alteracao, ano),
               ano = ifelse(ano_inicial > ano, ano_inicial, ano)) %>%
        select(-ano_inicial)
    }


    fluxo_caixa <- left_join(fluxo_caixa, premissas_regulatorias, by = "ano")

    fluxo_caixa <- left_join(fluxo_caixa, tarifas,
                             by = c("ano", "nome_4md",
                                    "segmento", "alternativa"))

    fluxo_caixa <- fluxo_caixa %>%
      mutate(tarifa_autoc_tusd = ifelse(binomia == TRUE,
                                        tarifa_autoc_bin_tusd,
                                        tarifa_autoc_tusd),
             tarifa_demanda = ifelse(demanda_g == TRUE,
                                     tarifa_demanda_g,
                                     tarifa_demanda_c),
             custo_obra = ifelse(segmento == "comercial_at_remoto" &
                                   row_number() == 1,
                                 - custo_reforco_rede * pot_sistemas, 0),
             consumo_disponibilidade = ifelse(segmento == "comercial_at" |
                                                binomia == TRUE,
                                              0,
                                              disponibilidade_kwh_mes)) %>%
      select(-tarifa_demanda_c, -tarifa_demanda_g, -tarifa_autoc_bin_tusd)


    fluxo_caixa <- fluxo_caixa %>%
      mutate(energia_ano = geracao_1_kwh * fator_construcao *
               (1 + degradacao)^(1 - ano_simulacao),
             energia_autoc = energia_ano * fator_autoconsumo,
             energia_inj = energia_ano - energia_autoc,
             capex = 0,
             troca_inversor = 0,
             bateria_capex = 0,
             capex_obra = 0,
             taxa_inflacao = (1 + inflacao)^(ano_simulacao - 1))

    fluxo_caixa[1, "capex"] <- -capex_inicial
    fluxo_caixa[ano_troca_inversor, "troca_inversor"] <- -capex_inversor
    fluxo_caixa[1, "bateria_capex"] <- -bateria_capex_inicial
    fluxo_caixa[ano_recapex_bat, "bateria_capex"] <- -bateria_recapex *
      (ano_recapex_bat - 1) * 365 * degradacao_bateria_mil_ciclos / 1000 #percentual
    #da bateria degradada até o recapex.

    fluxo_caixa <- fluxo_caixa %>% mutate(
      desconto_capex = desconto_capex_local,
      capex = ifelse(segmento %in% c("residencial", "comercial_bt",
                                     "comercial_at") & ano %in% anos_desconto, capex *
                       (1 - desconto_capex), capex))



    # ano auxiliar para possibilitar alteracoes com base no ano
    fluxo_caixa <- fluxo_caixa %>%
      mutate(ano_real = row_number() + first(ano) - 1,
             ano_real = ifelse(ano_real > 2060, 2060, ano_real))



    fluxo_caixa <- fluxo_caixa %>%
      mutate(receita_autoc = (taxa_inflacao * energia_autoc *
                                (tarifa_autoc_tusd + tarifa_autoc_te)) /
               (1 - impostos_cheio),
             receita_inj_completa = (taxa_inflacao * energia_inj *
                                       tarifa_inj_te / (1 - impostos_te)) +
               (taxa_inflacao * energia_inj * tarifa_inj_tusd /
                  (1 - impostos_tusd)),
             receita_bat_backup = taxa_inflacao * dec * (custo_deficit / 1000) *
               (geracao_1_kwh / 8760),
             receita_bat_arbitragem = taxa_inflacao * energia_inj * bateria_eficiencia *
               (tarifa_autoc_tusd + tarifa_autoc_te) / (1 - impostos_cheio),
             pag_compensacao = ((taxa_inflacao * energia_inj * pag_inj_te /
                                   (1 - impostos_te)) +
                                  (taxa_inflacao * energia_inj * pag_inj_tusd /
                                     (1 - impostos_tusd))) * -p_transicao,
             receita_bonus = ifelse(first(ano) >= ano_inicio_bonus,
                                    taxa_inflacao * energia_inj * tarifa_bonus,
                                    0),
             demanda_contratada = -taxa_inflacao * pot_sistemas *
               tarifa_demanda * 12 / (1 - impostos_cheio),
             capex_obra = 0,
             custo_disponibilidade = ifelse(ano_real <= 2022, #novo marco legal
                                            -pagamento_disponibilidade * 12 *
                                              consumo_disponibilidade *
                                              fator_construcao *
               taxa_inflacao * (tarifa_autoc_tusd + tarifa_autoc_te) /
               (1 - impostos_cheio),
               0)

      )


    fluxo_caixa <- fluxo_caixa %>%
      mutate(oem = -oem_anual * capex_inicial * fator_construcao,
             bateria_oem = -bateria_oem_anual,
             saldo_anual = capex + receita_autoc + receita_inj_completa +
               receita_bonus +
               pag_compensacao + demanda_contratada + oem +
               troca_inversor + capex_obra + custo_disponibilidade,
             saldo_anual_bateria = capex + bateria_capex + receita_autoc +
               receita_bonus + receita_bat_backup +
               receita_bat_arbitragem +
               demanda_contratada + oem + bateria_oem +
               troca_inversor + capex_obra + custo_disponibilidade,
             saldo_acumulado = cumsum(saldo_anual),
             saldo_acumulado_bateria = cumsum(saldo_anual_bateria),
             taxa_desconto = (1 + taxa_desconto_nominal)^(ano_simulacao - 1),
             saldo_anual_desc = saldo_anual / taxa_desconto,
             saldo_anual_desc_bateria = saldo_anual_bateria / taxa_desconto,
             saldo_acumulado_desc = cumsum(saldo_anual_desc),
             saldo_acumulado_desc_bateria = cumsum(saldo_anual_desc_bateria))

    metricas <- fluxo_caixa %>% summarise(
      .temp_payback_inteiro = sum(saldo_acumulado < 0),
      .temp_payback_inteiro_bateria = sum(saldo_acumulado_bateria < 0), #tempo payback com bateria
      .temp_menor_positivo = suppressWarnings(min(if_else(saldo_acumulado > 0,saldo_acumulado, NA_real_), na.rm = TRUE)),
      .temp_menor_positivo_bateria = suppressWarnings(min(if_else(saldo_acumulado_bateria > 0,
                                                                  saldo_acumulado_bateria,
                                                                  NA_real_),
                                                          na.rm = TRUE)),
      .temp_maior_negativo = max(if_else(saldo_acumulado < 0, saldo_acumulado, NA_real_), na.rm = TRUE),
      .temp_maior_negativo_bateria = max(if_else(saldo_acumulado_bateria < 0,
                                                 saldo_acumulado_bateria,
                                                 NA_real_), na.rm = TRUE),
      .temp_payback_frac = -.temp_maior_negativo/(.temp_menor_positivo -   .temp_maior_negativo),
      .temp_payback_frac_bateria = -.temp_maior_negativo_bateria /
        (.temp_menor_positivo_bateria - .temp_maior_negativo_bateria),
      payback = .temp_payback_inteiro + .temp_payback_frac,
      payback_bateria = .temp_payback_inteiro_bateria + .temp_payback_frac_bateria,
      .temp_payback_inteiro = sum(saldo_acumulado_desc < 0),
      temp_payback_inteiro_bateria = sum(saldo_acumulado_desc_bateria < 0), #bateria
      .temp_menor_positivo = suppressWarnings(min(if_else(saldo_acumulado_desc >0,saldo_acumulado_desc, NA_real_),na.rm = TRUE)),
      .temp_menor_positivo_bateria = suppressWarnings(min(if_else(saldo_acumulado_desc_bateria > 0,saldo_acumulado_desc_bateria,NA_real_), na.rm = TRUE)), #bateria
      .temp_maior_negativo = max(if_else(saldo_acumulado_desc <0, saldo_acumulado_desc, NA_real_), na.rm = TRUE),
      .temp_maior_negativo_bateria = max(if_else(saldo_acumulado_desc_bateria < 0,
                                                 saldo_acumulado_desc_bateria,
                                                 NA_real_), na.rm = TRUE), #bateria
      .temp_payback_frac = -.temp_maior_negativo/(.temp_menor_positivo - .temp_maior_negativo),
      .temp_payback_frac_bateria = -.temp_maior_negativo_bateria / (.temp_menor_positivo_bateria - .temp_maior_negativo_bateria), #bateria
      payback_desc = .temp_payback_inteiro + .temp_payback_frac,
      payback_desc_bateria = .temp_payback_inteiro_bateria + .temp_payback_frac_bateria) %>%
      select(-starts_with(".temp_")) %>%
      select(-temp_payback_inteiro_bateria)


    metricas <- metricas %>%
      mutate(tir_nominal = jrvFinance::irr(fluxo_caixa$saldo_anual),
             tir_nominal = ifelse(is.na(tir_nominal) & payback == 25,
                                  -0.2,
                                  tir_nominal),
             tir_real = (1 + tir_nominal) / (1 + inflacao) - 1)


  }

  future::plan(future::multisession)

  resultado_payback <- casos_payback %>%
    mutate(saida = furrr::future_pmap(.l = list(nome_4md, ano, segmento, vida_util,
                                                fator_autoconsumo, geracao_1_kwh, degradacao,
                                                capex_inicial, capex_inversor, oem_anual,
                                                pot_sistemas, bateria_eficiencia,
                                                bateria_recapex,
                                                bateria_capex_inicial, bateria_oem_anual, dec),
                                      .f = fluxo_de_caixa,
                                      .progress = TRUE)) %>%
    unnest(saida)

  resultado_payback

}
