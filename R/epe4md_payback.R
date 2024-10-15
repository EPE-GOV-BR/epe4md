# Carregar pacotes necessários
library(dplyr)
library(readr)
library(readxl)
library(epe4md)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)



payback <- function (casos_payback, premissas_reg, ano_base, altera_sistemas_existentes = TRUE,
          ano_decisao_alteracao = 2023, inflacao = 0.0375, taxa_desconto_nominal = 0.13,
          custo_reforco_rede = 200, ano_troca_inversor = 11, pagamento_disponibilidade = 0.3,
          disponibilidade_kwh_mes = 100, desconto_capex_local = 0,
          anos_desconto = 0, dir_dados_premissas = NA_character_, perc_pot_bateria = 0.35, deficit = 1)
{
  dir_dados_premissas <- if_else(is.na(dir_dados_premissas),
                                 system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                                             package = "epe4md"), dir_dados_premissas)
  fator_construcao <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/tempo_construcao.xlsx"),
                                sheet = "fator")
  tarifas <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/tarifas_4md.xlsx"))
  tarifas_comercial_at <- tarifas %>% filter(subgrupo == "A4") %>%
    mutate(tarifa_demanda_g = 0, tarifa_demanda_c = 0, segmento = "comercial_at")
  tarifas_demanda_a <- tarifas %>% filter(subgrupo == "A4") %>%
    select(nome_4md, ano, tarifa_demanda_c, tarifa_demanda_g) %>%
    unique()
  tarifas_comercial_at_remoto <- tarifas %>% filter(subgrupo ==
                                                      "B3") %>% mutate(segmento = "comercial_at_remoto") %>%
    select(-tarifa_demanda_c, -tarifa_demanda_g) %>% left_join(tarifas_demanda_a,
                                                               by = c("ano", "nome_4md"))
  tarifas_residencial <- tarifas %>% filter(subgrupo == "B1") %>%
    mutate(segmento = "residencial")
  tarifas_residencial_remoto <- tarifas %>% filter(subgrupo ==
                                                     "B1") %>% mutate(segmento = "residencial_remoto")
  tarifas_comercial_bt <- tarifas %>% filter(subgrupo == "B3") %>%
    mutate(segmento = "comercial_bt")
  tarifas <- bind_rows(tarifas_comercial_at, tarifas_comercial_at_remoto,
                       tarifas_residencial, tarifas_residencial_remoto, tarifas_comercial_bt)


  # premissas regulatorias

  anos <- tibble(ano = seq(2013, 2060, 1))
  premissas_regulatorias <- left_join(anos, premissas_reg,
                                      by = "ano") %>% mutate(across(.cols = c(binomia, demanda_g),
                                                                    .fns = ~as.logical(.x))) %>% fill(2:5) %>% mutate(p_transicao = ifelse(is.na(p_transicao),
                                                                                                                                           1, p_transicao), alternativa = ifelse(is.na(alternativa),
                                                                                                                                                                              0, alternativa), across(.cols = c(binomia, demanda_g),
                                                                                                                                                                                                         .fns = ~ifelse(is.na(.x), FALSE, .x)))
  premissas_regulatorias <- premissas_regulatorias %>% mutate(alternativa = ifelse(binomia ==
                                                                                     TRUE & alternativa %in% c(0, 1), 2, alternativa))


  #Fluxo de caixa


  fluxo_de_caixa <- function(nome_4md, ano, segmento, vida_util,
                             fator_autoconsumo, geracao_1_kwh, degradacao,
                             capex_inicial, capex_inversor, oem_anual,
                             pot_sistemas,bateria_eficiencia,
                             bateria_tempo_de_vida_anos,bateria_fc,bateria_degradacao,
                             pot_baterias, bateria_capex_inicial,bateria_oem_anual,perc_pot_bateria,dec,fec) {


    fluxo_caixa <- data.frame(ano_simulacao = 1:vida_util,
                              segmento = segmento,
                              nome_4md = nome_4md,
                              ano = ano)

    fluxo_caixa <- fluxo_caixa %>% left_join(fator_construcao,
                                             by = "segmento") %>%
      mutate(fator_construcao = ifelse(ano_simulacao == 1, fator_construcao, 1))

    if (altera_sistemas_existentes == TRUE && ano >= ano_decisao_alteracao) {
      fluxo_caixa <- fluxo_caixa %>% mutate(ano = row_number() +
                                              ano - 1, ano = ifelse(ano > 2060, 2060, ano))
    }
    fluxo_caixa <- left_join(fluxo_caixa, premissas_regulatorias,
                             by = "ano")
    fluxo_caixa <- left_join(fluxo_caixa, tarifas, by = c("ano",
                                                          "nome_4md", "segmento", "alternativa"))
    fluxo_caixa <- fluxo_caixa %>% mutate(tarifa_autoc_tusd = ifelse(binomia ==
                                                                       TRUE, tarifa_autoc_bin_tusd, tarifa_autoc_tusd),
                                          tarifa_demanda = ifelse(demanda_g == TRUE, tarifa_demanda_g,
                                                                  tarifa_demanda_c), custo_obra = ifelse(segmento ==
                                                                                                           "comercial_at_remoto" & row_number() == 1, -custo_reforco_rede *
                                                                                                           pot_sistemas, 0), consumo_disponibilidade = ifelse(segmento ==
                                                                                                                                                                "comercial_at" | binomia == TRUE, 0, disponibilidade_kwh_mes)) %>%
      select(-tarifa_demanda_c, -tarifa_demanda_g, -tarifa_autoc_bin_tusd)




    fluxo_caixa <- fluxo_caixa %>% mutate(energia_ano = geracao_1_kwh *
                                          fator_construcao * (1 + degradacao)^(1 - ano_simulacao),
                                          energia_autoc = energia_ano * fator_autoconsumo,
                                          armazenamento = (geracao_1_kwh*perc_pot_bateria)*(1 + bateria_degradacao)^(1 - ano_simulacao),#
                                          armazenamento_real = armazenamento*bateria_eficiencia, #
                                          energia_armazenada = armazenamento_real*bateria_fc , #Energia armazenada/ano
                                          energia_inj = energia_ano - energia_autoc - energia_armazenada, ###
                                          # energia_inj = energia_ano - energia_autoc,
                                          capex = 0,
                                          troca_inversor = 0,
                                          bateria_capex = 0,
                                          capex_obra = 0,
                                          taxa_inflacao = (1 + inflacao)^(ano_simulacao - 1)
                                          )


    fluxo_caixa[1, "capex"] <- -capex_inicial
    fluxo_caixa[ano_troca_inversor, "troca_inversor"] <- -capex_inversor
    fluxo_caixa[1, "bateria_capex"] <- -bateria_capex_inicial




    fluxo_caixa <- fluxo_caixa %>% mutate(desconto_capex = desconto_capex_local,
                                          capex = ifelse(segmento %in% c("residencial", "comercial_bt",
                                                                         "comercial_at") & ano %in% anos_desconto, capex *
                                                           (1 - desconto_capex), capex))
    fluxo_caixa <- fluxo_caixa %>% mutate(ano_real = row_number() +
                                            first(ano) - 1, ano_real = ifelse(ano_real > 2060,
                                                                              2060, ano_real))

    custo_deficit = 7.643
    custo_deficit = 5.000
    fluxo_caixa <- fluxo_caixa %>% mutate(receita_autoc = (taxa_inflacao *
                                                             energia_autoc * (tarifa_autoc_tusd + tarifa_autoc_te))/(1 - impostos_cheio),
                                          receita_inj_completa = (taxa_inflacao * energia_inj * tarifa_inj_te/(1 - impostos_te)) + (taxa_inflacao * energia_inj * tarifa_inj_tusd/(1 - impostos_tusd)),
                                          # custo_interrupção <- if_else(deficit == 1,dec * fec * custo_deficit*energia_armazenada,0),
                                          # custo_interrupcao= dec * fec * custo_deficit*energia_armazenada,
                                          custo_interrupcao= dec * fec * custo_deficit,
                                          # custo_interrupcao = fec,
                                          receita_armazenamento = (energia_armazenada + custo_interrupcao) * (tarifa_autoc_tusd + tarifa_autoc_te) / (1 - impostos_cheio), #Receita do armazenamento da bateria
                                          pag_compensacao = ((taxa_inflacao *energia_inj * pag_inj_te/(1 - impostos_te)) + (taxa_inflacao * energia_inj * pag_inj_tusd/(1 - impostos_tusd))) *  -p_transicao,
                                          demanda_contratada = -taxa_inflacao *  pot_sistemas * tarifa_demanda * 12/(1 - impostos_cheio),
                                          capex_obra = 0,
                                          custo_disponibilidade = ifelse(ano_real <= 2022, -pagamento_disponibilidade * 12 * consumo_disponibilidade * fator_construcao * taxa_inflacao * (tarifa_autoc_tusd + tarifa_autoc_te)/(1 - impostos_cheio), 0))




    fluxo_caixa <- fluxo_caixa %>% mutate(oem = -oem_anual * capex_inicial * fator_construcao,
                                          bateria_oem = -bateria_oem_anual,
                                          saldo_anual = capex +
                                            receita_autoc + receita_inj_completa + pag_compensacao +
                                            demanda_contratada + oem + troca_inversor + capex_obra +
                                            custo_disponibilidade,
                                          saldo_anual_bateria = capex + bateria_capex + receita_autoc + receita_armazenamento + receita_inj_completa + #saldo anual com bateria
                                            pag_compensacao + demanda_contratada + oem + bateria_oem +
                                            troca_inversor + capex_obra + custo_disponibilidade,
                                          saldo_acumulado = cumsum(saldo_anual),
                                          saldo_acumulado_bateria = cumsum(saldo_anual_bateria), #saldo acumulado com bateria
                                          taxa_desconto = (1 + taxa_desconto_nominal)^(ano_simulacao - 1),
                                          saldo_anual_desc = saldo_anual/taxa_desconto,
                                          saldo_anual_desc_bateria = saldo_anual_bateria / taxa_desconto, #saldo anual com desconto com bateria
                                          saldo_acumulado_desc = cumsum(saldo_anual_desc),
                                          saldo_acumulado_desc_bateria = cumsum(saldo_anual_desc_bateria)) #saldo acumulado com desconto com bateria)





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

  }

  colunas_relevantes <- c(
    "nome_4md", "ano", "segmento", "vida_util",
    "fator_autoconsumo", "geracao_1_kwh", "degradacao",
    "capex_inicial", "capex_inversor", "oem_anual",
    "pot_sistemas", "bateria_eficiencia",
    "bateria_tempo_de_vida_anos", "bateria_fc", "bateria_degradacao",
    "pot_baterias", "bateria_capex_inicial", "bateria_oem_anual",'dec','fec'
  )


  future::plan(future::multisession)

  resultado_payback_fv_bateria <- casos_payback %>%
    select(all_of(colunas_relevantes)) %>%
    mutate(saida = furrr::future_pmap(.l = list(
      nome_4md, ano, segmento, vida_util, fator_autoconsumo,
      geracao_1_kwh, degradacao, capex_inicial, capex_inversor,
      oem_anual, pot_sistemas, bateria_eficiencia,
      bateria_tempo_de_vida_anos, bateria_fc, bateria_degradacao,
      pot_baterias, bateria_capex_inicial, bateria_oem_anual,perc_pot_bateria,dec,fec
    ), .f = fluxo_de_caixa, .progress = TRUE)) %>%
    unnest(saida)

  resultado_payback_fv_bateria


}

#RODAR SCRIPTS

premissas_regulatorias <- readxl::read_xlsx(system.file("dados_premissas/2022/premissas_reg.xlsx", package = "epe4md"))


payback_fv_e_fv_bateria <- payback(
  casos_payback = casos_payback_fv_bateria,
  premissas_reg = premissas_regulatorias,
  ano_base = 2022,
  altera_sistemas_existentes = TRUE,
  ano_decisao_alteracao = 2023,
  inflacao = 0.0375,
  taxa_desconto_nominal = 0.13,
  custo_reforco_rede = 200,
  ano_troca_inversor = 11,
  desconto_capex_local = 0,
  anos_desconto = 0)

