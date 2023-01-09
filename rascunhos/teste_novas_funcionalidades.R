library(tidyverse)
library(readxl)
library(epe4md)
library(extrafont)
library(tsibble)
library(feasts)
library(lubridate)
library(scales)

options(scipen = 999)

entrada <- readxl::read_xlsx("dados_premissas/pde2031/premissas_reg.xlsx") %>%
  mutate(across(
    .cols = c(binomia, demanda_g),
    .fns = ~as.logical(.x)
  ))

resultado <- epe4md_calcula(premissas = entrada)

resumo <- resultado %>%
  epe4md_sumariza_resultados()

proj_potencia <- resultado$proj_potencia
part_adotantes <- resultado$part_adotantes

epe4md_graf_pot_acum(resultado, ano_inicio = 2017)

epe4md_graf_pot_anual(resultado)

epe4md_graf_pot_segmento(resultado)

epe4md_graf_pot_regiao(resultado)

epe4md_graf_part_adotantes(resultado)

geracao <- epe4md_geracao_mensal(lista_potencia = resultado)

epe4md_graf_geracao_mes(geracao)

epe4md_graf_geracao_ano(geracao)

epe4md_graf_part_fonte_geracao(geracao)

epe4md_graf_part_segmento(resultado)





# teste decomposicao potencia em meses ------------------------------------

pde_edicao <- 2031

dados_gd <- feather::read_feather(stringr::str_glue("dados_premissas/pde{pde_edicao}/base_mmgd.feather" %>%  here::here()))

historico_fatores <- dados_gd %>%
  mutate(mes_ano = tsibble::yearmonth(data_conexao)) %>%
  group_by(mes_ano, ano) %>%
  summarise(pot_mes_mw = sum(potencia_mw)) %>%
  ungroup() %>%
  filter(between(ano, 2014, 2019))

fatores_mensais <- historico_fatores %>%
  tsibble::as_tsibble(index = mes_ano) %>%
  model(feasts::classical_decomposition(pot_mes_mw ~ season(12), type = "mult")) %>%
  fabletools::components()

fatores_mensais <- fatores_mensais %>%
  as_tibble() %>%
  mutate(mes = lubridate::month(mes_ano)) %>%
  group_by(mes) %>%
  summarise(fator_mensal = mean(seasonal)) %>%
  ungroup()

# transformação da potencia anual em mensal com base nos fatores

teste_decomp <- historico_fatores %>%
  group_by(ano) %>%
  mutate(pot_ano = sum(pot_mes_mw),
         pot_mes_igual = pot_ano / 12,
         mes = month(mes_ano)) %>%
  ungroup()

teste_decomp <- left_join(teste_decomp, fatores_mensais, by = "mes")

teste_decomp <- teste_decomp %>%
  mutate(pot_igual_saz = pot_mes_igual * fator_mensal)

teste_decomp <- teste_decomp %>%
  select(- pot_ano) %>%
  pivot_longer(cols = starts_with("pot"), values_to = "potencia", names_to = "serie")

teste_decomp <- teste_decomp %>%
  filter(serie != "pot_mes_igual")

ggplot(teste_decomp) +
  geom_col(aes(x = mes, y = potencia, fill = serie), position = "dodge") +
  facet_wrap(~ano, scales = "free")

# graficos ----------------------------------------------------------------





resumo <- geracao %>%
  filter(ano >= 2013) %>%
  group_by(mes, ano) %>%
  summarise(energia_mwmed = sum(energia_mwmed)) %>%
  ungroup()

ggplot(resumo) +
  aes(x = as.Date(mes), y = energia_mwmed) +
  geom_area(fill = "red") +
  labs(x = "", y = "Geração [MWméd]") +
  theme_minimal() +
  theme(legend.position = "none", text=element_text(size = 14, family="Calibri Light")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","))



# compensacao -------------------------------------------------------------


geracao_detalhada <- geracao$proj_energia_detalhada



# geração -----------------------------------------------------------------

cenario <- read_xlsx("X:/Geracao Distribuida/PDE/PDE2031/cenarios/cenario_verao_encargos.xlsx")

resultado <- epe4md_calcula(premissas_reg = cenario,
                              pde_edicao = 2031,
                              ano_max_resultado = 2031)


pde_edicao = 2031

ano_max_resultado = 2031

lista_potencia <- resultado

dir_dados_premissas = "inst/dados_premissas"

dir_dados_premissas <- if_else(
  dir_dados_premissas == "inst/dados_premissas",
  system.file(stringr::str_glue("dados_premissas/pde{pde_edicao}"), package = "epe4md" ),
  dir_dados_premissas
)

#extrair proj potencia da lista
proj_potencia <- lista_potencia$proj_potencia

#Bases

fc_fv_mensal <- read_rds(stringr::str_glue("{dir_dados_premissas}/fc_distribuidoras_mensal.rds"))

fc_outras_mensal <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/fc_outras_fontes.xlsx")) %>%
  pivot_longer(cols = 3:14, names_to = "mes", values_to = "fc")

fc_outras_mensal$mes <- as.numeric(fc_outras_mensal$mes)

dados_gd <- feather::read_feather(stringr::str_glue("{dir_dados_premissas}/base_mmgd.feather"))

injecao <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/injecao.xlsx"))


#considera-se que os sitemas são instalados no dia 15 de cada mês
dia_instalacao <- 15

meses <- tibble(mes = seq(1,12))

anos_operacao <- tibble(ano_operacao = seq(2013, ano_max_resultado))

meses_operacao <- crossing(anos_operacao, meses) %>%
  mutate(mes_operacao = make_date(ano_operacao, mes),
         mes_operacao = ceiling_date(mes_operacao, "month") - 1) %>%
  select(mes_operacao)


projecao_energia <- proj_potencia %>%
  select(ano, nome_4md, uf, subsistema, segmento, fonte_resumo, pot_ano_mw) %>%
  crossing(meses) %>%
  mutate(mes_ano = make_date(ano, mes, 1),
         mes_ano = tsibble::yearmonth(mes_ano))

#historico com dados reais

historico <- dados_gd %>%
  mutate(mes_ano = tsibble::yearmonth(data_conexao)) %>%
  group_by(mes_ano, fonte_resumo, segmento, nome_4md) %>%
  summarise(pot_mes_mw = sum(potencia_mw)) %>%
  ungroup()

historico_mensal <- projecao_energia %>%
  filter(mes_ano < tsibble::yearmonth(make_date(pde_edicao - 10, 1, 1))) %>%
  left_join(historico, by = c("mes_ano", "fonte_resumo", "segmento", "nome_4md")) %>%
  mutate(pot_mes_mw = ifelse(is.na(pot_mes_mw), 0, pot_mes_mw))



# fatores historicos mensal retirando a sazonalidade

historico_fatores <- dados_gd %>%
  mutate(mes_ano = tsibble::yearmonth(data_conexao)) %>%
  group_by(mes_ano, ano) %>%
  summarise(pot_mes_mw = sum(potencia_mw)) %>%
  ungroup() %>%
  filter(between(ano, 2014, 2019))

fatores_mensais <- historico_fatores %>%
  tsibble::as_tsibble(index = mes_ano) %>%
  fabletools::model(feasts::classical_decomposition(pot_mes_mw ~ season(12), type = "mult")) %>%
  fabletools::components()


fatores_mensais <- fatores_mensais %>%
  as_tibble() %>%
  mutate(mes = lubridate::month(mes_ano)) %>%
  group_by(mes) %>%
  summarise(fator_mensal = mean(seasonal)) %>%
  ungroup()

# transformação da potencia anual em mensal com base nos fatores

projecao_energia <- projecao_energia %>%
  filter(mes_ano >= tsibble::yearmonth(make_date(pde_edicao - 10, 1, 1))) %>%
  left_join(fatores_mensais, by = "mes") %>%
  mutate(pot_mes_mw = fator_mensal * pot_ano_mw / 12) %>%
  select(-fator_mensal)


#junção historico e projecao
projecao_energia <- bind_rows(historico_mensal, projecao_energia)

projecao_energia <- projecao_energia %>%
  mutate(mes_instalacao = make_date(year = ano, month = mes, day = dia_instalacao)) %>%
  filter(pot_mes_mw != 0)

#crossing das instalacoes com os meses de operacao
projecao_energia <- crossing(projecao_energia, meses_operacao)

projecao_energia <- projecao_energia %>%
  mutate(dias_operando = mes_operacao - mes_instalacao) %>%
  filter(dias_operando > 0)

projecao_energia <- projecao_energia %>%
  mutate(mes = month(mes_operacao)) %>%
  left_join(fc_outras_mensal, by = c("subsistema", "mes", "fonte_resumo")) %>%
  left_join(fc_fv_mensal, by = c("nome_4md", "mes")) %>%
  mutate(fc = case_when(
    fonte_resumo == "Fotovoltaica" & segmento == "comercial_at_remoto" ~ fc_remoto,
    fonte_resumo == "Fotovoltaica" & segmento != "comercial_at_remoto" ~ fc_local,
    TRUE ~ fc)
  ) %>%
  select(-fc_local, -fc_remoto, -mes)

degradacao_diaria = (1+0.005)^(1/365)-1


#após 25 anos, assume-se que o sistema FV é renovado, zerando a degradação
vida_util_fv_dias <- 25 * 365

projecao_energia <- projecao_energia %>%
  mutate(dias_operando = ifelse(dias_operando > vida_util_fv_dias,
                                dias_operando - vida_util_fv_dias,
                                dias_operando),
         dias_operando_mes = days_in_month(mes_operacao),
         dias_operando_mes = ifelse(mes_operacao - mes_instalacao < 28,
                                    dias_operando_mes - dia_instalacao,
                                    dias_operando_mes))

projecao_energia <- projecao_energia %>%
  mutate(energia_mwh = ifelse(fonte_resumo == "Fotovoltaica",
                              pot_mes_mw * fc * dias_operando_mes * 24 * (1 - degradacao_diaria)^dias_operando,
                              pot_mes_mw * fc * dias_operando_mes * 24))

projecao_energia <- projecao_energia %>%
  left_join(injecao, by = c("segmento", "fonte_resumo")) %>%
  mutate(energia_autoc_mwh = energia_mwh * fator_autoconsumo,
         energia_inj_mwh = energia_mwh * (1 - fator_autoconsumo))

resumo_projecao_energia <- projecao_energia %>%
  group_by(mes_operacao, nome_4md, uf, subsistema, segmento, fonte_resumo) %>%
  summarise(energia_mwh = sum(energia_mwh),
            energia_autoc_mwh = sum(energia_autoc_mwh),
            energia_inj_mwh = sum(energia_inj_mwh)) %>%
  ungroup()

resumo_projecao_energia <- resumo_projecao_energia %>%
  mutate(mes = tsibble::yearmonth(mes_operacao),
         ano = year(mes),
         dias_mes = days_in_month(mes),
         energia_mwmed = energia_mwh / (24 * dias_mes)) %>%
  select(mes, ano, everything()) %>%
  select(-mes_operacao)

#lista resultados

lista_energia <- list(resumo_projecao_energia, projecao_energia)

names(lista_energia) <- c("proj_energia", "proj_energia_detalhada")

lista_energia



# investimentos -----------------------------------------------------------


potencia <- resultado$proj_potencia
pde_edicao <- 2031

#Investimentos -

# custo_unitario_fv <- read_xlsx(stringr::str_glue("dados_premissas/pde{pde_edicao}/capex_historico_fv.xlsx" %>%  here::here()))

custo_outras <- read_xlsx(stringr::str_glue("dados_premissas/pde{pde_edicao}/capex_historico_outras.xlsx" %>%  here::here())) %>%
  rename(custo_outras = custo_unitario)

custo_fv <- read_xlsx(stringr::str_glue("dados_premissas/pde{pde_edicao}/custos.xlsx" %>%  here::here())) %>%
  mutate(fonte_resumo = "Fotovoltaica")

potencia_custos <- left_join(potencia, custo_fv, by = c("ano", "fonte_resumo", "segmento"))

potencia_custos <- left_join(potencia_custos, custo_outras, by = "fonte_resumo")

potencia_custos <- potencia_custos %>%
  mutate(custo_unitario = ifelse(is.na(custo_unitario), custo_outras, custo_unitario)) %>%
  select(- custo_outras)

potencia_custos <- potencia_custos %>%
  mutate(investimento_ano_milhoes = pot_ano_mw * custo_unitario)



# outras fontes -----------------------------------------------------------


pde_edicao <- 2031
ano_max_resultado = 2031


dados_gd <- feather::read_feather(stringr::str_glue("dados_premissas/pde{pde_edicao}/base_mmgd.feather" %>%  here::here()))


potencia_tipica <- dados_gd %>%
  filter(fonte_resumo == "Fotovoltaica") %>%
  group_by(segmento) %>%
  summarise(pot_sistemas = median(potencia_instalada_k_w))


# Premissas

inflacao <- 0.0375
ano_troca_inversor <- 11
fator_custo_inversor <- 0.15 #percentual do CAPEX total do sistema FV

fc_fontes <- readr::read_rds(stringr::str_glue("dados_premissas/pde{pde_edicao}/fc_distribuidoras_com_eolica.rds" %>%  here::here())) %>%
  mutate(fc_hidro = 0.49,
         fc_carv = 0.9,
         fc_orc = 0.875)

injecao <- read_xlsx(stringr::str_glue("dados_premissas/pde{pde_edicao}/injecao.xlsx" %>%  here::here())) #%>%
  #filter(fonte_resumo == "Fotovoltaica")

injecao <- injecao %>%
  mutate(oem_anual = ifelse(segmento == "comercial_at_remoto", 0.02, 0.01))

fontes <- tribble(
  ~fonte_resumo,         ~vida_util, ~degradacao,
  "Fotovoltaica",             26,         0.005,
   "Eólica",                  21,           0,
   "Termelétrica",            16,           0,
   "Hidro",                   31,           0,
   "Gases de Carvoaria",      21,           0,
   "Calor de Cimenteira",     31,           0,
)



custos <- read_xlsx(stringr::str_glue("dados_premissas/pde{pde_edicao}/custos.xlsx" %>%  here::here()), sheet = "custos") %>%
  mutate(custo_inversor = fator_custo_inversor * custo_unitario)

casos <- merge(injecao, fc_fontes) %>%
  mutate(fc_fv = ifelse(segmento == "comercial_at_remoto", fc_remoto, fc_local)) %>%
  select(-fc_local,-fc_remoto) %>%
  rename(
    "Fotovoltaica" = fc_fv,
    "Eólica" = fc_eol,
    "Termelétrica" = fc_term,
    "Hidro" = fc_hidro,
    "Gases de Carvoaria" = fc_carv,
    "Calor de Cimenteira" = fc_orc
    )

casos <- pivot_longer(casos, cols = c("Fotovoltaica", "Eólica", "Termelétrica", "Hidro", "Gases de Carvoaria", "Calor de Cimenteira"),
                      names_to = "tecnologia", values_to = "fc")

casos <- casos %>%
  filter(fonte_resumo == tecnologia)

casos <- left_join(casos, fontes, by = "fonte_resumo")

casos <- left_join(casos, potencia_tipica, by = "segmento")

casos <- casos %>%
  mutate(geracao_1_kwh = pot_sistemas * fc * 8760)

casos <- casos %>%
  #filter(tecnologia == "fv") %>%
  select(-tecnologia)

casos <- casos %>%
  merge(custos)

casos <- casos %>%
  mutate(custo_unitario = case_when(
    fonte_resumo == "Eólica" ~ 6.9,
    fonte_resumo == "Termelétrica" ~ 8,
    fonte_resumo == "Hidro" ~ 7.5,
    fonte_resumo == "Gases de Carvoaria" ~ 2.5 * 5, #dolar
    fonte_resumo == "Calor de Cimenteira" ~ 4.3 * 5,
    TRUE ~ custo_unitario
  ))

casos <- casos %>%
  mutate(oem_kw = case_when(
    fonte_resumo == "Eólica" ~ 110,
    fonte_resumo == "Termelétrica" ~ 700,
    fonte_resumo == "Hidro" ~ 90,
    fonte_resumo == "Calor de Cimenteira" ~ 9.5 * 5 * 0.875 * 8.76, #transforma variavel em fixo
    TRUE ~ 0
  ))


casos <- casos %>%
  mutate(oem_anual = ifelse(fonte_resumo == "Gases de Carvoaria", 0.05, oem_anual),
         oem_anual = ifelse(fonte_resumo %in% c("Fotovoltaica", "Gases de Carvoaria"), oem_anual,
                            oem_kw / (custo_unitario * 1000)))

casos <- casos %>%
  mutate(capex_inicial = custo_unitario * pot_sistemas * 1000,
         capex_inversor = custo_inversor * pot_sistemas * 1000 * ((1 + inflacao)^(ano_troca_inversor - 1))) %>%
  arrange(nome_4md, segmento, ano)

casos <- casos %>%
  mutate(capex_inversor = ifelse(fonte_resumo == "Fotovoltaica", capex_inversor, 0))

casos <- casos %>%
  filter(ano <= ano_max_resultado)

casos_payback <- casos


entrada <- readxl::read_xlsx("X:\\Geracao Distribuida\\PDE\\PDE2031\\cenarios/cenario_verao_encargos.xlsx")


# calculo de payback

premissas_reg <- entrada
pde_edicao = 2031
altera_sistemas_existentes = FALSE
ano_decisao_alteracao = 2021
inflacao = 0.0375
taxa_desconto_nominal = 0.13
custo_reforco_rede = 200
ano_troca_inversor = 11
pagamento_disponibilidade = 0.3
disponibilidade_kwh_mes = 100


fator_construcao <- read_xlsx(stringr::str_glue("dados_premissas/pde{pde_edicao}/tempo_construcao.xlsx" %>%  here::here()), sheet = "fator")

tarifas <- feather::read_feather(stringr::str_glue("dados_premissas/pde{pde_edicao}/tarifas_4md.feather" %>%  here::here()))

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

tarifas <- bind_rows(tarifas_comercial_at,tarifas_comercial_at_remoto, tarifas_residencial,
                     tarifas_residencial_remoto, tarifas_comercial_bt)

# premissas regulatorias

anos <- tibble(ano = seq(2013, 2050, 1))

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
  mutate(alternativa = ifelse(binomia == TRUE & alternativa %in% c(0,1),2,alternativa))



fluxo_de_caixa <- function(nome_4md, ano, segmento, vida_util, fator_autoconsumo, geracao_1_kwh, degradacao,
                           capex_inicial, capex_inversor, oem_anual, pot_sistemas, fonte_resumo, uf) {

  fluxo_caixa <- data.frame("ano_simulacao" = 1:vida_util,
                            "segmento" = segmento,
                            "nome_4md" = nome_4md,
                            "ano" = ano,
                            "fonte_resumo" = fonte_resumo,
                            "uf" = uf)

  fluxo_caixa <- fluxo_caixa %>%
    left_join(fator_construcao, by = "segmento") %>%
    mutate(fator_construcao = ifelse(ano_simulacao == 1, fator_construcao, 1))


  if (altera_sistemas_existentes == TRUE & ano >= ano_decisao_alteracao) {
    fluxo_caixa <- fluxo_caixa %>%
      mutate(ano = row_number() + ano - 1,
             ano = ifelse(ano > 2050, 2050, ano))
  }

  fluxo_caixa <- left_join(fluxo_caixa, premissas_regulatorias, by = "ano")

  fluxo_caixa <- left_join(fluxo_caixa, tarifas, by = c("ano", "nome_4md", "segmento", "alternativa"))

  fluxo_caixa <- fluxo_caixa %>%
    mutate(tarifa_autoc_tusd = ifelse(binomia == TRUE, tarifa_autoc_bin_tusd, tarifa_autoc_tusd),
           tarifa_demanda = ifelse(demanda_g == TRUE, tarifa_demanda_g, tarifa_demanda_c),
           custo_obra = ifelse(segmento == "comercial_at_remoto" & row_number() == 1,
                               - custo_reforco_rede * pot_sistemas, 0),
           consumo_disponibilidade = ifelse(segmento == "comercial_at" | binomia == TRUE, 0, disponibilidade_kwh_mes)) %>%
    select(-tarifa_demanda_c, -tarifa_demanda_g, -tarifa_autoc_bin_tusd)


  fluxo_caixa <- fluxo_caixa %>%
    mutate(energia_ano = geracao_1_kwh * fator_construcao * (1 + degradacao)^(1 - ano_simulacao),
           energia_autoc = energia_ano * fator_autoconsumo,
           energia_inj = energia_ano - energia_autoc,
           capex = 0,
           troca_inversor = 0,
           capex_obra = 0,
           taxa_inflacao = (1 + inflacao)^(ano_simulacao - 1)
    )

  fluxo_caixa[1,"capex"] <- -capex_inicial
  fluxo_caixa[ano_troca_inversor,"troca_inversor"] <- -capex_inversor



  fluxo_caixa <- fluxo_caixa %>%
    mutate(receita_autoc = (taxa_inflacao * energia_autoc * (tarifa_autoc_tusd + tarifa_autoc_te)) /
             (1 - impostos_cheio),
           receita_inj_completa = (taxa_inflacao * energia_inj * tarifa_inj_te /
                                     (1 - ifelse(fonte_resumo == "Fotovoltaica" | uf == "MG" | segmento != "comercial_at_remoto", impostos_cheio, impostos_tusd))) +
             (taxa_inflacao * energia_inj * tarifa_inj_tusd / (1 - impostos_tusd)),
           pag_compensacao = ((taxa_inflacao * energia_inj * pag_inj_te /
                                 (1 - ifelse(fonte_resumo == "Fotovoltaica" | uf == "MG" | segmento != "comercial_at_remoto", impostos_cheio, impostos_tusd))) +
                                (taxa_inflacao * energia_inj * pag_inj_tusd / (1 - impostos_tusd))) * -p_transicao,
           demanda_contratada = -taxa_inflacao * pot_sistemas * tarifa_demanda * 12 / (1 - impostos_cheio),
           capex_obra = 0,
           custo_disponibilidade = -pagamento_disponibilidade * 12 * consumo_disponibilidade * fator_construcao *
             taxa_inflacao * (tarifa_autoc_tusd + tarifa_autoc_te) /
             (1 - impostos_cheio)
    )


  fluxo_caixa <- fluxo_caixa %>%
    mutate(oem = -oem_anual*capex_inicial*fator_construcao,
           saldo_anual = capex+receita_autoc+receita_inj_completa+pag_compensacao+demanda_contratada+oem+
             troca_inversor+capex_obra+custo_disponibilidade,
           saldo_acumulado = cumsum(saldo_anual),
           taxa_desconto = (1+taxa_desconto_nominal)^(ano_simulacao-1),
           saldo_anual_desc = saldo_anual/taxa_desconto,
           saldo_acumulado_desc = cumsum(saldo_anual_desc))

  metricas <- fluxo_caixa %>%
    summarise(
      .temp_payback_inteiro = sum(saldo_acumulado <0),
      .temp_menor_positivo = min( if_else(saldo_acumulado>0, saldo_acumulado, NA_real_ ), na.rm = TRUE   ),
      .temp_maior_negativo = max( if_else(saldo_acumulado<0, saldo_acumulado, NA_real_ ), na.rm = TRUE   ),
      .temp_payback_frac = -.temp_maior_negativo/(.temp_menor_positivo - .temp_maior_negativo ),
      payback = .temp_payback_inteiro + .temp_payback_frac,
      .temp_payback_inteiro = sum(saldo_acumulado_desc <0),
      .temp_menor_positivo = min( if_else(saldo_acumulado_desc>0, saldo_acumulado_desc, NA_real_ ), na.rm = TRUE   ),
      .temp_maior_negativo = max( if_else(saldo_acumulado_desc<0, saldo_acumulado_desc, NA_real_ ), na.rm = TRUE   ),
      .temp_payback_frac = -.temp_maior_negativo/(.temp_menor_positivo - .temp_maior_negativo ),
      payback_desc = .temp_payback_inteiro + .temp_payback_frac
    ) %>%
    select(
      -starts_with(".temp_")
    )


  metricas <- metricas %>%
    mutate(tir_nominal = jrvFinance::irr(fluxo_caixa$saldo_anual),
           tir_nominal = ifelse(is.na(tir_nominal) & payback == 25,-0.2,tir_nominal),
           tir_real = (1+tir_nominal)/(1+inflacao)-1)

}

future::plan(future::multisession)

resultado_payback <- casos_payback %>%
  mutate(saida = furrr::future_pmap(.l = list(nome_4md, ano, segmento, vida_util, fator_autoconsumo, geracao_1_kwh, degradacao,
                                              capex_inicial, capex_inversor, oem_anual, pot_sistemas, fonte_resumo, uf), .f = fluxo_de_caixa, .progress = TRUE)) %>%
  unnest(saida)







#
#
#
# resumo <- resultado_payback %>%
#   filter((segmento == "comercial_at_remoto" & fonte_resumo == "Termelétrica" & ano > 2021) |
#            (segmento != "comercial_at_remoto" & fonte_resumo == "Fotovoltaica") |
#            (segmento == "comercial_at_remoto" & fonte_resumo == "Fotovoltaica" & ano <= 2021))
#
#
# mercado_potencial <- epe4md_mercado_potencial()
#
# calibra <- epe4md_calibra_curva_s(resultado_payback = resumo, consumidores = mercado_potencial)
#
# adotantes <- epe4md_proj_adotantes(calibra)
#
# potencia <- epe4md_proj_potencia(adotantes)
#
# resumo_potncia <- epe4md_sumariza_resultados(potencia)
#
# epe4md_graf_pot_segmento(potencia)



# grafico <- resultado_payback %>%
#   filter(segmento == "comercial_at_remoto") %>%
#   group_by(ano, fonte_resumo) %>%
#   summarise(pb = median(payback_desc)) %>%
#   ungroup()


grafico <- resultado_payback %>%
  filter(segmento == "comercial_at_remoto",
         ano == 2021) %>%
  mutate(tir_real = ifelse(fonte_resumo == "Gases de Carvoaria" & nome_4md != "CEMIG", NA, tir_real),
         tir_real = ifelse(fonte_resumo == "Calor de Cimenteira" & uf %in% c("AC", "AP", "RR"), NA, tir_real),
         fonte_resumo = ifelse(fonte_resumo == "Termelétrica", "Biogás", fonte_resumo))

paleta <- c("#13475d", "#953735", "#f19759", "#94b2d0", "#7f7f7f", "#b3a2c7")

ggplot(grafico) +
  geom_point(aes(x = nome_4md, y = tir_real, color = fonte_resumo), size = 2.5) +
  labs(y = "TIR Real", x = "",
       title = "TIR de projetos de minigeração remotos A4 com compensação no setor comercial BT",
       caption = "Nota: projetos FV de 1 MW e de outras fontes de 5 MW.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "bottom",
        text = element_text(family = "Calibri Light"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0)) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_color_manual(values = rev(paleta))

ggsave("X:/Geracao Distribuida/PDE/PDE2031/resultados/box/tir_remoto_5mw_cimenteira.png", height = 5, width = 9)



# Modelo projeção Recomeçar curva S ---------------------------------------


premissas <- readxl::read_xlsx("dados_premissas/pde2031/cenario_inferior.xlsx") %>%
  mutate(across(
    .cols = c(binomia, demanda_g),
    .fns = ~as.logical(.x)
  ))


pde_edicao = 2031
ano_max_resultado = 2050
altera_sistemas_existentes = FALSE
ano_decisao_alteracao = 2021
inflacao = 0.0375
taxa_desconto_nominal = 0.13
custo_reforco_rede = 200 #R$/kW para sistemas comercial_at_remoto
ano_troca_inversor = 11
pagamento_disponibilidade = 0.3 #devido à variabilidade da FV, em alguns meses o cliente pode pagar disponibilidade
disponibilidade_kwh_mes = 100 #equivalente a um consumidor trifásico


casos_payback <- epe4md_casos_payback(ano_max_resultado = 2050)

resultado_payback <- epe4md_payback(
  casos_payback = casos_payback,
  premissas_reg = premissas,
  altera_sistemas_existentes = altera_sistemas_existentes,
  ano_decisao_alteracao = ano_decisao_alteracao,
  inflacao = inflacao,
  taxa_desconto_nominal = taxa_desconto_nominal,
  custo_reforco_rede = custo_reforco_rede,
  ano_troca_inversor = ano_troca_inversor,
  pagamento_disponibilidade = pagamento_disponibilidade,
  disponibilidade_kwh_mes = disponibilidade_kwh_mes

)

consumidores <- epe4md_mercado_potencial()

casos_otimizados <- epe4md_calibra_curva_s(
  resultado_payback = resultado_payback,
  consumidores = consumidores,
  pde_edicao = pde_edicao,
  ano_max_resultado = ano_max_resultado)




dados_gd <- feather::read_feather(stringr::str_glue("dados_premissas/pde{pde_edicao}/base_mmgd.feather" %>%  here::here()))

projecao <- casos_otimizados %>%
  mutate(mercado_potencial = round(exp(-spb*payback)*consumidores,0),
         mercado_potencial = ifelse(mercado_potencial == 0,1,mercado_potencial)) %>%
  group_by(nome_4md,segmento) %>%
  arrange(ano) %>%
  mutate(adotantes_acum = mercado_potencial*Ft,
         adotantes_ano = ifelse(ano == 2013,
                                adotantes_acum,
                                adotantes_acum - lag(adotantes_acum)),
         #adotantes_ano = ifelse(adotantes_ano < 0, 0, round(adotantes_ano,0)),
         adotantes_acum = cumsum(adotantes_ano)) %>%
  ungroup()

teste <- projecao %>%
  filter(nome_4md == "EMT",
         segmento == "residencial") %>%
  group_by(ano) %>%
  summarise(adot = round(sum(adotantes_acum)))

#suavização de mudanças abruptas

# projecao <- projecao %>%
#   group_by(nome_4md,segmento) %>%
#   arrange(ano) %>%
#   mutate(adotantes_ano_media = zoo::rollmean(adotantes_ano, 2, fill = NA, align = "left"),
#          adotantes_ano_media = round(ifelse(is.na(adotantes_ano_media),adotantes_ano,adotantes_ano_media),0))

# projecao <- projecao %>%
#   mutate(adotantes_ano_c = case_when(
#     adotantes_ano == 0 & ano > 2019 ~ adotantes_ano_media,
#     lag(adotantes_ano) == 0 & ano > 2019 ~ lag(adotantes_ano_media),
#     TRUE ~ adotantes_ano),
#     adotantes_acum_c = cumsum(adotantes_ano_c)) %>%
#   ungroup() %>%
#   select(-adotantes_ano,-adotantes_acum,-adotantes_ano_media) %>%
#   rename(
#     adotantes_ano = adotantes_ano_c,
#     adotantes_acum = adotantes_acum_c
#   )


projecao <- projecao %>%
  arrange(
    nome_4md,
    segmento,
    ano
  ) %>%
  group_by(
    nome_4md,
    segmento
  ) %>%
  mutate(
    comeco_contagem = is.na(lag(adotantes_ano)) | (lag(adotantes_ano) >= 0 & adotantes_ano < 0),
    id_contagem = cumsum(comeco_contagem)
  ) %>%
  ungroup() %>%
  group_by(
    nome_4md,
    segmento,
    id_contagem
  ) %>%
  mutate(
    ano_contagem = row_number()+2012
  ) %>%
  ungroup()


curva_s <- projecao %>%
  select(ano, segmento, nome_4md, Ft) %>%
  rename(Ft_c = Ft,
         ano_contagem = ano)

projecao <- left_join(projecao, curva_s, by = c("ano_contagem","segmento","nome_4md"))

projecao <- projecao %>%
  group_by(nome_4md, segmento, id_contagem) %>%
  arrange(ano) %>%
  mutate(adot_max = last(adotantes_acum)) %>%
  ungroup() %>%
  group_by(nome_4md, segmento) %>%
  mutate(adot_max = first(adot_max),
         mp_desc = mercado_potencial - adot_max,
         adotantes_acum_recomeca = Ft_c * mp_desc,
         adotantes_ano_recomeca = adotantes_acum_recomeca - lag(adotantes_acum_recomeca),
         adotantes_ano_recomeca = ifelse(is.na(adotantes_ano_recomeca) | adotantes_ano_recomeca < 0,
                                adotantes_acum_recomeca, adotantes_ano_recomeca),
         adotantes_ano_corr = ifelse(id_contagem == 1, adotantes_ano, adotantes_ano_recomeca),
         adotantes_acum_corr = cumsum(adotantes_ano_corr),
         adotantes_ano = ifelse(adotantes_ano < 0, 0, round(adotantes_ano,0)),
         adotantes_acum = cumsum(adotantes_ano)) %>%
  ungroup()



# projecao <- projecao %>%
#   group_by(nome_4md, segmento) %>%
#   mutate(adotantes_acum_recomeca = Ft_c * mercado_potencial,
#          adotantes_ano_recomeca = adotantes_acum_recomeca - lag(adotantes_acum_recomeca),
#          adotantes_ano_recomeca = ifelse(is.na(adotantes_ano_recomeca) | adotantes_ano_recomeca < 0,
#                                          adotantes_acum_recomeca, adotantes_ano_recomeca),
#          adotantes_acum_recomeca_c = cumsum(adotantes_ano_recomeca),
#          adotantes_ano = ifelse(adotantes_ano < 0, 0, round(adotantes_ano,0)),
#          adotantes_acum = cumsum(adotantes_ano)) %>%
#   ungroup()


projecao_br <- projecao %>%
  group_by(ano) %>%
  summarise(adotantes_acum = sum(adotantes_acum),
            adotantes_acum_recomeca = sum(adotantes_acum_corr)) %>%
  ungroup()

ggplot(projecao_br) +
  geom_line(aes(x = ano, y = adotantes_acum, color = "Método Convencional")) +
  geom_line(aes(x = ano, y = adotantes_acum_recomeca, color = "Método Recomeça")) +
  labs(title = "Adotantes em dois métodos de projeção")

ggsave("X:\\Geracao Distribuida\\PDE\\PDE2031\\resultados\\rascunhos\\recomeca_adot_acumulada_2050-2.png", width = 8, height = 5)


projecao <- projecao %>%
  mutate(adotantes_acum = adotantes_acum_corr,
         adotantes_ano = adotantes_ano_corr)

part_adot_fontes <- dados_gd %>%
  group_by(nome_4md,segmento,fonte_resumo) %>%
  summarise(adotantes_hist = sum(qtde_u_csrecebem_os_creditos)) %>%
  ungroup() %>%
  group_by(nome_4md,segmento) %>%
  mutate(adotantes_hist_total = sum(adotantes_hist),
         part_fonte = adotantes_hist / adotantes_hist_total) %>%
  ungroup()

# completar faltantes para serem encontrados no join
part_adot_fontes <- part_adot_fontes %>%
  complete(nome_4md,segmento,fonte_resumo) %>%
  group_by(nome_4md,segmento) %>%
  mutate(faltantes = sum(is.na(part_fonte))) %>%
  ungroup() %>%
  mutate(part_fonte = ifelse(faltantes == 4 & fonte_resumo == "Fotovoltaica", 1, part_fonte),
         part_fonte = ifelse(is.na(part_fonte),0,part_fonte)) %>%
  select(nome_4md,segmento,fonte_resumo,part_fonte)

# historico de adotantes para substituir anos iniciais da projeção

historico_adot_fontes <- dados_gd %>%
  group_by(ano, nome_4md, segmento, fonte_resumo) %>%
  summarise(adotantes_hist = sum(qtde_u_csrecebem_os_creditos)) %>%
  ungroup() %>%
  complete(ano,nome_4md,segmento,fonte_resumo) %>%
  mutate(adotantes_hist = ifelse(is.na(adotantes_hist),0,adotantes_hist))

projecao <- left_join(projecao, part_adot_fontes, by = c("nome_4md", "segmento")) %>%
  mutate(adotantes_ano = round(adotantes_ano * part_fonte, 0))

projecao <- left_join(projecao, historico_adot_fontes, by = c("nome_4md", "segmento", "ano", "fonte_resumo"))

projecao <- projecao %>%
  mutate(adotantes_ano = ifelse(ano < pde_edicao - 10, adotantes_hist, adotantes_ano)) %>%
  group_by(nome_4md, segmento, fonte_resumo) %>%
  arrange(ano) %>%
  mutate(adotantes_acum = cumsum(adotantes_ano)) %>%
  ungroup()

#calculo percentual de adocao frente ao numero de consumidores totais

lista_consumidores <- epe4md_mercado_potencial(pde_edicao = pde_edicao)

consumidores_totais <- lista_consumidores$consumidores_totais

adotantes_segmento <- projecao %>%
  mutate(segmento = ifelse(segmento == "residencial_remoto","residencial",segmento),
         segmento = ifelse(segmento == "comercial_at_remoto","comercial_bt",segmento)) %>%
  group_by(ano, segmento) %>%
  summarise(adotantes = sum(adotantes_acum)) %>%
  ungroup()

mercado_nicho <- lista_consumidores$consumidores %>%
  mutate(segmento = ifelse(segmento == "residencial_remoto","residencial",segmento),
         segmento = ifelse(segmento == "comercial_at_remoto","comercial_bt",segmento)) %>%
  group_by(ano, segmento) %>%
  summarise(mercado_nicho = sum(consumidores)) %>%
  ungroup()

part_adotantes <- left_join(adotantes_segmento, consumidores_totais, by = c("ano","segmento")) %>%
  mutate(penetracao_total = adotantes / total_ucs) %>%
  left_join(mercado_nicho, by = c("ano", "segmento")) %>%
  mutate(penetracao_nicho = adotantes / mercado_nicho)

#lista com resultados

proj_adotantes <- projecao

tabela_dist_subs <-  read_xlsx(stringr::str_glue("dados_premissas/pde{pde_edicao}/tabela_dist_subs.xlsx" %>%  here::here()))

dados_gd <- feather::read_feather(stringr::str_glue("dados_premissas/pde{pde_edicao}/base_mmgd.feather" %>%  here::here()))


potencia_tipica <- dados_gd %>%
  filter(fonte_resumo == "Fotovoltaica") %>%
  group_by(segmento) %>%
  summarise(pot_sistemas = median(potencia_instalada_k_w))

potencia_media <- dados_gd %>%
  group_by(nome_4md,segmento,fonte_resumo) %>%
  summarise(pot_total = sum(potencia_instalada_k_w),
            adotantes_total = sum(qtde_u_csrecebem_os_creditos),
            pot_media = pot_total/adotantes_total) %>%
  ungroup() %>%
  complete(nome_4md,segmento,fonte_resumo) %>%
  group_by(segmento,fonte_resumo) %>%
  mutate(pot_media = ifelse(is.na(pot_media),mean(pot_media, na.rm = TRUE),pot_media)) %>%
  ungroup() %>%
  select(nome_4md,segmento,fonte_resumo,pot_media) %>%
  left_join(potencia_tipica, by = "segmento") %>%
  mutate(pot_media = ifelse(is.na(pot_media), pot_sistemas, pot_media)) %>%
  select(- pot_sistemas)

proj_potencia <- left_join(proj_adotantes, potencia_media, by = c("nome_4md", "segmento", "fonte_resumo"))


# historico de adotantes para substituir anos iniciais da projeção

historico_pot_fontes <- dados_gd %>%
  group_by(ano, nome_4md, segmento, fonte_resumo) %>%
  summarise(pot_hist = sum(potencia_instalada_k_w)) %>%
  ungroup() %>%
  complete(ano,nome_4md,segmento,fonte_resumo) %>%
  mutate(pot_hist = ifelse(is.na(pot_hist), 0, pot_hist))


proj_potencia <- proj_potencia %>%
  mutate(pot_ano = adotantes_ano * pot_media)

proj_potencia <- left_join(proj_potencia, historico_pot_fontes, by = c("nome_4md", "segmento", "ano", "fonte_resumo"))

proj_potencia <- proj_potencia %>%
  mutate(pot_ano = ifelse(ano < pde_edicao - 10, pot_hist, pot_ano)) %>%
  ungroup()

proj_potencia <- proj_potencia %>%
  mutate(pot_ano_mw = pot_ano / 1000) %>%
  group_by(nome_4md, segmento, fonte_resumo) %>%
  arrange(ano) %>%
  mutate(pot_acum_mw = cumsum(pot_ano_mw)) %>%
  ungroup()


proj_potencia <- left_join(proj_potencia, tabela_dist_subs, by = "nome_4md")


resumo <- proj_potencia %>%
  group_by(ano) %>%
  summarise(pot = sum(pot_acum_mw/1000)) %>%
  mutate(metodologia = "Recomeçar")


verao <- read_xlsx("X:\\Geracao Distribuida\\PDE\\PDE2031\\resultados\\referencia\\revisao\\capacidade_mmgd_primavera.xlsx")

verao <- verao %>%
  group_by(ano) %>%
  summarise(pot = sum(pot_acum_mw/1000)) %>%
  mutate(metodologia = "Referência")

juntos <- bind_rows(resumo, verao)

juntos <- juntos %>%
  filter(ano <= 2031)

ultimos <- juntos %>%
  mutate(pot = round(pot, 1)) %>%
  group_by(metodologia) %>%
  top_n(1, ano) %>%
  pull(pot)

ggplot(juntos) +
  geom_line(aes(x = ano, y = pot, color = metodologia)) +
  labs(title = "Comparação de métodos de projeção",
       y = "Capacidade Instalada Acumulada [GW]",
       x = "") +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = ultimos, labels = scales::label_number(
    decimal.mark = ",", accuracy = 0.1)))

ggsave("X:\\Geracao Distribuida\\PDE\\PDE2031\\resultados\\rascunhos\\recomeca_pot_primavera_acumulada.png", width = 8, height = 5)



# teste prepara base ------------------------------------------------------


base_original <- readxl::read_xlsx("X:\\Geracao Distribuida\\PDE\\PDE2031_2RQ\\base_aneel.xlsx", skip = 7)

base_preparada <- epe4md_prepara_base(base_original, pde_edicao = 2031)

# base original

base <- feather::read_feather("inst/dados_premissas/pde2031/base_mmgd.feather")

base_mmgd <- base %>%
  mutate(num_geradores = 1) %>%
  group_by(data_conexao = mes, ano, nome_4md, uf, subsistema, fonte_resumo, classe, subgrupo, modalidade, segmento, mini_micro, atbt, local_remoto) %>%
  summarise(qtde_u_csrecebem_os_creditos = sum(qtde_u_csrecebem_os_creditos),
            num_geradores = sum(num_geradores),
            potencia_instalada_k_w = sum(potencia_instalada_k_w),
            potencia_mw = sum(potencia_mw))

writexl::write_xlsx(base_mmgd, "inst/dados_premissas/pde2031/base_mmgd.xlsx")

write_csv(base_mmgd, "inst/dados_premissas/pde2031/base_mmgd.csv")


# potencia tipica

pot_tipica <- feather::read_feather("inst/dados_premissas/pde2031/potencia_tipica.feather")

writexl::write_xlsx(pot_tipica, "inst/dados_premissas/pde2031/potencia_tipica.xlsx")

base <- feather::read_feather("inst/dados_premissas/pde203/tarifas_4md.feather")

writexl::write_xlsx(base, "inst/dados_premissas/2020/tarifas_4md.xlsx")


fc <- read_rds("inst/dados_premissas/2020/fc_distribuidoras.rds")
writexl::write_xlsx(fc, "inst/dados_premissas/2020/fc_distribuidoras.xlsx")

fc <- read_rds("inst/dados_premissas/2020/fc_distribuidoras_mensal.rds")
writexl::write_xlsx(fc, "inst/dados_premissas/2020/fc_distribuidoras_mensal.xlsx")



dados_gd <- feather::read_feather("inst/dados_premissas/pde203/base_mmgd.feather")
potencia_tipica <- readxl::read_xlsx("inst/dados_premissas/2020/potencia_tipica.xlsx")


potencia_media <- dados_gd %>%
  group_by(nome_4md, segmento, fonte_resumo) %>%
  summarise(pot_total = sum(potencia_instalada_k_w),
            adotantes_total = sum(qtde_u_csrecebem_os_creditos),
            pot_media = pot_total/adotantes_total) %>%
  ungroup() %>%
  complete(nome_4md, segmento, fonte_resumo) %>%
  group_by(segmento, fonte_resumo) %>%
  mutate(pot_media = ifelse(is.na(pot_media), mean(pot_media, na.rm = TRUE), pot_media)) %>%
  ungroup() %>%
  select(nome_4md, segmento, fonte_resumo, pot_media) %>%
  left_join(potencia_tipica, by = "segmento") %>%
  mutate(pot_media = ifelse(is.na(pot_media), pot_sistemas, pot_media)) %>%
  select(-pot_sistemas)


dados_gd <- readxl::read_xlsx("inst/dados_premissas/2020/base_mmgd.xlsx")


# debug com base agrupada -------------------------------------------------

#resultados anteriores PDE2031

potencia_pde2031 <- read_xlsx("X:\\Geracao Distribuida\\PDE\\PDE2031\\resultados\\referencia\\revisao/capacidade_mmgd_verao.xlsx")

potencia_pde2031 <- potencia_pde2031 %>%
  group_by(ano) %>%
  summarise(pot = sum(pot_acum_mw))

# teste 4md

cenario <- read_xlsx("X:\\Geracao Distribuida\\PDE\\PDE2031\\cenarios/cenario_verao_encargos.xlsx")

resultado <- epe4md::epe4md_calcula(premissas_reg = cenario,
                                      ano_base = 2020,
                                      ano_max_resultado = 2031)




