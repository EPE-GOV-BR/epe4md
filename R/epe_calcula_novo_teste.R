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
sumariza_resultados <- function(resultados_mensais) {

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
#' Máximo igual a 2060. Default igual a 2060.
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
#' @param filtro_comercial numeric. Fator percentual para definir o nicho do
#' segmento comercial. Default é calculado pelo modelo com base no nicho
#' residencial.
#' @param p_max numeric. Fator de inovação (p) máximo. Default igual a 0.01.
#' @param q_max numeric. Fator de imitação (q) máximo. DEfault igual a 1.
#' @param tx_cresc_grupo_a numeric. Taxa de crescimento anual dos consumuidores
#' cativos do Grupo A. Default igual a 0.
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
#'
library(readxl)
library(dplyr)
library(tidyr)


calcula <- function(
    premissas_reg,
    ano_base,
    ano_max_resultado = 2060,
    altera_sistemas_existentes = FALSE,
    ano_decisao_alteracao = 2023,
    inflacao = 0.0375,
    taxa_desconto_nominal = 0.13,
    custo_reforco_rede = 200,
    ano_troca_inversor = 11,
    pagamento_disponibilidade = 0.3,
    disponibilidade_kwh_mes = 100,
    filtro_renda_domicilio = "maior_3sm",
    fator_local_comercial = "residencial",
    desconto_capex_local = 0,
    anos_desconto = 0,
    tx_cresc_grupo_a = 0,
    p_max = 0.01,
    q_max = 1,
    filtro_comercial = NA_real_,
    ajuste_ano_corrente = FALSE,
    ultimo_mes_ajuste = NA_integer_,
    metodo_ajuste = NA_character_,
    dir_dados_premissas = NA_character_,
    batt_prices =  NA_character_,
    batt_performance =  NA_character_,
    path_scripts_bateria = path_scripts_bateria
)
{

  path_batt_price <- glue("{path_inputs_bateria}/batt_prices/preco_baterias_br.csv")
  path_batt_tech <- glue("{path_inputs_bateria}/batt_tech_performance/batt_tech_performance_SunLamp17.csv")
  path_dec_fec <- glue("{path_inputs_bateria}/dec_fec.csv")



  # # Carregar o script onde a função payback_casos está definida
  source(glue("{path_scripts_bateria}/epe4md_mercado_potencial.R"))
  source(glue("{path_scripts_bateria}/epe4md_casos_payback.R"))
  source(glue("{path_scripts_bateria}/epe4md_payback.R"))
  source(glue("{path_scripts_bateria}/epe4md_calibra_curva_s.R"))
  source(glue("{path_scripts_bateria}/epe4md_proj_adotantes.R"))
  source(glue("{path_scripts_bateria}/epe4md_proj_potencia.R"))
  source(glue("{path_scripts_bateria}/epe4md_proj_mensal.R"))
  source(glue("{path_scripts_bateria}/epe4md_proj_geracao.R"))
  source(glue("{path_scripts_bateria}/epe_sumariza_resultados.R"))
  source(glue("{path_scripts_bateria}/epe_4md_graficos_novo.R"))




# # Carregar o script onde a função payback_casos está definida
# source("C:/Users/User/OneDrive/EPE/R/4md_battery/Scripts/FV_BATERIA/epe4md_mercado_potencial.R")
# source("C:/Users/User/OneDrive/EPE/R/4md_battery/Scripts/FV_BATERIA/epe4md_casos_payback.R")
# source("C:/Users/User/OneDrive/EPE/R/4md_battery/Scripts/FV_BATERIA/epe4md_payback.R")
# source("C:/Users/User/OneDrive/EPE/R/4md_battery/Scripts/FV_BATERIA/epe4md_calibra_curva_s.R")
# source("C:/Users/User/OneDrive/EPE/R/4md_battery/Scripts/FV_BATERIA/epe4md_proj_adotantes.R")
# source("C:/Users/User/OneDrive/EPE/R/4md_battery/Scripts/FV_BATERIA/epe4md_proj_potencia.R")
# source("C:/Users/User/OneDrive/EPE/R/4md_battery/Scripts/FV_BATERIA/epe4md_proj_mensal.R")
# source("C:/Users/User/OneDrive/EPE/R/4md_battery/Scripts/FV_BATERIA/epe4md_proj_geracao.R")
# source("C:/Users/User/OneDrive/EPE/R/4md_battery/Scripts/FV_BATERIA/epe_sumariza_resultados.R")
# source("C:/Users/User/OneDrive/EPE/R/4md_battery/Scripts/FV_BATERIA/epe_4md_graficos_novo.R")






}
premissas_regulatorias <- readxl::read_xlsx(system.file("dados_premissas/2022/premissas_reg.xlsx", package = "epe4md"))
path_scripts_bateria = "C:/Users/User/OneDrive/EPE/R/4md_battery/Scripts/" #(Modificar  pasta dos seus arquivos)

calcula(ano_base = 2022,
        premissas_reg = premissas_regulatorias, batt_prices = precos_bateria,batt_performance= perfomance_bateria,
        ano_max_resultado = 2035,path_scripts_bateria = path_scripts_bateria)


