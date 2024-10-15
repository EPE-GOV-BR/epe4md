#' Cria a base de casos para serem simulados posteriormente no cálculo do
#' payback.
#'
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param ano_max_resultado numeric. Ano final para apresentação dos resultados.
#' Máximo igual a 2060. Default igual a 2060.
#' @param inflacao numeric. Taxa de inflacão considerada no reajuste das tarifas
#' e para calcular o retorno real de projetos.
#' @param ano_troca_inversor numeric. Ano, a partir do ano de instalação, em que
#' é realizada a troca do inversor fotovoltaico. Default igual a 11.
#' @param fator_custo_inversor numeric. Custo do inversor para a substituição em
#' percentual do CAPEX total do sistema de geração. Default igual a 0.15.
#' @param dir_dados_premissas Diretório onde se encontram as premissas. Se esse
#' parâmetro não for passado, a função usa os dados default que são instalados
#' com o pacote. É importante que os nomes dos arquivos sejam os mesmos da
#' pasta default.
#'
#' @return data.frame. Casos para serem simulados posteriormente no cálculo do
#' payback.
#' @export
#'
#'@import tidyr
#'@import purrr
#'@import readxl
#'@import dplyr
#'@import stringr
#'@import writexl

#'@encoding UTF-8
#'
#' @examples
#'

library(readr)
library(glue)


payback_casos <- function (ano_base, ano_max_resultado = 2060, inflacao = 0.0375,
          ano_troca_inversor = 11, fator_custo_inversor = 0.15, dir_dados_premissas = NA_character_,
          horas_por_dia_bateria = 4, perc_pot_bateria = 0.35, bateria_degrad = 0.05,path_inputs_bateria = path_inputs_bateria)
{
  dir_dados_premissas <- if_else(is.na(dir_dados_premissas),
                                 system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                                             package = "epe4md"), dir_dados_premissas)

  path_batt_price <- glue("{path_inputs_bateria}/batt_prices/preco_baterias_br.csv")
  path_batt_tech <- glue("{path_inputs_bateria}/batt_tech_performance/batt_tech_performance_SunLamp17.csv")
  path_dec_fec <- glue("{path_inputs_bateria}/dec_fec.csv")

  precos_bateria <- read_csv(path_batt_price)
  info_bateria <- read_csv(path_batt_tech)
  dec_fec <- read_delim(path_dec_fec, delim = ';')


  potencia_tipica <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/potencia_tipica.xlsx"))
  fc_fontes <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/fc_distribuidoras.xlsx"))
  injecao <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/injecao.xlsx")) %>%
    filter(fonte_resumo == "Fotovoltaica")
  injecao <- injecao %>% mutate(oem_anual = ifelse(segmento ==
                                                     "comercial_at_remoto", 0.02, 0.01))
  fontes <- tribble(~fonte_resumo, ~vida_util, ~degradacao,
                    "Fotovoltaica", 25, 0.005, )
  custos <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/custos.xlsx"),
                      sheet = "custos") %>% mutate(custo_inversor = fator_custo_inversor *
                                                     custo_unitario)
  casos <- merge(injecao, fc_fontes) %>% mutate(fc_fv = ifelse(segmento ==
                                                                 "comercial_at_remoto", fc_remoto, fc_local)) %>% select(-fc_local,
                                                                                                                         -fc_remoto) %>% rename(fv = fc_fv, eolica = fc_eol, termica = fc_term)
  casos <- pivot_longer(casos, cols = c("fv", "eolica", "termica"),
                        names_to = "tecnologia", values_to = "fc")
  casos <- left_join(casos, fontes, by = "fonte_resumo")
  casos <- left_join(casos, potencia_tipica, by = "segmento")
  casos <- left_join(casos, dec_fec, by = "nome_4md")
  casos <- casos %>% mutate(geracao_1_kwh = pot_sistemas *
                              fc * 8760)
  casos <- casos %>% filter(tecnologia == "fv") %>% select(-tecnologia)
  casos <- casos %>% merge(custos)
  casos <- casos %>% mutate(capex_inicial = custo_unitario *
                              pot_sistemas * 1000, capex_inversor = custo_inversor *
                              pot_sistemas * 1000 * ((1 + inflacao)^(ano_troca_inversor -
                                                                       1))) %>% arrange(nome_4md, segmento, ano)
  casos <- casos %>% filter(ano <= ano_max_resultado)


### Data.frame baterias ###
# Renomear colunas em batt_prices
batt_prices_cleaned <- precos_bateria %>%
  rename(
    bateria_capex = capex_kwh,
    bateria_oem = oem_kwh,
    ano = year
  )


# Eliminar colunas do dataset batt_performance
colunas_para_eliminar_performance <- c(
  'batt_eff_com', 'batt_eff_ind',
  'batt_lifetime_yrs_ind', 'batt_lifetime_yrs_com'
)
#
batt_performance_cleaned <- info_bateria %>%
  select(-one_of(colunas_para_eliminar_performance))

# # Renomear colunas em batt_performance
batt_performance_cleaned <- batt_performance_cleaned %>%
  rename(
    bateria_eficiencia = batt_eff_res,
    bateria_tempo_de_vida_anos = batt_lifetime_yrs_res,
    ano = year
  )
# #
# Unir os dataframes de bateria (performance e preços)
battery <- batt_prices_cleaned %>%
  left_join(batt_performance_cleaned, by = "ano")
# #
# # Calcular O&M e substituir no dataframe
battery <- battery %>%
  mutate(
    bateria_fc = horas_por_dia_bateria / 24,
    bateria_degradacao = bateria_degrad
  )

## Unir os data.frames de casos_payback e bateria
  casos_payback <- casos
  casos_payback

  df_unido <- left_join(casos_payback, battery, by = "ano")

  df_unido <- df_unido %>%
    mutate(
      pot_baterias = pot_sistemas * perc_pot_bateria,
      bateria_capex_inicial = if_else(
        segmento %in% c("comercial_at", "comercial_bt", "comercial_at_remoto"),
        bateria_capex*pot_baterias,
        bateria_capex*pot_baterias
      ),
      bateria_oem_anual = if_else(
        segmento %in% c("comercial_at", "comercial_bt", "comercial_at_remoto"),
        bateria_oem,
        bateria_oem
      ))

  df_unido <- df_unido %>%
    fill(everything(), .direction = "up")

  df_unido #União dos data.frames de FV e Bateria

}

#RODAR SCRIPTS

path_inputs_bateria = "C:/Users/User/OneDrive/EPE/R/4md_battery/Scripts/Inputs" #(Modificar  pasta dos seus arquivos)

casos_payback_fv_bateria <- payback_casos(
  ano_base = 2022,
  ano_max_resultado = 2060,
  path_inputs_bateria = path_inputs_bateria
)

