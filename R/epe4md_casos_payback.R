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
#' @param ano_recapex_bat Ano em que será feito um investimento adicional em baterias
#' para compensar a degradação. Default igual a 11.
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
#'
#'@encoding UTF-8
#'
#' @examples


epe4md_casos_payback <- function(ano_base,
                                 ano_max_resultado = 2060,
                                 inflacao = 0.0375,
                                 ano_troca_inversor = 11,
                                 fator_custo_inversor = 0.15,
                                 ano_recapex_bat = 11,
                                 dir_dados_premissas = NA_character_
                                 ) {

# Potência típica dos sistemas ------------------------------------------------


  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
      dir_dados_premissas
  )

  potencia_tipica <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/potencia_tipica.xlsx"))


  # Premissas ---------------------------------------------------------------

  # Fator de capacidade médio por distribuidora para a fonte fotovoltaica
    fc_fontes <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/fc_distribuidoras.xlsx"))

  # Fator de autoconsumo por fonte e segmento de consumidor
  injecao <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/injecao.xlsx")) %>%
    filter(fonte_resumo == "Fotovoltaica") # FIltra somente para fotovoltaica

  # Define o O&M anual por segmento de consumidor
  injecao <- injecao %>%
    mutate(oem_anual = ifelse(segmento == "comercial_at_remoto", 0.02, 0.01))

  # Define a vida útil e degradação anual da fonte fotovoltaica
  fontes <- tribble(
    ~fonte_resumo, ~vida_util, ~degradacao,
    "Fotovoltaica",    25,         0.005,
  )

  # Custos unitários inicial do sistema fotovoltaico em R$/Wp por ano e segmento de consumidor
  custos <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/custos.xlsx"),
                      sheet = "custos") %>%
    mutate(custo_inversor = fator_custo_inversor * custo_unitario) # calcula o custo unitário do inversor em R$/Wp em relação ao custo unitário da fonte por segmento de consumidor

  casos <- merge(injecao, fc_fontes) %>%
    mutate(fc_fv = ifelse(segmento == "comercial_at_remoto",
                          fc_remoto,
                          fc_local)) %>%
    select(-fc_local, -fc_remoto) %>%
    rename(
      "fv" = fc_fv,
      "eolica" = fc_eol,
      "termica" = fc_term)

  # Transpõe as colunas fv, eolica e térmica
  casos <- pivot_longer(casos, cols = c("fv", "eolica", "termica"),
                        names_to = "tecnologia", values_to = "fc")

  # Inclui vida útil e degradação para a fonte fotovoltaica
  casos <- left_join(casos, fontes, by = "fonte_resumo")

  # Inclui potência típica por segmento
  casos <- left_join(casos, potencia_tipica, by = "segmento")

  # Calcula a energia gerada em kWh em 1 ano
  casos <- casos %>%
    mutate(geracao_1_kwh = pot_sistemas * fc * 8760)

  # Filtra a tecnologia para somente fotovoltaica
  casos <- casos %>%
    filter(tecnologia == "fv") %>%
    select(-tecnologia)

  # Inclui os custos unitários da energia fotovoltaica e do inversor em R$/Wp
  casos <- casos %>%
    merge(custos)

  # Cálculo do capex para cada segmento de consumidor em cada distribuidora
  casos <- casos %>%
    mutate(capex_inicial = custo_unitario * pot_sistemas * 1000, # custo inicial de instalação do sistema fotovoltaico
           capex_inversor = custo_inversor * pot_sistemas * 1000 * # custo da troca do inversor atualizado pela inflação até o ano da troca
             ((1 + inflacao)^(ano_troca_inversor - 1))) %>%
    arrange(nome_4md, segmento, ano)

  casos <- casos %>%
    filter(ano <= ano_max_resultado)


  # Cálculo para baterias ---------------------------------------------------------------


    precos_bateria <- readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/precos_baterias.xlsx"))

    precos_recapex_bat <- precos_bateria %>%
      rename(bateria_custo_rec = bateria_custo,
             ano_recapex = ano) %>%
      select(-bateria_oem)

    dec_fec <- readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/dec_fec.xlsx"))

    casos <- casos %>%
      mutate(ano_recapex = ano + ano_recapex_bat) %>% # calcula o ano de troca do inversor a partir de um ano inicial
      left_join(dec_fec, by = "nome_4md") %>% # inclui os indicadores equivalentes de duração e de falta de energia
      left_join(precos_bateria, by = c("ano", "segmento")) %>%
      left_join(precos_recapex_bat, by = c("ano_recapex", "segmento")) %>%
      mutate(
        bateria_capacidade_kwh = geracao_1_kwh * (1 - fator_autoconsumo) / 365, # calcula a capacidade da bateria em kwh/dia em função da energia gerada pelos paineis fotovoltaicos em 1 ano: [kWh/ano] * [] / [dias/ano] = [kWh/dia]
        bateria_capex_inicial = bateria_custo * bateria_capacidade_kwh, # calcula o capex total da bateria com base na capacidade em kWh/dia
        bateria_recapex = bateria_custo_rec * bateria_capacidade_kwh, # calcula o capex total da troca da bateria com base na capacidade em kWh/dia
        bateria_oem_anual = bateria_oem
      ) %>%
      group_by(segmento, nome_4md) %>%
      fill(everything(), .direction = "updown") %>%
      ungroup()

  casos

}
