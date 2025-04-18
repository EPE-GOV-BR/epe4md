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


  fc_fontes <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/fc_distribuidoras.xlsx"))

  injecao <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/injecao.xlsx")) %>%
    filter(fonte_resumo == "Fotovoltaica")

  injecao <- injecao %>%
    mutate(oem_anual = ifelse(segmento == "comercial_at_remoto", 0.02, 0.01))


  fontes <- tribble(
    ~fonte_resumo, ~vida_util, ~degradacao,
    "Fotovoltaica",    25,         0.005,
  )


  custos <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/custos.xlsx"),
                      sheet = "custos") %>%
    mutate(custo_inversor = fator_custo_inversor * custo_unitario)

  casos <- merge(injecao, fc_fontes) %>%
    mutate(fc_fv = ifelse(segmento == "comercial_at_remoto",
                          fc_remoto,
                          fc_local)) %>%
    select(-fc_local, -fc_remoto) %>%
    rename(
      "fv" = fc_fv,
      "eolica" = fc_eol,
      "termica" = fc_term)

  casos <- pivot_longer(casos, cols = c("fv", "eolica", "termica"),
                        names_to = "tecnologia", values_to = "fc")

  casos <- left_join(casos, fontes, by = "fonte_resumo")

  casos <- left_join(casos, potencia_tipica, by = "segmento")

  casos <- casos %>%
    mutate(geracao_1_kwh = pot_sistemas * fc * 8760)

  casos <- casos %>%
    filter(tecnologia == "fv") %>%
    select(-tecnologia)

  casos <- casos %>%
    merge(custos)

  casos <- casos %>%
    mutate(capex_inicial = custo_unitario * pot_sistemas * 1000,
           capex_inversor = custo_inversor * pot_sistemas * 1000 *
             ((1 + inflacao)^(ano_troca_inversor - 1))) %>%
    arrange(nome_4md, segmento, ano)

  casos <- casos %>%
    filter(ano <= ano_max_resultado)


  # Baterias ---------------------------------------------------------------


    precos_bateria <- readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/precos_baterias.xlsx"))

    precos_recapex_bat <- precos_bateria %>%
      rename(bateria_custo_rec = bateria_custo,
             ano_recapex = ano) %>%
      select(-bateria_oem)

    dec_fec <- readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/dec_fec.xlsx"))

    casos <- casos %>%
      mutate(ano_recapex = ano + ano_recapex_bat) %>%
      left_join(dec_fec, by = "nome_4md") %>%
      left_join(precos_bateria, by = c("ano", "segmento")) %>%
      left_join(precos_recapex_bat, by = c("ano_recapex", "segmento")) %>%
      mutate(
        bateria_capacidade_kwh = geracao_1_kwh * (1 - fator_autoconsumo) / 365,
        bateria_capex_inicial = bateria_custo * bateria_capacidade_kwh,
        bateria_recapex = bateria_custo_rec * bateria_capacidade_kwh,
        bateria_oem_anual = bateria_oem
      ) %>%
      group_by(segmento, nome_4md) %>%
      fill(everything(), .direction = "updown") %>%
      ungroup()

  casos

}
