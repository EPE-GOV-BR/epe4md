#' Cria a base de casos para serem simulados posteriormente no cálculo do
#' payback.
#'
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param ano_max_resultado numeric. Ano final para apresentação dos resultados.
#' Máximo igual a 2050. Default igual a 2050.
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
#'@rawNamespace import(purrr, except=c(discard))
#'@import readxl
#'@import dplyr
#'@import stringr
#'
#'@encoding UTF-8
#'
#' @examples
#'
#' epe4md_casos_payback(
#'   ano_base,
#'   ano_max_resultado = 2050,
#'   inflacao = 0.0375,
#'   ano_troca_inversor = 11,
#'   fator_custo_inversor = 0.15,
#'   dir_dados_premissas = NA_character_
#' )


utils::globalVariables(c("custo_unitario","fc_remoto", "fc_local", "fc_fv", "fc_eol", "fc_term", "pot_sistemas",
                         "fc", "tecnologia", "custo_inversor", "mes"))


epe4md_casos_payback <- function(ano_base,
                                 ano_max_resultado = 2050,
                                 inflacao = 0.0375,
                                 ano_troca_inversor = 11,
                                 fator_custo_inversor = 0.15,
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

  casos <- left_join(casos, fontes, by = "fonte_resumo",
                     multiple = "all")

  casos <- left_join(casos, potencia_tipica, by = "segmento",
                     multiple = "all")

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

  casos_payback <- casos

  casos_payback

}
