#' Realiza a projeção do número de adotantes de micro e minigeração distribuída
#'
#' @param casos_otimizados data.frame. Resultado da funcao
#' [epe4md::epe4md_calibra_curva_s].
#' @param consumidores list. Resultado da função
#' [epe4md::epe4md_mercado_potencial].
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param dir_dados_premissas Diretório onde se encontram as premissas. Se esse
#' parâmetro não for passado, a função usa os dados default que são instalados
#' com o pacote. É importante que os nomes dos arquivos sejam os mesmos
#' da pasta default.
#'
#' @return list com dois data.frames. "proj_adotantes" possui os resultados da
#' projeção de adotantes de micro e minigeração distribuída. "part_adotantes"
#' possui o resultado em termos de participação do número de adotantes frente
#' ao total de unidades consumidoras.
#' @export
#'
#' @import tidyr
#' @import dplyr
#' @encoding UTF-8
#' @import tibble
#'
#' @examples
#'
#' casos_otimizados <-
#' structure(
#'   list(nome_4md = c("MUXENERGIA", "EPB", "EFLJC"),
#'        segmento = c("comercial_bt", "comercial_at", "residencial"),
#'        p = c(0.01, 0.000316252847258369, 0.00025238142355137),
#'        q = c(1, 1, 0.676434758626127),
#'        spb = c(0.3, 0.3, 0.3),
#'        ano = c(2029, 2025, 2026),
#'        Ft = c(0.999996472365327, 0.992930172901814, 0.829136572246132),
#'        consumidores = c(257, 1871, 847),
#'        payback = c(4.38236607048146, 7.216158855616, 4.25745937192023),
#'        mercado_potencial = c(69, 215, 236)),
#'   row.names = c(NA, -3L),
#'   class = c("tbl_df", "tbl", "data.frame"))
#'
#' consumidores <-
#'   list(
#'     structure(
#'       list(nome_4md = c("CPFL PIRATININGA", "EMS", "RGE"),
#'            ano = c(2020, 2028, 2034),
#'            segmento = c("comercial_at_remoto", "residencial", "comercial_at"),
#'            consumidores = c(6736, 113412, 13515)),
#'       class = c("tbl_df", "tbl", "data.frame"),
#'       row.names = c(NA, -3L)),
#'     structure(
#'       list(ano = c(2017, 2035, 2031),
#'            total_ucs = c(67352162.358959, 88092914.3204848, 83828643.0017198),
#'            segmento = c("residencial", "residencial", "residencial")),
#'       row.names = c(NA, -3L),
#'       class = c("tbl_df", "tbl", "data.frame"))
#'   )
#'
#' epe4md_proj_adotantes(
#'   casos_otimizados = casos_otimizados,
#'   consumidores = consumidores,
#'   ano_base = 2021
#' )

utils::globalVariables(c("Ft", "adotantes_ano", "adotantes_ano_media",
                         "adotantes_ano_c", "adotantes_acum_c",
                         "adotantes_hist_total", "part_fonte", "faltantes",
                         "adotantes", "total_ucs"))

epe4md_proj_adotantes <- function(casos_otimizados,
                                  consumidores,
                                  ano_base,
                                  dir_dados_premissas = NA_character_ ) {


  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )

  dados_gd <-
    readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx"))

  projecao <- casos_otimizados %>%
    group_by(nome_4md, segmento) %>%
    arrange(ano) %>%
    mutate(adotantes_acum = mercado_potencial * Ft,
           adotantes_ano = ifelse(ano == 2013,
                                  adotantes_acum,
                                  adotantes_acum - lag(adotantes_acum)),
           adotantes_ano = ifelse(adotantes_ano < 0,
                                  0,
                                  round(adotantes_ano, 0)),
           adotantes_acum = cumsum(adotantes_ano)) %>%
    ungroup()

  #suavização em caso de adotantes = 0

  projecao <- projecao %>%
    group_by(nome_4md, segmento) %>%
    arrange(ano) %>%
    mutate(adotantes_ano_media = zoo::rollmean(adotantes_ano, 2, fill = NA,
                                               align = "left"),
           adotantes_ano_media = round(ifelse(is.na(adotantes_ano_media),
                                              adotantes_ano,
                                              adotantes_ano_media), 0))

  projecao <- projecao %>%
    mutate(adotantes_ano_c = case_when(
      adotantes_ano == 0 & ano > 2019 ~ adotantes_ano_media,
      lag(adotantes_ano) == 0 & ano > 2019 ~ lag(adotantes_ano_media),
      TRUE ~ adotantes_ano),
      adotantes_acum_c = cumsum(adotantes_ano_c)) %>%
    ungroup() %>%
    select(-adotantes_ano, -adotantes_acum, -adotantes_ano_media) %>%
    rename(
      adotantes_ano = adotantes_ano_c,
      adotantes_acum = adotantes_acum_c
    )


  # Abertura dos adotantes por fonte ----------------------------------------

  part_adot_fontes <- dados_gd %>%
    group_by(nome_4md, segmento, fonte_resumo) %>%
    summarise(adotantes_hist = sum(qtde_u_csrecebem_os_creditos)) %>%
    ungroup() %>%
    group_by(nome_4md, segmento) %>%
    mutate(adotantes_hist_total = sum(adotantes_hist),
           part_fonte = adotantes_hist / adotantes_hist_total) %>%
    ungroup()

  # completar faltantes para serem encontrados no join
  part_adot_fontes <- part_adot_fontes %>%
    complete(nome_4md, segmento, fonte_resumo) %>%
    group_by(nome_4md, segmento) %>%
    mutate(faltantes = sum(is.na(part_fonte))) %>%
    ungroup() %>%
    mutate(part_fonte = ifelse(faltantes == 4 & fonte_resumo == "Fotovoltaica",
                               1,
                               part_fonte),
           part_fonte = ifelse(is.na(part_fonte), 0, part_fonte)) %>%
    select(nome_4md, segmento, fonte_resumo, part_fonte)

  # historico de adotantes para substituir anos iniciais da projeção

  historico_adot_fontes <- dados_gd %>%
    filter(ano <= ano_base) %>%
    group_by(ano, nome_4md, segmento, fonte_resumo) %>%
    summarise(adotantes_hist = sum(qtde_u_csrecebem_os_creditos)) %>%
    ungroup() %>%
    complete(ano, nome_4md, segmento, fonte_resumo) %>%
    mutate(adotantes_hist = ifelse(is.na(adotantes_hist), 0, adotantes_hist))

  projecao <- left_join(projecao, part_adot_fontes,
                        by = c("nome_4md", "segmento"),
                        multiple = "all") %>%
    mutate(adotantes_ano = round(adotantes_ano * part_fonte, 0))

  projecao <- left_join(projecao, historico_adot_fontes,
                        by = c("nome_4md", "segmento", "ano", "fonte_resumo"),
                        multiple = "all")

  projecao <- projecao %>%
    mutate(adotantes_ano = ifelse(ano <= ano_base,
                                  adotantes_hist,
                                  adotantes_ano)) %>%
    group_by(nome_4md, segmento, fonte_resumo) %>%
    arrange(ano) %>%
    mutate(adotantes_acum = cumsum(adotantes_ano)) %>%
    ungroup() %>%
    mutate(mercado_potencial = mercado_potencial / 4)


  #calculo percentual de adocao frente ao numero de consumidores totais

  consumidores_totais <- consumidores$consumidores_totais

  adotantes_segmento <- projecao %>%
    mutate(segmento = ifelse(segmento == "residencial_remoto",
                             "residencial",
                             segmento),
           segmento = ifelse(segmento == "comercial_at_remoto",
                             "comercial_bt",
                             segmento)) %>%
    group_by(ano, segmento) %>%
    summarise(adotantes = sum(adotantes_acum),
              mercado_potencial = sum(mercado_potencial)) %>%
    ungroup()

  mercado_nicho <- consumidores$consumidores %>%
    mutate(segmento = ifelse(segmento == "residencial_remoto",
                             "residencial",
                             segmento),
           segmento = ifelse(segmento == "comercial_at_remoto",
                             "comercial_bt",
                             segmento)) %>%
    group_by(ano, segmento) %>%
    summarise(mercado_nicho = sum(consumidores)) %>%
    ungroup()

  part_adotantes <- left_join(adotantes_segmento, consumidores_totais,
                              by = c("ano", "segmento"),
                              multiple = "all") %>%
    mutate(penetracao_total = adotantes / total_ucs) %>%
    left_join(mercado_nicho, by = c("ano", "segmento"),
              multiple = "all") %>%
    mutate(penetracao_nicho = adotantes / mercado_nicho,
           penetracao_potencial = adotantes / mercado_potencial)

  #lista com resultados

  proj_adotantes <- projecao

  lista_adotantes <- list(proj_adotantes, part_adotantes)
  names(lista_adotantes) <- c("proj_adotantes", "part_adotantes")

  lista_adotantes

}
