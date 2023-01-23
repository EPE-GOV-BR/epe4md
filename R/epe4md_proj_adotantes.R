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
#'@import tidyr
#'@import dplyr
#'@encoding UTF-8
#'
#' @examples

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
                        by = c("nome_4md", "segmento")) %>%
    mutate(adotantes_ano = round(adotantes_ano * part_fonte, 0))

  projecao <- left_join(projecao, historico_adot_fontes,
                        by = c("nome_4md", "segmento", "ano", "fonte_resumo"))

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
                              by = c("ano", "segmento")) %>%
    mutate(penetracao_total = adotantes / total_ucs) %>%
    left_join(mercado_nicho, by = c("ano", "segmento")) %>%
    mutate(penetracao_nicho = adotantes / mercado_nicho,
           penetracao_potencial = adotantes / mercado_potencial)

  #lista com resultados

  proj_adotantes <- projecao

  lista_adotantes <- list(proj_adotantes, part_adotantes)
  names(lista_adotantes) <- c("proj_adotantes", "part_adotantes")

  lista_adotantes

}
