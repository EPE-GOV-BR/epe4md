#' Realiza a projeção da capacidade instalada de micro e minigeração distribuída
#'
#' @param lista_adotantes list. Resultado da função
#' [epe4md::epe4md_proj_adotantes].
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param dir_dados_premissas Diretório onde se encontram as premissas.
#' Se esse parâmetro não for passado, a função usa os dados default que são
#' instalados com o pacote. É importante que os nomes dos arquivos sejam os
#' mesmos da pasta default.
#' @param perc_pot_bateria numeric. Percentual de potência atribuída a baterias.
#' Default igual a 0.35 (35%).
#'
#' @return list com dois data.frames. "proj_potencia" possui os resultados da
#' projeção de capacidade instalada de micro e minigeração distribuída.
#' "part_adotantes" possui o resultado em termos de participação do
#' número de adotantes frente ao total de unidades consumidoras.
#' @export
#'
#'@import tidyr
#'@import dplyr
#'
#'@encoding UTF-8
#' @examples

epe4md_proj_potencia <- function(lista_adotantes,
                                 ano_base,
                                 dir_dados_premissas = NA_character_) {

  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )

  dados_gd <-
    readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx"))

  proj_adotantes <- lista_adotantes$proj_adotantes

  potencia_tipica <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/potencia_tipica.xlsx"))

  potencia_media <- dados_gd %>%
    group_by(nome_4md, segmento, fonte_resumo) %>%
    summarise(
      pot_total = sum(potencia_instalada_k_w),
      adotantes_total = sum(qtde_u_csrecebem_os_creditos),
      pot_media = pot_total / adotantes_total
    ) %>%
    ungroup() %>%
    complete(nome_4md, segmento, fonte_resumo) %>%
    group_by(segmento, fonte_resumo) %>%
    mutate(
      pot_media = ifelse(is.na(pot_media), mean(pot_media, na.rm = TRUE), pot_media)
    ) %>%
    ungroup() %>%
    select(nome_4md, segmento, fonte_resumo, pot_media) %>%
    left_join(potencia_tipica, by = "segmento") %>%
    mutate(
      pot_media = ifelse(is.na(pot_media), pot_sistemas, pot_media)
    ) %>%
    select(-pot_sistemas)

  proj_potencia <- left_join(proj_adotantes, potencia_media,
                             by = c("nome_4md", "segmento", "fonte_resumo"))

  historico_pot_fontes <- dados_gd %>%
    filter(ano <= ano_base) %>%
    group_by(ano, nome_4md, segmento, fonte_resumo) %>%
    summarise(pot_hist = sum(potencia_instalada_k_w)) %>%
    ungroup() %>%
    complete(ano, nome_4md, segmento, fonte_resumo) %>%
    mutate(pot_hist = ifelse(is.na(pot_hist), 0, pot_hist))

  proj_potencia <- proj_potencia %>%
    mutate(
      pot_ano = adotantes_ano * pot_media,
      pot_ano_bateria = adotantes_ano_bateria * pot_media
    )

  proj_potencia <- left_join(proj_potencia, historico_pot_fontes,
                             by = c("nome_4md", "segmento", "ano", "fonte_resumo"))

  proj_potencia <- proj_potencia %>%
    mutate(pot_ano = ifelse(ano <= ano_base, pot_hist, pot_ano)) %>%
    ungroup() %>%
    select(-pot_hist)

  #dimensionamento bateria
  fc_fontes <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/fc_distribuidoras.xlsx")) %>%
    select(nome_4md, fc_local)

  injecao <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/injecao.xlsx")) %>%
    filter(fonte_resumo == "Fotovoltaica") %>%
    select(-fonte_resumo)

  proj_potencia <- proj_potencia %>%
    left_join(fc_fontes, by = "nome_4md") %>%
    left_join(injecao, by = "segmento")

  proj_potencia <- proj_potencia %>%
    mutate(bateria_capacidade_kwh = adotantes_ano_bateria * pot_media * fc_local *
    8760 * (1 - fator_autoconsumo) / 365)

  #resumo
  proj_potencia <- proj_potencia %>%
    mutate(
      pot_ano_mw = pot_ano / 1000,
      pot_ano_bateria_mw = pot_ano_bateria / 1000,
      cap_ano_bateria_mwh = bateria_capacidade_kwh / 1000
    ) %>%
    group_by(nome_4md, segmento, fonte_resumo) %>%
    arrange(ano) %>%
    mutate(
      pot_acum_mw = cumsum(pot_ano_mw),
      pot_acum_bateria_mw = cumsum(pot_ano_bateria_mw),
      cap_acum_bateria_mwh = cumsum(cap_ano_bateria_mwh),
    ) %>%
    ungroup()

  lista_potencia <- list(proj_potencia, lista_adotantes$part_adotantes)
  names(lista_potencia) <- c("proj_potencia", "part_adotantes")

  lista_potencia
}
