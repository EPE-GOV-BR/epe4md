#' Realiza a projecao da capacidade instalada de micro e minigeracao distribuida
#'
#' [epe4md::epe4md_proj_adotantes].
#' @param proj_adotantes data.frame. Resultado da função
#' @param lista_adotantes data.frame. Data.frame que contém dados relativos aos
#' adotantes.
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param dir_dados_premissas Diretório onde se encontram as premissas.
#' Se esse parâmetro não for passado, a função usa os dados default que são
#' instalados com o pacote. É importante que os nomes dos arquivos sejam os
#' mesmos da pasta default.
#'
#' @return list com dois data.frames. "proj_potencia" possui os resultados da
#' projeção de capacidade instalada de micro e minigeração distribuída.
#' "part_adotantes" possui o resultado em termos de participação do
#' número de adotantes frente ao total de unidades consumidoras.
#' @export
#'
#' @import tidyr
#' @import dplyr
#'
#' @encoding UTF-8
#'
#' @examples
#'
#' lista_adotantes <- list(
#'  proj_adotantes <- tibble::tibble(
#'    nome_4md = c("OUTRA", "RGE", "RORAIMA", "SULGIPE", "UHENPAL"),
#'    segmento = "comercial_at",
#'    p = c(0.0004484362, 0.0005572423, 0.0001223451, 0.0002427430, 0.0001034085),
#'    q = 1,
#'    spb = 0.3,
#'    ano = 2021,
#'    Ft = c(0.7847763, 0.8193315, 0.4980477, 0.6633618, 0.4560837),
#'    consumidores = c(2801, 10995, 791, 116, 48),
#'    payback = c(9.300698, 7.295061, 11.896997, 12.448897, 11.590019),
#'    mercado_potencial = c(43.00, 308.00, 5.50, 0.75, 0.25),
#'    adotantes_ano = 0,
#'    adotantes_acum = c(0, 1, 0, 0, 0),
#'    fonte_resumo = "Eólica",
#'    part_fonte = c(0.0000000000, 0.0006309148, 0.0000000000, 0.0000000000, 0.0000000000),
#'    adotantes_hist = 0
#'  ),
#'  part_adotantes <- tibble::tibble(
#'    ano = 2021,
#'    segmento = c("comercial_at", "comercial_bt", "residencial"),
#'    adotantes = c(11857, 315815, 798073),
#'    mercado_potencial = c(16718, 412539, 3370592),
#'    total_ucs = c(181394, 12098612, 72212363),
#'    penetracao_total = c(0.06536600, 0.02610341, 0.01105175),
#'    mercado_nicho = c(181394, 1930557, 11522806),
#'    penetracao_nicho = c(0.0653660, 0.1635875, 0.0692603),
#'    penetracao_potencial = c(0.7092356, 0.7655397, 0.2367753)
#'  )
#' )
#'
#' epe4md_proj_potencia(
#'    lista_adotantes = lista_adotantes,
#'    ano_base = 2021,
#'    dir_dados_premissas = NA_character_
#' )

utils::globalVariables(c("pot_media", "pot_hist", "geracao_gwh"))

epe4md_proj_potencia <- function(lista_adotantes,
                                 ano_base,
                                 dir_dados_premissas = "inst/dados_premissas") {

  dir_dados_premissas <- if_else(
    dir_dados_premissas == "inst/dados_premissas",
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
    summarise(pot_total = sum(potencia_instalada_k_w),
              adotantes_total = sum(qtde_u_csrecebem_os_creditos),
              pot_media = pot_total / adotantes_total) %>%
    ungroup() %>%
    complete(nome_4md, segmento, fonte_resumo) %>%
    group_by(segmento, fonte_resumo) %>%
    mutate(pot_media = ifelse(is.na(pot_media),
                              mean(pot_media, na.rm = TRUE),
                              pot_media)) %>%
    ungroup() %>%
    select(nome_4md, segmento, fonte_resumo, pot_media) %>%
    left_join(potencia_tipica, by = "segmento",
              multiple = "all") %>%
    mutate(pot_media = ifelse(is.na(pot_media), pot_sistemas, pot_media)) %>%
    select(-pot_sistemas)

  proj_potencia <- left_join(proj_adotantes, potencia_media,
                             by = c("nome_4md", "segmento", "fonte_resumo"),
                             multiple = "all")


  # historico de adotantes para substituir anos iniciais da projeção

  historico_pot_fontes <- dados_gd %>%
    filter(ano <= ano_base) %>%
    group_by(ano, nome_4md, segmento, fonte_resumo) %>%
    summarise(pot_hist = sum(potencia_instalada_k_w)) %>%
    ungroup() %>%
    complete(ano, nome_4md, segmento, fonte_resumo) %>%
    mutate(pot_hist = ifelse(is.na(pot_hist), 0, pot_hist))


  proj_potencia <- proj_potencia %>%
    mutate(pot_ano = adotantes_ano * pot_media)

  proj_potencia <- left_join(proj_potencia, historico_pot_fontes,
                             by = c("nome_4md", "segmento",
                                    "ano", "fonte_resumo"),
                             multiple = "all")

  proj_potencia <- proj_potencia %>%
    mutate(pot_ano = ifelse(ano <= ano_base, pot_hist, pot_ano)) %>%
    ungroup()

  proj_potencia <- proj_potencia %>%
    mutate(pot_ano_mw = pot_ano / 1000) %>%
    group_by(nome_4md, segmento, fonte_resumo) %>%
    arrange(ano) %>%
    mutate(pot_acum_mw = cumsum(pot_ano_mw)) %>%
    ungroup()


  #lista resultados

  lista_potencia <- list(proj_potencia, lista_adotantes$part_adotantes)

  names(lista_potencia) <- c("proj_potencia", "part_adotantes")

  lista_potencia

}

