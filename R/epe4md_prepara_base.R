#' Prepara a base de dados da ANEEL com geradores existentes para ser utilizada
#' nas fun\u00E7\u00F5es seguintes.
#'
#' @param base_aneel Dataframe com a base de dados disponibilizada pela ANEEL
#' na sua p\u00E1gina de dados abertos com dados individualizados dos micro e
#' minigeradores distribu\u00EDdos. Sugest\u00E3o ler base com read.csv2
#' @param ano_base numeric. Ano base da proje\u00E7\u00E3o. Define o ano em que a
#' fun\u00E7\u00E3o ir\u00E1 buscar a base de dados. \u00DAltimo ano completo realizado.
#' @param resumida Logic. Se TRUE, retorna a base resumida (Default). Se
#' FALSE retorna base com mais desagrega\u00E7\u00F5es.
#' @param dir_dados_premissas Diret\u00F3rio onde se encontram as premissas. Se esse
#' par\u00E2metro n\u00E3o for passado, a fun\u00E7\u00E3o usa os dados default que s\u00E3o instalados
#' com o pacote. \u00E9 importante que os nomes dos arquivos sejam os mesmos da
#' pasta default.
#'
#' @return data.frame. Base tratada e resumida.
#' @export
#'
#'@import tidyr
#'@import readxl
#'@import dplyr
#'@import stringr
#'
#'@encoding UTF-8
#'
#' @examples
#' epe4md_prepara_base(
#'    base_aneel,
#'    ano_base = 2021,
#'    resumida = TRUE,
#'    dir_dados_premissas = NA_character_
#' )

utils::globalVariables(c("mes_ano", "sig_agente", "cod_municipio_ibge", "dth_atualiza_cadastral_empreend",
                         "data_conexao", "dsc_porte", "uf", "subsistema", "sig_uf",
                         "mda_potencia_instalada_kw", "qtd_uc_recebe_credito", "atbt",
                         "dsc_fonte_geracao", "potencia_instalada_k_w", "classe",
                         "dsc_sub_grupo_tarifario", "dsc_classe_consumo", "modalidade",
                         "sig_modalidade_empreendimento", "nom_municipio", "mini_micro",
                         "local_remoto", "num_geradores", "data_conexao"))

epe4md_prepara_base <- function(base_aneel,
                                ano_base,
                                resumida = TRUE,
                                dir_dados_premissas = NA_character_

) {

  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue('dados_premissas/{ano_base}'),
                package = 'epe4md'),
    dir_dados_premissas
  )

  base_mmgd <- base_aneel %>%
    janitor::clean_names()

  base_mmgd <- base_mmgd %>%
    drop_na(sig_agente, cod_municipio_ibge)

  base_mmgd <- base_mmgd %>%
    mutate(fonte_resumo = case_when(
      sig_tipo_geracao == 'UFV' ~ 'Fotovoltaica',
      sig_tipo_geracao == 'UTE' ~ 'Termel\u00E9trica',
      sig_tipo_geracao == 'EOL' ~ 'E\u00F3lica',
      TRUE ~ 'Hidro'
    ))

  base_mmgd$dth_atualiza_cadastral_empreend <- ymd(
    base_mmgd$dth_atualiza_cadastral_empreend)

  base_mmgd <- base_mmgd %>%
    mutate(data_conexao = ifelse(dth_atualiza_cadastral_empreend <
                                   dmy('01-01-2013'),
                                 dmy('01-01-2013'),
                                 dth_atualiza_cadastral_empreend))

  base_mmgd$data_conexao <- as_date(base_mmgd$data_conexao)


  base_mmgd <- base_mmgd %>%
    mutate(ano = year(data_conexao)) %>%
    select(-dth_atualiza_cadastral_empreend)

  base_mmgd$mes <- as.Date(cut(base_mmgd$data_conexao, breaks = 'month'))

  base_mmgd <- base_mmgd %>%
    arrange(data_conexao)

  base_mmgd <- base_mmgd %>%
    mutate(mini_micro = ifelse(dsc_porte == 'Microgeracao',
                               'MicroGD',
                               'MiniGD'))

  nomes_dist <-
    read_xlsx(stringr::str_glue('{dir_dados_premissas}/nomes_dist_powerbi.xlsx')) %>%
    janitor::clean_names()

  base_mmgd <- base_mmgd %>%
    left_join(nomes_dist, by = 'sig_agente',
              multiple = "all")

  tabela_regiao <-
    read_xlsx(stringr::str_glue('{dir_dados_premissas}/tabela_dist_subs.xlsx')) %>%
    janitor::clean_names() %>%
    select(uf, subsistema) %>%
    distinct()

  base_mmgd <- base_mmgd %>%
    rename(uf = sig_uf,
           potencia_instalada_k_w = mda_potencia_instalada_kw,
           qtde_u_csrecebem_os_creditos = qtd_uc_recebe_credito,
           fonte = dsc_fonte_geracao) %>%
    left_join(tabela_regiao, by = 'uf',
              multiple = "all")

  base_mmgd <- base_mmgd %>%
    mutate(potencia_mw = potencia_instalada_k_w / 1000)


    # nova coluna atbt
  base_mmgd <- base_mmgd %>%
    rename(subgrupo = dsc_sub_grupo_tarifario,
           classe = dsc_classe_consumo) %>%
    mutate(atbt = ifelse(subgrupo %in% c('B1', 'B2', 'B3', 'B4'), 'BT', 'AT'),
           local_remoto = ifelse(sig_modalidade_empreendimento %in% c('R', 'C'),
                                 'remoto',
                                 'local'))

  base_mmgd$classe <- gsub('Ilumina\u00E7\u00E3o p\u00DAblica', 'Ilum. P\u00DAb.', base_mmgd$classe)

  # divisao segmentos

  segmento <-
    read_xlsx(stringr::str_glue('{dir_dados_premissas}/segmento.xlsx'))

  base_mmgd <- left_join(base_mmgd, segmento,
                         by = c('classe', 'atbt', 'local_remoto'),
                         multiple = "all")

  base_mmgd <- base_mmgd %>%
    mutate(modalidade = case_when(
      sig_modalidade_empreendimento == 'P' ~ 'Gera\u00E7\u00E3o na pr\u00F3pria UC',
      sig_modalidade_empreendimento == 'R' ~ 'Autoconsumo remoto',
      sig_modalidade_empreendimento == 'C' ~ 'Gera\u00E7\u00E3o compartilhada',
      TRUE ~ 'Condom\u00EDnios'))

  base_mmgd <- base_mmgd %>%
    rename(
      municipio = nom_municipio
    )
  base_mmgd$qtde_u_csrecebem_os_creditos <- as.numeric(
    base_mmgd$qtde_u_csrecebem_os_creditos)

  base_mmgd_resumo <- base_mmgd %>%
    mutate(num_geradores = 1) %>%
    group_by(data_conexao = mes, ano, nome_4md, uf, subsistema, fonte_resumo,
             classe, subgrupo, modalidade, segmento,
             mini_micro, atbt, local_remoto) %>%
    summarise(qtde_u_csrecebem_os_creditos = sum(qtde_u_csrecebem_os_creditos),
              num_geradores = sum(num_geradores),
              potencia_instalada_k_w = sum(potencia_instalada_k_w),
              potencia_mw = sum(potencia_mw)) %>%
    ungroup()

  if (resumida == TRUE) {

    base_mmgd_resumo <- base_mmgd_resumo %>%
      group_by(data_conexao, ano, nome_4md, fonte_resumo, segmento) %>%
      summarise(qtde_u_csrecebem_os_creditos = sum(qtde_u_csrecebem_os_creditos),
                num_geradores = sum(num_geradores),
                potencia_instalada_k_w = sum(potencia_instalada_k_w),
                potencia_mw = sum(potencia_mw)) %>%
      ungroup()

  } else {

    base_mmgd_resumo <- base_mmgd_resumo

  }

  base_mmgd_resumo

}
