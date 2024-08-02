#' Prepara a base de dados da ANEEL com geradores existentes para ser utilizada
#' nas funções seguintes.
#'
#' @param base_aneel. Dataframe com a base de dados disponibilizada pela ANEEL
#' na sua página de dados abertos com dados individualizados dos micro e
#' minigeradores distribuídos.
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a
#' função irá buscar a base de dados. Último ano completo realizado.
#' @param resumida. Logic. Se TRUE, retorna a base resumida (Default). Se
#' FALSE retorna base com mais desagregações.
#' @param dir_dados_premissas Diretório onde se encontram as premissas. Se esse
#' parâmetro não for passado, a função usa os dados default que são instalados
#' com o pacote. É importante que os nomes dos arquivos sejam os mesmos da
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


epe4md_prepara_base <- function(base_aneel,
                                ano_base,
                                resumida = TRUE,
                                dir_dados_premissas = NA_character_

) {

  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )

  base_mmgd <- base_aneel %>%
    janitor::clean_names()

  base_mmgd <- base_mmgd %>%
    drop_na(sig_agente, cod_municipio_ibge, sig_modalidade_empreendimento,
            sig_tipo_consumidor, sig_tipo_geracao, dth_atualiza_cadastral_empreend,
            dsc_porte, dsc_sub_grupo_tarifario, dsc_classe_consumo, sig_uf,
            mda_potencia_instalada_kw,
            qtd_uc_recebe_credito,
            dsc_fonte_geracao)

  base_mmgd <- base_mmgd %>%
    mutate(fonte_resumo = case_when(
      sig_tipo_geracao == "UFV" ~ "Fotovoltaica",
      sig_tipo_geracao == "UTE" ~ "Termelétrica",
      sig_tipo_geracao == "EOL" ~ "Eólica",
      TRUE ~ "Hidro"
    ))

  base_mmgd$dth_atualiza_cadastral_empreend <- ymd(
    base_mmgd$dth_atualiza_cadastral_empreend)

  base_mmgd <- base_mmgd %>%
    mutate(data_conexao = ifelse(dth_atualiza_cadastral_empreend <
                                   dmy("01-01-2013"),
                                 dmy("01-01-2013"),
                                 dth_atualiza_cadastral_empreend))

  base_mmgd$data_conexao <- as_date(base_mmgd$data_conexao)


  base_mmgd <- base_mmgd %>%
    mutate(ano = year(data_conexao)) %>%
    select(-dth_atualiza_cadastral_empreend)

  base_mmgd$mes <- as.Date(cut(base_mmgd$data_conexao, breaks = "month"))

  base_mmgd <- base_mmgd %>%
    arrange(data_conexao)

  base_mmgd <- base_mmgd %>%
    mutate(mini_micro = ifelse(dsc_porte == "Microgeracao",
                               "MicroGD",
                               "MiniGD"))

  nomes_dist <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/nomes_dist_powerbi.xlsx")) %>%
    janitor::clean_names()

  base_mmgd <- base_mmgd %>%
    left_join(nomes_dist, by = "sig_agente")
  
  #conferir se o join encontrou relação de nome para todas as distribuidoras
  
  if (sum(is.na(base_mmgd$nome_4md)) > 0) {
    
    faltantes <- base_mmgd %>%
      filter(is.na(nome_4md)) %>%
      distinct(nome_4md, sig_agente) %>%
      pull(sig_agente)
    
    mensagem_erro <- paste("Existem valores NA na coluna nome_4md. Atualize a planilha nomes_dist_powerbi.xlsx com", paste(faltantes,collapse = ", "))
    
    stop(mensagem_erro)
  }

  tabela_regiao <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/tabela_dist_subs.xlsx")) %>%
    janitor::clean_names() %>%
    select(uf, subsistema) %>%
    distinct()

  base_mmgd <- base_mmgd %>%
    rename(uf = sig_uf,
           potencia_instalada_k_w = mda_potencia_instalada_kw,
           qtde_u_csrecebem_os_creditos = qtd_uc_recebe_credito,
           fonte = dsc_fonte_geracao) %>%
    left_join(tabela_regiao, by = "uf")

  base_mmgd <- base_mmgd %>%
    mutate(potencia_mw = potencia_instalada_k_w / 1000)


    # nova coluna atbt
  base_mmgd <- base_mmgd %>%
    rename(subgrupo = dsc_sub_grupo_tarifario,
           classe = dsc_classe_consumo) %>%
    mutate(atbt = ifelse(subgrupo %in% c("B1", "B2", "B3", "B4"), "BT", "AT"),
           local_remoto = ifelse(sig_modalidade_empreendimento %in% c("R", "C"),
                                 "remoto",
                                 "local"))

  base_mmgd$classe <- gsub("Iluminação pública", "Ilum. Púb.", base_mmgd$classe)

  # divisao segmentos

  segmento <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/segmento.xlsx"))

  base_mmgd <- left_join(base_mmgd, segmento,
                         by = c("classe", "atbt", "local_remoto"))

  base_mmgd <- base_mmgd %>%
    mutate(modalidade = case_when(
      sig_modalidade_empreendimento == "P" ~ "Geração na própria UC",
      sig_modalidade_empreendimento == "R" ~ "Autoconsumo remoto",
      sig_modalidade_empreendimento == "C" ~ "Geração compartilhada",
      TRUE ~ "Condomínios"))

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
      group_by(data_conexao, ano, nome_4md, fonte_resumo, segmento, local_remoto) %>%
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
