#' Cria a base do mercado potencial inicial para a adoção.
#'
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param filtro_renda_domicilio string. Define o filtro aplicado a consumidores
#' residenciais, de acordo com a renda mensal do responsável, em salários
#' mínimos. Permite: "total", "maior_1sm, maior_2sm", "maior_3sm" ou
#' "maior_5sm". Default igual a "maior_3sm".
#' @param filtro_comercial numeric. Fator percentual para definir o nicho do
#' segmento comercial. Default é calculado pelo modelo com base no nicho
#' residencial.
#' @param fator_local_comercial string. Define a origem dos dados do Fator de 
#' Aptidão Local "FAL" para os consumidores não residenciais atendidos em baixa
#' tensão. Como default, são utilizados os mesmos valores dos consumidores
#' residenciais. Caso selecionado "historico", utiliza o histórico do percentual
#' de adotantes locais por distribuidora até o ano base.
#' @param dir_dados_premissas Diretório onde se encontram as premissas. Se esse
#' parâmetro não for passado, a função usa os dados default que são instalados
#' com o pacote. É importante que os nomes dos arquivos sejam os mesmos da pasta
#' default.
#' @param tx_cresc_grupo_a numeric. Taxa de crescimento anual dos consumuidores
#' cativos do Grupo A. Default igual a 0.
#'
#' @return list com dois data.frames. "consumidores" possui o mercado
#' potencial incial. "consumidores_totais" possui dados de mercado total.
#' @export
#'
#'@import tidyr
#'@import purrr
#'@import readxl
#'@import dplyr
#'
#'@encoding UTF-8
#'
#' @examples

epe4md_mercado_potencial <- function(ano_base,
                                     filtro_renda_domicilio = "maior_3sm",
                                     filtro_comercial = NA,
                                     fator_local_comercial = "residencial",
                                     tx_cresc_grupo_a = 0,
                                     dir_dados_premissas = NA_character_
                                     ) {

  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )

  #teste

  
  total_domicilios <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/total_domicilios.xlsx"))
  taxa_crescimento_a <- tx_cresc_grupo_a

  #residencial
  crescimento_mercado <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/crescimento_mercado.xlsx"))

  crescimento_mercado <- crescimento_mercado %>%
    mutate(crescimento_acumulado = cumprod(1 + taxa_crescimento_mercado)) %>%
    select(-taxa_crescimento_mercado)

  anos_faltantes_res <- data.frame("ano" = 2010:2060)

  consumidores_residenciais <- read_xlsx(
    stringr::str_glue("{dir_dados_premissas}/consumidores_residenciais_renda.xlsx")) %>%
    mutate(maior_5sm = domicilios_5a10sm +
             domicilios_10a15sm +
             domicilios_15a20sm +
             domicilios_maior20sm,
           maior_3sm = maior_5sm + domicilios_3a5sm,
           maior_2sm = maior_3sm + domicilios_2a3sm,
           maior_1sm = maior_2sm + domicilios_1a2sm,
           total = domicilios_pp) %>%
    select(nome_4md, starts_with("maior")) %>%
    pivot_longer(cols = starts_with("maior"), names_to = "renda",
                 values_to = "domicilios") %>%
    mutate(ano = 2010)

  lista_consumidores_residenciais <- consumidores_residenciais %>%
    group_by(nome_4md, renda) %>%
    tally() %>%
    select(-n) %>%
    ungroup()

  lista_consumidores_residenciais <- crossing(lista_consumidores_residenciais,
                                              anos_faltantes_res)

  consumidores_residenciais <- left_join(lista_consumidores_residenciais,
                                         consumidores_residenciais,
                                         by = c("nome_4md", "ano", "renda"))

  consumidores_residenciais <- left_join(consumidores_residenciais,
                                         crescimento_mercado, by = "ano")

  consumidores_residenciais <- consumidores_residenciais %>%
    group_by(nome_4md, renda) %>%
    arrange(ano) %>%
    fill(domicilios) %>%
    ungroup() %>%
    mutate(consumidores_proj = round(domicilios * crescimento_acumulado, 0)) %>%
    select(nome_4md, ano, renda, consumidores_proj) %>%
    filter(ano > 2012)


  # consumidores b2 e b3

  consumidores_b2b3 <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/consumidores_b2b3.xlsx")) %>%
    pivot_longer(cols = starts_with("20"), names_to = "ano",
                 values_to = "consumidores") %>%
    group_by(nome_4md, ano) %>%
    summarise(consumidores = sum(consumidores)) %>%
    ungroup() %>% 
    mutate(ano = as.numeric(ano)) %>% 
    filter(between(ano, 2013, ano_base))

  consumidores_b2b3$ano <- as.numeric(consumidores_b2b3$ano)

  ano_ultimo_comercial <- max(consumidores_b2b3$ano)

  lista_consumidores_b2b3 <- consumidores_b2b3 %>%
    distinct(nome_4md)

  lista_consumidores_b2b3 <- crossing(lista_consumidores_b2b3,
                                      anos_faltantes_res)

  consumidores_b2b3 <- left_join(lista_consumidores_b2b3, consumidores_b2b3,
                                 by = c("nome_4md", "ano")) %>%
    filter(ano > 2012)

  crescimento_mercado <- read_xlsx(stringr::str_glue(
    "{dir_dados_premissas}/crescimento_mercado.xlsx")) %>%
    filter(ano > ano_ultimo_comercial) %>%
    mutate(crescimento_acumulado = cumprod(1 + taxa_crescimento_mercado)) %>%
    select(-taxa_crescimento_mercado)

  consumidores_b2b3 <- left_join(consumidores_b2b3, crescimento_mercado,
                                 by = "ano")

  consumidores_b2b3 <- consumidores_b2b3 %>%
    group_by(nome_4md) %>%
    arrange(ano) %>%
    fill(consumidores) %>%
    ungroup() %>%
    mutate(consumidores_proj = ifelse(is.na(crescimento_acumulado),
                                      consumidores,
                                      round(consumidores *
                                              crescimento_acumulado, 0))) %>%
    select(nome_4md, ano, consumidores_proj)


  # consumidores a

  consumidores_a <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/consumidores_a.xlsx")) %>%
    pivot_longer(cols = starts_with("20"), names_to = "ano",
                 values_to = "consumidores") %>%
    group_by(nome_4md, ano) %>%
    summarise(consumidores = sum(consumidores)) %>%
    ungroup() %>% 
    mutate(ano = as.numeric(ano)) %>% 
    filter(between(ano, 2013, ano_base))

  consumidores_a$ano <- as.numeric(consumidores_a$ano)

  consumidores_a <- left_join(lista_consumidores_b2b3, consumidores_a,
                              by = c("nome_4md", "ano")) %>%
    filter(ano > 2012)


    consumidores_a <- consumidores_a %>%
    mutate(crescimento = ifelse(is.na(consumidores),
                                1 + taxa_crescimento_a,
                                1)) %>%
    group_by(nome_4md) %>%
    arrange(ano) %>%
    mutate(taxa_acumulada = cumprod(crescimento)) %>%
    fill(consumidores) %>%
    mutate(consumidores_proj = round(consumidores * taxa_acumulada, 0)) %>%
    ungroup() %>%
    select(nome_4md, ano, consumidores_proj)


 #consumidores totais para avaliacao de share posterior

  consumidores_totais_domicilios <- total_domicilios %>%
    mutate(segmento = "residencial") %>%
    rename(total_ucs = domicilios)

  consumidores_totais_b2b3 <- consumidores_b2b3 %>%
    group_by(ano) %>%
    summarise(total_ucs = sum(consumidores_proj)) %>%
    mutate(segmento = "comercial_bt")

  consumidores_totais_a <- consumidores_a %>%
    group_by(ano) %>%
    summarise(total_ucs = sum(consumidores_proj)) %>%
    mutate(segmento = "comercial_at")

  consumidores_totais <- bind_rows(consumidores_totais_domicilios,
                                   consumidores_totais_b2b3) %>%
    bind_rows(consumidores_totais_a)


  # Calculo mercado nicho ---------------------------------------------------

  fator_tecnico <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/fator_tecnico.xlsx")) %>%
      select(nome_4md, fator_tecnico)
  
  fator_tecnico_comercial <- fator_tecnico
  
  if(fator_local_comercial == "historico") {
  
  dados_gd <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx"))
  
  fator_tecnico_comercial <- dados_gd %>%
    filter(ano <= ano_base,
           segmento %in% c("comercial_bt", "comercial_at_remoto")) %>% 
    group_by(nome_4md, local_remoto) %>%
    summarise(qtde_clientes = sum(qtde_u_csrecebem_os_creditos)) %>% 
    ungroup() %>% 
    group_by(nome_4md) %>% 
    mutate(total_clientes = sum(qtde_clientes)) %>% 
    ungroup() %>% 
    filter(local_remoto == "local") %>% 
    mutate(fator_tecnico = qtde_clientes / total_clientes) 

  } 
  
  consumidores_residenciais <- consumidores_residenciais %>%
    filter(renda == filtro_renda_domicilio)

  fator_comercial <- consumidores_residenciais %>%
    group_by(ano) %>%
    summarise(consumidores_nicho = sum(consumidores_proj)) %>%
    left_join(total_domicilios, by = "ano") %>%
    mutate(fator_nicho_comercial = consumidores_nicho / domicilios) %>%
    ungroup() %>%
    select(ano, fator_nicho_comercial)

  if (!is.na(filtro_comercial)) {
   fator_comercial <- fator_comercial %>%
     mutate(fator_nicho_comercial = filtro_comercial)

  }

  consumidores_residenciais <- left_join(consumidores_residenciais,
                                         fator_tecnico, by = "nome_4md")

  consumidores_residenciais <- consumidores_residenciais %>%
    mutate(residencial = round(consumidores_proj * fator_tecnico, 0),
           residencial_remoto = round(consumidores_proj *
                                        (1 - fator_tecnico), 0)) %>%
    select(nome_4md, ano, residencial, residencial_remoto) %>%
    pivot_longer(cols = starts_with("residen"), names_to = "segmento",
                 values_to = "consumidores")


  consumidores_b2b3 <- left_join(consumidores_b2b3, fator_comercial,
                                 by = "ano")

  consumidores_b2b3 <- left_join(consumidores_b2b3, fator_tecnico_comercial,
                                 by = "nome_4md")


  consumidores_b2b3 <- consumidores_b2b3 %>%
    mutate(comercial_bt = round(consumidores_proj * fator_tecnico *
                                  fator_nicho_comercial, 0),
           comercial_at_remoto = round(consumidores_proj *
                                         fator_nicho_comercial *
                                         (1 - fator_tecnico), 0)) %>%
    select(nome_4md, ano, comercial_bt, comercial_at_remoto) %>%
    pivot_longer(cols = starts_with("comerc"), names_to = "segmento",
                 values_to = "consumidores")

  consumidores_a <- consumidores_a %>%
    mutate(segmento = "comercial_at") %>%
    rename(consumidores = "consumidores_proj")

  consumidores <- bind_rows(consumidores_residenciais, consumidores_b2b3,
                            consumidores_a)

  lista_consumidores <- list(consumidores, consumidores_totais)

  names(lista_consumidores) <- c("consumidores", "consumidores_totais")

  lista_consumidores

}
