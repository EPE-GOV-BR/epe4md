#' Cria a base do mercado potencial inicial para a adoção.
#'
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param filtro_renda_domicilio string. Define o filtro aplicado a consumidores
#' residenciais, de acordo com a renda mensal do responsável, em salários
#' mínimos. Permite: "total", "maior_1sm, maior_2sm", "maior_3sm" ou
#' "maior_5sm". Default igual a "maior_3sm".
#' @param filtro_renda_bateria string. Define o filtro aplicado a consumidores
#' residenciais para investimento em baterias, de acordo com a renda mensal do
#' responsável, em salários mínimos. Permite: "total", "maior_1sm, maior_2sm",
#' "maior_3sm", "maior_5sm" ou "maior_10sm". Default igual a "maior_10sm".
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
#' @param tx_cresc_grupo_a numeric. Taxa de crescimento anual dos consumidores
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
                                     filtro_renda_bateria = "maior_10sm",
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

  # 1a etapa do funil: Cálculo do total de unidades consumidoras com e sem MMGD ou baterias ----


  # Total de domicílios no Brasil + projeção futura
  total_domicilios <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/total_domicilios.xlsx"))
  taxa_crescimento_a <- tx_cresc_grupo_a

  # Taxa de crescimento de mercado para o cenário analizado
  crescimento_mercado <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/crescimento_mercado.xlsx"))

  # Calcula a taxa de crescimento acumulada
  crescimento_mercado <- crescimento_mercado %>%
    mutate(crescimento_acumulado = cumprod(1 + taxa_crescimento_mercado)) %>%
    select(-taxa_crescimento_mercado)

  anos_faltantes_res <- data.frame("ano" = 2010:2060)

  # Faz o agrupamento da quantidade de consumidores residenciais por faixa de renda
  consumidores_residenciais <- read_xlsx(
    stringr::str_glue("{dir_dados_premissas}/consumidores_residenciais_renda.xlsx")) %>%
    mutate(maior_10sm = domicilios_10a15sm + domicilios_15a20sm + domicilios_maior20sm,
           maior_5sm = maior_10sm + domicilios_5a10sm,
           maior_3sm = maior_5sm + domicilios_3a5sm,
           maior_2sm = maior_3sm + domicilios_2a3sm,
           maior_1sm = maior_2sm + domicilios_1a2sm,
           total = domicilios_pp) %>%
    select(nome_4md, starts_with("maior")) %>%
    pivot_longer(cols = starts_with("maior"), names_to = "renda",
                 values_to = "domicilios") %>%
    mutate(ano = 2010)

  # Cria uma tabela dimensão com faixas de renda por distribuidora
  lista_consumidores_residenciais <- consumidores_residenciais %>%
    group_by(nome_4md, renda) %>%
    tally() %>%
    select(-n) %>%
    ungroup()

  # Adiciona uma coluna de anos à tabela dimensão de faixas de renda por distribuidora
  lista_consumidores_residenciais <- crossing(lista_consumidores_residenciais,
                                              anos_faltantes_res)

  # Cria uma tabela com a quantidade de domicílios
  consumidores_residenciais <- left_join(lista_consumidores_residenciais,
                                         consumidores_residenciais,
                                         by = c("nome_4md", "ano", "renda"))

  # Adiciona a taxa de crescimento a ser usada para atualizar a quantidade de domicilios dos anos sem dados
  consumidores_residenciais <- left_join(consumidores_residenciais,
                                         crescimento_mercado, by = "ano")

  consumidores_residenciais <- consumidores_residenciais %>%
    group_by(nome_4md, renda) %>%
    arrange(ano) %>%
    fill(domicilios) %>% # repete a quantidade de domicílios para cada distribuidora e renda para os anos sem dados
    ungroup() %>%
    mutate(consumidores_proj = round(domicilios * crescimento_acumulado, 0)) %>% # atualiza o valor para os anos que estavam vazios antes do fill() com a taxa de crescimento acumulada esperada para aquele ano a partir do ano inicial
    select(nome_4md, ano, renda, consumidores_proj) %>%
    filter(ano > 2012)


  # consumidores b2 e b3

  # Cria dataframe com qunatidade de consumidores B2 e B3 por distribuidora e por ano
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

  # Identifica o último (maior) ano disponível no dataframe
  ano_ultimo_comercial <- max(consumidores_b2b3$ano)

  # Cria um dataframe com a quantidade de consumidores B2 e B3 por ano e distribuidora
  # Mantém os registros para os anos que não possuem consumidores (aqueles maiores que "ano_ultimo_comercial")
  lista_consumidores_b2b3 <- consumidores_b2b3 %>%
    distinct(nome_4md)

  lista_consumidores_b2b3 <- crossing(lista_consumidores_b2b3,
                                      anos_faltantes_res)

  consumidores_b2b3 <- left_join(lista_consumidores_b2b3, consumidores_b2b3,
                                 by = c("nome_4md", "ano")) %>%
    filter(ano > 2012)

  # Calcula a taxa de crescimento acumulada para cada ano a partir do ano seguinte ao "ano_ultimo_comercial"
  crescimento_mercado <- read_xlsx(stringr::str_glue(
    "{dir_dados_premissas}/crescimento_mercado.xlsx")) %>%
    filter(ano > ano_ultimo_comercial) %>%
    mutate(crescimento_acumulado = cumprod(1 + taxa_crescimento_mercado)) %>%
    select(-taxa_crescimento_mercado)

  # Inclui a taxa de crescimento no dataframe de consumidores B3 e B3
  consumidores_b2b3 <- left_join(consumidores_b2b3, crescimento_mercado,
                                 by = "ano")

  consumidores_b2b3 <- consumidores_b2b3 %>%
    group_by(nome_4md) %>%
    arrange(ano) %>%
    fill(consumidores) %>% # Repete a quantidade de consumidores, por distribuidora, do último ano com dado disponível
    ungroup() %>%
    mutate(consumidores_proj = ifelse(is.na(crescimento_acumulado),
                                      consumidores,
                                      round(consumidores *
                                              crescimento_acumulado, 0))) %>% # atualiza a quantidade de consumidores
    select(nome_4md, ano, consumidores_proj)


  # consumidores a

  # Cria dataframe com a quantidade de consumidores do grupo A por distribuidora e Ano
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

  # Atualiza o dataframe para alocar a quantidade de consumidores para cada ano em cada distribuidora
  # Para os anos em que não há dado, mantém nulo
  consumidores_a <- left_join(lista_consumidores_b2b3, consumidores_a,
                              by = c("nome_4md", "ano")) %>%
    filter(ano > 2012)

  # Inclui a taxa de crescimento do grupo A a ser utilizado nos anos em que não há dado de quantidade de consumidores
    consumidores_a <- consumidores_a %>%
    mutate(crescimento = ifelse(is.na(consumidores),
                                1 + taxa_crescimento_a,
                                1)) %>%
    group_by(nome_4md) %>%
    arrange(ano) %>%
    mutate(taxa_acumulada = cumprod(crescimento)) %>% # Calcula a taxa acumulada de crescimento por distribuidora para a sequência de anos
    fill(consumidores) %>% # Repete a quantidade de consumidores do último ano com dados por distribuidora
    mutate(consumidores_proj = round(consumidores * taxa_acumulada, 0)) %>% # atualiza a quantidade de consumidores pela taxa acumulada (que só começa a ser acumuada a partir do ano seguinte ao último disponível com dados)
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

  # Esse é o fim da primeira etapa do funil para projetar o número de adotantes: "consumidores_totais" é o total de unidades consumidoras com e sem MMGD ou baterias
  consumidores_totais <- bind_rows(consumidores_totais_domicilios,
                                   consumidores_totais_b2b3) %>%
    bind_rows(consumidores_totais_a)


  # 2a etapa do funil: Cálculo do mercado nicho ----

  # 2.1 Define o Fator de Aptidão Local (FAL) para consumidores residenciais, nomeado de fator_tecnico
  fator_tecnico <-
    read_xlsx(stringr::str_glue("{dir_dados_premissas}/fator_tecnico.xlsx")) %>%
      select(nome_4md, fator_tecnico)

  # 2.2 Calcula o fator técnico (FAL) para unidades comerciais e industriais, nomeado fator_tecnico_comercial

  # Na ausência de base detalhada das unidades comerciais, foi utilizado o mesmo fator residencial (FAL ou fator_tecnico) para consumidores atendidos em BT.
  # Se fator_local_comercial == "historico", fator_tecnico_comercial será substituído pelo FAL calculado a partir do histórico de consumidores comerciais
  fator_tecnico_comercial <- fator_tecnico

  # Caso selecionado "historico", utiliza o histórico do percentual de adotantes locais por distribuidora até o ano base para calcular o fator de aptidão local para consumidores comerciais
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

  # 2.3 Define o mercado nicho de consumidores residenciais com ou sem baterias

  # Define o mercado nicho de consumidores residenciais sem bateria
  consumidores_residenciais_sem_bat <- consumidores_residenciais %>%
    filter(renda == filtro_renda_domicilio)

  #  Define o mercado nicho de de consumidores residenciais com bateria
  consumidores_residenciais_com_bat <- consumidores_residenciais %>%
    filter(renda == filtro_renda_bateria)

  # os filtros de renda para definição de consumidores com e sem baterias não têm objetivo de ser mutuamente exclusivos

  # 3a etapa do funil: Cálculo do mercado potencial inicial ----

  # 3.1 Calcula o Fator Comercial a ser aplicado nos mercados comercial BT local e comercial BT/AT remoto

  # Por não possui base de dados detalhada por renda,
  # é feita uma aproximação do cálculo a partir da base de dados de consumidores residenciais.
  # se o parâmetro filtro_comercial é repassado, fator_comercial e fator_comercial_bat == filtro_comercial

  # 3.2 Calcula o fator_comercial para consumidores comerciais sem baterias
  fator_comercial <- consumidores_residenciais_sem_bat %>%
    group_by(ano) %>%
    summarise(consumidores_nicho = sum(consumidores_proj)) %>%
    left_join(total_domicilios, by = "ano") %>%
    mutate(fator_nicho_comercial = consumidores_nicho / domicilios) %>%
    ungroup() %>%
    select(ano, fator_nicho_comercial)

  # 3.3 Calcula o fator_comercial para consumidores comerciais com baterias
  fator_comercial_bat <- consumidores_residenciais_com_bat %>%
    group_by(ano) %>%
    summarise(consumidores_nicho = sum(consumidores_proj)) %>%
    left_join(total_domicilios, by = "ano") %>%
    mutate(fator_nicho_comercial = consumidores_nicho / domicilios) %>%
    ungroup() %>%
    select(ano, fator_nicho_comercial)

  if (!is.na(filtro_comercial)) {
   fator_comercial <- fator_comercial %>%
     mutate(fator_nicho_comercial = filtro_comercial)

   fator_comercial_bat <- fator_comercial %>%
     mutate(fator_nicho_comercial = filtro_comercial)

  }

  # 3.4 Calcula o mercado potencial para consumidores residenciais sem baterias
  consumidores_residenciais_sem_bat <- left_join(consumidores_residenciais_sem_bat,
                                         fator_tecnico, by = "nome_4md")

  consumidores_residenciais_sem_bat <- consumidores_residenciais_sem_bat %>%
    mutate(residencial = round(consumidores_proj * fator_tecnico, 0), # Para o segmento "Residencial (Local)” foi aplicado o FAL sobre o mercado nicho para calcular o mercado potencial
           residencial_remoto = round(consumidores_proj *
                                        (1 - fator_tecnico), 0)) %>%
    select(nome_4md, ano, residencial, residencial_remoto) %>%
    pivot_longer(cols = starts_with("residen"), names_to = "segmento",
                 values_to = "consumidores")

  # 3.5 Calcula o mercado potencial para consumidores comerciais BT local e AT remoto sem baterias
  consumidores_b2b3_sem_bat <- left_join(consumidores_b2b3, fator_comercial,
                                 by = "ano")

  consumidores_b2b3_sem_bat <- left_join(consumidores_b2b3_sem_bat, fator_tecnico_comercial,
                                 by = "nome_4md")


  consumidores_b2b3_sem_bat <- consumidores_b2b3_sem_bat %>%
    mutate(comercial_bt = round(consumidores_proj * fator_tecnico *
                                  fator_nicho_comercial, 0),
           comercial_at_remoto = round(consumidores_proj *
                                         fator_nicho_comercial *
                                         (1 - fator_tecnico), 0)) %>%
    select(nome_4md, ano, comercial_bt, comercial_at_remoto) %>%
    pivot_longer(cols = starts_with("comerc"), names_to = "segmento",
                 values_to = "consumidores")

  # 3.6 Define o mercado potencial para comercial AT local sem baterias (que não aplica FAL e fator comercial)
  consumidores_a <- consumidores_a %>%
    mutate(segmento = "comercial_at") %>%
    rename(consumidores = "consumidores_proj")

  # 3.7 Consolida todo o mercado nicho sem baterias
  consumidores_sem_bat <- bind_rows(consumidores_residenciais_sem_bat, consumidores_b2b3_sem_bat,
                                    consumidores_a)



  # 3.8 Calcula o mercado potencial para consumidores residenciais com baterias
  consumidores_residenciais_com_bat <- left_join(consumidores_residenciais_com_bat,
                                                 fator_tecnico, by = "nome_4md")

  consumidores_residenciais_com_bat <- consumidores_residenciais_com_bat %>%
    mutate(residencial = round(consumidores_proj * fator_tecnico, 0),
           residencial_remoto = round(consumidores_proj *
                                        (1 - fator_tecnico), 0)) %>%
    select(nome_4md, ano, residencial, residencial_remoto) %>%
    pivot_longer(cols = starts_with("residen"), names_to = "segmento",
                 values_to = "consumidores")

  # 3.9 Calcula o mercado potencial para consumidores comerciais BT local e AT remoto com baterias
  consumidores_b2b3_bat <- left_join(consumidores_b2b3, fator_comercial_bat,
                                 by = "ano")

  consumidores_b2b3_bat <- left_join(consumidores_b2b3_bat, fator_tecnico_comercial,
                                 by = "nome_4md")

  consumidores_b2b3_bat <- consumidores_b2b3_bat %>%
    mutate(comercial_bt = round(consumidores_proj * fator_tecnico *
                                  fator_nicho_comercial, 0),
           comercial_at_remoto = round(consumidores_proj *
                                         fator_nicho_comercial *
                                         (1 - fator_tecnico), 0)) %>%
    select(nome_4md, ano, comercial_bt, comercial_at_remoto) %>%
    pivot_longer(cols = starts_with("comerc"), names_to = "segmento",
                 values_to = "consumidores")


  # 3.10 Define o mercado potencial para comercial AT local com baterias (que não aplica FAL e fator comercial)
  consumidores_com_bat <- bind_rows(consumidores_residenciais_com_bat, consumidores_b2b3_bat,
                                    consumidores_a)

  # 3.11 Consolida todo o mercado potencial inicial com e sem baterias
    mercado <- consumidores_sem_bat %>%
      left_join(consumidores_com_bat %>% select(nome_4md, ano, segmento, consumidores),
                by = c("nome_4md", "ano", "segmento")) %>%
      rename(consumidores_bateria = consumidores.y,
             consumidores = consumidores.x)

  mercado <- mercado %>%
    mutate(consumidores_bateria = ifelse(segmento == "comercial_at_remoto", 0, consumidores_bateria))

  lista <- list(consumidores = mercado, consumidores_totais = consumidores_totais)
}
