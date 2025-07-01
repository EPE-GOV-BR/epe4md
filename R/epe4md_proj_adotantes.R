#' Realiza a projeção do número de adotantes de micro e minigeração distribuída
#'
#' @param casos_otimizados data.frame. Resultado da funcao
#' [epe4md::epe4md_calibra_curva_s].
#' #' @param consumidores list. Resultado da função
#' [epe4md::epe4md_mercado_potencial].
#' @param ano_base numeric. Ano base da projeção. Define o ano em que a função
#' irá buscar a base de dados. Último ano completo realizado.
#' @param cresc_fv numeric. Taxa de crescimento da participação da fonte
#' fotovoltaica. Default igual a 0 (mantém participação histórica).
#' @param cresc_eol numeric. Taxa de crescimento da participação da fonte
#' eólica. Default igual a 0 (mantém participação histórica).
#' @param cresc_cgh numeric. Taxa de crescimento da participação da fonte
#' hidráulica. Default igual a 0 (mantém participação histórica).
#' @param cresc_ute numeric. Taxa de crescimento da participação da fonte
#' termelétrica. Default igual a 0 (mantém participação histórica).
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
                                  cresc_fv = 0,
                                  cresc_eol = 0,
                                  cresc_cgh = 0,
                                  cresc_ute = 0,
                                  dir_dados_premissas = NA_character_ ) {

  dir_dados_premissas <- if_else(
    is.na(dir_dados_premissas),
    system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                package = "epe4md"),
    dir_dados_premissas
  )

  dados_gd <-
    readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx"))

  # Como o mercado potencial estimado pode diminuir em função de aumentos do payback (no caso de uma mudança das regras de
  # compensação, por exemplo), pode resultar numa diminuição do número acumulado de adotantes.
  # Esse resultado não parece realista, pois indicaria que alguns adotantes
  # desinstalariam seus sistemas de GD.

  # Para contornar esse efeito, é calculado o número incremental de adotantes e então, se
  # esse for negativo, é atribuído o valor 0, que indica que naquele ano não haveria instalações.

  # 4a etapa do funil: Cálculo do total de adotantes anual de MMGD ou baterias ----

  # Cria dataframe com quantidade de adotantes estimados (através de otimização) para MMGD e para baterias
  projecao <- casos_otimizados %>%
    group_by(nome_4md, segmento) %>%
    arrange(ano) %>%
    mutate(adotantes_acum = mercado_potencial * Ft,
           adotantes_ano = ifelse(ano == 2013,
                                  adotantes_acum,
                                  adotantes_acum - lag(adotantes_acum)),
           adotantes_ano = ifelse(adotantes_ano < 0, # para evitar números negativos na projeção de adotantes, que levam à interpretação de desistalação de unidades geradoras
                                  0,
                                  round(adotantes_ano, 0)),
           adotantes_acum = cumsum(adotantes_ano),
           adotantes_acum_bateria = mercado_potencial_bateria * Ft_bateria,
           adotantes_ano_bateria = ifelse(ano == 2013,
                                          adotantes_acum_bateria,
                                          adotantes_acum_bateria - lag(adotantes_acum_bateria)),
           adotantes_ano_bateria = ifelse(adotantes_ano_bateria < 0, # para evitar números negativos na projeção de adotantes, que levam à interpretação de desistalação da bateria
                                          0,
                                          round(adotantes_ano_bateria, 0)),
           adotantes_acum_bateria = cumsum(adotantes_ano_bateria)) %>%
    ungroup()

  # Realiza a suavização em caso de adotantes = 0
  # É calculada uma média simples entre anos (t) em que adotantes são zero e o ano
  # subsequente (t+1). Depois ambos os anos (t e t+1) são substituídos pela média.
  projecao <- projecao %>%
    group_by(nome_4md, segmento) %>%
    arrange(ano) %>%
    mutate(adotantes_ano_media = zoo::rollmean(adotantes_ano, 2, fill = NA,
                                               align = "left"),
           adotantes_ano_media = round(ifelse(is.na(adotantes_ano_media),
                                              adotantes_ano,
                                              adotantes_ano_media), 0),
           adotantes_ano_media_bateria = zoo::rollmean(adotantes_ano_bateria, 2, fill = NA,
                                                       align = "left"),
           adotantes_ano_media_bateria = round(ifelse(is.na(adotantes_ano_media_bateria),
                                                      adotantes_ano_bateria,
                                                      adotantes_ano_media_bateria), 0)) %>%
    mutate(adotantes_ano_c = case_when(
      adotantes_ano == 0 & ano > 2019 ~ adotantes_ano_media,
      lag(adotantes_ano) == 0 & ano > 2019 ~ lag(adotantes_ano_media),
      TRUE ~ adotantes_ano),
      adotantes_acum_c = cumsum(adotantes_ano_c),
      adotantes_ano_bateria_c = case_when(
        adotantes_ano_bateria == 0 & ano > 2019 ~ adotantes_ano_media_bateria,
        lag(adotantes_ano_bateria) == 0 & ano > 2019 ~ lag(adotantes_ano_media_bateria),
        TRUE ~ adotantes_ano_bateria),
      adotantes_acum_bateria_c = cumsum(adotantes_ano_bateria_c)) %>%
    ungroup() %>%
    select(-adotantes_ano,-adotantes_ano_bateria, -adotantes_acum, -adotantes_acum_bateria
           , -adotantes_ano_media, -adotantes_ano_media_bateria) %>%
    rename(adotantes_ano = adotantes_ano_c,
           adotantes_acum = adotantes_acum_c,
           adotantes_ano_bateria = adotantes_ano_bateria_c,
           adotantes_acum_bateria = adotantes_acum_bateria_c
           )

  # Abertura dos adotantes por fonte ----------------------------------------

  taxa_crescimento <- tibble(
    fonte_resumo = c("Fotovoltaica", "Eólica", "Hidro", "Termelétrica"), # Exemplos de fontes
    crescimento_anual = c(cresc_fv, cresc_eol, cresc_cgh, cresc_ute) # Percentual de crescimento/decrescimento anual
  )

  # Cria dataframe com quantidade de adotantes total e participação percentual da fonte_resumo, considerando todo o histórico, por nome_4md e segmento
  part_adot_fontes <- dados_gd %>%
    group_by(nome_4md, segmento, fonte_resumo) %>%
    summarise(adotantes_hist = sum(qtde_u_csrecebem_os_creditos)) %>% # Atribui a quantidade de UC recebendo créditos como sendo os adotantes de MMGD
    ungroup() %>%
    group_by(nome_4md, segmento) %>%
    mutate(adotantes_hist_total = sum(adotantes_hist), # calcula a quantidade de adotantes
           part_fonte = adotantes_hist / adotantes_hist_total) %>% # calcula a quantidade de adotantes por fonte
    ungroup()

  # Completar faltantes para serem encontrados no join
  part_adot_fontes <- part_adot_fontes %>%
    complete(nome_4md, segmento, fonte_resumo) %>% # mesmo que uma certa fonte_resumo não esteja presente para um nome_4md e segmento, uma linha será criada com NA em part_fonte
    group_by(nome_4md, segmento) %>%
    mutate(faltantes = sum(is.na(part_fonte))) %>% # verifica a quantidade de fontes faltantes para cada nome_4md e segmento
    ungroup() %>%
    mutate(part_fonte = ifelse(faltantes == 4 & fonte_resumo == "Fotovoltaica", # se, para um dado nome_4md e segmento, as 4 fontes faltarem, assume que é fotovoltaica
                               1, # e que, por consequencia, a participação por fonte é de 100%
                               part_fonte),
           part_fonte = ifelse(is.na(part_fonte), 0, part_fonte)) %>% # Caso exista ao menos uma fonte com participação, as que estão sem participação recebem participação igual a zero
    select(nome_4md, segmento, fonte_resumo, part_fonte)

  # Cria dataframe com histórico de adotantes para substituir anos iniciais da projeção
  historico_adot_fontes <- dados_gd %>%
    filter(ano <= ano_base) %>%
    group_by(ano, nome_4md, segmento, fonte_resumo) %>%
    summarise(adotantes_hist = sum(qtde_u_csrecebem_os_creditos)) %>%
    ungroup() %>%
    complete(ano, nome_4md, segmento, fonte_resumo) %>%
    mutate(adotantes_hist = ifelse(is.na(adotantes_hist), 0, adotantes_hist))

  # Cria dataframe com projeção por participação percentual histórica por fonte, segmento e distribuidora
  # Adiciona 4 linhas, uma para cada fonte, para cada match das chaves.
  projecao <- left_join(projecao, part_adot_fontes,
                        by = c("nome_4md", "segmento"),
                        relationship = "many-to-many") %>%
    left_join(taxa_crescimento, by = "fonte_resumo") %>% # inclui taxa de crescimento definida para cada fonte
    group_by(nome_4md, segmento, ano) %>%
    mutate(part_fonte = ifelse(ano <= ano_base, part_fonte, # se menor ou igual ao ano base, mantém participação média dos dados históricos para cada fonte_resumo
                               part_fonte * (1 + crescimento_anual) ^ (ano - ano_base)), # se maior que o ano base, atualiza valores pela taxa de crescimento esperada para cada ano
           part_fonte = part_fonte / sum(part_fonte)) %>% # calcula a participação de cada fonte_resumo por nome_4md, segmento e ano
    ungroup() %>%
    mutate(adotantes_ano = round(adotantes_ano * part_fonte, 0), # distribui a quantidade de adotantes de cada nome_4md, segmento e ano em cada fonte_resumo
           adotantes_ano_bateria = ifelse(fonte_resumo == "Fotovoltaica", adotantes_ano_bateria, 0)) %>% # Define que somente sistemas fotovoltaicos têm baterias
    select(-crescimento_anual)

  # Inclui a quantidade histórica de adotantes por nome_4md, segmento, ano e fonte_resumo
  projecao <- left_join(projecao, historico_adot_fontes,
                        by = c("nome_4md", "segmento", "ano", "fonte_resumo"),
                        relationship = "many-to-many")

  # Define a quantidade de adotantes para cada nome_4md, segmento, ano e fonte_resumo
  # Se ano for menor ou igual ao ano base, mantém o histórico
  # Se ano maior que o ano base, considera as projeções
  projecao <- projecao %>%
    mutate(adotantes_ano = ifelse(ano <= ano_base,
                                  adotantes_hist,
                                  adotantes_ano)) %>%
    group_by(nome_4md, segmento, fonte_resumo) %>%
    arrange(ano) %>%
    mutate(adotantes_acum = cumsum(adotantes_ano),
           adotantes_acum_bateria = cumsum(adotantes_ano_bateria)) %>%
    ungroup() %>%
    # Fazer a divisão por 4 devido ao left_join(projecao, part_adot_fontes) realizado anteriormente
    # Esse join adiciona 4 linhas para cada match das chaves.
    mutate(mercado_potencial = mercado_potencial / 4,
           mercado_potencial_bateria = mercado_potencial_bateria / 4)

  # Calcula percentual de adoção frente ao número de consumidores totais

  consumidores_totais <- consumidores$consumidores_totais

  adotantes_segmento <- projecao %>%
    mutate(segmento = ifelse(segmento == "residencial_remoto", "residencial", segmento),
           segmento = ifelse(segmento == "comercial_at_remoto", "comercial_bt", segmento)) %>%
    group_by(ano, segmento) %>%
    summarise(adotantes = sum(adotantes_acum),
              mercado_potencial = sum(mercado_potencial)) %>%
    ungroup()

  mercado_nicho <- consumidores$consumidores %>%
    mutate(segmento = ifelse(segmento == "residencial_remoto", "residencial", segmento),
           segmento = ifelse(segmento == "comercial_at_remoto", "comercial_bt", segmento)) %>%
    group_by(ano, segmento) %>%
    summarise(mercado_nicho = sum(consumidores)) %>%
    ungroup()

  part_adotantes <- left_join(adotantes_segmento, consumidores_totais,
                              by = c("ano", "segmento")) %>%
    mutate(penetracao_total = adotantes / total_ucs) %>%
    left_join(mercado_nicho, by = c("ano", "segmento")) %>%
    mutate(penetracao_nicho = adotantes / mercado_nicho,
           penetracao_potencial = adotantes / mercado_potencial)

  # Cria lista de dataframes com os resultados das projeções de adotantes
  proj_adotantes <- projecao

  lista_adotantes <- list(proj_adotantes, part_adotantes)
  names(lista_adotantes) <- c("proj_adotantes", "part_adotantes")

  lista_adotantes
}
