

proj_potencia <- function (lista_adotantes, ano_base, dir_dados_premissas = NA_character_,perc_pot_bateria = 0.35)
{
  dir_dados_premissas <- if_else(is.na(dir_dados_premissas),
                                 system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                                             package = "epe4md"), dir_dados_premissas)

  dados_gd <- readxl::read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx"))
  proj_adotantes <- lista_adotantes$proj_adotantes
  potencia_tipica <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/potencia_tipica.xlsx"))

  potencia_media <- dados_gd %>% group_by(nome_4md, segmento,
                                          fonte_resumo) %>%
    summarise(pot_total = sum(potencia_instalada_k_w),
              pot_total_bateria = sum(potencia_instalada_k_w*perc_pot_bateria),#Estimativa 35% da potência DO FV (Só bateria)
              adotantes_total = sum(qtde_u_csrecebem_os_creditos),
              pot_media = pot_total/adotantes_total,
              pot_media_bateria = pot_total_bateria/adotantes_total) %>%
    ungroup() %>%
    complete(nome_4md, segmento, fonte_resumo) %>%
    group_by(segmento, fonte_resumo) %>%
    mutate(pot_media = ifelse(is.na(pot_media), mean(pot_media, na.rm = TRUE), pot_media))   %>%
    mutate(pot_media_bateria = ifelse(is.na(pot_media_bateria), mean(pot_media_bateria, na.rm = TRUE), pot_media_bateria))   %>%
    ungroup() %>%

    select(nome_4md, segmento, fonte_resumo, pot_media,pot_media_bateria) %>%

    left_join(potencia_tipica, by = "segmento") %>%
    mutate(pot_media = ifelse(is.na(pot_media),  pot_sistemas, pot_media)) %>%
    mutate(pot_media_bateria = ifelse(is.na(pot_media_bateria),  pot_sistemas*perc_pot_bateria, pot_media_bateria)) %>%
    select(-pot_sistemas)

  proj_potencia <- left_join(proj_adotantes, potencia_media,
                             by = c("nome_4md", "segmento", "fonte_resumo"))
  historico_pot_fontes <- dados_gd %>% filter(ano <= ano_base) %>%
    group_by(ano, nome_4md, segmento, fonte_resumo) %>% summarise(pot_hist = sum(potencia_instalada_k_w)) %>%
    ungroup() %>% complete(ano, nome_4md, segmento, fonte_resumo) %>%
    mutate(pot_hist = ifelse(is.na(pot_hist), 0, pot_hist))


  proj_potencia <- proj_potencia %>%
    mutate(pot_ano = adotantes_ano * pot_media)%>%
    mutate(pot_ano_bateria = adotantes_ano_bateria * pot_media_bateria)

  proj_potencia <- left_join(proj_potencia, historico_pot_fontes,by = c("nome_4md", "segmento", "ano", "fonte_resumo"))

  proj_potencia <- proj_potencia %>%
    mutate(pot_ano = ifelse(ano <= ano_base, pot_hist, pot_ano)) %>%
    ungroup()

  proj_potencia <- proj_potencia %>%
    mutate(pot_ano_mw = pot_ano/1000) %>%
    mutate(pot_ano_bateria_mw = pot_ano_bateria/1000) %>%

    group_by(nome_4md, segmento, fonte_resumo) %>%
    arrange(ano) %>%
    mutate(pot_acum_mw = cumsum(pot_ano_mw)) %>%
    mutate(pot_acum_bateria_mw = cumsum(pot_ano_bateria_mw)) %>%
    ungroup()

  lista_potencia <- list(proj_potencia, lista_adotantes$part_adotantes)
  names(lista_potencia) <- c("proj_potencia", "part_adotantes")
  lista_potencia
}


potencia <- proj_potencia(
  lista_adotantes = adotantes,
  ano_base = 2022
)


dados_potencia <- potencia$proj_potencia
