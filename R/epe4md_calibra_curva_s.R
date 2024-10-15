


calibra_curva_s <- function (resultado_payback, consumidores, ano_base, ano_max_resultado = 2028,
          p_max = 0.01, q_max = 1, dir_dados_premissas = NA_character_)
{
  dir_dados_premissas <- if_else(is.na(dir_dados_premissas),
                                 system.file(stringr::str_glue("dados_premissas/{ano_base}"),
                                             package = "epe4md"), dir_dados_premissas)
  dados_gd <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/base_mmgd.xlsx"))
  dados_gd <- dados_gd %>% filter(ano <= ano_base)

  consumidores <- consumidores$consumidores
  consumidores2 <- consumidores$consumidores_bateria
  consumidores_bateria <- consumidores$consumidores_bateria

  casos_otimizacao <- resultado_payback %>% group_by(nome_4md,
                                                     segmento) %>% tally() %>% select(-n) %>% ungroup()
  historico <- dados_gd %>% group_by(nome_4md, segmento, ano) %>%
    summarise(adotantes_hist = sum(qtde_u_csrecebem_os_creditos)) %>%
    ungroup()

  resultado_payback_historico <- resultado_payback %>% filter(ano <= ano_base) %>%
    select(nome_4md, segmento, ano, payback,payback_desc,payback_bateria, payback_desc_bateria)#Adicionado payback bateria


  base_otimizacao <- left_join(resultado_payback_historico,
                               consumidores, by = c("nome_4md", "ano", "segmento"))
  base_otimizacao <- left_join(base_otimizacao, historico,
                               by = c("nome_4md", "ano", "segmento"))
  tipo_payback <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/tipo_payback.xlsx"))
  base_otimizacao <- left_join(base_otimizacao, tipo_payback,
                               by = "segmento")


  base_otimizacao <- base_otimizacao %>%
    mutate(payback = ifelse(tipo_payback =="simples", payback, payback_desc)) %>%

    mutate(payback_bateria = ifelse(tipo_payback == "simples", payback_bateria, payback_desc_bateria))




  base_otimizacao$adotantes_hist <- base_otimizacao$adotantes_hist %>%
    replace_na(0)
  base_otimizacao <- base_otimizacao %>% arrange(ano) %>% group_by(nome_4md,
                                                                   segmento) %>% mutate(adotantes_acum = cumsum(adotantes_hist)) %>%
    ungroup() %>% select(nome_4md, consumidores, ano, payback, payback_bateria,
                         adotantes_acum, segmento) %>% mutate(ano = ano - 2012)
  fator_sbp <- read_xlsx(stringr::str_glue("{dir_dados_premissas}/spb.xlsx"))
  base_otimizacao <- left_join(base_otimizacao, fator_sbp,
                               by = "segmento")
  base_otimizacao <- base_otimizacao %>% rename(nome_dist = nome_4md,
                                                nome_seg = segmento)
  otimiza_casos <- function(nome_4md, segmento) {
    caso <- base_otimizacao %>% filter(nome_dist == nome_4md,
                                       nome_seg == segmento)
    otimiza_curva_s <- function(params, y) {
      p <- params[1]
      q <- params[2]
      spb <- y$spb
      consumidores <- y$consumidores
      # consumidores_bateria <- y$consumidores_bateria
      t <- y$ano
      payback <- y$payback
      payback_bateria <- y$payback_bateria
      historico_adotantes <- y$adotantes_acum
      taxa_difusao <- (1 - exp(-(p + q) * t))/(1 + (q/p) *
                                                 exp(-(p + q) * t))


      mercado_potencial <- exp(-spb * payback) * consumidores
      mercado_potencial_bateria <- exp(-spb * payback_bateria) * consumidores_bateria #adicionar consumidores com outros filtros aqui

      projecao <- taxa_difusao * mercado_potencial
      projecao_bateria <- taxa_difusao * mercado_potencial_bateria

      erro <- sum((historico_adotantes - projecao)^2)
      return(erro)
    }
    otimizador <- stats::optim(c(0.005, 0.3), otimiza_curva_s,
                               y = caso, method = "L-BFGS-B", lower = c(1e-04, 0.01),
                               upper = c(p_max, q_max), control = list(parscale = c(0.005,
                                                                                    0.3)))
    parametros <- tibble::enframe(otimizador$par)
    parametros <- pivot_wider(parametros, names_from = name,
                              values_from = value) %>% rename(p = 1, q = 2)
  }
  casos_otimizados <- casos_otimizacao %>% mutate(saida = pmap(.l = list(nome_4md,
                                                                         segmento), .f = otimiza_casos)) %>% unnest(saida)


  casos_otimizados <- casos_otimizados %>% left_join(fator_sbp,
                                                     by = "segmento")
  anos_simulacao <- tibble::tibble(ano = seq(1, 38))
  casos_otimizados <- crossing(casos_otimizados, anos_simulacao)

  casos_otimizados <- casos_otimizados %>% mutate(Ft = (1 - exp(-(p + q) * ano))/(1 + (q/p) * exp(-(p + q) * ano)))


  casos_otimizados <- casos_otimizados %>% mutate(ano = ano + 2012) %>%
    left_join(consumidores, by = c("nome_4md", "segmento", "ano"))


  resultado_payback <- resultado_payback %>%
    left_join(tipo_payback,  by = "segmento") %>%
    select(nome_4md, segmento, ano, payback, payback_desc, payback_bateria, payback_desc_bateria, tipo_payback) %>%
    mutate(payback = ifelse(tipo_payback ==  "simples", payback, payback_desc)) %>%

    mutate(payback_bateria = ifelse(tipo_payback == "simples",
                                    payback_bateria,
                                    payback_desc_bateria)) %>%
    select(-payback_desc, -payback_desc_bateria)



  casos_otimizados <- left_join(casos_otimizados, resultado_payback,
                                by = c("nome_4md", "segmento", "ano"))


  casos_otimizados <- casos_otimizados %>%
    mutate(mercado_potencial = round(exp(-spb *payback) * consumidores, 0),
           mercado_potencial = ifelse(mercado_potencial == 0, 1, mercado_potencial)) %>%


  mutate(mercado_potencial_bateria = round(exp(-spb * payback_bateria) * consumidores_bateria, 0),
         mercado_potencial_bateria = ifelse(mercado_potencial_bateria == 0,
                                            1,

                                                                                  mercado_potencial_bateria)) #Mercado_potencial final bateria

}

#RODAR SCRIPTS

mercado_calibrado_fv_e_bateria <- calibra_curva_s(
  resultado_payback = payback_fv_e_fv_bateria,
  consumidores = mercado_fv_bateria,
  ano_base = 2022,
  ano_max_resultado = 2060,
  p_max = 0.01,
  q_max = 1
)


