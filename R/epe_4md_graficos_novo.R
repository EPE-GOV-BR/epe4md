library(ggplot2)

#
grafico_mercado_potencial <- function(dados, tamanho_texto = 14, ano_inicio = 2011, ano_fim = 2045, intervalo_anos = 5) {

  grafico_mercado_potencial <- ggplot(dados, aes(x = ano)) +
    geom_smooth(aes(y = (mercado_potencial)/1000, color = "FV"), method = "loess", se = FALSE, size = 1) +
    geom_smooth(aes(y = (mercado_potencial_bateria)/1000, color = "FV com armazenamento"), method = "loess", se = FALSE, size = 1) +
    labs(x = "Ano", y = "Mercado Potencial", color = "Legenda") +
    theme_minimal() +
    theme(text = element_text(size = tamanho_texto)) +
    scale_color_manual(values = c("FV" = "#13475d", "FV com armazenamento" = "#953735")) +
    scale_x_continuous(breaks = seq(ano_inicio, ano_fim, by = intervalo_anos))

  # Exibir o gráfico
  print(grafico_mercado_potencial)
}




graf_pot_acum <- function (dados, ano_inicio = 2013, cor1 = "#13475d", cor2 = "#2ca02c", tamanho = 14) {
  resumo <- dados %>%
    sumariza_resultados() %>%
    filter(ano >= ano_inicio)

  grafico_potencia_acumulada <- ggplot(resumo) +
    aes(x = ano) +
    geom_line(aes(y = pot_acum, color = "FV"), size = 1) +
    geom_point(aes(y = pot_acum, color = "FV"), fill = "white", shape = 21, stroke = 1.25) +
    geom_line(aes(y = pot_acum_bateria, color = "Armazenamento"), size = 1) +
    geom_point(aes(y = pot_acum_bateria, color = "Armazenamento"), fill = "white", shape = 21, stroke = 1.25) +
    labs(x = "", y = "Potência [GW]", color = "Legenda") +
    theme_minimal() +
    theme(legend.position = "right", text = element_text(size = tamanho)) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 4)) +
    scale_color_manual(values = c("FV" = cor1, "Armazenamento" = cor2))

  # Exibir o gráfico
  print(grafico_potencia_acumulada )
}


# Função para plotar o gráfico
graf_potencia_anual <- function(dados, ano_inicio = 2013, cor1 = "#13475d", cor2 = "#953735", tamanho = 14) {
  # Resumir e filtrar os dados
  resumo <- dados %>%
    sumariza_resultados() %>%
    filter(ano >= ano_inicio)

  # Verifique as colunas no resumo
  print(colnames(resumo))

  # Criar o gráfico
  grafico_potencia_anual <- ggplot(resumo) +
    aes(x = ano) +
    geom_col(aes(y = pot_ano), fill = cor1) +
    geom_col(aes(y = pot_ano_bateria), fill = cor2, alpha = 0.7) +
    labs(x = "", y = "Potência [GW]") +
    theme_minimal() +
    theme(legend.position = "none", text = element_text(size = tamanho)) +
    scale_x_continuous(breaks = seq(ano_inicio, max(resumo$ano), by = 10))

  # Exibir o gráfico
  print(grafico_potencia_anual)

}



graf_pot_acum_segmento <- function(dados, ano_inicio = 2013, sistema = "fv", tamanho = 14) {

  # Resumir e filtrar os dados
  resumo <- dados %>%
    epe4md_fatores_publicacao() %>%
    group_by(ano, segmento) %>%
    summarise(
      pot_ano = sum(pot_mes_mw),
      pot_ano_bateria = sum(pot_mes_bateria_mw)
    ) %>%
    ungroup() %>%
    group_by(segmento) %>%
    mutate(
      pot_acum = cumsum(pot_ano) / 1000,
      pot_acum_bateria = cumsum(pot_ano_bateria) / 1000
    ) %>%
    ungroup() %>%
    filter(ano >= ano_inicio)

  # Seleção de plotagem
  if (sistema == "fv") {
    grafico_pot_acumulada_segmento <- ggplot(resumo) +
      aes(x = ano, color = segmento) +
      geom_line(aes(y = pot_acum), size = 1) +
      geom_point(aes(y = pot_acum), fill = "white", shape = 21, stroke = 1.25) +
      labs(x = "", y = "Potência  FV [GW]") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        text = element_text(size = tamanho),
        legend.title = element_blank()
      ) +
      scale_color_manual(values = paleta_epe) +
      guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
      scale_x_continuous(breaks = seq(ano_inicio, max(resumo$ano), by = 6))
  } else if (sistema == "bateria") {
    grafico_pot_acumulada_segmento <- ggplot(resumo) +
      aes(x = ano, color = segmento) +
      geom_line(aes(y = pot_acum_bateria), linetype = "dashed", size = 1) +
      geom_point(aes(y = pot_acum_bateria), fill = "grey", shape = 21, stroke = 1.25) +
      labs(x = "", y = "Potência armaz [GW]") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        text = element_text(size = tamanho),
        legend.title = element_blank()
      ) +
      scale_color_manual(values = paleta_epe) +
      guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
      scale_x_continuous(breaks = seq(ano_inicio, max(resumo$ano), by = 6))
  } else {
    stop("Opção inválida para o argumento 'sistema'. Use 'fv' ou 'bateria'.")
  }

  # Exibir o gráfico
  print(grafico_pot_acumulada_segmento)

}

graf_geracao_ano <-function (dados, ano_inicio = 2013, cor_fv = "#953735", cor_arm = "#228B22", tamanho = 14)
{
  resumo <- dados %>% filter(ano >= ano_inicio) %>% group_by(ano,
                                                             mes) %>% summarise(energia_mwh = sum(energia_mwh)) %>%
    mutate(dias_mes = lubridate::days_in_month(mes)) %>%
    ungroup() %>% group_by(ano) %>% summarise(dias_ano = sum(dias_mes),
                                              energia_mwh = sum(energia_mwh), energia_mwmed = energia_mwh/(24 *
                                                                                                             dias_ano)) %>% ungroup()

  # Assumindo que você tem dados separados para FV e Armazenamento, você deve criar duas séries de dados separadas:
  resumo_fv <- resumo %>% filter(tipo == "FV")
  resumo_arm <- resumo %>% filter(tipo == "Armazenamento")

  # Criar o gráfico
  grafico_geracao_ano <- ggplot() +
    geom_area(data = resumo_fv, aes(x = ano, y = energia_mwmed), fill = cor_fv) +
    geom_area(data = resumo_arm, aes(x = ano, y = energia_mwmed), fill = cor_arm) +
    labs(x = "", y = "Geração [MWméd]") +
    theme_minimal() +
    theme(legend.position = "none", text = element_text(size = tamanho)) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    scale_x_continuous(breaks = seq(ano_inicio,last(resumo$ano), by = 2)) +
    # Adiciona legendas manuais
    annotate("text", x = max(resumo$ano)-10, y = max(resumo_fv$energia_mwmed), label = "FV", color = cor_fv, size = 5, hjust = 1) +
    annotate("text", x = max(resumo$ano)-10, y = max(resumo_arm$energia_mwmed), label = "Armazenamento", color = cor_arm, size = 5, hjust = 1)

  # Exibir o gráfico
  print(grafico_geracao_ano)
}

graf_geracao_ano <- function(dados, ano_inicio = 2013, cor_fv = "#953735", cor_bateria = "#2ca02c", tamanho = 14) {

  # Resumir e filtrar os dados
  resumo <- dados %>%
    filter(ano >= ano_inicio) %>%
    group_by(ano, mes) %>%
    summarise(
      energia_mwh = sum(energia_mwh),
      energia_bateria_mwh = sum(energia_bateria_mwh)  # Considera também a geração das baterias
    ) %>%
    mutate(dias_mes = lubridate::days_in_month(mes)) %>%
    ungroup() %>%
    group_by(ano) %>%
    summarise(
      dias_ano = sum(dias_mes),
      energia_mwh = sum(energia_mwh),
      energia_bateria_mwh = sum(energia_bateria_mwh),
      energia_mwmed = energia_mwh / (24 * dias_ano),
      energia_bateria_mwmed = energia_bateria_mwh / (24 * dias_ano)
    ) %>%
    ungroup()

  # Criar o gráfico
  ggplot(resumo) +
    aes(x = ano) +
    geom_area(aes(y = energia_mwmed), fill = cor_fv, alpha = 0.5) +  # Área para geração FV
    geom_area(aes(y = energia_bateria_mwmed), fill = cor_bateria, alpha = 0.5) +  # Área para geração das baterias
    labs(x = "", y = "Geração [MWméd]") +
    theme_minimal() +
    theme(
      legend.position = "none",
      text = element_text(size = tamanho)
    ) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    scale_x_continuous(breaks = seq(ano_inicio, max(resumo$ano), by = 5))
}

graf_pot_regiao <- function (dados, ano_inicio = 2013, tamanho = 14)
{
  resumo <- dados %>% epe4md_fatores_publicacao() %>% group_by(ano,
                                                               regiao) %>% summarise(pot_ano = sum(pot_mes_mw)) %>%
    ungroup() %>% group_by(regiao) %>% mutate(pot_acum = cumsum(pot_ano)/1000) %>%
    ungroup() %>% filter(ano >= ano_inicio)
  ggplot(resumo) + aes(x = ano, y = pot_acum, color = regiao) +
    geom_line(size = 1) + geom_point(fill = "white", shape = 21,
                                     stroke = 1.25) + labs(x = "", y = "Potência armaz [GW]") +
    theme_minimal() + theme(legend.position = "bottom", text = element_text(size = tamanho),
                            legend.title = element_blank()) + scale_color_manual(values = paleta_epe) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano),
                                    by = 2))
}

graf_pot_regiao <- function(dados, ano_inicio = 2013, sistema = 'fv', tamanho = 14) {

  # Resumir e filtrar os dados com base na escolha do sistema
  resumo <- dados %>%
    epe4md_fatores_publicacao() %>%
    group_by(ano, regiao) %>%
    summarise(
      pot_ano = sum(if (sistema == 'fv') pot_mes_mw else 0),
      pot_ano_bateria = sum(if (sistema == 'bateria') pot_mes_bateria_mw else 0)
    ) %>%
    ungroup() %>%
    group_by(regiao) %>%
    mutate(
      pot_acum = cumsum(pot_ano) / 1000,
      pot_acum_bateria = cumsum(pot_ano_bateria) / 1000
    ) %>%
    ungroup() %>%
    filter(ano >= ano_inicio)

  # Criar o gráfico
  ggplot(resumo) +
    aes(x = ano, color = regiao) +
    geom_line(aes(y = if (sistema == 'fv') pot_acum else pot_acum_bateria), size = 1) +
    geom_point(aes(y = if (sistema == 'fv') pot_acum else pot_acum_bateria), fill = "white", shape = 21, stroke = 1.25) +

    labs(x = "", y = "Potência armaz [GW]") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      text = element_text(size = tamanho),
      legend.title = element_blank()
    ) +
    scale_color_manual(values = paleta_epe) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(ano_inicio, max(resumo$ano), by = 5))
}


graf_adotantes_acumulados <- function(dados, ano_inicio = 2013, tamanho = 14) {
  # Verificar se as colunas existem
  if (!"adotantes_acum" %in% colnames(dados) || !"adotantes_acum_bateria" %in% colnames(dados)) {
    stop("As colunas adotantes_acum ou adotantes_acum_bateria não existem no dataframe.")
  }

  # Resumir os dados
  resumo <- dados %>%
    filter(ano >= ano_inicio) %>%
    group_by(ano) %>%
    summarise(
      adotantes_acum = sum(adotantes_acum, na.rm = TRUE),
      adotantes_acum_bateria = sum(adotantes_acum_bateria, na.rm = TRUE)
    ) %>%
    ungroup()

  # Verificar o resumo antes de plotar
  print(resumo)

  # Criar o gráfico
  grafico_adotantes_acumulados <- ggplot(resumo) +
    aes(x = ano) +
    geom_line(aes(y = (adotantes_acum)/1000000, color = "FV"), size = 1) +
    geom_line(aes(y = (adotantes_acum_bateria)/1000000, color = "FV com armazenamento"), size = 1, linetype = "dashed") +
    geom_point(aes(y = (adotantes_acum)/1000000, color = "FV"), shape = 21, fill = "white", stroke = 1.25) +
    geom_point(aes(y = (adotantes_acum_bateria)/1000000, color = "FV com armazenamento"), shape = 21, fill = "grey", stroke = 1.25) +
    labs(x = "Ano", y = "Número de adotantes [milhões]") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      text = element_text(size = tamanho),
      legend.title = element_blank()
    ) +
    scale_color_manual(values = c("FV" = "#13475d", "FV com armazenamento" = "#953735")) +
    scale_x_continuous(breaks = seq(ano_inicio, max(resumo$ano), by = 5))
  # print(adotantes_acum, n = Inf, width = Inf)
  print(as.data.frame(adotantes))
    print(grafico_adotantes_acumulados)
}




graf_energia_acum_bateria <- function(dados, ano_inicio = 2013, cor = "#953735", tamanho = 14) {
  # Resumir e filtrar os dados
  resumo <- dados %>%
    filter(ano >= ano_inicio) %>%
    group_by(ano) %>%
    reframe(
      energia_bateria_kwh_acum = sum(energia_bateria_kwh, na.rm = TRUE)   # Converter para MWh para manter consistência
    )

  # Criar o gráfico
  grafico_energia_acumulada <- ggplot(resumo) +
    aes(x = ano, y = energia_bateria_kwh_acum) +
    geom_line(color = cor, size = 1) +
    geom_point(color = cor, fill = "white", shape = 21, stroke = 1.25) +
    labs(x = "Ano", y = "Armazenamento [MWh]") +
    theme_minimal() +
    theme(
      legend.position = "none",
      text = element_text(size = tamanho)
    ) +
    scale_x_continuous(breaks = seq(ano_inicio, max(resumo$ano), by = 5)) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","))

  # Exibir o gráfico
  print(grafico_energia_acumulada)
}

# Exemplo de uso da função



graf_energia_acum_regiao <- function(dados, ano_inicio = 2013, cor = "#953735", tamanho = 14) {
  # Resumir e filtrar os dados
  resumo <- dados %>%
    filter(ano >= ano_inicio) %>%
    group_by(ano, regiao) %>%
    summarise(energia_bateria_kwh_acum = sum(energia_bateria_kwh, na.rm = TRUE) ) %>%
    ungroup()

  # Criar o gráfico
  grafico_energia_acumulada_regiao <- ggplot(resumo) +
    aes(x = ano, y = energia_bateria_kwh_acum, color = regiao) +
    geom_line(size = 1) +
    geom_point(fill = "white", shape = 21, stroke = 1.25) +
    labs(x = "Ano", y = "Armazenamento [MWh]") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      text = element_text(size = tamanho),
      legend.title = element_blank()
    ) +
    scale_color_manual(values = paleta_epe) +  # Assumindo que paleta_epe está definida
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(ano_inicio, max(resumo$ano), by = 5)) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","))

  # Exibir o gráfico
  print(grafico_energia_acumulada_regiao)
}

# Exemplo de uso da função




graf_energia_acum_segmento <- function(dados, ano_inicio = 2013, cor = "#953735", tamanho = 14) {
  # Resumir e filtrar os dados
  resumo <- dados %>%
    filter(ano >= ano_inicio) %>%
    group_by(ano, segmento) %>%
    summarise(energia_bateria_kwh_acum = sum(energia_bateria_kwh, na.rm = TRUE) ) %>%
    ungroup()

  # Criar o gráfico
  grafico_energia_acumulada_segmento <- ggplot(resumo) +
    aes(x = ano, y = energia_bateria_kwh_acum, color = segmento) +
    geom_line(size = 1) +
    geom_point(fill = "white", shape = 21, stroke = 1.25) +
    labs(x = "Ano", y = "Armazenamento [MWh]") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      text = element_text(size = tamanho),
      legend.title = element_blank()
    ) +
    scale_color_manual(values = paleta_epe) +  # Assumindo que paleta_epe está definida
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(ano_inicio, max(resumo$ano), by = 5)) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","))

  # Exibir o gráfico
  print(grafico_energia_acumulada_segmento)
}

# Exemplo de uso da função

grafico_mercado_potencial(mercado_calibrado_fv_e_bateria)
graf_adotantes_acumulados(dados_adotantes)
graf_pot_acum(geracao)
graf_potencia_anual(geracao)
graf_pot_acum_segmento(geracao, sistema ='bateria')
graf_geracao_ano(geracao)
graf_pot_regiao(geracao,sistema ='bateria')
graf_energia_acum_regiao(geracao)
graf_energia_acum_segmento(geracao)
graf_energia_acum_bateria(geracao)


