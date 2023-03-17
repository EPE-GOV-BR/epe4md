#' Gr\u00E1fico da capacidade instalada acumulada
#'
#' Mostra a soma acumulada da pot\u00EAncia entre os par\u00E3metros `ano_inicio` e `ano_max_resultado`
#' passado na fun\u00E7\u00E3o `epe4md::epe4md_calcula`.
#'
#' @param dados data.frame. Resultados mensais de pot\u00EAncia e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013.
#'
#' @param cor string. Escolha da cor do gr\u00E1fico. Default igual a Azul Escuro.
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return Gráfico que mostra a soma acumulada das potências entre os parâmetros
#' ano_inicio e ano_max_resultado.
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' epe4md_graf_pot_acum(
#'   dados,
#'   ano_inicio = 2013,
#'   cor = "#13475d",
#'   tamanho = 14
#' )

utils::globalVariables(c("potencia_mw", "energia_mwh", "dias_mes", "dias_ano", "energia_mwmed",
                         "energia_mwh_total", "part_energia", "pot_mes_mw",
                         "pot_total", "pot_acum", "part_potencia", "pot_total_ano",
                         "pot_ano_mw", "pot_ano_total", "pot_ano", "cenario", "regiao"))

epe4md_graf_pot_acum <- function(
    dados,
    ano_inicio = 2013,
    cor = "#13475d",
    tamanho = 14)
  {

  resumo <- dados %>%
    epe4md_sumariza_resultados() %>%
    filter(ano >= ano_inicio)

  ggplot(resumo) +
    aes(x = ano, y = pot_acum) +
    geom_line(color = cor, size = 1) +
    geom_point(color = cor, fill = "white", shape = 21, stroke = 1.25) +
    labs(x = "", y = "Pot\u00EAncia [GW]") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(size = tamanho)) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))
  }

#' Gr\u00E1fico da capacidade instalada anual
#'
#' Gr\u00E1fico de colunas que mostra a soma simples da pot\u00EAncia em cada ano, entre os par??metros `ano_inicio` e
#' `ano_max_resultado` passado na fun\u00E7\u00E3o `epe4md::epe4md_calcula`.
#'
#' @param dados data.frame. Resultados mensais de pot\u00EAncia e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013.
#'
#' @param cor string. Escolha da cor do gr\u00E1fico. Default igual a Azul Escuro.
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return Gráfico de colunas que mostra a soma simples da potência em cada ano,
#' entre os parâmetros ano_inicio e ano_max_resultado passado na função epe4md_calcula.
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' epe4md_graf_pot_anual(
#'   dados,
#'   ano_inicio = 2013,
#'   cor = "#13475d",
#'   tamanho = 14
#' )

epe4md_graf_pot_anual <- function(
    dados,
    ano_inicio = 2013,
    cor = "#13475d",
    tamanho = 14)
{

  resumo <- dados %>%
    epe4md_sumariza_resultados() %>%
    filter(ano >= ano_inicio)

  ggplot(resumo) +
    aes(x = ano, y = pot_ano) +
    geom_col(fill = cor) +
    labs(x = "", y = "Pot\u00EAncia [GW]") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(size = tamanho)) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))
}

#' Gr\u00E1fico da capacidade instalada acumulada por segmento
#'
#' Mostra a soma acumulada da pot\u00EAncia entre o intervalo de anos solicitado, agrupada entre os segmentos : ' Comercial (AT) ',
#' Comercial (BT) ', ' Comercial Remoto (AT/BT) ', ' Residencial ' e ' Residencial Remoto '.
#'
#'
#' @param dados data.frame. Resultados mensais de pot\u00EAncia e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013.
#'
#' @param tamanho numeric. Altera tamanho da fonte da legenda. Default igual a 14.
#'
#' @param paleta_epe data.frame. Paleta de cores da EPE.
#'
#' @return Gráfico que mostra a soma acumulada da potência entre o intervalo de
#' anos solicitado, agrupada entre os segmentos : ' Comercial (AT) ', Comercial (BT) ',
#' ' Comercial Remoto (AT/BT) ', ' Residencial ' e ' Residencial Remoto '.
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' epe4md_graf_part_segmento(
#'   dados,
#'   ano_inicio = 2013,
#'   tamanho = 14
#' )

epe4md_graf_pot_segmento <- function(
    dados,
    ano_inicio = 2013,
    tamanho = 14)
{

  resumo <- dados %>%
    epe4md_fatores_publicacao() %>%
    group_by(ano, segmento) %>%
    summarise(pot_ano = sum(pot_mes_mw)) %>%
    ungroup() %>%
    group_by(segmento) %>%
    mutate(pot_acum = cumsum(pot_ano) / 1000) %>%
    ungroup() %>%
    filter(ano >= ano_inicio)

  ggplot(resumo) +
    aes(x = ano, y = pot_acum, color = segmento) +
    geom_line(size = 1) +
    geom_point(fill = "white", shape = 21, stroke = 1.25) +
    labs(x = "", y = "Pot\u00EAncia [GW]") +
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(size = tamanho),
          legend.title = element_blank()) +
    scale_color_manual(values = paleta_epe) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))
}

#' Gr\u00E1fico da capacidade instalada acumulada por regi\u00E3o
#'
#' @param dados data.frame. Resultados mensais de pot\u00EAncia e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013.
#'
#' @param tamanho numeric. Altera tamanho da fonte da legenda. Default igual a 14.
#'
#' @param paleta_epe data.frame. Paleta de cores da EPE.
#'
#' @return Gráfico da capacidade instalada acumulada por região.
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' epe4md_graf_pot_regiao(
#'   dados,
#'   ano_inicio = 2013,
#'   tamanho = 14
#' )


epe4md_graf_pot_regiao <- function(
    dados,
    ano_inicio = 2013,
    tamanho = 14)
{

  resumo <- dados %>%
    epe4md_fatores_publicacao() %>%
    group_by(ano, regiao) %>%
    summarise(pot_ano = sum(pot_mes_mw)) %>%
    ungroup() %>%
    group_by(regiao) %>%
    mutate(pot_acum = cumsum(pot_ano) / 1000) %>%
    ungroup() %>%
    filter(ano >= ano_inicio)

  ggplot(resumo) +
    aes(x = ano, y = pot_acum, color = regiao) +
    geom_line(size = 1) +
    geom_point(fill = "white", shape = 21, stroke = 1.25) +
    labs(x = "", y = "Pot\u00EAncia [GW]") +
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(size = tamanho),
          legend.title = element_blank()) +
    scale_color_manual(values = paleta_epe) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))
}

#' Gr\u00E1fico da participa\u00E7\u00E3o de capacidade por fonte
#'
#' Gr\u00E1fico de barras que mostra a participa\u00E7\u00E3o de cada fonte ('Fotovoltaica', 'Termel\u00E9trica', 'Hidro' e 'E\u00F3lica')
#' na capacidade instalada.
#'
#' @param dados data.frame. Resultados mensais de pot\u00EAncia e energia.
#'
#' @param cor string. Escolha da cor do gr\u00E1fico.
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return Gráfico de barras que mostra a participação de cada fonte ('Fotovoltaica',
#' 'Termelétrica', 'Hidro' e 'Eólica') na capacidade instalada.
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import forcats
#' @import scales
#' @import ggplot2
#'
#' @examples
#' epe4md_graf_part_fonte_potencia(
#'   dados,
#'   ano_inicio = 2013,
#'   cor = "#112446",
#'   tamanho = 14
#' )



epe4md_graf_part_fonte_potencia <- function(
    dados,
    cor = "#112446",
    tamanho = 14)
{

  resumo <- dados %>%
    mutate(pot_total = sum(pot_mes_mw)) %>%
    group_by(fonte_resumo) %>%
    summarise(pot_acum = sum(pot_mes_mw),
              pot_total = mean(pot_total)) %>%
    ungroup() %>%
    mutate(part_potencia = pot_acum / pot_total,
           fonte_resumo = forcats::fct_reorder(fonte_resumo, part_potencia))

  ggplot(resumo) +
    aes(x = fonte_resumo, y = part_potencia) +
    geom_col(fill = cor) +
    labs(x = "", y = "Participa\u00E7\u00E3o em Capacidade Instalada") +
    geom_text(aes(label = scales::percent(round(part_potencia, 3),
                                          decimal.mark = ",",
                                          accuracy = 0.1),
                  hjust = -0.2)) +
    scale_y_continuous(labels = scales::label_percent(),
                       breaks = scales::breaks_width(0.25)) +
    coord_flip(ylim = c(0, 1.1)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(size = 14),
          legend.title = element_blank())

}



#' Gr\u00E1fico da gera\u00E7\u00E3o mensal em MWmed
#'
#' Gr\u00E1fico de \u00E1rea que mostra a soma da gera\u00E7\u00E3o de energia em MWmed agrupada por m\u00EAs.
#'
#' @param dados data.frame. Resultados mensais de pot\u00EAncia e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013.
#'
#' @param cor string. Escolha da cor do gr\u00E1fico. Default igual a Vermelho.
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return Gráfico de área que mostra a soma da geração de energia em MWmed agrupada por mês.
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' epe4md_graf_geracao_mes(
#'    dados,
#'    ano_inicio = 2013,
#'    cor = "#953735",
#'    tamanho = 14
#' )

epe4md_graf_geracao_mes <- function(
    dados,
    ano_inicio = 2013,
    cor = "#953735",
    tamanho = 14)
{

  resumo <- dados %>%
    filter(ano >= ano_inicio) %>%
    group_by(data) %>%
    summarise(energia_mwmed = sum(energia_mwmed)) %>%
    ungroup()

  ggplot(resumo) +
    aes(x = as.Date(data), y = energia_mwmed) +
    geom_area(fill = cor) +
    labs(x = "", y = "Gera\u00E7\u00E3o [MWm\u00E9d]") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(size = tamanho)) +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","))

}

#' Gr\u00E1fico da gera\u00E7\u00E3o anual em MWmed
#'
#' Gr\u00E1fico de \u00E1rea que mostra a gera\u00E7\u00E3o de energia em MWmed agrupada por ano.
#'
#' @param dados data.frame. Resultados mensais de pot\u00EAncia e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013.
#'
#' @param cor string. Escolha da cor do gr\u00E1fico. Default igual a Vermelho.
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return Gráfico de área que mostra a soma da geração de energia em MWmed agrupada por mês.
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import scales
#' @import ggplot2
#'
#' @examples
#' epe4md_graf_geracao_ano(
#'    dados,
#'    ano_inicio = 2013,
#'    cor = "#953735",
#'    tamanho = 14
#' )

epe4md_graf_geracao_ano <- function(dados, ano_inicio = 2013,
                                    cor = "#953735", tamanho = 14) {

  resumo <- dados %>%
    filter(ano >= ano_inicio) %>%
    group_by(ano, mes) %>%
    summarise(energia_mwh = sum(energia_mwh)) %>%
    mutate(dias_mes = lubridate::days_in_month(mes)) %>%
    ungroup() %>%
    group_by(ano) %>%
    summarise(dias_ano = sum(dias_mes),
              energia_mwh = sum(energia_mwh),
              energia_mwmed = energia_mwh / (24 * dias_ano)) %>%
    ungroup()

  ggplot(resumo) +
    aes(x = ano, y = energia_mwmed) +
    geom_area(fill = cor) +
    # geom_line(color = "#7f7f7f", size = 1) +
    # geom_point(fill = "white", color = cor, shape = 21, stroke = 1.25) +
    labs(x = "", y = "Gera\u00E7\u00E3o [MWm\u00E9d]") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(size = tamanho)) +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))

}

#' Gr\u00E1fico da participa\u00E7\u00E3o de cada tecnologia na gera\u00E7\u00E3o de energia
#'
#' Gr\u00E1fico de barras que mostra a participa\u00E7\u00E3o de cada fonte ('Fotovoltaica', 'Termel\u00E9trica', 'Hidro', 'E\u00F3lica')
#' na gera\u00E7\u00E3o de energia somente no par??metro `ano_max_resultado` passado na fun\u00E7\u00E3o `epe4md::epe4md_calcula`.
#'
#' @param dados data.frame. Resultados mensais de pot\u00EAncia e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013.
#'
#' @param cor string. Escolha da cor do gr\u00E1fico. Default igual a Vermelho.
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return Gráfico de barras que mostra a participação de cada fonte ('Fotovoltaica',
#' 'Termelétrica', 'Hidro', 'Eólica') na geração de energia somente no parâmetro
#' ano_max_resultado passado na função epe4md_calcula.
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import forcats
#' @import scales
#' @import ggplot2
#'
#' @examples
#' epe4md_graf_part_fonte_geracao(
#'  dados,
#'  cor = "#953735",
#'  tamanho = 14
#' )



epe4md_graf_part_fonte_geracao <- function(
    dados,
    cor = "#953735",
    tamanho = 14)
{

  resumo <- dados %>%
    filter(ano == max(ano)) %>%
    mutate(energia_mwh_total = sum(energia_mwh)) %>%
    group_by(ano, fonte_resumo) %>%
    summarise(energia_mwh = sum(energia_mwh),
              energia_mwh_total = mean(energia_mwh_total)) %>%
    ungroup() %>%
    mutate(part_energia = energia_mwh / energia_mwh_total,
           fonte_resumo = forcats::fct_reorder(fonte_resumo, part_energia))

  ggplot(resumo) +
    aes(x = fonte_resumo, y = part_energia) +
    geom_col(fill = cor) +
    labs(x = "", y = "Participa\u00E7\u00E3o na Gera\u00E7\u00E3o") +
    geom_text(aes(label = scales::percent(round(part_energia, 3),
                                          decimal.mark = ",",
                                          accuracy = 0.1),
                  hjust = -0.2)) +
    scale_y_continuous(labels = scales::label_percent(),
                       breaks = scales::breaks_width(0.25)) +
    coord_flip(ylim = c(0, 1.1)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(size = 14),
          legend.title = element_blank())

}


#' Gr\u00E1fico da evolu\u00E7\u00E3o da participa\u00E7\u00E3o por segmento
#'
#' Gr\u00E1fico de colunas que mostra a participa\u00E7\u00E3o de cada segmento 'Comercial (AT)', 'Comercial(BT)', 'Comercial Remoto (AT/BT)', Residencial' e 'Residencial Remoto'.
#' na gera\u00E7\u00E3o de energia em cada ano.
#'
#' @param dados data.frame. Resultados mensais de pot\u00EAncia e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013.
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @param paleta_epe data.frame. Paleta de cores da EPE.
#'
#' @return Gráfico de colunas que mostra a participação de cada segmento 'Comercial (AT)',
#' 'Comercial(BT)', 'Comercial Remoto (AT/BT)', Residencial' e 'Residencial Remoto'
#' na geração de energia em cada ano.
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import scales
#' @import ggplot2
#'
#' @examples
#' epe4md_graf_part_segmento(
#'   dados,
#'   ano_inicio = 2013,
#'   tamanho = 14
#' )



epe4md_graf_part_segmento <- function(
    dados,
    ano_inicio = 2013,
    tamanho = 14)
{

  resumo <- dados %>%
    epe4md_fatores_publicacao() %>%
    filter(ano >= ano_inicio) %>%
    group_by(ano) %>%
    mutate(pot_total_ano = sum(pot_mes_mw)) %>%
    ungroup() %>%
    group_by(ano, segmento) %>%
    summarise(pot_ano_mw = sum(pot_mes_mw),
              pot_ano_total = mean(pot_total_ano)) %>%
    ungroup() %>%
    mutate(part_potencia = pot_ano_mw / pot_ano_total)


  ggplot(resumo) +
    geom_col(aes(x = ano, y = part_potencia, fill = segmento),
             position = "stack") +
    labs(x = "", y = "Participa\u00E7\u00E3o em Pot\u00EAncia") +
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(size = tamanho),
          legend.title = element_blank()) +
    scale_fill_manual(values = paleta_epe) +
    scale_y_continuous(labels = scales::label_percent()) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))

}

#' Gr\u00E1fico da capacidade instalada acumulada por cen\u00E1rio
#'
#' Mostra a soma acumulada das pot\u00EAncia entre os par??metros `ano_inicio` e `ano_max_resultado`
#' passado na fun\u00E7\u00E3o `epe4md::epe4md_calcula` em cada um dos cen\u00E1rios.
#'
#' @param dados data.frame Resultado resumido com coluna cenario.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013.
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @param paleta_epe data.frame. Paleta de cores da EPE.
#'
#' @return Gráfico que mostra a soma acumulada das potência entre os parâmetros
#' ano_inicio e ano_max_resultado passado na função epe4md_calcula em cada um dos cenários.
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import scales
#' @import ggplot2
#'
#' @examples
#' epe4md_graf_pot_cenario(
#'   dados,
#'   ano_inicio = 2013,
#'   tamanho = 14
#' )



epe4md_graf_pot_cenario <- function(
    dados,
    ano_inicio = 2013,
    tamanho = 14)
{

  ultimos <- dados %>%
    mutate(pot_acum = round(pot_acum, 1)) %>%
    group_by(cenario) %>%
    top_n(1, ano) %>%
    pull(pot_acum)

  resumo <- dados %>%
    filter(ano >= ano_inicio)

  ggplot(resumo) +
    aes(x = ano, y = pot_acum, color = cenario) +
    geom_line(size = 1) +
    geom_point(fill = "white", shape = 21, stroke = 1.25) +
    labs(x = "", y = "Pot\u00EAncia [GW]", color = "Cen\u00E1rio") +
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(size = tamanho)) +
    scale_color_manual(values = paleta_epe) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    #scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(
      sec.axis = sec_axis(~ ., breaks = ultimos, labels = scales::label_number(
      decimal.mark = ",", accuracy = 0.1
    ))) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))

}



