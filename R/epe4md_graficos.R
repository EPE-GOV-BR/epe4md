#' Gráfico da capacidade instalada acumulada
#'
#' @param dados data.frame. Resultados mensais de potência e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013
#'
#' @param cor string. Escolha da cor do gráfico. Default igual a Azul Escuro
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples

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
    labs(x = "", y = "Potência [GW]") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(size = tamanho, family = "Calibri Light")) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))
  }

#' Gráfico da capacidade instalada anual
#'
#'
#' @param dados data.frame. Resultados mensais de potência e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013
#'
#' @param cor string. Escolha da cor do gráfico. Default igual a Azul Escuro
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples



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
    labs(x = "", y = "Potência [GW]") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(size = tamanho, family = "Calibri Light")) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))
}

#' Gráfico da capacidade instalada acumulada por segmento
#'
#'
#' @param dados data.frame. Resultados mensais de potência e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013
#'
#' @param tamanho numeric. Altera tamanho da fonte da legenda. Default igual a 14.
#'
#' @return
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples

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
    labs(x = "", y = "Potência [GW]") +
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(size = tamanho, family = "Calibri Light"),
          legend.title = element_blank()) +
    scale_color_manual(values = paleta_epe) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))
}

#' Gráfico da capacidade instalada acumulada por região
#'
#' @param dados data.frame. Resultados mensais de potência e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013
#'
#' @param tamanho numeric. Altera tamanho da fonte da legenda. Default igual a 14.
#'
#' @return
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples


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
    labs(x = "", y = "Potência [GW]") +
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(size = tamanho, family = "Calibri Light"),
          legend.title = element_blank()) +
    scale_color_manual(values = paleta_epe) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))
}

#' Gráfico da participação de capacidade por fonte
#'
#' @param dados data.frame. Resultados mensais de potência e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013
#'
#' @param cor string. Escolha da cor do gráfico.
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#' @import forcats
#' @import scales
#'
#' @examples



epe4md_graf_part_fonte_potencia <- function(
    dados,
    ano_inicio = 2013,
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
    labs(x = "", y = "Participação em Capacidade Instalada") +
    geom_text(aes(label = scales::percent(round(part_potencia, 3),
                                          decimal.mark = ",",
                                          accuracy = 0.1),
                  hjust = -0.2)) +
    scale_y_continuous(labels = scales::label_percent(),
                       breaks = scales::breaks_width(0.25)) +
    coord_flip(ylim = c(0, 1.1)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(size = 14, family = "Calibri Light"),
          legend.title = element_blank())

}



#' Gráfico da geração mensal em MWmed
#'
#' @param dados data.frame. Resultados mensais de potência e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013
#'
#' @param cor string. Escolha da cor do gráfico. Default igual a Vermelho.
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#'
#' @examples



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
    labs(x = "", y = "Geração [MWméd]") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(size = tamanho, family = "Calibri Light")) +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","))

}

#' Gráfico da geração anual em MWmed
#'
#' @param dados data.frame. Resultados mensais de potência e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013
#'
#' @param cor string. Escolha da cor do gráfico. Default igual a Vermelho.
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#'
#' @examples



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
    labs(x = "", y = "Geração [MWméd]") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(size = tamanho, family = "Calibri Light")) +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))

}

#' Gráfico da participação de cada tecnologia na geração de energia
#'
#' @param dados data.frame. Resultados mensais de potência e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013
#'
#' @param cor string. Escolha da cor do gráfico. Default igual a Vermelho.
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#' @import forcats
#' @import scales
#'
#' @examples



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
    labs(x = "", y = "Participação na Geração") +
    geom_text(aes(label = scales::percent(round(part_energia, 3),
                                          decimal.mark = ",",
                                          accuracy = 0.1),
                  hjust = -0.2)) +
    scale_y_continuous(labels = scales::label_percent(),
                       breaks = scales::breaks_width(0.25)) +
    coord_flip(ylim = c(0, 1.1)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(size = 14, family = "Calibri Light"),
          legend.title = element_blank())

}


#' Gráfico da evolução da participação por segmento
#'
#' @param dados data.frame. Resultados mensais de potência e energia.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#'
#' @examples



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
    labs(x = "", y = "Participação em Potência") +
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(size = tamanho, family = "Calibri Light"),
          legend.title = element_blank()) +
    scale_fill_manual(values = paleta_epe) +
    scale_y_continuous(labels = scales::label_percent()) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))

}

#' Gráfico da capacidade instalada acumulada por cenário
#'
#' @param dados data.frame Resultado resumido com coluna cenario.
#'
#' @param ano_inicio numeric. Define o ano em que se inicia o eixo x. Default igual a 2013.
#'
#' @param tamanho numeric. Altera o tamanho da fonte da legenda. Default igual a 14.
#'
#' @return
#'
#' @export
#'
#' @encoding UTF-8
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#'
#' @examples



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
    labs(x = "", y = "Potência [GW]", color = "Cenário") +
    theme_minimal() +
    theme(legend.position = "bottom",
          text = element_text(size = tamanho, family = "Calibri Light")) +
    scale_color_manual(values = paleta_epe) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    #scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(
      sec.axis = sec_axis(~ ., breaks = ultimos, labels = scales::label_number(
      decimal.mark = ",", accuracy = 0.1
    ))) +
    scale_x_continuous(breaks = seq(ano_inicio, last(resumo$ano), by = 2))

}
