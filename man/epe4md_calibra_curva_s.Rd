% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epe4md_calibra_curva_s.R
\encoding{UTF-8}
\name{epe4md_calibra_curva_s}
\alias{epe4md_calibra_curva_s}
\title{Calibra o modelo de Bass com dados históricos e gera curvas S de adoção.}
\usage{
epe4md_calibra_curva_s(
  resultado_payback,
  consumidores,
  ano_base,
  ano_max_resultado = 2060,
  p_max = 0.01,
  q_max = 1,
  dir_dados_premissas = NA_character_
)
}
\arguments{
\item{resultado_payback}{data.frame. Resultado da função
\link{epe4md_payback}.}

\item{consumidores}{list. Resultado da função
\link{epe4md_mercado_potencial}.}

\item{ano_base}{numeric. Ano base da projeção. Define o ano em que a função
irá buscar a base de dados. Último ano completo realizado.}

\item{ano_max_resultado}{numeric. Ano final para apresentação dos resultados.
Máximo igual a 2060. Default igual a 2060.}

\item{p_max}{numeric. Fator de inovação (p) máximo. Default igual a 0.01.}

\item{q_max}{numeric. Fator de imitação (q) máximo. DEfault igual a 1.}

\item{dir_dados_premissas}{Diretório onde se encontram as premissas. Se esse
parâmetro não for passado, a função usa os dados default que são instalados
com o pacote. É importante que os nomes dos arquivos sejam os mesmos da
pasta default.}
}
\value{
data.frame com curvas de difusão e mercado potencial
}
\description{
Calibra o modelo de Bass com dados históricos e gera curvas S de adoção.
}
