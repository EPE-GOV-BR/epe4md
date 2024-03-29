% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epe4md_proj_mensal.R
\encoding{UTF-8}
\name{epe4md_proj_mensal}
\alias{epe4md_proj_mensal}
\title{Faz a abertura mensal da projeção de potência}
\usage{
epe4md_proj_mensal(
  lista_potencia,
  ano_base,
  ano_max_resultado = 2060,
  ajuste_ano_corrente = FALSE,
  ultimo_mes_ajuste = NA,
  metodo_ajuste = NA,
  dir_dados_premissas = NA_character_
)
}
\arguments{
\item{lista_potencia}{list. Resultado da função
\link{epe4md_proj_potencia}.}

\item{ano_base}{numeric. Ano base da projeção. Define o ano em que a função
irá buscar a base de dados. Último ano completo realizado.}

\item{ano_max_resultado}{numeric. Ano final para apresentação dos resultados.
Máximo igual a 2060. Default igual a 2060.}

\item{ajuste_ano_corrente}{logic. Se TRUE indica que a projeção deverá
incorporar o histórico mensal recente, verificado em parte do primeiro ano
após o ano base. Default igual a FALSE. O arquivo base_mmgd.xlsx deve
incorporar esse histórico.}

\item{ultimo_mes_ajuste}{numeric. Último mês com dados completos na
base_ano_corrente. Default igual a NA. Só tem efeito caso ajuste_ano_corrente
seja igual a TRUE.}

\item{metodo_ajuste}{string. Se igual a "extrapola" o modelo irá extrapolar a
potência e o número de adotantes até o final do ano base + 1 com base no
verificado até o ultimo_mes_ajuste. Se igual a "substitui", o modelo
substitui a projeção até o ultimo_mes_ajuste e mantém o restante do ano com a
projeção normal. Só tem efeito caso ajuste_ano_corrente seja igual a TRUE.}

\item{dir_dados_premissas}{Diretório onde se encontram as premissas.
Se esse parâmetro não for passado, a função usa os dados default que são
instalados com o pacote. É importante que os nomes dos arquivos sejam os
mesmos da pasta default.}
}
\value{
data.frame com os resultados da projeção de capacidade instalada
de micro e minigeração distribuída e número de adotantes mensal
}
\description{
Faz a abertura mensal da projeção de potência
}
