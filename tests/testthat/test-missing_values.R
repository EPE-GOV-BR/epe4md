
library(tidyr)
suppressWarnings(library(dplyr))
library(magrittr)

test_that("there is no missing values", {
  expect_equal(sum(is.na(epe4md_casos_payback(ano_base = 2021,
                                              ano_max_resultado = 2032))), 0)

  expect_equal(sum(is.na(epe4md_mercado_potencial(ano_base = 2021))), 0)

  expect_equal(epe4md_mercado_potencial(ano_base = 2021) %>%
                 magrittr::extract2(1) %>%
                 distinct(nome_4md) %>%
                 nrow(), 54)

  expect_equal(epe4md_casos_payback(ano_base = 2021,
                                    ano_max_resultado = 2032) %>%
                 distinct(nome_4md) %>%
                 nrow(), 54)
})
