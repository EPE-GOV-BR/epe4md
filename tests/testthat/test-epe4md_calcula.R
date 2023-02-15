
library(readxl)

test_that("epe4md_calcula works", {
  expect_snapshot_value(
    epe4md_calcula(
      premissas_reg =
        read_xlsx(system.file("dados_premissas/2021/premissas_reg.xlsx",
                              package = "epe4md")),
      ano_base = 2021,
      sequencial = TRUE,
      filter_uf = "RJ",
      ano_max_resultado = 2032,
      altera_sistemas_existentes = TRUE,
      ano_decisao_alteracao = 2023
    ) %>%
      epe4md_sumariza_resultados(),
    style = "serialize"
  )
})


test_that("epe4md_calcula erro > 2050", {
  expect_error(
    epe4md_calcula(
      premissas_reg =
        read_xlsx(system.file("dados_premissas/2021/premissas_reg.xlsx",
                              package = "epe4md")),
      ano_base = 2021,
      sequencial = TRUE,
      filter_uf = "RJ",
      ano_max_resultado = 2051,
      altera_sistemas_existentes = TRUE,
      ano_decisao_alteracao = 2023
    )
  )
})




test_that("epe4md_calcula tipo errado", {
  expect_error(
    epe4md_calcula(
      premissas_reg =
        read_xlsx(system.file("dados_premissas/2021/premissas_reg.xlsx",
                              package = "epe4md")) %>%
        mutate(
          ano = as.character(ano)
        ),
      ano_base = 2021,
      sequencial = TRUE,
      filter_uf = "RJ",
      ano_max_resultado = 2050,
      altera_sistemas_existentes = TRUE,
      ano_decisao_alteracao = 2023
    )
  )
})




test_that("epe4md_calcula filtro_renda errado", {
  expect_error(
    epe4md_calcula(
      premissas_reg =
        read_xlsx(system.file("dados_premissas/2021/premissas_reg.xlsx",
                              package = "epe4md")),
      ano_base = 2021,
      sequencial = TRUE,
      filter_uf = "RJ",
      ano_max_resultado = 2050,
      altera_sistemas_existentes = TRUE,
      ano_decisao_alteracao = 2023,
      filtro_renda_domicilio = "nada"
    )
  )
})



test_that("epe4md_calcula filtro_renda certo", {
  expect_silent(
    epe4md_calcula(
      premissas_reg =
        read_xlsx(system.file("dados_premissas/2021/premissas_reg.xlsx",
                              package = "epe4md")),
      ano_base = 2021,
      sequencial = TRUE,
      filter_uf = "RJ",
      ano_max_resultado = 2050,
      altera_sistemas_existentes = TRUE,
      ano_decisao_alteracao = 2023,
      filtro_renda_domicilio = "maior_5sm"
    )
  )
})


