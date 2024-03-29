


test_that("epe4md_calcula works", {
  
  library(here)
  library(readxl)
  
  expect_snapshot_value(
    epe4md_calcula(
      premissas_reg =
        read_xlsx(system.file("dados_premissas/2022/premissas_reg.xlsx",
                              package = "epe4md")),
      ano_base = 2022,
      ano_max_resultado = 2032,
      altera_sistemas_existentes = TRUE,
      ano_decisao_alteracao = 2023
    ) %>%
      epe4md_sumariza_resultados(),
    style = "serialize"
  )
})


test_that("epe4md_calcula erro > 2060", {
  
  library(here)
  library(readxl)
  
  expect_error(
    epe4md_calcula(
      premissas_reg =
        read_xlsx(system.file("dados_premissas/2022/premissas_reg.xlsx",
                              package = "epe4md")),
      ano_base = 2022,
      ano_max_resultado = 2061,
      altera_sistemas_existentes = TRUE,
      ano_decisao_alteracao = 2023
    )
  )
})




test_that("epe4md_calcula tipo errado", {
  
  library(here)
  library(readxl)
  
  expect_error(
    epe4md_calcula(
      premissas_reg =
        read_xlsx(system.file("dados_premissas/2022/premissas_reg.xlsx",
                              package = "epe4md")) %>%
        mutate(
          ano = as.character(ano)
        ),
      ano_base = 2022,
      ano_max_resultado = 2060,
      altera_sistemas_existentes = TRUE,
      ano_decisao_alteracao = 2023
    )
  )
})




test_that("epe4md_calcula filtro_renda errado", {
  
  library(here)
  library(readxl)
  
  expect_error(
    epe4md_calcula(
      premissas_reg =
        read_xlsx(system.file("dados_premissas/2022/premissas_reg.xlsx",
                              package = "epe4md")),
      ano_base = 2022,
      ano_max_resultado = 2060,
      altera_sistemas_existentes = TRUE,
      ano_decisao_alteracao = 2023,
      filtro_renda_domicilio = "nada"
    )
  )
})



test_that("epe4md_calcula filtro_renda certo", {
  
  library(here)
  library(readxl)
  
  expect_silent(
    epe4md_calcula(
      premissas_reg =
        read_xlsx(system.file("dados_premissas/2022/premissas_reg.xlsx",
                              package = "epe4md")),
      ano_base = 2022,
      ano_max_resultado = 2060,
      altera_sistemas_existentes = TRUE,
      ano_decisao_alteracao = 2023,
      filtro_renda_domicilio = "maior_5sm"
    )
  )
})


