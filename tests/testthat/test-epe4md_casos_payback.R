


test_that("epe4md_casos_payback works", {

    resultado <- epe4md_casos_payback(
        ano_base = 2021,
        ano_max_resultado = 2032,
        inflacao = 0.0375
      ) %>%
      dplyr::filter(
        ano %in% c(min(ano), max(ano))
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = where(is.character),
          .fns = as.factor
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = where(is.numeric),
          .fns = ~as.integer(.x * 1000)
        )
      )

  resultado <- expect_snapshot_value(
      resultado,
      style = "serialize"
  )

})
