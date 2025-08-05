#' Copia as planilhas de premissas originais do modelo para um novo diretório.
#'
#' @param ano_base numeric. Ano base das premissas que serão copiadas.
#' @param destino string. Diretório de destino das planilhas. Se não for
#' preenchido, a função cria uma pasta chamada dados_premissas na raiz do
#' projeto atual do RStudio.
#'
#' @return Cópia das planilhas de premissas.
#' @export
#'
#'@encoding UTF-8
#'
#'@examples
#' \dontrun{
#' # Copia as planilhas para uma pasta personalizada
#' epe4md_copia_premissas(ano_base = 2024)
#' }

epe4md_copia_premissas <- function(
    ano_base,
    destino = "dados_premissas"
) {

  if (file.exists(paste0(destino, "/", ano_base))) {

    } else {

      dir.create(paste0(destino, "/", ano_base), recursive = TRUE)

    }

  arquivos <- list.files(paste0(system.file(package = "epe4md"), "/dados_premissas/", ano_base))

  file.copy(from = paste0(system.file(package = "epe4md"), "/dados_premissas/", ano_base, "/", arquivos),
            to = paste0(destino, "/", ano_base, "/", arquivos), overwrite = TRUE)


}
