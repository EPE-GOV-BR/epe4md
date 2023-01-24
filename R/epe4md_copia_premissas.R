#' Copia as planilhas de premissas originais do modelo para um novo diretório
#' chamado dados_premissas, na raiz do projeto atual.
#'
#' @param ano_base numeric. Ano base das premissas que serão copiadas.
#'
#' @return data.frame. Base tratada e resumida.
#' @export
#'
#'@encoding UTF-8
#'
#'@examples
#'

epe4md_copia_premissas <- function(
    ano_base
) {
  
  if (file.exists(paste0("dados_premissas/", ano_base))) {
    
    } else {
    
  dir.create(paste0("dados_premissas/", ano_base), recursive = TRUE)
      
    }
  
  arquivos <- list.files(paste0(system.file(package = "epe4md"), "/dados_premissas/", ano_base)) 
  
  file.copy(from = paste0(system.file(package = "epe4md"), "/dados_premissas/", ano_base, "/", arquivos),   
            to = paste0("dados_premissas/", ano_base, "/", arquivos), overwrite = TRUE)
  

}
