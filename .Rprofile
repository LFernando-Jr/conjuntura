
# Lista de pacotes a serem carregados -------------------------------------

pacotes = c("tidyverse", 
            "magrittr",
            "seasonal",
            "sidrar")

# Carregar cada pacote ----------------------------------------------------

lapply(pacotes, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
})

# Mensagem ----------------------------------------------------------------

message("Pacotes carregados: ", paste(pacotes, collapse = ", "))

rm(pacotes)

# Encoding ----------------------------------------------------------------

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")
