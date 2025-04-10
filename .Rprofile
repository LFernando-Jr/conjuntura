
# Lista de pacotes a serem carregados -------------------------------------

pacotes = c("tidyverse", 
            "magrittr",
            "seasonal",
            "tsibble",
            "sidrar",
            "fredr",
            "KFAS",
            "blsR",
            "gets")

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

# API ---------------------------------------------------------------------

fredr_set_key("b91359d656cb0ff9263d424b336241c8")

Sys.setenv(bls_api_key = "cf991eb8104f44dfa7b990525c7ca5d3")
