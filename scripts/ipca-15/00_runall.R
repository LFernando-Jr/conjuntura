
scripts = c(
  "01_coleta.R",
  "02_grupos.R",
  "03_aberturas_bc.R",
  "04_alimentos.R",
  "05_servicos.R",
  "06_servicos_rti.R",
  "07_industriais.R",
  "08_tradables.R"
  )

for (i in scripts) {
  
  source(paste0("scripts/ipca-15/", i), encoding = "UTF-8")
  
}
