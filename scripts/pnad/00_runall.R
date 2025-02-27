
scripts = c(
  "01_coleta.R",
  "02_desocupacao.R",
  "03_categoria.R",
  "04_atividade.R",
  "05_rendimento.R",
  "06_massa.R"
)

for (i in scripts) {
  
  source(paste0("scripts/pnad/", i), encoding = "UTF-8")
  
}
