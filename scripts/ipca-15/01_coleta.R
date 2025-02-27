
# Setup -------------------------------------------------------------------

rm(list = ls())

# aux
aux_classificacao = read_csv2(file = paste0(getwd(),
                                            "/dados/classificacao.csv"),
                              show_col_types = FALSE)

aux_hist_7062 = read_csv2(file = paste0(getwd(),
                                        "/dados/Tabela 7062.csv"),
                          show_col_types = FALSE)

# Coleta de dados ---------------------------------------------------------

data_raw = get_sidra(x         = 7062, 
                     variable  = c(355,357), 
                     classific = "all",
                     period    = "202401-202512", 
                     geo       = "Brazil")

data_raw = rbind(aux_hist_7062, data_raw) %>% as_tibble()

data_raw %<>% 
  mutate(date  = parse_date(`Mês (Código)`, format = '%Y%m'),
         cod   = as.integer(`Geral, grupo, subgrupo, item e subitem (Código)`),
         name  = `Geral, grupo, subgrupo, item e subitem`,
         value = Valor,
         var   = case_when(`Variável (Código)` == 355 ~"var",
                           `Variável (Código)` == 357 ~"weight"),
         .keep  = "none") %>%
  pivot_wider() %>% 
  rename_with(~ make.unique(str_remove_all(., "\\d+\\."), sep = ".")) %>%
  pivot_longer(cols = -c(1:3), 
               values_drop_na = TRUE) %>% 
  pivot_wider(id_cols = c(date, cod, name),
              names_from = var,
              values_from = value)

data_raw %<>% left_join(aux_classificacao, by = "cod") 

save(data_raw, file = "dados/ipca-15.RData")
