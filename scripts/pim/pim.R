
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(sidrar)

# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Coleta de dados ---------------------------------------------------------

data = get_sidra(x = 3653,
                 api = paste0("/t/8888/n1/all/v/all/p/all/c544/all/d/",
                              "v11601%201,v11602%201,v11603%201,v11604%201,",
                              "v12606%205,v12607%205")) %>%
  mutate(date = parse_date(`Mês (Código)`, format = '%Y%m'),
         name = str_remove(`Seções e atividades industriais (CNAE 2.0)`,
                           "^\\d+(\\.\\d+)*\\s+"),
         type = `Variável`,
         index = Valor,
        .keep = "none") %>% 
  as_tibble()

# Tratamento de dados -----------------------------------------------------

data %>%
  arrange(date) %>%
  mutate(var_mom = (index/lag(index,1) - 1)*100, 
         var_yoy = (index/lag(index,12) - 1)*100,
         var_qoq = (((index/lag(index,3))^4) - 1)*100,
        .by = name)
  mutate(contrib_mom = var_mom*weight/100, 
         contrib_yoy = var_yoy*weight/100,
         contrib_qoq = var_qoq*weight/100) %>% 
  mutate(mm3m = round(zoo::rollmean(var_qoq, k = 3,
                                    fill = NA, align = "right"), 2),
         mm6m = round(zoo::rollmean(var_qoq, k = 6,
                                    fill = NA, align = "right"), 2),
         mm12m = round(zoo::rollmean(var_qoq, k = 12,
                                     fill = NA, align = "right"), 2),
         .by = name) %>% 
  as_tibble() 


# Visualização de dados ---------------------------------------------------


