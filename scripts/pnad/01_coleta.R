
# Setup -------------------------------------------------------------------

rm(list = ls())

# Coleta de dados ---------------------------------------------------------

aux_data = get_sidra(x   = 6022,
                     api = "/t/6022/n1/all/v/606/p/all") %>%
  mutate(date  = parse_date(`Trimestre Móvel (Código)`, format = "%Y%m"),
         name  = "População",
         value = Valor,
        .keep  = "none") %>%
  as_tibble()

data = get_sidra(x   = 6318,
                 api = "/t/6318/n1/all/v/1641/p/all/c629/all") %>%
  mutate(
    date  = parse_date(`Trimestre Móvel (Código)`, format = "%Y%m"),
    name  = `Condição em relação à força de trabalho e condição de ocupação`,
    value = Valor,
  .keep  = "none"
  ) %>%
  as_tibble()

data_raw = data %>% rbind(.,
                          aux_data)

save(data_raw, file = "dados/pnad.RData")

categoria = get_sidra(x   = 6320,
                      api = "/t/6320/n1/all/v/4090/p/all/c11913/allxt") %>%
  mutate(
    date  = parse_date(`Trimestre Móvel (Código)`, format = "%Y%m"),
    name  = `Posição na ocupação e categoria do emprego no trabalho principal`,
    value = Valor,
   .keep  = "none") %>%
  pivot_wider() %>%
  rename("Empregado s/ Carteira" = paste0("Empregado no setor privado, ", 
                                          "exclusive trabalhador doméstico - ",
                                          "com carteira de trabalho assinada"),
         "Empregado c/ Carteira" = paste0("Empregado no setor privado, ",
                                          "exclusive trabalhador doméstico - ",
                                          "sem carteira de trabalho ",
                                          "assinada")) %>%
  pivot_longer(-1) %>% 
  as_tibble()

save(categoria, file = "dados/pnad_categoria.RData")

atividade = get_sidra(x   = 6323,
                       api = "/t/6323/n1/all/v/4090/p/all/C888/allxt") %>%
  mutate(date  = parse_date(`Trimestre Móvel (Código)`, format = "%Y%m"),
         name  = `Grupamento de atividade no trabalho principal`,
         value = Valor,
        .keep  = "none") %>% 
  as_tibble()

save(atividade, file = "dados/pnad_atividade.RData")

rendimento = get_sidra(x   = 6390,
                       api = "/t/6390/n1/all/v/5929,5933/p/all") %>%
  mutate(date  = parse_date(`Trimestre Móvel (Código)`, format = "%Y%m"),
         name  = `Variável`,
         value = Valor,
         .keep = "none") %>%
  pivot_wider() %>%
  rename("Rendimento nominal" = paste0("Rendimento médio mensal nominal das ",
                                       "pessoas de 14 anos ou mais de idade ",
                                       "ocupadas na semana de referência com ",
                                       "rendimento de trabalho, habitualmente ",
                                       "recebido em todos os trabalhos"),
         "Rendimento real" = paste0("Rendimento médio mensal real das ",
                                    "pessoas de 14 anos ou mais de idade ",
                                    "ocupadas na semana de referência com ",
                                    "rendimento de trabalho, habitualmente ",
                                    "recebido em todos os trabalhos")) %>%
  pivot_longer(-1) %>% 
  as_tibble()

save(rendimento, file = "dados/pnad_rendimento.RData")

massa = get_sidra(x   = 6392,
                  api = "/t/6392/n1/all/v/6288,6293/p/all") %>%
  mutate(date  = parse_date(`Trimestre Móvel (Código)`, format = "%Y%m"),
         name  = `Variável`,
         value = Valor,
        .keep  = "none") %>%
  pivot_wider() %>% 
  rename("Massa de rendimento nominal" = paste0("Massa de rendimento mensal ",
                                                "nominal das pessoas de 14 ", 
                                                "anos ou mais de idade ",
                                                "ocupadas na semana de ",
                                                "referência com rendimento",
                                                " de trabalho, habitualmente ",
                                                "recebido em todos os ",
                                                "trabalhos"),
         "Massa de rendimento real" = paste0("Massa de rendimento mensal real ",
                                             "das pessoas de 14 anos ou mais ",
                                             "de idade ocupadas na semana de ",
                                             "referência com rendimento de ",
                                             "trabalho, habitualmente ",
                                             "recebido em todos os ",
                                             "trabalhos")) %>%
  pivot_longer(-1) %>% 
  as_tibble()

save(massa, file = "dados/pnad_massa.RData")
