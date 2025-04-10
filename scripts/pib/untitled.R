
# Setup -------------------------------------------------------------------

rm(list = ls())

# Coleta de dados ---------------------------------------------------------

data = get_sidra(x = 1620,
                 api = paste0("/t/1620/n1/all/v/all/p/all/c11255/",
                              "90687,90691,90696,90707,93404,93405,",
                              "93406,93407,93408/d/v583%202")) %>% 
  mutate(date  = as.yearqtr(`Trimestre (Código)`, format = '%Y%q'),
         name  = `Setores e subsetores`,
         value = Valor,
         .keep  = "none") %>%
  pivot_wider() %>% 
  set_names(c("date",
              "Agropecuária",
              "Indústria",
              "Serviços",
              "PIB",
              "Consumo",
              "Governo",
              "Investimento",
              "Exportação",
              "Importação")) %>%
  pivot_longer(-1) %>% 
  as_tibble()

data_sa = get_sidra(x = 1621, 
                 api = paste0("/t/1621/n1/all/v/all/p/all/c11255/",
                              "90687,90691,90696,90707,93404,93405,",
                              "93406,93407,93408/d/v584%202")) %>% 
  mutate(date  = as.yearqtr(`Trimestre (Código)`, format = '%Y%q'),
         name  = `Setores e subsetores`,
         value = Valor,
         .keep  = "none") %>%
  pivot_wider() %>% 
  set_names(c("date",
              "Agropecuária",
              "Indústria",
              "Serviços",
              "PIB",
              "Consumo",
              "Governo",
              "Investimento",
              "Exportação",
              "Importação")) %>%
  pivot_longer(-1) %>% 
  as_tibble()

# Tratamento --------------------------------------------------------------

data_sa %<>% 
  arrange(date) %>% 
  mutate(qoq = (value/dplyr::lag(value,1) - 1)*100,
         .by = name)

data %<>% 
  arrange(date) %>% 
  mutate(qoq  = (value/dplyr::lag(value,1) - 1)*100,
         yoy  = (value/dplyr::lag(value,4) - 1)*100,
         acum = (zoo::rollapply(value, 
                                width = 4, 
                                FUN   = sum, 
                                align = 'right', 
                                fill  = NA)),
         acum = (acum/dplyr::lag(acum, 4)-1)*100,
        .by = name)

# Visualização de dados ---------------------------------------------------

data %>% 
  dplyr::filter(date >= "2014 Q1") %>% 
  ggplot() +
  aes(x = date, y = acum) +
  geom_line(size = .75, colour = "darkblue") +
  facet_wrap(~name, scales = 'free') +
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 90)) +
  # scale_x_date(expand = c(0,0), 
  #              date_labels = "%b/%y", 
  #              date_breaks = "6 months") +
  # scale_fill_manual(values = colors) +
  # scale_color_manual(values = "black") +
  labs(subtitle = "PIB SA - Taxa trimestre contra trimestre imediatamente anterior",
       y = NULL, x = NULL,
       caption = "Elaboração do autor com dados do IBGE")
