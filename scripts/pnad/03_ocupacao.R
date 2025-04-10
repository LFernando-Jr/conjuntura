
# Setup -------------------------------------------------------------------

rm(list = ls())

load("dados/pnad.RData")

# Tratamento de dados -----------------------------------------------------

data = data_raw %>% 
  pivot_wider(id_cols = date) %>% 
  mutate(date = date,
         inativos = `População` - Total,
         emprego = `Força de trabalho - ocupada`/`Força de trabalho`*100,
         emprego_sa = seas(ts(emprego,
                              frequency = 12,
                              start = c(min(year(date)),
                                        month(min(date)))), 
                           x11 = "")$series$d11,
         participacao = `Força de trabalho`/Total*100,
         participacao_sa = seas(ts(participacao,
                                   frequency = 12,
                                   start = c(min(year(date)),
                                             month(min(date)))), 
                                x11 = "")$series$d11,
         .keep = "none")

# Visualização de dados ---------------------------------------------------

# Desocupação -------------------------------------------------------------

data %>% 
  select(date, emprego, emprego_sa) %>% 
  pivot_longer(-1) %>% 
  ggplot() + 
  aes(x = date, y = value, colour = name) +
  geom_line(linewidth = .75) +
  scale_x_date(expand = c(0,0), date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + 
  theme(axis.line       = element_line(colour = "black"), 
        legend.position = "none", 
        legend.title    = element_blank(),
        axis.text.x     = element_text(angle = 90),
        axis.title      = element_blank()) +
  scale_colour_manual(values = c("#2F47AD",
                                 "#E47632")) +
  labs(title    = "Taxa de ocupação (%)",
       subtitle = "PNAD Contínua", 
       caption  = "Fonte: Elaboração do autor com dados do IBGE")

ggsave("ocupacao.png",
       width  = 4800,
       height = 2160,
       units  = "px",
       dpi    = 576,
       path   = paste0(getwd(), "/saidas/pnad"))


# Participação ------------------------------------------------------------

data %>% 
  select(date, participacao, participacao_sa) %>% 
  pivot_longer(-1) %>% 
  ggplot() + 
  aes(x = date, y = value, colour = name) +
  geom_line(linewidth = .75) +
  scale_x_date(expand = c(0,0), date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + 
  theme(axis.line       = element_line(colour = "black"), 
        legend.position = "none", 
        legend.title    = element_blank(),
        axis.text.x     = element_text(angle = 90),
        axis.title      = element_blank()) +
  scale_colour_manual(values = c("#2F47AD",
                                 "#E47632")) +
  labs(title    = "Taxa de participação (%)",
       subtitle = "PNAD Contínua", 
       caption  = "Fonte: Elaboração do autor com dados do IBGE")

ggsave("participacao.png",
       width  = 4800,
       height = 2160,
       units  = "px",
       dpi    = 576,
       path   = paste0(getwd(), "/saídas/pnad"))

# Combo -------------------------------------------------------------------

data %>% 
  select(-inativos) %>% 
  pivot_longer(-1) %>% 
  mutate(group = ifelse(name %in% c("desemprego","desemprego_sa"),"B","A")) %>% 
  ggplot() + 
  aes(x = date, y = value, colour = name) +
  geom_line(linewidth = .75) +
  facet_wrap(~group, scales = "free") +
  scale_x_date(expand = c(0,0), date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + 
  theme(strip.text      = element_blank(),
        axis.line       = element_line(colour = "black"), 
        legend.position = "none", 
        legend.title    = element_blank(),
        axis.text.x     = element_text(angle = 90),
        axis.title      = element_blank()) +
  scale_colour_manual(values = c("#2F47AD",
                                 "#E47632",
                                 "#2F47AD",
                                 "#E47632")) +
  labs(title    = "Taxa de participação e de desocupação (%)",
       subtitle = "PNAD Contínua", 
       caption  = "Fonte: Elaboração do autor com dados do IBGE")

ggsave("combo.png",
       width  = 4800,
       height = 2160,
       units  = "px",
       dpi    = 576,
       path   = paste0(getwd(), "/saídas/pnad"))
