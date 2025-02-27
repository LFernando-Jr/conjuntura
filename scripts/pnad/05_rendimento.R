
# Setup -------------------------------------------------------------------

rm(list = ls())

load("dados/pnad_rendimento.RData")

# Tratamento e visualização de dados --------------------------------------

rendimento %>% 
  dplyr::filter(name == "Rendimento real") %>% 
  ggplot() +
  aes(x = date, y = value) + 
  geom_line(linewidth = .75) +
  theme(legend.position = "none") +
  scale_x_date(expand = c(0,0), date_labels = "%Y", date_breaks = "1 year") +
  theme_bw() + 
  theme(axis.line       = element_line(colour = "black"), 
        legend.position = "none", 
        legend.title    = element_blank(),
        axis.text.x     = element_text(angle = 90),
        axis.title      = element_blank()) +
  labs(title    = "Rendimento real (R$)",
       subtitle = "PNAD Contínua", 
       caption  = "Fonte: Elaboração do autor com dados do IBGE")

ggsave("rendimento.png",
       width  = 4800,
       height = 2160,
       units  = "px",
       dpi    = 576,
       path   = paste0(getwd(), "/saídas/pnad"))
