
# Setup -------------------------------------------------------------------

rm(list = ls())

Sys.setenv("LANGUAGE" = "Pt")
Sys.setlocale("LC_ALL", "Portuguese")

# Coleta de dados ---------------------------------------------------------

data = read.csv2(paste0(getwd(), "/dados/dot-plot_ffr.csv"), 
                 check.names = FALSE)

# Tratamento de dados -----------------------------------------------------

data %<>% 
  mutate(date = as.Date(SEP, format = "%m/%d/%Y"),
         SEP = factor(as.Date(SEP, format = "%m/%d/%Y"))) %>%
  dplyr::filter(date >= "2023-01-12") %>% 
  pivot_longer(cols = 3:8,
               names_to = "year",
               values_to = "count") %>%
  dplyr::filter(!is.na(count)) %>%
  uncount(weights = count) %>%
  mutate(FFR = FFR + .125) %>% 
  as_tibble()

# Visualização de dados ---------------------------------------------------

data %>%
  dplyr::filter(!year %in% c("2023", "2024")) %>% 
  ggplot() +
  aes(x = SEP, y = FFR) +
  geom_boxplot() +
  facet_wrap(~year, ncol = 5) +
  theme_bw() + theme(panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.title = element_blank(),
                     axis.title = element_blank(),
                     strip.background = element_blank(),
                     axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(n.breaks = 10) +
  scale_x_discrete(breaks = levels(data$SEP), 
                   labels = function(x) format(as.Date(x, format = "%Y-%d-%m"),
                                               "%b %y")) +
  labs(
    title = "Summary of Economic Projections",
    subtitle = "Expectativa da FFR em varios horizontes ao longo dos períodos",
    caption = "Fonte: Capri Family Office com dados do Federal Reserve"
    )

ggsave("box-plot_ffr.png", 
       width  = 21, 
       height = 11.900, 
       units  = "in",
       dpi    = 800, 
       path   = paste0(getwd(), "/saidas/dot-plot"))
