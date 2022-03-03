# Criação de gráficos com base nos dados do Atlas do Estado Brasileiro
# Fonte: https://www.ipea.gov.br/atlasestado/filtros-series/26/remuneracoes-no-setor-publico

# Carregar pacotes ---------------------------------------------------------------------
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(deflateBR)
library(lubridate)
library(ggplot2)
library(ggpubr)

rm(list=ls()) # limpar as variáveis carregadas

# Função que retorna tabela com valores deflacionados por sexo e raça ------------------
 deflac_tabela <- function(filtro = 'all') {

  if(filtro == 'all') filtro <- c('Mulher Negra','Mulher Branca','Homem Negro','Homem Branco')

  dados <- 'data/5233-liquidosexoraca.csv' %>%
    read_csv2() %>%
    janitor::clean_names() %>%
    select(sexo_raca, ano, liquido) %>%
    dplyr::filter(sexo_raca %in% filtro) %>%
    mutate(liquido_defl = round(deflate(nominal_values = liquido,
                                        nominal_dates =  ymd(ano, truncated = 2L),
                                        real_date = "12/2019",
                                        index = "ipca"), 2))

  return(dados)
}

# Tabela com a média geral -----------------------------------------------------
tabela_geral <- deflac_tabela() %>%
  group_by(ano) %>%
  summarise(liquido = mean(liquido, na.rm=TRUE),
            liquido_defl = mean(liquido_defl, na.rm=TRUE))

# Deflacionar por grupo --------------------------------------------------------
mulher_n <- deflac_tabela(filtro = 'Mulher Negra')
mulher_b <- deflac_tabela(filtro = 'Mulher Branca')
homem_n <- deflac_tabela(filtro = 'Homem Negro')
homem_b <- deflac_tabela(filtro = 'Homem Branco')

# Gráfico com valores nominais -------------------------------------------------

ggplot()+
  geom_line(data = tabela_geral, aes(x = ano, y = liquido, colour = 'Média Geral'), size = 1) +
  geom_line(data = mulher_n, aes(x = ano, y = liquido, colour = 'Mulher Negra'), size = 1) +
  geom_line(data = mulher_b, aes(x = ano, y = liquido, colour = 'Mulher Branca'), size = 1) +
  geom_line(data = homem_n, aes(x = ano, y = liquido, colour = 'Homem Negro'), size = 1) +
  geom_line(data = homem_b, aes(x = ano, y = liquido, colour = 'Homem Branco'), size = 1) +
  scale_color_manual(name = "Legenda:", values = c("Média Geral" = "#000000",
                                                   "Mulher Negra" = "#58508d",
                                                   "Mulher Branca" = "#bc5090",
                                                   "Homem Negro" = "#ff6361",
                                                   "Homem Branco" = "#ffa600")) +
  ggtitle('Remuneração líquida média mensal por\nsexo e raça de 1999 até 2020 - valores nominais') +
  xlab('Ano') + ylab('Remuneração líquida média\nmensal (em reais)') +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".",
                                           decimal.mark = ","),
                     n.breaks = 8) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5), legend.position="bottom") +
  labs(caption = "Fonte: Atlas do Estado Brasileiro (IPEA).")


ggsave('output/remuneração_nominal.png', width = 9, height = 6, dpi = 300)


# Gráfico com valores reais ----------------------------------------------------

ggplot() +
  geom_line(data = tabela_geral, aes(x = ano, y = liquido_defl, colour = 'Média Geral'), size = 1) +
  geom_line(data = mulher_n, aes(x = ano, y = liquido_defl, colour = 'Mulher Negra'), size = 1) +
  geom_line(data = mulher_b, aes(x = ano, y = liquido_defl, colour = 'Mulher Branca'), size = 1) +
  geom_line(data = homem_n, aes(x = ano, y = liquido_defl, colour = 'Homem Negro'), size = 1) +
  geom_line(data = homem_b, aes(x = ano, y = liquido_defl, colour = 'Homem Branco'), size = 1) +
  scale_color_manual(name = "Legenda:", values = c("Média Geral" = "#000000",
                                                   "Mulher Negra" = "#58508d",
                                                   "Mulher Branca" = "#bc5090",
                                                   "Homem Negro" = "#ff6361",
                                                   "Homem Branco" = "#ffa600")) +
  ggtitle('Remuneração líquida média mensal por\nsexo e raça de 1999 até 2020 - valores reais') +
  xlab('Ano') + ylab('Remuneração líquida média\nmensal (em reais)') +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".",
                                                   decimal.mark = ","),
                     n.breaks = 8) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  labs(caption = "Fonte: Atlas do Estado Brasileiro (IPEA). Valores deflacionados pelo IPCA.")

ggsave('output/remuneração_real.png', width = 9, height = 6, dpi = 300)


# Desigualdade entre os grupos --------------------------------------------------------

dados_difer <- deflac_tabela() %>%
  select(1:3) %>%
  pivot_wider(names_from = sexo_raca, values_from = liquido) %>%
  janitor::clean_names() %>%
  mutate(dif_sexo_b = round(mulher_branca/homem_branco - 1, 2),
         dif_sexo_n = round(mulher_negra/homem_negro - 1, 2),
         dif_raca_m = round(mulher_negra/mulher_branca - 1, 2),
         dif_raca_h = round(homem_negro/homem_branco - 1, 2),
         intersec = round(mulher_negra/homem_branco - 1, 2)
         )

cor_graficos <- '#addd8e'

# Gráfico - Mulheres brancas e homens brancos -----------------------------------------
a <- ggplot() +
  geom_bar(data = dados_difer, aes(x = ano, y = -dif_sexo_b), stat='identity', fill = cor_graficos) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     labels = scales::label_percent(decimal.mark = ",",
                                                    suffix = " %")) +
  ggtitle('Mulheres brancas x homens brancos') +
  xlab('Ano') + ylab('') +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 10), legend.position = "bottom",
        axis.title.x = element_text(size = 10))

# Gráfico - Mulheres negras e homens negros --------------------------------------------

b <- ggplot() +
  geom_bar(data = dados_difer, aes(x = ano, y = -dif_sexo_n), stat='identity', fill = cor_graficos) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     labels = scales::label_percent(decimal.mark = ",",
                                                    suffix = " %")) +
  ggtitle('Mulheres negras x homens negros') +
  xlab('Ano') + ylab('') +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 10), legend.position = "bottom",
        axis.title.x = element_text(size = 10))

# Gráfico - Homens negros e brancos --------------------------------------------

c <- ggplot() +
  geom_bar(data = dados_difer, aes(x = ano, y = -dif_raca_h), stat='identity', fill = cor_graficos) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     labels = scales::label_percent(decimal.mark = ",",
                                                    suffix = " %")) +
  ggtitle('Homens negros x brancos') +
  xlab('Ano') + ylab('') +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 10), legend.position = "bottom",
        axis.title.x = element_text(size = 10))

# Gráfico - Mulheres negras e brancas ----------------------------------------------------

d <- ggplot() +
  geom_bar(data = dados_difer, aes(x = ano, y = -dif_raca_m), stat='identity', fill = cor_graficos) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     labels = scales::label_percent(decimal.mark = ",",
                                                    suffix = " %")) +
  ggtitle('Mulheres negras x brancas') +
  xlab('Ano') + ylab('') +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, size = 10), legend.position = "bottom",
        axis.title.x = element_text(size = 10))


# Gráfico síntese ----------------------------------------
figura <- ggarrange(a, b, c, d + font("x.text", size = 10),
                    ncol = 2, nrow = 2)
annotate_figure(figura,
                top = text_grob("Diferença salarial por segmentos", face = "bold", size = 14),
                bottom = text_grob("Fonte: Atlas do Estado Brasileiro (IPEA).",
                                   hjust = 1, x = 1, face = "italic", size = 10)
                )


ggsave('output/figura_síntese.png', width = 9, height = 6, dpi = 300)


# Gráfico - Interseccionalidade mulher negra e homem branco --------------------------------
ggplot() +
  geom_bar(data = dados_difer, aes(x = ano, y = -intersec), stat='identity', fill = cor_graficos) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     labels = scales::label_percent(decimal.mark = ",",
                                                    suffix = " %")) +
  ggtitle('Diferença salarial entre homens brancos e mulheres negras') +
  xlab('Ano') + ylab('') +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  labs(caption = "Fonte: Atlas do Estado Brasileiro (IPEA).")

ggsave('output/interseccionalidade.png', width = 9, height = 6, dpi = 300)

