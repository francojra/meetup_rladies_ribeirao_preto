
# Definir diretório de trabalho -------------------------------------------------------------------------------------------------------------

# Para saber qual seu diretório atual
getwd() 
# Para criar um novo diretório de trabalho
dir.create("evento_rladies_ribeirao_preto") 

# Para estabelecer o novo diretório criado
setwd("evento_rladies_ribeirao_preto")

# Estabelecer diretório manualmente
#Session > Set Working Directory > Choose Directory

# Carregar pacotes --------------------------------------------------------------------------------------------------------------------------

# Pacote para produção dos gráficos
install.packages("ggplot2")
library(ggplot2)

# Pacote usado para análises descritivas
install.packages("dplyr")
library(dplyr) 

# Conjunto de pacotes para manipulação e visualização de dados
install.packages("devtools")
devtools::install_github("tidyverse/tidyverse")
library(tidyverse)

# Pacote auxiliar para mudar cores do gráfico 
install.packages("devtools")
remotes::install_github("mtennekes/cols4all")
library(cols4all)

# Criar ou carregar conjunto de dados -------------------------------------------------------------------------------------------------------

library(dados)
dados::diamante

# Gráfico de histograma e densidade ---------------------------------------------------------------------------------------------------------

ggplot(diamante, aes(x = preco)) +
  geom_histogram(bins = 30)

ggplot(diamante, aes(x = quilate)) +
  geom_histogram(bins = 30, fill = "blue", col = "lightblue")

ggplot(diamante, aes(x = preco)) +
  geom_histogram(bins = 30)

ggplot(diamante, aes(x = quilate)) +
  geom_histogram(bins = 30)

ggplot(diamante, aes(x = profundidade)) +
  geom_histogram(bins = 30)

ggplot(diamante, aes(x = preco, fill = cor)) +
  geom_density()

ggplot(diamante, aes(x = quilate, fill = corte)) +
  geom_density()

ggplot(diamante, aes(x = profundidade, fill = transparencia)) +
  geom_density(alpha = 0.5)

# Gráfico de boxplot e violino --------------------------------------------------------------------------------------------------------------

ggplot(diamante, aes(x = cor, y = preco)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = "mean", geom = "point", 
               size = 2, col = "green")

ggplot(diamante, aes(x = cor, y = preco)) +
  geom_violin() +
  geom_boxplot(outlier.shape = NA,  width = 0.1, 
               color = "black") +
  stat_summary(fun = "mean", geom = "point", size = 3) 

ggplot(diamante, aes(x = cor, y = preco)) +
  geom_jitter(alpha = 0.2) +
  geom_violin(fill = "orange", alpha = 0.3, trim = F) +
  geom_boxplot(outlier.shape = NA, width = 0.1, color = "black") +
  stat_summary(fun = "mean", geom = "point", size = 2.7)

ggplot(diamante, aes(x = corte, y = preco)) +
  geom_violin(fill = "orange", alpha = 0.6, trim = F) +
  geom_boxplot(outlier.shape = NA, width = 0.1, color = "black") +
  stat_summary(fun = "mean", geom = "point", size = 2.7)


# Gráfico de dispersão ----------------------------------------------------------------------------------------------------------------------

ggplot(diamante, aes(x = quilate, y = preco, col = cor)) +
  geom_point()


ggplot(diamante, aes(x = quilate, y = preco, col = cor)) +
  geom_point()

p1 <- ggplot(diamante, aes(x = quilate, y = preco, col = cor)) +
  geom_point(alpha = 0.8) +
  theme(legend.position = "bottom", 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, color = "black"))

p1

# with marginal histogram

library(ggExtra)

ggMarginal(p1, type = "histogram")


# Gráfico de barras -------------------------------------------------------------------------------------------------------------------------

diamante1 <- diamante %>%
  group_by(corte) %>%
  dplyr::summarise(media = mean(preco),
            sd = sd(preco),
            n = n(),
            se = sd/sqrt(n)) %>%
  view()

ggplot(diamante1, aes(x = fct_reorder(corte, media), y = media)) +
  geom_col(fill = "forestgreen", alpha = 0.6) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                size = 0.8, width = 0.3) +
  labs(x = "Corte", y = "Média de preços (US dollars)") +
  geom_text(aes(label = n), vjust = 5) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12))

# labs = labels

ggplot(diamante1, aes(x = fct_reorder(corte, media), 
                      y = media)) +
  geom_col(fill = "orange", alpha = 0.7) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                size = 0.8, width = 0.3) + 
  geom_text(aes(label = n), vjust = 7) +
  labs(x = "Tipo de corte", y = "Preço médio (dollars)", 
       title = "Preços de diamantes por tipo de corte") +
  theme(axis.text = element_text(colour = "black"))

# Gráfico de linhas -------------------------------------------------------------------------------------------------------------------------

diamante2 <- diamante %>%
  select(transparencia, cor, quilate) %>%
  group_by(cor, transparencia) %>%
  dplyr::summarise(media = mean(quilate)) %>%
  view()
  
ggplot(diamante2, aes(x = transparencia, y = media,
                     group = cor, color = cor)) +
  geom_point() +
  geom_line()

diamante3 <- diamante %>%
  select(transparencia, corte, quilate) %>%
  group_by(corte, transparencia) %>%
  dplyr::summarise(media = mean(quilate)) %>%
  view()

library(viridis)

ggplot(diamante3, aes(x = transparencia, y = media,
                     group = corte, color = corte)) +
  geom_point() +
  geom_line() +
  scale_color_viridis(discrete = T, 
                      labels = c("FAIR", "GOOD", "VERY GOOD",
                                 "PREMIUM", "IDEAL")) +
  labs(x = "Transparência", y = "Preço média (US dollars",
       col = "Tipo de corte")

