
# Vacinação --------------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 15/09/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/vaccination ---------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Os dados apresentam a cobertura global da vacinação de crianças de um ano
### com algumas das vacinas mais importantes recomendadas pela OMS. Para muitas
### vacinas essenciais, a cobertura é maior que 80%. Entretanto, a taxa de vacinação
### ainda não é suficiente.

### A cobertura global da vacinação tem aumentado ao longo do tempo e varia com
### a renda per capita de cada país.

### A vacina contra difteria, tétano e coqueluche, é frequentemente usada
### como métrica chave para cobertura global da vacinação porque é um bom
### indicador para acessar a rotina dos serviços de imunização. Em 2018, a 
### cobertura da 3ª dose de DTP foi de 86%. Isso significa que das 135 milhões de
### de crianças com menos de um ano de idade, 19 milhões não receberam a imunização
### completa. A cobertura da 1ª dose de DTP foi de 90%, indicando que 13,5 milhões
### de crianças não foram vacinadas em 2018.

### Em 2018, apenas 35% das crianças no mundo receberam a vacina contra o
### rotavirus, que protege crianças de doenças diarréicas - uma das principais
### causas de mortalidade infantil. Similarmente, a vacina pneumocócica que protege
### crianças da pneumonia - a principal causa de mortalidade infantil - apenas
### alcançou 47% das crianças com um ano de idade.

### Os dados também mostram que em países mais pobres a cobertura da vacinação é 
### baixa. A vacina contra difteria, coqueluche e tétano é um bom marcador das forças
### dos programas de imunização de um país, uma vez que são necessárias várias 
### administrações. Todos os países ricos tem taxa de cobertura da vacinação maior
### que 90%. Em países de renda média e baixa, a cobertura da vacinação é baixa -
### em alguns países abaixo de 50%.

### Mas os dados também mostram que em alguns países pobres como Burundi, Rwanda, 
### e Bangladesh, a vacinação alcança uma alta porcentagem de cobertura. Da mesma 
### forma, países que tem a maior parte da população vivendo em extrema pobreza
### tem frequentemente - mas nem sempre - taxas de imunização mais baixas.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse) # ggplot2, dplyr, tidyr (manipula os NAs), forcats
library(cols4all) # paleta de cores
c4a_gui()

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

vacina_cob <- read.csv("global-vaccination-coverage.csv")
view(vacina_cob)
names(vacina_cob)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

## Função select

vacina_cob <- vacina_cob %>%
  select(-Code, -MCV1....of.one.year.olds.immunized.,
         -IPV1....of.one.year.olds.immunized.) %>%
  rename(tuberculose = BCG....of.one.year.olds.immunized.,
         hepatite_b = HepB3....of.one.year.olds.immunized.,
         influenza = Hib3....of.one.year.olds.immunized.,
         poliomielite = Pol3....of.one.year.olds.immunized.,
         sarampo = MCV2....of.children.immunized.,
         pneumonia = PCV3....of.one.year.olds.immunized.,
         rubeola = RCV1....of.one.year.olds.immunized.,
         rotavirus = RotaC....of.one.year.olds.immunized.,
         febre_amarela = YFV....of.one.year.olds.immunized.,
         difteria_tetano_coquel = DTP3....of.one.year.olds.immunized.) %>%
  view()

## Função filter

vacina_cob1 <- vacina_cob %>%
  filter(Entity %in% c("Nigeria", "Ethiopia", "Pakistan",
                       "Brazil", "Argentina", "China", "Canada", 
                       "United States", "Austria", "Norway")) %>%
  view()

## Função summarise 

vacina_cob2 <- vacina_cob1 %>%
  select(pneumonia, Entity) %>%
  drop_na() %>%
  group_by(Entity) %>%
  summarise(media = mean(pneumonia),
            n = n(), sd = sd(pneumonia),
            se = sd/sqrt(n)) %>%
  view()

## Função arrange

vacina_cob3 <- vacina_cob1 %>%
  select(pneumonia, Entity) %>%
  drop_na() %>%
  group_by(Entity) %>%
  arrange(desc(pneumonia)) %>%
  view()

vacina_cob3 <- vacina_cob1 %>%
  select(pneumonia, Entity) %>%
  drop_na() %>%
  group_by(Entity) %>%
  arrange(Entity, desc(pneumonia)) %>%
  view()

## Função mutate

vacina_cob4 <- vacina_cob1 %>%
  select(pneumonia, Entity) %>%
  drop_na() %>%
  mutate(pneumonia_total = pneumonia * 100) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

### Por país

ggplot(vacina_cob2, aes(x = Entity, y = media)) +
  geom_col() +
  geom_label(aes(label = n))

ggplot(vacina_cob2, aes(x = fct_reorder(Entity, media), y = media)) +
  geom_col() +
  geom_label(aes(label = n), size = 3, col = "black", fill = "gray")

ggplot(vacina_cob2, aes(x = fct_reorder(Entity, media), y = media)) +
  geom_col() + 
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.3, size = 0.8) +
  theme_bw()

c4a_gui()
c4a("br_bg", 11)

ggplot(vacina_cob2, aes(x = fct_reorder(Entity, media), y = media,
                        fill = Entity)) +
  geom_col() +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.3, size = 0.8) +
  scale_fill_manual(values = c("#8C510A", "#BF812D",
"#DFC27D", "#F6E8C3",
"#F5F5F5", "#C7EAE5",
"#80CDC1", "#35978F")) +
  theme(legend.position = "none")

### Por tempo e país

vacina_cob_year <- vacina_cob1 %>%
  select(pneumonia, Entity, Year) %>%
  filter(Year %in% c("2010", "2011", "2012", "2013", "2014", "2015", 
                     "2016", "2017", "2018", "2019")) %>%
  drop_na() %>%
  group_by(Entity, Year) %>%
  summarise(media = mean(pneumonia)) %>%
  view()

c4a_gui()
c4a("muted", 9)

ggplot(vacina_cob_year, aes(x = factor(Year), y = media, 
                        group = Entity, col = Entity)) +
  geom_point(size = 3) +
  geom_line(size = 1.8) +
  scale_color_manual(values = c("#CC6677", "#332288",
"#DDCC77", "#117733",
"#88CCEE", "#882255",
"#44AA99", "#999933"))



