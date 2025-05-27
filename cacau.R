library(geobr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(ggstatsplot)
library(patchwork)
library(grid)
library(ggpubr)

# Dados históricos de produção do cacau (Brasil)
hist_prod_cacau <- data.frame(historico_prod_cacau)
hist_prod_cacau <- hist_prod_cacau[,-6:-14]
hist_prod_cacau['Mes_ano'] <- paste(hist_prod_cacau$Mes, hist_prod_cacau$Ano,
                                    sep="-")
hist_prod_cacau$Ano <- as.integer(hist_prod_cacau$Ano)
hist_prod_cacau$Mes <- as.integer(hist_prod_cacau$Mes)
hist_prod_cacau$Area <- gsub(" ", "", hist_prod_cacau$Area)
hist_prod_cacau$Area <- as.integer(hist_prod_cacau$Area)
hist_prod_cacau$Producao <- gsub(" ", "", hist_prod_cacau$Producao)
hist_prod_cacau$Producao <- as.integer(hist_prod_cacau$Producao)
hist_prod_cacau$Rendimento_medio <- as.integer(hist_prod_cacau$Rendimento_medio)

## Evolução da área plantada (Brasil)
evol_area <- filter(hist_prod_cacau, hist_prod_cacau[2] == 12)
evol_area <- data.frame(evol_area) |>
  drop_na()
#names(hist_prod_cacau)

g_evol_area <- ggplot(data=evol_area, 
                      aes(x=evol_area$Ano, y=evol_area$Area,
                          group=1))+
  geom_line()+
  geom_point()+
  ggtitle("Área plantada de cacau em dezembro de cada ano")+
  labs(y="Área plantada (m²)", x=element_blank(),
       caption=)+
  theme(plot.title = element_text(hjust = 0.5))
g_evol_area

# área média de produções (m^2) (Brasil)
area_media <- round(mean(hist_prod_cacau$Area, na.rm=TRUE), digits=2)

# variação percentual da área plantada (inicio e final) (Brasil)
df_limpo <- drop_na(hist_prod_cacau)
area_inicial <- df_limpo$Area[nrow(df_limpo)]
area_final <- df_limpo$Area[1]
var_area <- round(((area_final - area_inicial)/area_inicial) * 100, digits=2)

## Evolução do rendimento médio ao longo dos meses (Brasil)
hist_cacau_rend <- hist_prod_cacau |>
  arrange(Ano, Mes) |> 
  apply(2, rev) |>
  as.data.frame()
  
evol_rend_med <- hist_cacau_rend[5:6] |>
  drop_na()
evol_rend_med$Mes_ano <- c(seq(from=length(evol_rend_med$Mes_ano), to=1, by=-1))

tend_rend_med <- lm(Rendimento_medio ~ Mes_ano, data=evol_rend_med)

prim_rend <- as.integer(evol_rend_med[nrow(evol_rend_med),1])
ult_rend <- as.integer(evol_rend_med[1,1])
var_rend <- ((ult_rend - prim_rend)/prim_rend) * 100  # var % do rend_medio

g_evol_rend <- ggplot(data=evol_rend_med, 
                      aes(x=Mes_ano,
                          y=Rendimento_medio,
                          group=1))+
  geom_line()+
  geom_point()+
  stat_smooth(method = "lm", se = TRUE, color = "red", alpha=0.06, size=0.7)+
  labs(x="Meses (Jan20 - Nov24)", y="Rendimento médio (kg/ha)",
       caption=paste("Aumento do rendimento médio: ", 
                     as.character(round(tend_rend_med$coefficients[2]*100, 
                                        digits=0)), "%"))+
  ggtitle("Rendimento médio das produções (2020 - 2024)")+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.caption = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.title.y = element_text(vjust = +3))
g_evol_rend
ggsave(filename="rendimento_medio.svg", plot=g_evol_rend, width=10, height=6)

# Rendimento médio geral das produções em kg/ha (Brasil)
rend_medio_geral <- round(mean(as.numeric(hist_prod_cacau$Rendimento_medio), na.rm=TRUE),
                          digits=2)

desvio_padrao_rend <- round(sd(hist_prod_cacau$Rendimento_medio, na.rm=TRUE),
                            digits=2)
coef_var <- (desvio_padrao_rend / rend_medio_geral) * 100  # em porcentagem
coef_var <- round(coef_var, digits=2)

## Produções na Bahia (mil reais)
prod_reg <- producao_regional_bahia[,1:2]
prod_reg$valor_corrigido <- gsub(".", "", prod_reg$valor)
prod_reg <- prod_reg |> separate(localidade, 
                                 into = c("name_muni", "estado"), 
                                 sep = " - ")
prod_reg <- prod_reg[c("name_muni", "valor")]

maior_prod <- drop_na(prod_reg) |>
  filter(valor == max(prod_limpo$valor))

## Produção no Brasil (toneladas)
evol_prod_ton <- hist_prod_cacau[, c(1, 2, 4)] |>
  arrange(Ano, Mes) |>
  apply(2, rev) |>
  as.data.frame() |>
  drop_na()
evol_prod_ton$Mes <- c(seq(from=length(evol_prod_ton$Mes), to=1, by=-1))
evol_prod_ton$Ano <- NULL

tend_prod_ton <- lm(Producao ~ Mes, data=evol_prod_ton)

ult_prod <- evol_prod_ton[1,2]
prim_prod <- evol_prod_ton[nrow(evol_prod_ton),2]
var_prod <- round(((ult_prod - prim_prod)/prim_prod)*100, digits=2)

g_prod_ton <- ggplot(data=evol_prod_ton, 
                      aes(x=Mes,
                          y=Producao,
                          group=1))+
  geom_line()+
  geom_point()+
  stat_smooth(method = "lm", se = TRUE, color = "red", alpha=0.06, size=0.7)+
  labs(x="Meses (Jan20 - Nov24)", y="Produção (ton)",
       caption=paste("Aumento da produção: ", 
                     as.character(var_prod), "%"))+
  ggtitle("Produção total (2020 - 2024)")+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.caption = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.title.y = element_text(vjust = +3))
g_prod_ton
ggsave(filename="prod_ton.svg", plot=g_prod_ton, width=10, height=6)

# média de produção (ton)
med_prod <- round(mean(evol_prod_ton$Producao), digits=2)

## Combinação de gráficos históricos
g_rend <- ggplot(data=evol_rend_med, 
                 aes(x=Mes_ano,
                     y=Rendimento_medio,
                     group=1))+
  geom_line()+
  geom_point()+
  stat_smooth(method = "lm", se = TRUE, color = "red", alpha=0.06, size=0.7)+
  labs(x="Meses (Jan20 - Nov24)", y="Rendimento médio (kg/ha)",
       caption=paste("Aumento do rendimento médio: ", 
                     as.character(round(tend_rend_med$coefficients[2]*100, 
                                        digits=0)), "%"))+
  ggtitle("Rendimento médio das produções (2020 - 2024)")+
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        plot.caption = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.title.y = element_text(vjust = +3))

g_combinado <- grid.arrange(g_evol_area, g_rend, g_prod_ton, nrow=3)
g_combinado
ggsave(filename="combinado.svg", plot=g_combinado, width=10, height=10)

## Teste de correlação
hist_ord <- hist_prod_cacau |>
  arrange(Ano, Mes) |>
  apply(2, rev) |>
  as.data.frame() |>
  drop_na()

hist_ord$Ano <- as.integer(hist_ord$Ano)
hist_ord$Mes <- as.integer(hist_ord$Mes)
hist_ord$Area <- as.integer(hist_ord$Area)
hist_ord$Rendimento_medio <- as.integer(hist_ord$Rendimento_medio)
hist_ord$Producao <- as.integer(hist_ord$Producao)
hist_ord$Num_mes <- c(seq(from=length(hist_ord$Mes_ano), to=1, by=-1))

## Análise de correlações
# correlação praticamente nula entre Area e Rendimento médio (-0,08)
g_corr_area_rendimento <- ggscatterstats(
                            data=hist_ord,
                            x=Area,
                            y=Rendimento_medio,
                            type="parametric",
                            xlab="Área",
                            ylab="Rendimento médio"
                          )

g_corr_tempo_prod <- ggscatterstats(
  data=hist_ord,
  x=Num_mes,
  y=Producao,
  type="parametric",
  xlab="Meses (1-59)",
  ylab="Produção"
)

g_corr_tempo_rendimento <- ggscatterstats(
  data=hist_ord,
  x=Num_mes,
  y=Rendimento_medio,
  type="parametric",
  xlab="Meses (1-59)",
  ylab="Rendimento médio"
)

g_correlacoes <- grid.arrange(g_corr_tempo_prod, 
                              g_corr_tempo_rendimento, 
                              g_corr_area_rendimento, ncol=3,
                              top=textGrob("Testes de correlação", 
                                           gp = gpar(fontsize = 14, 
                                                     fontface = "bold")))

## Censo Agro 2017
prod_2017 <- comparacao_producoes_2017
names(prod_2017)[2] <- "Estabelecimentos"
prod_2017[6] <- NULL
prod_2017[6] <- NULL
prod_2017 <- prod_2017[-c(1),]

# gráficos de barra comparações agricultura familiar e comercial
g_comp_estab <- ggplot(data=prod_2017[,c(1,2)], 
                       aes(x=prod_2017$Tipo, y=prod_2017$Estabelecimentos))+
  geom_bar(stat="identity", width=0.5)+
  ggtitle("Gráfico 1 - Estabelecimentos")+
  labs(x=element_blank(), y="Estabelecimentos")+
  theme(plot.title = element_text(hjust = 0.5))
g_comp_estab

g_comp_area_plant <- ggplot(data=prod_2017[,c(1,3)], 
                       aes(x=prod_2017$Tipo, y=prod_2017$Area_plantada_ha))+
  geom_bar(stat="identity", width=0.5)+
  ggtitle("Gráfico 2 - Área plantada")+
  labs(x=element_blank(), y="Hectares")+
  theme(plot.title = element_text(hjust = 0.5))
g_comp_area_plant

g_comp_area_col <- ggplot(data=prod_2017[,c(1,4)], 
                      aes(x=prod_2017$Tipo, y=prod_2017$Area_colhida_ha))+
  geom_bar(stat="identity", width=0.5)+
  ggtitle("Gráfico 3 - Área colhida")+
  labs(x=element_blank(), y="Hectares")+
  theme(plot.title = element_text(hjust = 0.5))
g_comp_area_col

g_comp_pes_col <- ggplot(data=prod_2017[,c(1,5)], 
                          aes(x=prod_2017$Tipo, y=prod_2017$Pes_colhidos))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  ggtitle("Gráfico 4 - Pés colhidos")+
  labs(x=element_blank(), y="Pés")+
  theme(plot.title = element_text(hjust = 0.5))
g_comp_pes_col

g_comp_qtd_prod <- ggplot(data=prod_2017[,c(1,6)], 
                         aes(x=prod_2017$Tipo, y=prod_2017$Qtd_produzida_ton))+
  geom_bar(stat="identity", width=0.5)+
  #scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  ggtitle("Gráfico 5 - Quantidade produzida")+
  labs(x=element_blank(), y="Toneladas")+
  theme(plot.title = element_text(hjust = 0.5))
g_comp_qtd_prod

g_comp_valor_prod <- ggplot(data=prod_2017[,c(1,7)], 
                          aes(x=prod_2017$Tipo, y=prod_2017$Valor_produzido))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  ggtitle("Gráfico 6 - Valor produzido")+
  labs(x=element_blank(), y="R$")+
  theme(plot.title = element_text(hjust = 0.5))
g_comp_valor_prod

g_comp_qtd_vend <- ggplot(data=prod_2017[,c(1,8)], 
                          aes(x=prod_2017$Tipo, y=prod_2017$Qtd_vendida_ton))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  ggtitle("Gráfico 7 - Quantidade vendida")+
  labs(x=element_blank(), y="Toneladas")+
  theme(plot.title = element_text(hjust = 0.5))
g_comp_qtd_vend

g_comp_valor_vend <- ggplot(data=prod_2017[,c(1,9)], 
                          aes(x=prod_2017$Tipo, y=prod_2017$Valor_vendido))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  ggtitle("Gráfico 8 - Valor vendido")+
  labs(x=element_blank(), y="R$")+
  theme(plot.title = element_text(hjust = 0.5))
g_comp_valor_vend

list_comparativos <- list(g_comp_estab,
                          g_comp_area_plant,
                          g_comp_area_col,
                          g_comp_pes_col,
                          g_comp_qtd_prod,
                          g_comp_valor_prod,
                          g_comp_qtd_vend,
                          g_comp_valor_vend
                          )

linha2 <- grid.arrange(
  list_comparativos[[2]], list_comparativos[[3]], 
  list_comparativos[[4]], list_comparativos[[5]], 
  list_comparativos[[6]], list_comparativos[[7]], 
  list_comparativos[[8]], 
  ncol = 4, nrow=2
)

# Definir o layout personalizado
layout <- grid.layout(
  nrow = 2,             # Duas linhas: Gráfico 1 e os gráficos abaixo
  ncol = 3,             # Três colunas para ajustar a largura do Gráfico 1
  heights = c(1, 2),    # Primeira linha menor, segunda linha maior
  widths = c(1, 0.5, 1) # Gráfico 1 ocupa menos largura (coluna central)
)

# Criar o título com formatação em negrito e fonte 14
titulo <- textGrob(
  "Tipos de agricultura e métricas",
  gp = gpar(fontsize = 14, fontface = "bold")
)

# Combinar os gráficos com o layout definido
g_comparativos <- grid.arrange(
  grobs = list(
    nullGrob(),               # Espaço vazio na primeira coluna
    list_comparativos[[1]],   # Gráfico 1 - Estabelecimentos (diminuído)
    nullGrob(),               # Espaço vazio na terceira coluna
    linha2                    # Segunda linha com os outros gráficos
  ),
  layout_matrix = rbind(
    c(1, 2, 3), # Primeira linha com Gráfico 1 centralizado
    c(4, 4, 4)  # Segunda linha ocupa todo o espaço
  ),
  top = titulo
)

g_comparativos
ggsave(filename="comparativos.svg", plot=g_comparativos, width = 15, height=8)

# Análise cooperativas
coop <- cooperativas_2017
names(coop)[2] <- "Estabelecimentos"
coop <- coop |>
  mutate(Porcentagem = (Estabelecimentos / sum(Estabelecimentos) * 100))

# Criar o gráfico de pizza - feito no sheets

## Testes de sazonalidade
# Produção (ton) - STL
hist_ord_rev <- hist_prod_cacau |>
  arrange(Ano, Mes) |>
  as.data.frame() |>
  drop_na()

s_prod <- ts(hist_ord_rev$Producao, frequency=12, start=c(2020, 1))
plot(s_prod)

decomp <- decompose(s_prod, type="additive")
#plot(decomp)

decomp_df <- data.frame(
  Time = time(decomp$x),
  Observed = as.numeric(decomp$x),
  Trend = as.numeric(decomp$trend),
  Seasonal = as.numeric(decomp$seasonal),
  Random = as.numeric(decomp$random)
)

decomp_df_long <- pivot_longer(
  decomp_df,
  cols = c(Observed, Trend, Seasonal, Random),
  names_to = "Component",
  values_to = "Value"
)

decomp_df_long$Component <- factor(decomp_df_long$Component, 
                  levels = c("Observed", "Trend", "Seasonal", "Random"))

g_decomp_prod <- ggplot(decomp_df_long, aes(x = Time, y = Value)) +
  geom_line() +
  facet_wrap(~Component, scales = "free_y", ncol = 1) +
  labs(
    title = "Produção (ton) - Decomposição da Série Temporal (Aditiva)",
    x = "Tempo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank()
  )
g_decomp_prod
ggsave(filename="Decomposicao Producao.svg", plot=g_decomp_prod, 
       width=9, height = 6)

# Produção - ACF e PACF - Descartado! Imprecisão e sazonalidades não relevantes
prod_d1 <- diff(s_prod, differences = 1)
#plot(prod_d1)
acf_prod <- acf(prod_d1, lag.max = 12, plot=FALSE)
plot(acf_prod, main="Autocorrelação - Produção (ton)")
#ggsave(filename = "acf - producao.svg", plot=acf_prod)

pacf_prod <- pacf(prod_d1, lag.max = 12, plot=FALSE)
plot(pacf_prod, main="Autocorrelação parcial - Produção (ton)")
#ggsave(filename = "pacf - producao.svg", plot=pacf_prod)

# Rendimento médio (kg/ha) - STL
s_rend_med <- ts(hist_ord_rev$Rendimento_medio, frequency=12, start=c(2020, 1))
plot(s_rend_med)

decomp_rend_med <- decompose(s_rend_med, type="additive")
#plot(decomp)

decomp_df <- data.frame(
  Time = time(decomp_rend_med$x),
  Observed = as.numeric(decomp_rend_med$x),
  Trend = as.numeric(decomp_rend_med$trend),
  Seasonal = as.numeric(decomp_rend_med$seasonal),
  Random = as.numeric(decomp_rend_med$random)
)

decomp_df_long <- pivot_longer(
  decomp_df,
  cols = c(Observed, Trend, Seasonal, Random),
  names_to = "Component",
  values_to = "Value"
)

decomp_df_long$Component <- factor(decomp_df_long$Component, 
                                   levels = c("Observed", "Trend", "Seasonal", "Random"))

g_decomp_rend_med <- ggplot(decomp_df_long, aes(x = Time, y = Value)) +
  geom_line() +
  facet_wrap(~Component, scales = "free_y", ncol = 1) +
  labs(
    title = "Rendimento médio (kg/ha) - Decomposição da Série Temporal (Aditiva)",
    x = "Tempo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank()
  )
g_decomp_rend_med
ggsave(filename="Decomposicao Rendimento medio.svg", plot=g_decomp_rend_med,
       width=9, height = 6)

# Rendimento médio - ACF e PACF - Descartado! Sazonalidades não relevantes
rend_med_d1 <- diff(s_rend_med, differences = 1)
#plot(rend_med_d1)
acf_rend_med <- acf(rend_med_d1, plot=FALSE)
plot(acf_rend_med, main="Autocorrelação - Rendimento médio (kg/ha)")

pacf_rend_med <- pacf(rend_med_d1, plot=FALSE)
plot(pacf_rend_med, main="Autocorrelação parcial - Rendimento médio (kg/ha)")

## Valor de mercado
library(quantmod)

# Período completo (2020 - 2024)
precos_cacau <- getSymbols("CC=F", src='yahoo',
                           from="2019-12-31", to="2024-12-22",
                           auto.assign = FALSE)
colnames(precos_cacau) <- c("Open", "High", "Low", 
                            "Close", "Volume", "Adjusted")
precos_cacau <- na.locf(precos_cacau, na.rm=TRUE, fromLast=FALSE, maxgap=Inf)

decomp_cacau <- decompose(ts(precos_cacau$Close, frequency=365), type='additive')

decomp_df <- data.frame(
  Time = time(decomp_cacau$x),
  Observed = as.numeric(decomp_cacau$x),
  Trend = as.numeric(decomp_cacau$trend),
  Seasonal = as.numeric(decomp_cacau$seasonal),
  Random = as.numeric(decomp_cacau$random)
)

decomp_df_long <- pivot_longer(
  decomp_df,
  cols = c(Observed, Trend, Seasonal, Random),
  names_to = "Component",
  values_to = "Value"
)

decomp_df_long$Component <- factor(decomp_df_long$Component, 
                                   levels = c("Observed", "Trend", "Seasonal", "Random"))

g_decomp_cacau <- ggplot(decomp_df_long, aes(x = Time, y = Value)) +
  geom_line() +
  facet_wrap(~Component, scales = "free_y", ncol = 1) +
  labs(
    title = "Cacau (NY) 2020 a 2024 - Decomposição da Série Temporal (Aditiva)",
    x = "Tempo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank()
  )
g_decomp_cacau
ggsave(filename="Decomposicao cacau stock - 2020 a 2024.svg", 
       plot=g_decomp_cacau, width=9, height = 6)


# Período anterior a alta de 2024
precos_cacau_2 <- getSymbols("CC=F", src='yahoo',
                               from="2019-12-31", to="2024-01-01",
                               auto.assign = FALSE)
precos_cacau_2 <- na.locf(precos_cacau_2, na.rm=TRUE, fromLast=FALSE, maxgap=Inf)
colnames(precos_cacau_2) <- c("Open", "High", "Low", 
                            "Close", "Volume", "Adjusted")

decomp_cacau_2 <- decompose(ts(precos_cacau_2$Close, frequency=365), type='additive')

decomp_df <- data.frame(
  Time = time(decomp_cacau_2$x),
  Observed = as.numeric(decomp_cacau_2$x),
  Trend = as.numeric(decomp_cacau_2$trend),
  Seasonal = as.numeric(decomp_cacau_2$seasonal),
  Random = as.numeric(decomp_cacau_2$random)
)

decomp_df_long <- pivot_longer(
  decomp_df,
  cols = c(Observed, Trend, Seasonal, Random),
  names_to = "Component",
  values_to = "Value"
)

decomp_df_long$Component <- factor(decomp_df_long$Component, 
                                   levels = c("Observed", "Trend", "Seasonal", "Random"))

g_decomp_cacau_2 <- ggplot(decomp_df_long, aes(x = Time, y = Value)) +
  geom_line() +
  facet_wrap(~Component, scales = "free_y", ncol = 1) +
  labs(
    title = "Cacau (NY) 2020 a 2023 - Decomposição da Série Temporal (Aditiva)",
    x = "Tempo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.y = element_blank()
  )
g_decomp_cacau_2
ggsave(filename="Decomposicao cacau stock - 2020 a 2023.svg", 
       plot=g_decomp_cacau_2, width=9, height = 6)
