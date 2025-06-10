library(dplyr)

#Média PIB per capita com IDH abaixo de 0.75
media_pib_abaixo <- Desafio_Prático_IV %>% 
  filter(IDH < 0.75) %>% 
  summarise(media = mean(PIB_per_capita, na.rm = TRUE)) %>% 
  pull(media)

#Média PIB per capita com IDH acima de 0.75
media_pib_acima <- Desafio_Prático_IV %>% 
  filter(IDH >= 0.75) %>% 
  summarise(media = mean(PIB_per_capita, na.rm = TRUE)) %>% 
  pull(media)

#media dataxa de semprego em cidades com IDH >= 0.75
media_taxa_acima <- Desafio_Prático_IV %>% 
  filter(IDH >= 0.75) %>% 
  summarise(media = mean(TaxaDesemprego, na.rm = TRUE)) %>% 
  pull(media)

#media dataxa de semprego em cidades com IDH < 0.75

media_taxa_abaixo <- Desafio_Prático_IV %>% 
  filter(IDH < 0.75) %>% 
  summarise(media = mean(TaxaDesemprego, na.rm = TRUE)) %>% 
  pull(media)
# Configurar margens do gráfico (aumentei a margem esquerda para 5)
par(mar = c(5, 5, 4, 2))  # baixo, esquerdo, topo, direito

# Criar o gráfico de dispersão com PIB em milhares
plot(Desafio_Prático_IV$Gini, Desafio_Prático_IV$PIB_per_capita / 1000, 
     main = "Relação entre Índice de Gini e PIB per capita",
     xlab = "Índice de Gini (desigualdade)",
     ylab = "PIB per capita (mil R$)",
     pch = 16,                     # Pontos sólidos
     col = adjustcolor("darkgreen", alpha.f = 0.6),  # Cor com transparência
     cex = 1.2,                    # Tamanho dos pontos
     las = 1,                      # Eixos com labels horizontais
     bty = "n"                     # Remove a caixa ao redor do gráfico
)

# Adicionar linha de regressão CORRIGIDA (sem dividir Gini por 1000)
abline(
  lm(I(PIB_per_capita/1000) ~ Gini, data = Desafio_Prático_IV),  # Use I() para transformação
  col = "red",
  lwd = 2.5,
  lty = "dashed"  # Linha tracejada
)

# Adicionar grid para melhor leitura
grid(col = "gray80", lty = "dotted")

# Incluir legenda da linha de tendência
legend(
  "topright",                   # Posição da legenda
  legend = "Linha de regressão", 
  col = "red",
  lwd = 2.5,
  lty = "dashed",
  bty = "n"                     # Remove caixa da legenda
)


#Filtrando os três países com maior Gini
top3_gini <- Desafio_Prático_IV %>%
  arrange(desc(Gini)) %>%
  head(3)
print(top3_gini)

top3_gini_simplificado <- top3_gini %>%
  select(Municipio, Gini, PIB_per_capita, TaxaDesemprego)
print(top3_gini_simplificado)





tabela_menor_idh = Desafio_Prático_IV[order(Desafio_Prático_IV$IDH, decreasing = FALSE), ]

tabela_menor_pib = Desafio_Prático_IV[order(Desafio_Prático_IV$PIB_per_capita, decreasing = FALSE), ]















