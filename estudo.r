### ======== Estudo de Câmbio:
###  elevador para desvalorização x escada para valorização

library(urca)
library(tseries)
library(GetBCBData)
library(dplyr)
library(ggplot2)
library(tidyr)
library(moments)
library(zoo)
library(writexl)
library(MSGARCH)
library(beepr)


### Definindo o diretório
choose.files()
diretorio <- choose.dir()
setwd(diretorio)
getwd()

####### ========== Coleta e tratamento dos dados ======= #########

### ============ 1. Coleta de dados
ptaxv <- GetBCBData::gbcbd_get_series(
    id = c("cambio" = 1),
    first.date = "2002-01-01",
    last.date = Sys.Date(),
    format.data = "wide"
)

colnames(ptaxv) <- c("data", "cambio")

tail(ptaxv)



### ============ 2. Tratamento dos dados
# criando a variável de retorno logarítmico
ptaxv <- ptaxv %>%
        arrange(data) %>%
        mutate(ret = c(NA, diff(log(cambio)))) %>%
        na.omit()

# criando a variável de retornos logaritmicos ao quadrado
ptaxv <- ptaxv %>%
        arrange(data) %>%
        mutate(retsq = ret^2) %>%
        na.omit()

# criando a variável de variação simples
ptaxv <- ptaxv %>%
        arrange(data) %>%
        mutate(variacao = c(NA, diff(cambio))) %>%
        na.omit()

head(ptaxv)




####### =========== Criação dos gráficos ======= #########


# Gráfico 1: Série PTAXV
p1 <- ggplot(ptaxv, aes(x = data, y = cambio)) +
    geom_line(color = "blue", linewidth = 0.5) +
    labs(title = "Evolução do PTAXV (2002 a 2025)", x = "Data", y = "PTAX (R$ por US$)") +
    theme_classic() +
    theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
)

print(p1)
ggsave("ptaxv.png", p1, width = 8, height = 6, dpi = 100, bg = "transparent")



# Gráfico 2: Log-retornos diários
p2 <- ggplot(ptaxv, aes(x = data, y = ret)) +
    geom_line(color = "#00474A", linewidth = 0.5) +
    labs(title = "Log-retornos diários do PTAXV", 
        x = "Data", 
        y = "Log Retornos de USD/BRL") +
    theme_classic() +
    theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
)

print(p2)
ggsave("logret_ptaxv.png", p2, width = 8, height = 6, dpi = 100, bg = "transparent")


# Gráfico 3: Variação simples diária
p3 <- ggplot(ptaxv, aes(x = data, y = variacao)) +
    geom_line(color = "#31f5d4", linewidth = 0.5) +
    labs(title = "Variação diária em R$ do PTAXV", 
        x = "Data", 
        y = "Variação diária em R$ do PTAXV") +
    theme_classic() +
    theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
)

print(p3)
ggsave("varsimples_ptaxv.png", p3, width = 8, height = 6, dpi = 100, bg = "transparent")


# Gráfico 4: Histograma com densidade (estilo R base)
# Calcular densidade e limites
dens <- density(ptaxv$ret)
x_lim <- range(ptaxv$ret, na.rm = TRUE)
y_lim_max <- max(dens$y) * 1.15

p4 <- ggplot(ptaxv, aes(x = ret)) +
    geom_histogram(aes(y = after_stat(density)), 
                    bins = 90, 
                    fill = "#777777", 
                    color = "white") +
    geom_density(color = "red", linewidth = 1) +
    scale_x_continuous(breaks = seq(floor(x_lim[1]*100)/100, 
                                ceiling(x_lim[2]*100)/100, 
                                by = 0.01)) +
    coord_cartesian(xlim = x_lim, ylim = c(0, y_lim_max)) +
    labs(title = "Log-retornos do câmbio (USD/BRL)",
        x = "Log-retornos diários",
        y = "Density") +
    theme_classic() +
    theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
)

print(p4)
ggsave("hist_logret.png", p4, width = 8, height = 6, dpi = 100, bg = "transparent")


# Gráfico 5: quadrado dos Log-retornos diários
p5 <- ggplot(ptaxv, aes(x = data, y = retsq*100)) +
    geom_line(color = "#0f2c8b", linewidth = 0.5) +
    labs(title = "Volatilidade (log-retornos ao quadrado*100)", 
        x = "Data", 
        y = "Variância*100") +
    theme_classic() +
    theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
)

print(p5)
ggsave("volret_ptaxv.png", p5, width = 8, height = 6, dpi = 100, bg = "transparent")







####### ======== Estatísticas descritivas dos log-retornos ======= ########
# Remover NAs
retornos <- na.omit(ptaxv$ret)

tail(retornos)

# Valores abaixo do 1º quartil (25% menores)
Q1 <- quantile(retornos, 0.25)
cauda_esquerda <- retornos[retornos < Q1]

# Valores acima do 3º quartil (25% maiores)
Q3 <- quantile(retornos, 0.75)
cauda_direita <- retornos[retornos > Q3]

# Comparar 10% piores quedas vs 10% maiores altas
perc_05 <- quantile(retornos, 0.05)  # 5% piores
perc_95 <- quantile(retornos, 0.95)  # 5% melhores

cat("5% piores retornos (média):", mean(retornos[retornos <= perc_05]), "\n")
cat("5% melhores retornos (média):", mean(retornos[retornos >= perc_95]), "\n")

# Criar tabela com estatísticas
stats <- data.frame(
        Estatistica = c(
    "Mínimo",
    "1º Quartil",
    "Mediana",
    "Média",
    "3º Quartil",
    "Máximo",
    "Desvio Padrão",
    "Assimetria",
    "Curtose", "Média Cauda Esquerda (25% menores)",
    "Média Cauda Direita (25% maiores)"),
        Valor = c(
    min(retornos),
    quantile(retornos, 0.25),
    median(retornos),
    mean(retornos),
    quantile(retornos, 0.75),
    max(retornos),
    sd(retornos),
    skewness(retornos),
    kurtosis(retornos),
    mean(cauda_esquerda),
    mean(cauda_direita)
    )
)

# Visualizar no R
print(stats)

# Criar tabela de distribuição de sinais
resumo_sinal <- data.frame(
  Categoria = c("Negativos (<0)", "Positivos (>0)", "Zero (=0)"),
  Quantidade = c(sum(retornos < 0), sum(retornos > 0), sum(retornos == 0)),
  Percentual = c(
    mean(retornos < 0) * 100,
    mean(retornos > 0) * 100,
    mean(retornos == 0) * 100
  )
)

print(resumo_sinal)

# Salvar em Excel com múltiplas planilhas
write_xlsx(
  list(
    "Estatisticas_Descritivas" = stats,
    "Distribuicao_Sinais" = resumo_sinal
  ),
  "estatisticas_descritivas_ptaxv.xlsx"
)


###### ======= Preparando para o GARCH ======== #######
#Testes de estacionariedade
adf_test <- ur.df(ptaxv$ret, type = "none", selectlags = "AIC")
summary(adf_test)

estacionariedade_txt <- capture.output(summary(adf_test))
writeLines(estacionariedade_txt, "estacionariedade.txt")


# ACF dos log-retornos - visualizar e salvar
acf(retornos, main = "ACF dos log-retornos")
dev.copy(png, "acf_logret.png", width = 800, height = 600, bg = "transparent")
dev.off()

# PACF dos log-retornos - visualizar e salvar
pacf(retornos, main = "PACF dos log-retornos")
dev.copy(png, "pacf_logret.png", width = 800, height = 600, bg = "transparent")
dev.off()

# ACF dos log-retornos ao quadrado - visualizar e salvar
acf(retornos^2, main = "ACF dos log-retornos ao quadrado")
dev.copy(png, "acf_logret_squared.png", width = 800, height = 600, bg = "transparent")
dev.off()

# PACF dos log-retornos ao quadrado - visualizar e salvar
pacf(retornos^2, main = "PACF dos log-retornos ao quadrado")
dev.copy(png, "pacf_logret_squared.png", width = 800, height = 600, bg = "transparent")
dev.off()
dev.off()



######## ======== Modelagem MSGARCH ======= #######

### Especificação do Modelo

# Garantindo que "retornos" é um vetor numérico simples
retornos_vec <- as.numeric(retornos)

spec_ms <- MSGARCH::CreateSpec(
    variance.spec = list(model = c("sGARCH")), # GARCH(1,1) padrão
    distribution.spec = list(distribution = c("std")),  # t-Student
    switch.spec = list(do.mix = FALSE, K = 2) # 2 regimes
)

# Imprimir a especificação para verificar
print(spec_ms)


### Estimação do Modelo

# A função FitML implementa a Estimação por Máxima Verossimilhança (MLE).
fit_ms <- FitML(spec = spec_ms, data = retornos_vec)

# Exibimos o sumário dos resultados
summary(fit_ms)

# Capturar como texto
resultado_txt <- capture.output(summary(fit_ms))
writeLines(resultado_txt, "summary_fit_ms.txt")

# Criar dataframe simples
summary_df <- data.frame(
  Output = resultado_txt
)

# Adicionar ao Excel existente
write_xlsx(
  list(
    "Estatisticas_Descritivas" = stats,
    "Distribuicao_Sinais" = resumo_sinal,
    "Summary_MSGARCH" = summary_df  # Nova aba
  ),
  "estatisticas_descritivas_ptaxv.xlsx"
)

# Repetindo o sumário para visualização
summary(fit_ms)


###### ======== Visualização dos Regimes ======= ########

# Extrair o array 3D de probabilidades suavizadas
prob_array <- State(object = fit_ms)$SmoothProb

# Corrigir o problema de dimensão e alinhar o número de observações
# Removemos a primeira linha (t=0) e selecionamos a única simulação ("draw #1")
#                   [para criar uma matriz 2D com as probabilidades corretas.
prob_matriz <- prob_array[-1, 1, ]

# Adicionar estas probabilidades ao nosso data frame original 'ptaxv'
# Agora 'prob_matriz' é uma matriz 2D e o código funcionará.
ptaxv$prob_regime1 <- prob_matriz[, 1]
ptaxv$prob_regime2 <- prob_matriz[, 2]

# Gráfico 6.: probabilidade do Regime de Crise (Regime 2)
p6 <- ggplot(ptaxv, aes(x = data, y = prob_regime2)) +
    geom_line(color = "darkred", linewidth = 0.6) +
    labs(
        title = "Probabilidade de se Estar no Regime de Crise (Alta Volatilidade)",
        x = "Data",
        y = "Probabilidade"
    ) +
    theme_classic() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
    )

print(p6)
ggsave("prob_crise.png", p6, width = 8, height = 6, dpi = 100, bg = "transparent")


####### Gráfico 7: Visualização Combinada (Preço do Câmbio e Regimes de Crise)
# 1. Criar um novo data frame contendo apenas os dias de alta probabilidade de crise
periodos_crise <- ptaxv %>%
    filter(prob_regime2 > 0.5)

# 2. Gerar o gráfico
p7 <- ggplot(ptaxv, aes(x = data)) +
    
    # Camada 1: Regiões Sombreadas APENAS para os períodos de crise (>50%)
    geom_rect(
        data = periodos_crise,
        aes(xmin = data - 0.5, xmax = data + 0.5, ymin = -Inf, ymax = Inf),
        fill = "red",
        alpha = 0.4
    ) +
    
    # Camada 2: Linha do Câmbio (usando o data frame completo)
    geom_line(aes(y = cambio), color = "blue", linewidth = 0.6) +
    
    # Legendas e Títulos
    labs(
        title = "Evolução do Câmbio e Períodos de Crise",
        subtitle = "A área sombreada destaca os dias em que o modelo identifica o regime de crise com alta confiança",
        x = "Data",
        y = "PTAX (R$ por US$)"
    ) +
    
    # Estética e Configurações
    theme_classic() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
        # A legenda não é mais necessária, pois a informação está no título/subtítulo
    )

print(p7)
ggsave("grafico7_crise_50pct.png", p7, width = 8, height = 6, dpi = 150, bg = "transparent")


# Gráfico 8: Sobreposição do Retorno Diário com Regimes de Crise
p8 <- ggplot(ptaxv, aes(x = data)) +
    
    # Camada 1: Regiões Sombreadas APENAS para os períodos de crise (>75%)
    # Note que agora usamos o data frame 'periodos_crise' nesta camada.
    # O alpha é um valor fixo, pois não há mais gradualismo.
    geom_rect(
        data = periodos_crise,
        aes(xmin = data - 0.5, xmax = data + 0.5, ymin = -Inf, ymax = Inf),
        fill = "red",
        alpha = 0.4
    ) +
    
    # Camada 2: Linha do Câmbio (usando o data frame completo)
    geom_line(aes(y = ret), color = "#00474A", linewidth = 0.5) +
    
    # Legendas e Títulos
    labs(
        title = "Log-Retornos diários do Câmbio e Períodos de Crise",
        subtitle = "A área sombreada destaca os dias em que o modelo identifica o regime de crise com alta confiança",
        x = "Data",
        y = "Log Retornos de USD/BRL"
    ) +
    
    # Estética e Configurações
    theme_classic() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
        # A legenda não é mais necessária, pois a informação está no título/subtítulo
    )

print(p8)
ggsave("grafico8_crise_50pct.png", p8, width = 10, height = 7, dpi = 150, bg = "transparent")




###### ======= Análise Ex-Post das Médias dos Regimes ===== #######

# Classificar cada dia no regime mais provável
ptaxv <- ptaxv %>%
    mutate(regime_classificado = ifelse(prob_regime2 > 0.5, "Crise (R2)", "Normal (R1)"))

# Calcular a média dos retornos para cada regime classificado
medias_por_regime <- ptaxv %>%
    group_by(regime_classificado) %>%
    summarise(
        media_retornos = mean(ret),
        desvio_padrao = sd(ret),
        n_observacoes = n()
    )

print("Médias e Estatísticas por Regime Classificado:")
print(medias_por_regime)

# Realizar um teste t formal para a diferença de médias
teste_t <- t.test(ret ~ regime_classificado, data = ptaxv)

print("Teste t para a Diferença de Médias entre Regimes:")
print(teste_t)




# Capturar resultado do teste t como texto
teste_t_output <- capture.output(print(teste_t))

# Criar dataframe do teste t
teste_t_df <- data.frame(
  Output = teste_t_output
)

# Atualizar Excel com as novas análises
write_xlsx(
  list(
    "Estatisticas_Descritivas" = stats,
    "Distribuicao_Sinais" = resumo_sinal,
    "Summary_MSGARCH" = summary_df,
    "Medias_por_Regime" = medias_por_regime,
    "Teste_t_Regimes" = teste_t_df
  ),
  "estatisticas_descritivas_ptaxv.xlsx"
)

beep(2)
