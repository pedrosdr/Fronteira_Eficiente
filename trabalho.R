setwd('C:/Users/pedro/Documents/Finanças e Negócios/Financas/Trabalho')

library(quantmod)
library(tidyverse)
library(flextable)
library(ggcorrplot)
library(ggrepel)


start_date = "2020-05-08"
end_date = "2024-06-08"

# Definir o período de análise
start_date <- as.Date(start_date)
end_date <- as.Date(end_date)

# Coletar dados históricos dos ativos
tickers <- c("^BVSP", "WEGE3.SA", "PSSA3.SA", "TAEE11.SA", "PETR4.SA", "ITSA4.SA")
getSymbols(tickers, src = "yahoo", from = start_date, to = end_date)
tickers <- c("BVSP", "WEGE3.SA", "PSSA3.SA", "TAEE11.SA", "PETR4.SA", "ITSA4.SA")

# Combinar os dados em um único dataframe
prices <- do.call(merge, lapply(tickers, function(ticker) Ad(get(ticker))))
colnames(prices) <- tickers
prices = as.data.frame(prices)

# Printar dados Históricos
pricesHistoricos = prices
pricesHistoricos$index = 1:nrow(pricesHistoricos)
dadosHistoricosJuntos = pivot_longer(pricesHistoricos, 1:(ncol(pricesHistoricos)-1), names_to = 'Ativos', values_to = 'Fechamento')
ggplot() +
  theme_light() +
  geom_line(data=dadosHistoricosJuntos %>% filter(Ativos != 'BVSP'), aes(x=index, y=Fechamento, color=Ativos), linewidth=0.9) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.border = element_blank(),
    axis.title.y = element_text(margin = margin(r=15)),
    axis.title.x = element_text(margin = margin(t=15)),
    legend.title = element_blank(),
    legend.position = 'bottom'
  ) +
  scale_y_continuous(labels = function(x) gsub('\\.', ',', sprintf("R$ %.2f", x))) +
  scale_x_continuous(n.breaks = 10) +
  ylab('Fechamento do ativo') +
  xlab('Dias transcorridos desde a data de início') +
  scale_color_manual(
    breaks=c("WEGE3.SA",  "PSSA3.SA",  "TAEE11.SA", "PETR4.SA",  "ITSA4.SA" ),
    values=c(c('#f564e3', '#00bfc4', '#619cff', '#00ba38', '#b79f00'))
  )

ggsave('img/historico.png')

# Calcular os retornos mensais
returns <- na.omit(ROC(prices, type = "discrete", n=30))
returns1 = returns
returns1$index = 1:nrow(returns1)
returnsPivot = pivot_longer(returns1, 1:(ncol(returns1)-1), names_to = 'Ativo', values_to = 'retorno')

# Printando os retornos mensais
ggplot() +
  theme_light() +
  geom_line(data=returnsPivot, aes(x=index, y=retorno, color=Ativo), linewidth=0.9) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.border = element_blank(),
    axis.title.y = element_text(margin = margin(r=15)),
    axis.title.x = element_text(margin = margin(t=15)),
    legend.title = element_blank(),
    legend.position = 'right'
  ) +
  scale_y_continuous(labels = function(x) gsub('\\.', ',', sprintf("%.2f%%", 100*x))) +
  scale_x_continuous(n.breaks = 10) +
  ylab('Retorno mensal do ativo') +
  xlab('Dias transcorridos desde a data de início') +
  scale_color_discrete(
    name='Empresa',
    labels=c("Ibovespa", "Itaúsa", "Petrobras", "Porto Seguro", "Taesa", "Weg"))
ggsave('img/retornos.png')

# Calculando as estatísticas descritivas
df_stats = returnsPivot  %>%
  group_by(Ativo) %>%
  reframe(
    'Média' = sprintf("%.3f", mean(retorno)),
    'Desvio Padrão' = sprintf("%.3f", sd(retorno)),
    'Variância' = sprintf("%.3f", var(retorno)),
    'Mediana' = sprintf("%.3f", median(retorno)),
    'Intervalo interquartil' = sprintf("%.3f", IQR(retorno))
  )
flextable(df_stats)

# Fazendo o boxplot dos ativos
ggplot(returnsPivot) +
  theme_light() +
  stat_boxplot( aes(x=Ativo, y=retorno), geom='errorbar', linetype=1, width=0.5) +
  geom_boxplot(aes(x=Ativo, y=retorno, fill=Ativo)) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.border = element_blank(),
    axis.title.y = element_text(margin = margin(r=15)),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = 'none',
    axis.ticks = element_blank()
  ) +
  scale_y_continuous(labels = function(x) gsub('\\.', ',', sprintf("%.1f%%", 100*x)), n.breaks = 10) +
  scale_x_discrete(labels = c("Ibovespa", "Itaúsa", "Petrobras", "Porto Seguro", "Taesa", "Weg")) +
  ylab('Retorno do ativo')
ggsave('img/boxplot_retornos.png')

# Fazendo as correlações entre os retornos
ggcorrplot(
  cor(returns), 
  type = 'lower', 
  lab = TRUE,
  colors = c('#de3507', '#ffffff', '#15e30e')
) +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank()
  )
ggsave('img/cor.png')

# Distribuições dos retornos
colors = c('#f8766d', '#f564e3', '#00bfc4', '#619cff', '#00ba38', '#b79f00')
for(i in 1:length(tickers)) {
  
  plot = ggplot() +
    theme_light() +
    geom_histogram(aes(x=returns[,tickers[i]], y=after_stat(density)), fill=colors[i], bins=15) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      text = element_text(size=15)
    ) +
    geom_vline(xintercept = 0, linetype='dashed', linewidth=1) +
    xlab(paste0('Retorno ', gsub('\\.SA', '', tickers[i]))) +
    ylab('Densidade de Probabilidade') +
    scale_y_continuous(label = function(x) sprintf("%.2f", x)) +
    scale_x_continuous(label = function(x) sprintf("%.1f%%", x*100))
  print(plot)
  ggsave(paste0('img/hist_',tolower(gsub('\\.SA', '', tickers[i])),'.png'))
  
}

# Regressões
colors = c('#f564e3', '#00bfc4', '#619cff', '#00ba38', '#b79f00')
for(i in 2:length(tickers)) {
  retornoX = na.omit(returns[,'BVSP'])
  posicaoX = max(retornoX) - 0.45 * (max(retornoX) - min(retornoX))
  
  model = lm(paste0(tickers[i], '~BVSP'),data=returns)
  sumModel = summary(model)
  legend = sprintf(
    "%s = %.3f + %.3f %s\nR²: %.2f\np-valor (α): %.4f\np-valor (β): %.4f",
    tolower(gsub('\\.SA', '', tickers[i])), 
    model$coefficients[1], 
    model$coefficients[2], 
    'bvsp',
    sumModel$r.squared,
    sumModel$coefficients[1,4],
    sumModel$coefficients[2,4])
  
  plot = ggplot() +
    theme_light() +
    geom_point(aes(y=returns[,tickers[i]], x=returns[,tickers[1]]), color=colors[i-1]) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      text = element_text(size=15)
    ) +
    ylab(paste0('Retorno ', gsub('\\.SA', '', tickers[i]))) +
    xlab('Retorno Ibovespa') +
    geom_smooth(aes(y=returns[,tickers[i]], x=returns[,tickers[1]]), method='lm', color='red') +
    scale_y_continuous(label = function(x) sprintf("%.1f%%", x*100)) +
    scale_x_continuous(label = function(x) sprintf("%.1f%%", x*100)) +
    annotate("text", x=posicaoX, y=-0.10, label=legend, size=4, hjust=0)
  print(plot)
  ggsave(paste0('img/reg_',tolower(gsub('\\.SA', '', tickers[i])),'.png'))
  
}

# model = lm('WEGE3.SA~BVSP',data=returns)
# plot(model)

# Hipóteses de Retornos
# Assumimos que os retornos esperados, variâncias e correlações dos ativos se mantêm estáveis durante o período analisado.


# Incorporando a Selic
selic = read.csv('selic.csv', sep = ';')
returnsRf = returns
returnsRf$data = rownames(returnsRf)
returnsRf = merge(returnsRf, selic, by='data')
returnsRf = returnsRf %>% rename(selic = valor)

# Convertendo para taxa mensal
selic = returnsRf$selic
selic = as.numeric(gsub(',', '.', selic))
selic = (selic/100 + 1)^22 - 1

returnsRf$selic = selic

# Subtraindo a selic dos retornos
returnsRf = returns - returnsRf$selic



# Refazendo as estatísticas com os retornos subtraidos da selic
returns1 = returnsRf
returns1$index = 1:nrow(returns1)
returnsPivot = pivot_longer(
  returns1, 1:(ncol(returns1)-1), names_to = 'Ativo', values_to = 'retorno')

# Printando os retornos mensais
ggplot() +
  theme_light() +
  geom_line(data=returnsPivot, aes(x=index, y=retorno, color=Ativo), linewidth=0.9) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.border = element_blank(),
    axis.title.y = element_text(margin = margin(r=15)),
    axis.title.x = element_text(margin = margin(t=15)),
    legend.title = element_blank(),
    legend.position = 'bottom'
  ) +
  scale_y_continuous(labels = function(x) gsub('\\.', ',', sprintf("%.2f%%", 100*x))) +
  scale_x_continuous(n.breaks = 10) +
  ylab('Retorno mensal do ativo') +
  xlab('Dias transcorridos desde a data de início') +
  scale_color_discrete(
    name='Empresa',
    labels=c("Ibovespa", "Itaúsa", "Petrobras", "Porto Seguro", "Taesa", "Weg"))
ggsave('img/retornosRf.png')

df_stats = returnsPivot  %>%
  group_by(Ativo) %>%
  reframe(
    'Média' = sprintf("%.3f", mean(retorno)),
    'Desvio Padrão' = sprintf("%.3f", sd(retorno)),
    'Variância' = sprintf("%.3f", var(retorno)),
    'Mediana' = sprintf("%.3f", median(retorno)),
    'Intervalo interquartil' = sprintf("%.3f", IQR(retorno))
  )
flextable(df_stats)

# Fazendo o boxplot dos ativos
ggplot(returnsPivot) +
  theme_light() +
  stat_boxplot( aes(x=Ativo, y=retorno), geom='errorbar', linetype=1, width=0.5) +
  geom_boxplot(aes(x=Ativo, y=retorno, fill=Ativo)) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.border = element_blank(),
    axis.title.y = element_text(margin = margin(r=15)),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = 'none',
    axis.ticks = element_blank()
  ) +
  scale_y_continuous(labels = function(x) gsub('\\.', ',', sprintf("%.1f%%", 100*x)), n.breaks = 10) +
  scale_x_discrete(labels = c("Ibovespa", "Itaúsa", "Petrobras", "Porto Seguro", "Taesa", "Weg")) +
  ylab('Retorno do ativo')
ggsave('img/boxplot_retornosRf.png')

# Fazendo as correlações entre os retornos
ggcorrplot(
  cor(returnsRf), 
  type = 'lower', 
  lab = TRUE,
  colors = c('#de3507', '#ffffff', '#15e30e')
) +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank()
  )
ggsave('img/corRf.png')

# Distribuições dos retornos
colors = c('#f8766d', '#f564e3', '#00bfc4', '#619cff', '#00ba38', '#b79f00')
for(i in 1:length(tickers)) {
  
  plot = ggplot() +
    theme_light() +
    geom_histogram(aes(x=returnsRf[,tickers[i]], y=after_stat(density)), fill=colors[i], bins=15) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      text = element_text(size=15)
    ) +
    geom_vline(xintercept = 0, linetype='dashed', linewidth=1) +
    xlab(paste0('Retorno ', gsub('\\.SA', '', tickers[i]))) +
    ylab('Densidade de Probabilidade') +
    scale_y_continuous(label = function(x) sprintf("%.2f", x)) +
    scale_x_continuous(label = function(x) sprintf("%.1f%%", x*100))
  print(plot)
  ggsave(paste0('img/histRf_',tolower(gsub('\\.SA', '', tickers[i])),'.png'))
  
}

# Regressões
colors = c('#f564e3', '#00bfc4', '#619cff', '#00ba38', '#b79f00')
for(i in 2:length(tickers)) {
  retornoX = na.omit(returnsRf[,'BVSP'])
  posicaoX = max(retornoX) - 0.3 * (max(retornoX) - min(retornoX))
  
  model = lm(paste0(tickers[i], '~BVSP'),data=returnsRf)
  sumModel = summary(model)
  legend = sprintf(
    "%s = %.3f + %.3f %s\nR²: %.2f\np-valor (α): %.4f\np-valor (β): %.4f",
    tolower(gsub('\\.SA', '', tickers[i])), 
    model$coefficients[1], 
    model$coefficients[2], 
    'bvsp',
    sumModel$r.squared,
    sumModel$coefficients[1,4],
    sumModel$coefficients[2,4])
  
  plot = ggplot() +
    theme_light() +
    geom_point(aes(y=returnsRf[,tickers[i]], x=returnsRf[,tickers[1]]), color=colors[i-1]) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      text = element_text(size=15)
    ) +
    ylab(paste0('Retorno ', gsub('\\.SA', '', tickers[i]))) +
    xlab('Retorno Ibovespa') +
    geom_smooth(aes(y=returnsRf[,tickers[i]], x=returnsRf[,tickers[1]]), method='lm', color='red') +
    scale_y_continuous(label = function(x) sprintf("%.1f%%", x*100)) +
    scale_x_continuous(label = function(x) sprintf("%.1f%%", x*100)) +
    annotate("text", x=posicaoX, y=-0.10, label=legend, size=3, hjust=0)
  print(plot)
  ggsave(paste0('img/regRf_',tolower(gsub('\\.SA', '', tickers[i])),'.png'))
}


# Removendo o bvsp
returns = returnsRf[,2:ncol(returnsRf)]

# Calculando a fronteira eficiente

# covariancias e correlações
cov_returns = cov(returns)
cor_returns = cor(returns)

# números de ativos e carteiras
num_ativos = ncol(returns)
num_carteiras = 10000

# criando o dataframe dos dados
dados = data.frame()

# criando o dataframe dos pesos
dfPesos = data.frame()
for(i in 1:num_carteiras) {
  pesos = runif(num_ativos)
  pesos = pesos / sum(pesos)
  
  # retorno esperado
  retornoEsperado = as.numeric(pesos %*% as.matrix(colMeans(returns)))
  
  # volatilidade
  volatilidade = as.numeric(sqrt(t(pesos) %*% (cov_returns %*% pesos)))
  
  # Índice de Sharpe
  sharpe = retornoEsperado / volatilidade
  
  # Criando dataframe com os dados
  dados_i = data.frame(
    RetornoEsperado = retornoEsperado,
    Volatilidade = volatilidade,
    Sharpe = sharpe
  )
  
  dados = if(nrow(dados) == 0) dados_i else rbind(dados, dados_i)
  
  # Concatenando os pesos
  dfPesos_i = as.data.frame(matrix(pesos, ncol = length(pesos)))
  colnames(dfPesos_i) = colnames(returns)
  dfPesos = if(nrow(dfPesos) == 0) dfPesos_i else rbind(dfPesos, dfPesos_i)
}
dados = cbind(dados, dfPesos)

# Carteira de maior sharpe
carteiraOtima = dados %>% filter(Sharpe == max(Sharpe))

# Carteira de menor variância
carteiraMenorVariancia = dados %>% filter(Volatilidade == min(Volatilidade))


# Montando as carteiras dos ativos
carteirasAtivos = data.frame()
for(i in 1:ncol(returns)) {
  pesos = rep(0, ncol(returns))
  pesos[i] = 1
  
  # retorno esperado
  retornoEsperado = as.numeric(pesos %*% as.matrix(colMeans(returns)))
  
  # volatilidade
  volatilidade = as.numeric(sqrt(t(pesos) %*% (cov_returns %*% pesos)))
  
  # Índice de Sharpe
  sharpe = retornoEsperado / volatilidade
  
  # Criando dataframe com os dados
  dados_i = data.frame(
    Ativo = gsub('\\.SA', '', colnames(returns)[i]),
    RetornoEsperado = retornoEsperado,
    Volatilidade = volatilidade,
    Sharpe = sharpe
  )
  
  carteirasAtivos = if(nrow(carteirasAtivos) == 0) dados_i else rbind(carteirasAtivos, dados_i)
}

# Calculando a selic Esperada
selicEsperada = mean(selic)

# linha da carteira livre de risco
dfLinha = data.frame(
  RetornoEsperado = c(selicEsperada, carteiraOtima$RetornoEsperado),
  Volatilidade = c(0, carteiraOtima$Volatilidade)
)

# Fazendo uma regressão para montar a linha
modelLinha = lm('RetornoEsperado~Volatilidade', data=dfLinha)

# Montando a linha
linha = data.frame(Volatilidade=runif(100, -0.01, 0.12))
retornoLinha = predict(modelLinha, newdata = linha)
linha$RetornoEsperado = retornoLinha

# Plotando a Fronteira eficiente
ggplot() +
  theme_light() +
  geom_point(data=dados, aes(x=Volatilidade, y=RetornoEsperado), color='#cccccc', size=3) +
  
  geom_line(data=linha, aes(x=Volatilidade, y=RetornoEsperado), linewidth=1, linetype='dashed', color='#3ca8f0') +
  
  geom_point(data = carteiraOtima, aes(x=Volatilidade, y=RetornoEsperado), size=5, shape=21, fill='#d11b33') +
  geom_label_repel(data = carteiraOtima, aes(x=Volatilidade, y=RetornoEsperado), label='Carteira de maior Sharpe', box.padding = 4) +
  
  geom_point(data = carteiraMenorVariancia, aes(x=Volatilidade, y=RetornoEsperado), size=5, shape=21, fill='#d11b33') +
  geom_label_repel(data = carteiraMenorVariancia, aes(x=Volatilidade, y=RetornoEsperado), label='Carteira de menor variância', box.padding = 5) +
  
  geom_point(data = carteirasAtivos, aes(x=Volatilidade, y=RetornoEsperado, fill=Ativo), size=4, shape=21) +
  
  geom_vline(xintercept = 0, linewidth=1.5, color="#555555") +
  geom_hline(yintercept = 0, linewidth=1.5, color="#555555") +
  
  ylab('Retorno Esperado') +
  xlab('Volatilidade') +
  
  scale_x_continuous(n.breaks = 7, labels = function(x) gsub('\\.', ',', sprintf("%.2f%%", x*100))) +
  scale_y_continuous(n.breaks = 15, labels = function(x) gsub('\\.', ',', sprintf("%.2f%%", x*100))) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(margin = margin(r=15)),
    axis.title.x = element_text(margin = margin(t=15)),
    legend.title = element_blank(),
    legend.position = 'bottom'
  ) +
  scale_fill_manual(
    breaks=c("WEGE3",  "PSSA3",  "TAEE11", "PETR4",  "ITSA4" ),
    values=c(c('#f564e3', '#00bfc4', '#619cff', '#00ba38', '#b79f00'))
  )
ggsave('img/fronteira.png')


# Gráfico de donut da carteira ótima
donutOtima = pivot_longer(carteiraOtima, 4:ncol(carteiraOtima), values_to = 'Percentual', names_to = 'Ativo')

ggplot(donutOtima) +
  theme_light() +
  
  scale_y_continuous(n.breaks = 7, labels = function(x) sprintf("%.0f%%", x*100)) +
  
  geom_col(aes(x=reorder(Ativo, -Percentual), y=Percentual, fill=Ativo)) +
  geom_label(aes(x=reorder(Ativo, -Percentual), y=Percentual, label=gsub('\\.', ',', sprintf("%.1f%%", Percentual*100)))) +
  
  ylab('Percentual de alocação') +
  xlab('Ativo') +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(margin = margin(r=15)),
    axis.title.x = element_text(margin = margin(t=15)),
    legend.position = 'none'
  ) +
  scale_fill_manual(
    breaks=c("WEGE3.SA",  "PSSA3.SA",  "TAEE11.SA", "PETR4.SA",  "ITSA4.SA" ),
    values=c(c('#f564e3', '#00bfc4', '#619cff', '#00ba38', '#b79f00'))
  )
ggsave('img/perc_otima.png')

# Gráfico de donut da carteira de menor Variancia
donutMenorVariancia = pivot_longer(carteiraMenorVariancia, 4:ncol(carteiraMenorVariancia), values_to = 'Percentual', names_to = 'Ativo')

ggplot(donutMenorVariancia) +
  theme_light() +
  
  scale_y_continuous(n.breaks = 7, labels = function(x) sprintf("%.0f%%", x*100)) +
  
  geom_col(aes(x=reorder(Ativo, -Percentual), y=Percentual, fill=Ativo)) +
  geom_label(aes(x=reorder(Ativo, -Percentual), y=Percentual, label=gsub('\\.', ',', sprintf("%.1f%%", Percentual*100)))) +
  
  ylab('Percentual de alocação') +
  xlab('Ativo') +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(margin = margin(r=15)),
    axis.title.x = element_text(margin = margin(t=15)),
    legend.position = 'none'
  ) +
  scale_fill_manual(
    breaks=c("WEGE3.SA",  "PSSA3.SA",  "TAEE11.SA", "PETR4.SA",  "ITSA4.SA" ),
    values=c(c('#f564e3', '#00bfc4', '#619cff', '#00ba38', '#b79f00'))
  )
ggsave('img/perc_menor_variancia.png')

# Juntando as carteiras
carteirasOtimas = rbind(carteiraOtima[1:3], carteiraMenorVariancia[1:3])
flextable(carteirasOtimas)
