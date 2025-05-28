
##############################################################################
#Essse script deve ser rodados apos o primeiro que ja sabemos qual a copula 
#marginais e parametros. Aqui rodamos o script pra cada mes, mas poderia ser 
#para o ano todo ou qualquer dada especifica.
#############################################################################

#############################
#Script copulas condicionais
#############################


####################
#Instalar os pacotes
####################

library(VineCopula)
library(readxl)
library(dplyr)
library(openxlsx)
library(moments)
library(ggplot2)

#options(scipen = 999)

###############
#Base de dados
##############

setwd("C:/Users/tuany/OneDrive/Área de Trabalho/dados Tese Tuany") #setar o caminho da sua
                                                                   #maquina
set.seed(123)  # Para reprodutibilidade

#######################################################
#Primeira base ENA E FC - Nordeste - dados historicos
#####################################################

base_hist <- read.xlsx("final_1.xlsx")

###############################################################
#Aqui comecamos a manipular a base conforme a nossa necessidade
###############################################################

#################################################
# Filtrar cada variável por mes da base historica
#################################################

##################################
# Loop para cada variável da base
##################################

filtrar_dados_hist <- function(base_hist) {
  dados_hist_filtrados <- list()
  
  for (var in names(base_hist)[-1]) {
    meses_list <- list()
    
    for (mes in 1:12) {
      dados_mensais <- base_hist[base_hist$Mes == mes, c("Mes", var)]
      meses_list[[paste("mes", mes, sep = "_")]] <- dados_mensais
    }
    
    dados_hist_filtrados[[var]] <- meses_list
  }
  
  return(dados_hist_filtrados)
}

dados_filtrados_hist <- filtrar_dados_hist(base_hist)

##########################################################################
#Segunda base dados de ENA do REE Nordeste Simulada pelo par-p do Newave
#########################################################################

base_sim = read_excel("dados_formatados.xlsx")

##############################
#Filtrar cada variável por mês
#############################

# Criar uma lista vazia dados_filtrados antes do loop
dados_filtrados <- list()

# Loop para cada variável da base
for (var in names(base_sim)[-1]) {  # Começamos do segundo coluna, pois a primeira é o mês
  # Criar uma lista vazia para cada mês
  meses_list <- list()
  
  # Loop para cada mês
  for (mes in 1:12) {
    # Filtrar os dados para o mês e a variável atual
    dados_mensais <- base_sim[base_sim$mes == mes, c("mes", var)]
    
    # Armazenar os dados na lista de meses
    meses_list[[paste("mes", mes, sep = "_")]] <- dados_mensais
  }
  
  # Armazenar a lista de meses para a variável atual na lista geral dados_filtrados
  dados_filtrados[[var]] <- meses_list
}


dados_filtrados$ENA_NORDESTE$mes_1 #mes_1 pq estamos fazendo para o mes de janeiro

################################
#Definir o número de simulações
################################

n_simulations <- 2000

###############################################################################
#Parâmetros da cópula encontrada no passo anterior - Essas informacoes estao no
#script anterior
###############################################################################

################################
#Fazendo para o mês de janeiro
###############################

hist_jan <- data.frame(ENA_Jan = as.numeric(dados_filtrados_hist$ENANORDESTE$mes_1[,-1]),
                       FC_Jan = as.numeric(dados_filtrados_hist$FC_ENANORDESTE$mes_1[,-1]))


sim_jan <- as.numeric(unlist(dados_filtrados$ENA_NORDESTE$mes_1[[2]]))


parametros$enanordeste$jan
melhor_dist$enanordeste$jan

################################################
#Bivariate copula: Joe (par = 1.13, tau = 0.07) 
################################################

par_copula <- 1.13

##########################################################################
# Calcular as probabilidades correspondentes à vazão incremental simulada
#variavel       distribuicao ks_estatistica p_valor param1 param2
#* <chr>          <chr>                 <dbl>   <dbl>  <dbl>  <dbl>
#  1 enanordeste    lnorm                0.0883   0.852   9.07  0.430
#2 fc_enanordeste beta                 0.0721   0.964  15.4  20.5  
##########################################################################

prob_ENA_simulada <- plnorm(sim_jan, meanlog =  9.07 , sdlog = 0.430)

##############################################################
#Simular o FC condicional à ENA simulada pelo par-p do Newave
##############################################################

sim_FC <- BiCopCondSim(N = n_simulations, 
                       cond.val = prob_ENA_simulada, 
                       cond.var = 1, 
                       family = 6,  #Joe
                       par = par_copula)

#########################
#Visualizar os resultados
#########################

summary(sim_FC)

#####################################
#Voltar para a escala original de FC
#####################################

sim_FC_original <- qbeta(sim_FC, shape1 = 15.4, shape2 = 20.5)

summary(sim_FC_original)

summary(dados_filtrados_hist$FC_ENANORDESTE$mes_1)

###########################
# Metricas Dados Simulados
##########################

# Função para calcular as métricas
calcular_metricas <- function(vetor) {
  min_val <- min(vetor, na.rm = TRUE)
  media_val <- mean(vetor, na.rm = TRUE)
  mediana_val <- median(vetor, na.rm = TRUE)
  max_val <- max(vetor, na.rm = TRUE)
  sd_val <- sd(vetor, na.rm = TRUE)
  cv_val <- sd_val / media_val
  assimetria_val <- skewness(vetor, na.rm = TRUE)
  curtose_val <- kurtosis(vetor, na.rm = TRUE)
  
  # Criar uma lista com as métricas
  metricas <- list(
    Mínimo = min_val,
    Média = media_val,
    Mediana = mediana_val,
    Máximo = max_val,
    `Desvio Padrão` = sd_val,
    `Coeficiente de Variação` = cv_val,
    Assimetria = assimetria_val,
    Curtose = curtose_val
  )
  
  return(metricas)
}


# Calcular métricas
metricas_sim <- calcular_metricas(sim_FC_original)

# Criar um dataframe para exportar
df_metricas <- as.data.frame(t(metricas_sim))

# Definir o caminho e nome do arquivo
caminho_arquivo <- "C:/Users/tuany/OneDrive/Área de Trabalho/dados Tese Tuany/Metricas_Sim_Jan.xlsx"

# Salvar o dataframe em um arquivo xlsx
write.xlsx(df_metricas, file = caminho_arquivo, row.names = FALSE)

cat("Métricas calculadas e salvas em", caminho_arquivo, "\n")

###########################
#Metricas Dados Historicos
##########################

# Extrair o vetor da variável FC_ENANORDESTE
variavel_hist <- dados_filtrados_hist$FC_ENANORDESTE$mes_1$FC_ENANORDESTE

# Função para calcular as métricas
calcular_metricas <- function(vetor) {
  min_val <- min(vetor, na.rm = TRUE)
  media_val <- mean(vetor, na.rm = TRUE)
  mediana_val <- median(vetor, na.rm = TRUE)
  max_val <- max(vetor, na.rm = TRUE)
  sd_val <- sd(vetor, na.rm = TRUE)
  cv_val <- sd_val / media_val
  assimetria_val <- skewness(vetor, na.rm = TRUE)
  curtose_val <- kurtosis(vetor, na.rm = TRUE)
  
  # Criar uma lista com as métricas
  metricas <- list(
    Mínimo = min_val,
    Média = media_val,
    Mediana = mediana_val,
    Máximo = max_val,
    `Desvio Padrão` = sd_val,
    `Coeficiente de Variação` = cv_val,
    Assimetria = assimetria_val,
    Curtose = curtose_val
  )
  
  return(metricas)
}

# Calcular métricas para a variável FC_ENANORDESTE
metricas_hist <- calcular_metricas(variavel_hist)

# Criar um dataframe para exportar
df_metricas_hist <- as.data.frame(t(metricas_hist))

# Definir o caminho e nome do arquivo
caminho_arquivo_hist <- "C:/Users/tuany/OneDrive/Área de Trabalho/dados Tese Tuany/Metricas_Hist_Jan.xlsx"

# Salvar o dataframe em um arquivo xlsx
write.xlsx(df_metricas_hist, file = caminho_arquivo_hist, row.names = FALSE)


cat("Métricas históricas calculadas e salvas em", caminho_arquivo_hist, "\n")

############
#Metricas
##########

# Função para calcular as métricas
calcular_metricas <- function(vetor) {
  min_val <- min(vetor, na.rm = TRUE)
  media_val <- mean(vetor, na.rm = TRUE)
  mediana_val <- median(vetor, na.rm = TRUE)
  max_val <- max(vetor, na.rm = TRUE)
  sd_val <- sd(vetor, na.rm = TRUE)
  cv_val <- sd_val / media_val
  assimetria_val <- skewness(vetor, na.rm = TRUE)
  curtose_val <- kurtosis(vetor, na.rm = TRUE)
  
  # Criar uma lista com as métricas
  metricas <- list(
    Mínimo = min_val,
    Média = media_val,
    Mediana = mediana_val,
    Máximo = max_val,
    `Desvio Padrão` = sd_val,
    `Coeficiente de Variação` = cv_val,
    Assimetria = assimetria_val,
    Curtose = curtose_val
  )
  
  return(metricas)
}

# Calcular métricas para a variável `sim_FC_original`
# Supondo que `sim_FC_original` seja um vetor de dados
metricas_sim <- calcular_metricas(sim_FC_original)
df_metricas_sim <- as.data.frame(t(metricas_sim))

# Calcular métricas para a variável `FC_ENANORDESTE`
variavel_hist <- dados_filtrados_hist$FC_ENANORDESTE$mes_1$FC_ENANORDESTE
metricas_hist <- calcular_metricas(variavel_hist)
df_metricas_hist <- as.data.frame(t(metricas_hist))

# Adicionar uma coluna identificadora
df_metricas_sim$Tipo <- "Simulado"
df_metricas_hist$Tipo <- "Histórico"

# Combinar os dois dataframes em um único
df_metricas_combinado <- rbind(df_metricas_sim, df_metricas_hist)

# Definir o caminho e nome do arquivo
caminho_arquivo_final <- "C:/Users/tuany/OneDrive/Área de Trabalho/dados Tese Tuany/Metricas_Hist_Sim_Jan.xlsx"

# Criar um novo arquivo xlsx
wb <- createWorkbook()
addWorksheet(wb, "Métricas")

# Escrever o dataframe combinado na aba
writeData(wb, sheet = "Métricas", df_metricas_combinado)

# Salvar o arquivo
saveWorkbook(wb, file = caminho_arquivo_final, overwrite = TRUE)

cat("Métricas simuladas e históricas calculadas e salvas em", caminho_arquivo_final, "\n")


#########################
#Para organizacao grafica
#########################

# Criar um data frame para os dados simulados e históricos
data_sim <- data.frame(ENA = sim_jan, FC = sim_FC_original, Type = "Simulation Data")
data_hist <- data.frame(ENA = dados_filtrados_hist$ENANORDESTE$mes_1[,-1], 
                        FC = dados_filtrados_hist$FC_ENANORDESTE$mes_1[,-1], Type = "Historical data")

# Unir os data frames simulados e históricos
data_combined <- rbind(data_sim, data_hist)

#######################
# Gráfico de dispercao
######################

ggplot(data_combined, aes(x = ENA, y = FC, color = Type)) +
  geom_point(alpha = 0.8, shape = 16, size = 3) +  # Aumentar o tamanho das bolinhas
  labs(title = "CF conditional on NAE - January",
       x = "NAE of ERR Nordeste (MWmed)", y = "Capacity Factor (p.u.)") +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_blank(),  # Remover as linhas de grade
    panel.grid.minor = element_blank(),  # Remover as linhas de grade menores
    axis.line = element_line(color = "black"),  # Adicionar linhas nos eixos X e Y
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = c("Simulation Data" = "gray", "Historical data" = "darkred")) +  # Alterar cores
  guides(color = guide_legend(override.aes = list(size = 5, shape = 16)))

#################
#Grafico Box-Plot
#################

box_jan =ggplot(data_combined, aes(x = Type, y = FC, fill = Type)) +
  geom_boxplot() +
  labs(title = "January",
       x = "Data Type", y = "Capacity Factor (p.u.)") +
  theme_minimal(base_size = 15) +
  scale_fill_manual(values = c("Simulation Data" = "gray", "Historical data" = "tomato3")) +
  ylim(0, 1)  # ou você pode usar scale_y_continuous(limits = c(0, 1))

box_jan





#######################
# Gráfico de densidade 
#######################

# Gráfico de densidade para ENA - Lembrando que essa ENA é do modelo par-p e NAO da copula
ggplot(data_combined, aes(x = ENA, fill = Type)) +
  geom_density(alpha = 0.5) +
  labs(title = "ENA Density - January",
       x = "ANE of ERR Nordeste (MWmed)", y = "Density") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "lightgrey", size = 0.5),
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  scale_fill_manual(values = c("Simulation Data" = "darkgray", "Historical data" = "darkred")) +
  guides(fill = guide_legend(override.aes = list(size = 5)))

# Gráfico de densidade para FC
ggplot(data_combined, aes(x = FC, fill = Type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Capacity Factor Density - January",
       x = "Capacity Factor (p.u.)", y = "Density") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "lightgrey", size = 0.5),
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  scale_fill_manual(values = c("Simulation Data" = "darkgray", "Historical data" = "darkred")) +
  guides(fill = guide_legend(override.aes = list(size = 5)))


##############################
# Gráfico de densidade para FC
###############################
ggplot(data_combined, aes(x = FC, fill = Type)) +
  geom_density(alpha = 0.6) +  # Aumentar a opacidade para cores mais vivas
  labs(title = "Capacity Factor Density - January",
       x = "Capacity Factor (p.u.)", 
       y = "Density") +
  theme_minimal(base_size = 15) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),  # Remover linhas de grade maiores
    panel.grid.minor = element_blank(),  # Remover linhas de grade menores
    axis.text = element_text(size = 14),  # Aumentar tamanho das fontes dos eixos
    axis.title = element_text(size = 16),  # Aumentar tamanho das fontes dos títulos dos eixos
    plot.title = element_text(size = 18, hjust = 0.5),  # Aumentar tamanho do título e centralizar
    legend.position = c(0.85, 0.85),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.background = element_blank(),  # Remover fundo da legenda
    legend.box.background = element_blank()  # Remover fundo da caixa da legenda
  ) +
  scale_fill_manual(values = c("Simulation Data" = "gray", "Historical data" = "darkred")) +  # Cores mais vivas
  guides(fill = guide_legend(override.aes = list(size = 5)))



