#########################################
#Script para gerar o grafico de cenarios
#########################################

#########
#Pacotes
#########

library(openxlsx)
library(ggplot2)
library(dplyr)

##########################################
#Bases - Manipuladas para gerar o grafico
#########################################

base_hist<- read.xlsx("final_1.xlsx")
base_sim <- read.xlsx("dados_consolidados.xlsx")

#######################################
#Verificar a estrutura da base de dados
#######################################

head(base_sim)

###################################
#Comecando a construcao grafica FC
##################################

###########################################
#Calcular a média das simulações e quantis
##########################################
mean_data <- base_sim %>%
  group_by(Mes) %>%
  summarise(mean_sim_FC_original = mean(sim_FC_original))

quantiles_data <- base_sim %>%
  group_by(Mes) %>%
  summarise(
    quantile_5 = quantile(sim_FC_original, 0.05),
    quantile_95 = quantile(sim_FC_original, 0.95)
  )

####################################
# Calcular a média histórica por mês
####################################

mean_hist_data <- base_hist %>%
  group_by(Mes) %>%
  summarise(mean_hist_FC_ENANORDESTE = mean(FC_ENANORDESTE))

#########
#Salvando
#########

#######################################
# Definir o caminho e nome do arquivo
######################################

caminho_arquivo_final <- "C:/Users/tuany/OneDrive/Área de Trabalho/dados Tese Tuany/mean_data.xlsx"

#############################
# Criar um novo arquivo xlsx
#############################
wb <- createWorkbook()
addWorksheet(wb, "mean_data ")

#######################################
# Escrever o dataframe combinado na aba
#######################################
writeData(wb, sheet = "mean_data ", mean_data )

###################
# Salvar o arquivo
###################
saveWorkbook(wb, file = caminho_arquivo_final, overwrite = TRUE)

#####################
# Criar o gráfico FC
#####################

ggplot() +
  # Add scenario lines in light blue
  geom_line(data = base_sim, aes(x = Mes, y = sim_FC_original, group = Cenario), color = "gray", alpha = 0.5) +
  # Add mean simulation line in purple
  geom_line(data = mean_data, aes(x = Mes, y = mean_sim_FC_original, color = "Simulated Mean"), size = 0.75, linetype = "dashed") +
  # Add 5% percentile line in green
  geom_line(data = quantiles_data, aes(x = Mes, y = quantile_5, color = "5th Percentile"), size = 0.75, linetype = "dashed") +
  # Add 95% percentile line in orange
  geom_line(data = quantiles_data, aes(x = Mes, y = quantile_95, color = "95th Percentile"), size = 0.75, linetype = "dashed") +
  # Add historical mean line in black
  geom_line(data = mean_hist_data, aes(x = Mes, y = mean_hist_FC_ENANORDESTE, color = "Historical Mean"), size = 0.75, linetype = "dashed") +
  # Adjust axes and labels
  scale_x_continuous(breaks = 1:12, labels = paste0("Month ", 1:12)) +
  labs(x = "Months", y = "Capacity Factor (p.u.)", title = "Scenario") +
  scale_color_manual(
    name = "Legend",
    values = c(
      "Simulated Mean" = "darkred",
      "5th Percentile" = "tomato",
      "95th Percentile" = "tomato",
      "Historical Mean" = "black"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Place the legend on the right
    legend.title = element_blank(),  # Remove the legend title
    legend.background = element_rect(fill = "white", color = "black"),  # Add a border to the legend
    legend.key.size = unit(0.99, "cm")  # Size of the legend keys
  )

###############################
# Grafico de cenarios para ENA
###############################

# Calculate the mean of simulations and quantiles
mean_data <- base_sim %>%
  group_by(Mes) %>%
  summarise(mean_sim_FC_original = mean(ENA_NORDESTE))

quantiles_data <- base_sim %>%
  group_by(Mes) %>%
  summarise(
    quantile_5 = quantile(ENA_NORDESTE, 0.05),
    quantile_95 = quantile(ENA_NORDESTE, 0.95)
  )

# Calculate the historical mean per month
mean_hist_data <- base_hist %>%
  group_by(Mes) %>%
  summarise(mean_hist_FC_ENANORDESTE = mean(ENANORDESTE))

###########
# Plot ENA
###########

ggplot() +
  # Add scenario lines in light grey
  geom_line(data = base_sim, aes(x = Mes, y = ENA_NORDESTE, group = Cenario), color = "gray90", alpha = 0.5) +
  # Add mean simulation line in purple with increased thickness
  geom_line(data = mean_data, aes(x = Mes, y = mean_sim_FC_original, color = "Simulated Mean"), size = 1.0, linetype = "dashed") +
  # Add 5th percentile line in green with increased thickness
  geom_line(data = quantiles_data, aes(x = Mes, y = quantile_5, color = "5th Percentile"), size = 1.0, linetype = "dashed") +
  # Add 95th percentile line in orange with increased thickness
  geom_line(data = quantiles_data, aes(x = Mes, y = quantile_95, color = "95th Percentile"), size = 1.0, linetype = "dashed") +
  # Add historical mean line in black with increased thickness
  geom_line(data = mean_hist_data, aes(x = Mes, y = mean_hist_FC_ENANORDESTE, color = "Historical Mean"), size = 1.0, linetype = "dashed") +
  # Adjust axes and labels
  scale_x_continuous(breaks = 1:12, labels = paste0("Month ", 1:12)) +
  scale_y_continuous(breaks = pretty(range(base_sim$ENA_NORDESTE), n = 5)) +
  labs(x = "Months", y = "NAE (MWmed)") +
  scale_color_manual(
    name = "Legend",
    values = c(
      "Simulated Mean" = "darkred",
      "5th Percentile" = "tomato",
      "95th Percentile" = "tomato",
      "Historical Mean" = "black"
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.x = element_text(size = 14),  # Font size for x-axis labels
    axis.text.y = element_text(size = 14),  # Font size for y-axis labels
    axis.title.x = element_text(size = 16), # Font size for x-axis title
    axis.title.y = element_text(size = 16), # Font size for y-axis title
    legend.text = element_text(size = 12),  # Font size for legend text
    legend.position = "right",  # Place legend on the right
    legend.background = element_rect(fill = "white", color = "black"),  # Add border to the legend
    legend.key.size = unit(0.99, "cm")  # Size of the legend keys
  )
