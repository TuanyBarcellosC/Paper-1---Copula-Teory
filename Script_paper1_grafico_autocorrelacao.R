##################################
#Script grafico de autocorrelacao
#################################

######################
# Carregar os pacotes
#####################
library(readxl)
library(ggplot2)
library(forecast)

#####################################
# Definir o caminho do arquivo Excel
####################################
file_path <- "C:/Users/tuany/OneDrive/Área de Trabalho/dados Tese Tuany/geral_cor.xlsx"  # Replace with the correct path

#######################
# Ler a planilha Excel
######################
data <- read_excel(file_path)

######################
# Separar as variáveis
######################
FC_Historical <- data$FC_Historica
FC_Simulated <- data$FC_Simulada

#####################
# Remover NA se tiver
#####################
FC_Historical <- na.exclude(FC_Historical)
FC_Simulated <- na.exclude(FC_Simulated)

# Function to create a combined autocorrelation plot for both series
plot_acf_combined <- function(data1, data2, title) {
  acf_values1 <- acf(data1, plot = FALSE, lag.max = 40)
  acf_values2 <- acf(data2, plot = FALSE, lag.max = 40)
  
  # Create a dataframe with autocorrelation values and labels
  acf_df <- data.frame(
    Lag = rep(acf_values1$lag, 2),
    ACF = c(acf_values1$acf, acf_values2$acf),
    Series = rep(c("Historical", "Simulated"), each = length(acf_values1$acf))
  )
  
  # Confidence interval for the ACF
  conf_interval <- qnorm((1 + 0.95)/2)/sqrt(length(data1))  # Use length(data1) for confidence interval
  
  # Create the plot
  ggplot(acf_df, aes(x = Lag, y = ACF, fill = Series)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.4) +  # Side-by-side bars
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = c(-conf_interval, conf_interval), linetype = "dashed", color = "red") +  # Confidence interval
    labs(title = title, x = "Lag", y = "Autocorrelation") +
    scale_fill_manual(values = c("Historical" = "darkred", "Simulated" = "darkgray")) +  # Define colors
    theme_minimal() +
    theme(panel.grid = element_blank()) +  # Remove grid lines
    ylim(-1, 1)  # Limit y-axis for visual consistency
}

# Generate the combined plot
plot_acf_combined(FC_Historical, FC_Simulated, "Autocorrelation - Historical vs Simulated")



