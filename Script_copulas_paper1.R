
##############################################################
#Script referente ao paper 1 da tese de doutorado
#Aplicação da teoria de copulas nos dados de energia renovavel
#Variáveis ENA, Fator de Capacidade, ENA (simulada pelo par-p)
##############################################################


##########
#Pacotes
#########

library(janitor)
library(ggExtra)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(psych)
library(ggExtra)
library(VineCopula)


######################
#script com as funções
######################

source("functions_qualify_Mes.R") #Nesse script temos todas as funções automatizadas

###################################################
#Iniciando as funções especificas para este script
##################################################

###############
#Tratar a base
##############

remove_mes_column <- function(raw_data) {
  final_data <- list()
  for (j in seq_along(raw_data)) {
    mes_list <- list()
    for (i in seq_along(raw_data[[j]])) {
      mes_list[[i]] <- raw_data[[j]][[i]] %>%
        as.data.frame() %>%
        dplyr::select(-1)
    }
    final_data[[j]] <- mes_list |> purrr::set_names(names(raw_data[[j]]))
  }
  final_data |> purrr::set_names(names(raw_data))
}

##############################################
#Encontrar as função de distribuicao marginal
#############################################

encontrar_melhor_dist <- function(data, dist) {
  resultado <- list()
  for (j in seq_along(data)) {
    mes_list <- list()
    for (i in seq_along(data[[j]])) {
      mes_list[[i]] <- melhor_distribuicao(as.data.frame(data[[j]][[i]]), distribuicoes = dist)
    }
    resultado[[j]] <- mes_list |> purrr::set_names(names(data[[j]]))
  }
  resultado |> purrr::set_names(names(data))
}

####
#CDF
####

pegando_a_cdf <- function(data, dist) {
  final_data <- list()
  for (j in seq_along(data)) {
    second_list <- list()
    for (i in seq_along(data[[j]])) {
      # Verificar se data[[j]][[i]] é um vetor e não uma lista
      if (!is.list(data[[j]][[i]])) {
        # Repetir o mesmo valor para todos os elementos de dist[[j]][[i]]
        second_list[[i]] <- purrr::map(dist[[j]][[i]], ~ create_cdf(data[[j]][[i]], .x))
      } else {
        # Se data[[j]][[i]] for uma lista, não fazer nada
        second_list[[i]] <- data[[j]][[i]]
      }
    }
    final_data[[j]] <- second_list |> purrr::set_names(names(data[[j]]))
  }
  return(final_data |> purrr::set_names(names(data)))
}

#############################################################################
#Verificando a familia de copulas que melhor se ajusta aos dados
############################################################################


encontrando_a_melhor_familia <- function(data, B) {
  final_data <- list()
  for (j in seq_along(data)) {
    second_list <- list()
    for (i in seq_along(data[[j]])) {
      # Verificar se data[[j]][[i]] é um dataframe
      if (is.data.frame(data[[j]][[i]])) {
        # Aplicar find_family() apenas se for um dataframe
        result <- find_family(data[[j]][[i]], B = B, criterio = "logLik")
        if (!is.null(result)) {
          second_list[[i]] <- result
        } else {
          message("find_family() retornou NULL para data[[", j, "]][[", i, "]]")
        }
      } else {
        # Se não for um dataframe, manter o elemento intacto
        second_list[[i]] <- data[[j]][[i]]
      }
      cat("\rAcabou a fase", i, "de", length(data[[j]]), "\n")
    }
    if (length(second_list) > 0) {
      final_data[[j]] <- second_list |> purrr::set_names(names(data[[j]]))
    } else {
      message("second_list está vazia para j = ", j)
    }
    message("Acabou a cidade ", j, " de ", length(data), "\n")
  }
  return(final_data |> purrr::set_names(names(data)))
}



######################
#Estimando as copulas
######################

estimando_as_copulas <- function(data, familia) {
  final_data <- list()
  for (j in seq_along(data)) {
    second_list <- list()
    for (i in seq_along(data[[j]])) {
      second_list[[i]] <- estimate_copula(data[[j]][[i]], familia[[j]][[i]])
    }
    final_data[[j]] <- second_list |> purrr::set_names(names(data[[j]]))
  }
  return(final_data |> purrr::set_names(names(data)))
}

######################
#Guardando parametros
#####################

pegando_os_parametros <- function(data) {
  final_data <- list()
  for (j in seq_along(data)) {
    second_list <- list()
    for (i in seq_along(data[[j]])) {
      # Verificar se data[[j]][[i]] é um objeto BiCop
      if (inherits(data[[j]][[i]], "BiCop")) {
        second_list[[i]] <- purrr::map(data[[j]][[i]], ~ copula_parameters(.x)) |>
          dplyr::bind_rows(.id = "mes")
      } else {
        # Se não for um objeto BiCop, manter como está
        second_list[[i]] <- data[[j]][[i]]
      }
    }
    final_data[[j]] <- second_list |> purrr::set_names(names(data[[j]]))
  }
  return(final_data |> purrr::set_names(names(data)))
}


###################################
#Comparando simulacao com historico
###################################

comparar_simulacao_original <- function(copulas, data, dist, sim){
  outer_list <- list()
  for (j in seq_along(data)) {
    inner_list <- list()
    for (i in seq_along(data[[j]])) {
      inner_list[[i]] <- check_copula_ajust(
        copulas = copulas[[j]][[i]],
        orig_data = data[[j]][[i]],
        melhor_dist = melhor_dist[[j]][[i]],
        simulations = sim,
        seed = 123
      )
    }
    outer_list[[j]] <- inner_list |> purrr::set_names(names(data[[j]]))
  }
  return(outer_list |> purrr::set_names(names(data)))
}

#######################
#Simulacao com a copula
#######################

simulando_os_dados <- function(copulas, data, dist, sim){
  outer_list <- list()
  for (j in seq_along(data)) {
    inner_list <- list()
    for (i in seq_along(data[[j]])) {
      inner_list[[i]] <- copula_sim(
        copulas = copulas[[j]][[i]],
        orig_data = data[[j]][[i]],
        melhor_dist = melhor_dist[[j]][[i]],
        simulations = sim,
        seed = 123
      )
    }
    outer_list[[j]] <- inner_list |> purrr::set_names(names(data[[j]]))
  }
  return(outer_list |> purrr::set_names(names(data)))
}



#####################
# Iniciando a Analise
#####################

#################
# Lendo os dados
#################

raw_data <- read_data("final_1.xlsx")

# remover os dados das listas de enanordeste de 2023 (ultimo dado da lista)
# remover os dados das listas de fc de 1980 (primeiro dado da lista)

###################################################
#Removendo a coluna "mes" para deixar só os dados
##################################################

data <- remove_mes_column(raw_data)
data <- data[1]

################################
#Testando a melhor distribuição
################################

distribuicoes <- c("beta","lnorm","weibull") #pode por inumeras distrib. parametricas

melhor_dist <- encontrar_melhor_dist(data, dist = distribuicoes)

melhor_dist$enanordeste$abril #testando se esta funcionando

########################
#Pegando a cdf dos dados
##########################

cdf <- pegando_a_cdf(data, melhor_dist)


#########################################
#Encontrando a melhor familia de copulas
#########################################

melhor_familia <- encontrando_a_melhor_familia(cdf, B = 300)

melhor_familia$enanordeste$jan #testando se funciona


########################
#Estimando as cópulas
########################

copulas <- estimando_as_copulas(data, melhor_familia)


#####################################
#Pegando os parametros de cada copula
######################################

parametros <- pegando_os_parametros(copulas)


###############################################################################
#Simulando os dados (2.000 simulacoes, pode alterar o numero) (fazendo a volta)
###############################################################################

dados_simulados <- simulando_os_dados(
  copula = copulas,
  data = data,
  dist = melhor_dist,
  sim = 2000
)


##########################
#métricas Dados Simulados
###########################

# Função para calcular métricas
calcular_metricas <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      Variable = character(),
      Min = numeric(),
      Mean = numeric(),
      Median = numeric(),
      Max = numeric(),
      SD = numeric(),
      CV = numeric(),
      Skewness = numeric(),
      Kurtosis = numeric()
    ))
  }
  
  # Calcular métricas para 'enanordeste'
  metricas_enanordeste <- df %>%
    summarise(
      Min = min(enanordeste, na.rm = TRUE),
      Mean = mean(enanordeste, na.rm = TRUE),
      Median = median(enanordeste, na.rm = TRUE),
      Max = max(enanordeste, na.rm = TRUE),
      SD = sd(enanordeste, na.rm = TRUE),
      CV = sd(enanordeste, na.rm = TRUE) / mean(enanordeste, na.rm = TRUE),
      Skewness = skew(enanordeste, na.rm = TRUE),
      Kurtosis = kurtosi(enanordeste, na.rm = TRUE)
    ) %>%
    mutate(Variable = "enanordeste")
  
  # Calcular métricas para 'fc_enanordeste'
  metricas_fc_enanordeste <- df %>%
    summarise(
      Min = min(fc_enanordeste, na.rm = TRUE),
      Mean = mean(fc_enanordeste, na.rm = TRUE),
      Median = median(fc_enanordeste, na.rm = TRUE),
      Max = max(fc_enanordeste, na.rm = TRUE),
      SD = sd(fc_enanordeste, na.rm = TRUE),
      CV = sd(fc_enanordeste, na.rm = TRUE) / mean(fc_enanordeste, na.rm = TRUE),
      Skewness = skew(fc_enanordeste, na.rm = TRUE),
      Kurtosis = kurtosi(fc_enanordeste, na.rm = TRUE)
    ) %>%
    mutate(Variable = "fc_enanordeste")
  
  # Combine as métricas em um único data frame
  metricas <- bind_rows(metricas_enanordeste, metricas_fc_enanordeste)
  return(metricas)
}

# Listando os meses com nomes corretos
meses <- c("jan", "fev", "mar", "abril", "maio", "jun", "jul", "ago", "set", 
           "out", "nov", "dez")

# Verificar a existência e conteúdo dos dados para cada mês
nomes_meses <- names(dados_simulados$enanordeste)
print(nomes_meses)

# Criar uma lista para armazenar as planilhas
planilhas <- list()

# Calculando métricas para cada mês
for (mes in meses) {
  if (!(mes %in% nomes_meses)) {
    warning(paste("O mês", mes, "não está presente nos dados simulados."))
    next
  }
  
  df_mes <- dados_simulados$enanordeste[[mes]]
  
  # Verificar se df_mes é nulo ou vazio
  if (is.null(df_mes) || nrow(df_mes) == 0) {
    warning(paste("Dados para o mês", mes, "estão vazios ou não existem."))
    next
  }
  
  metricas_mes <- calcular_metricas(df_mes)
  planilhas[[mes]] <- metricas_mes
}

# Criar um arquivo Excel e adicionar as planilhas
arquivo_excel <- "C:/Users/tuany/OneDrive/Área de Trabalho/dados Tese Tuany/metricas_mensais.xlsx"
wb <- createWorkbook()

for (mes in meses) {
  addWorksheet(wb, sheetName = mes)
  if (!is.null(planilhas[[mes]])) {
    writeData(wb, sheet = mes, planilhas[[mes]])
  } else {
    warning(paste("Não há métricas para o mês", mes, "para adicionar ao Excel."))
  }
}

saveWorkbook(wb, file = arquivo_excel, overwrite = TRUE)

cat("Métricas salvas em", arquivo_excel)


#####################
#Metricas Históricas
####################

# Função para calcular métricas
calcular_metricas <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame(
      Variable = character(),
      Min = numeric(),
      Mean = numeric(),
      Median = numeric(),
      Max = numeric(),
      SD = numeric(),
      CV = numeric(),
      Skewness = numeric(),
      Kurtosis = numeric()
    ))
  }
  
  # Calcular métricas para 'enanordeste'
  metricas_enanordeste <- df %>%
    summarise(
      Min = min(enanordeste, na.rm = TRUE),
      Mean = mean(enanordeste, na.rm = TRUE),
      Median = median(enanordeste, na.rm = TRUE),
      Max = max(enanordeste, na.rm = TRUE),
      SD = sd(enanordeste, na.rm = TRUE),
      CV = sd(enanordeste, na.rm = TRUE) / mean(enanordeste, na.rm = TRUE),
      Skewness = skew(enanordeste, na.rm = TRUE),
      Kurtosis = kurtosi(enanordeste, na.rm = TRUE)
    ) %>%
    mutate(Variable = "enanordeste")
  
  # Calcular métricas para 'fc_enanordeste'
  metricas_fc_enanordeste <- df %>%
    summarise(
      Min = min(fc_enanordeste, na.rm = TRUE),
      Mean = mean(fc_enanordeste, na.rm = TRUE),
      Median = median(fc_enanordeste, na.rm = TRUE),
      Max = max(fc_enanordeste, na.rm = TRUE),
      SD = sd(fc_enanordeste, na.rm = TRUE),
      CV = sd(fc_enanordeste, na.rm = TRUE) / mean(fc_enanordeste, na.rm = TRUE),
      Skewness = skew(fc_enanordeste, na.rm = TRUE),
      Kurtosis = kurtosi(fc_enanordeste, na.rm = TRUE)
    ) %>%
    mutate(Variable = "fc_enanordeste")
  
  # Combine as métricas em um único data frame e organiza a coluna 'Variable'
  #como a primeira
  metricas <- bind_rows(metricas_enanordeste, metricas_fc_enanordeste)
  metricas <- metricas[, c("Variable", setdiff(names(metricas), "Variable"))]
  
  return(metricas)
}

# Listando os meses com nomes corretos
meses <- c("jan", "fev", "mar", "abril", "maio", "jun", "jul", "ago", "set", 
           "out", "nov", "dez")

# Verificar a existência e conteúdo dos dados para cada mês
nomes_meses <- names(data$enanordeste)
print(nomes_meses)

# Criar uma lista para armazenar as planilhas
planilhas <- list()

# Calculando métricas para cada mês
for (mes in meses) {
  if (!(mes %in% nomes_meses)) {
    warning(paste("O mês", mes, "não está presente nos dados."))
    next
  }
  
  df_mes <- data$enanordeste[[mes]]
  
  # Verificar se df_mes é nulo ou vazio
  if (is.null(df_mes) || nrow(df_mes) == 0) {
    warning(paste("Dados para o mês", mes, "estão vazios ou não existem."))
    next
  }
  
  metricas_mes <- calcular_metricas(df_mes)
  planilhas[[mes]] <- metricas_mes
}

# Criar um arquivo Excel e adicionar as planilhas
arquivo_excel <- "C:/Users/tuany/OneDrive/Área de Trabalho/dados Tese Tuany/metricas_mensais_data.xlsx"
wb <- createWorkbook()

for (mes in meses) {
  addWorksheet(wb, sheetName = mes)
  if (!is.null(planilhas[[mes]])) {
    writeData(wb, sheet = mes, planilhas[[mes]])
  } else {
    warning(paste("Não há métricas para o mês", mes, "para adicionar ao Excel."))
  }
}

saveWorkbook(wb, file = arquivo_excel, overwrite = TRUE)

cat("Métricas salvas em", arquivo_excel)


