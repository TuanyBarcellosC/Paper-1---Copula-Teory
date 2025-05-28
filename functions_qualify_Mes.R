#'   # Seleciona as colunas de interesse, excluindo a primeira coluna
#'   data_list <- data_list |>
#'     purrr::map(~ dplyr::select(.x, -1))
#'
#'   # Retorna a lista de data frames
#'   return(data_list)
#' }

#' Encontra a melhor distribuição estatística para cada coluna numérica de um dataframe
#' A função vai relaziar o teste de Kolmogorov-Smirnov em todas as distribuições
#' do vetor "distribuicoes", e vai retornar a distribuição que apresentar o maior p-valor
#'
#' @param df Dataframe contendo as colunas a serem testadas.
#' @param distribuicoes Vetor de strings com os nomes das distribuições a serem testadas.
#'
#' @return Um dataframe com a melhor distribuição para cada coluna, incluindo estatísticas do teste KS e parâmetros estimados.
#' @examples
#' # Exemplo de uso:
#' df <- data.frame(x = rnorm(100), y = rexp(100))
#' distribuicoes <- c("norm", "exp")
#' resultados <- melhor_distribuicao(df, distribuicoes)
#' print(resultados)
#'
#' @export
#'
melhor_distribuicao <- function(df, distribuicoes) {
  # Verifica se o dataframe e as distribuições são válidos
  if (is.data.frame(df) == FALSE) {
    stop("O argumento 'df' deve ser um dataframe.")
  }
  if (length(df) == 0) {
    stop("O dataframe 'df' está vazio.")
  }
  if (is.null(colnames(df))) {
    stop("O dataframe 'df' deve ter nomes de colunas.")
  }
  if (is.character(distribuicoes) == FALSE || length(distribuicoes) == 0) {
    stop("O argumento 'distribuicoes' deve ser um vetor de strings com nomes de distribuições.")
  }
  
  # Criando uma lista para armazenar os resultados
  resultados_lista <- list()
  
  # Loop em cada coluna do dataframe
  for (coluna in colnames(df)) {
    if (!is.numeric(df[[coluna]])) {
      stop(paste("A coluna", coluna, "não é numérica e será ignorada."))
    }
    
    # Inicializa vetores para armazenar resultados
    melhor_p_valor <- estatistica <- p_valores <- param1 <- param2 <- numeric(length(distribuicoes))
    
    
    # Loop em cada distribuição
    for (i in seq_along(distribuicoes)) {
      dist <- distribuicoes[i]
      
      # Ajustar a Distribuição para pegar os parametros
      ajuste <- tryCatch(
        {
          if (dist == "beta") {
            fitdistrplus::fitdist(df[[2]], dist, method = "mme")#, start = list(df = 5))
          }
          else if (dist == "t") {
            fitdistrplus::fitdist(df[[coluna]], dist, start = list(df = 5))
          } else {
            fitdistrplus::fitdist(df[[coluna]], dist)
          }
        },
        error = function(e) {
          stop(paste("Erro ao ajustar a distribuição", dist, "para a coluna", coluna, ":", e$message))
        }
      )
      
      # Verificar o número de parâmetros para a distribuição
      num_params <- length(ajuste$estimate)
      # Teste de Kolmogorov-Smirnov
      ks <- tryCatch(
        {
          if (num_params == 1) {
            ks.test(df[[coluna]], paste0("p", dist), ajuste$estimate[[1]])
          } else if (num_params == 2) {
            ks.test(df[[coluna]], paste0("p", dist), ajuste$estimate[[1]], ajuste$estimate[[2]])
          } else {
            stop(paste("A distribuição", dist, "com mais de dois parâmetros não é suportada."))
          }
        },
        error = function(e) {
          stop(paste("Erro no teste KS para a distribuição", dist, "na coluna", coluna, ":", e$message))
        }
      )
      
      estatistica[i] <- ks$statistic
      p_valores[i] <- ks$p.value
      if (num_params >= 1) param1[i] <- ajuste$estimate[[1]]
      if (num_params >= 2) param2[i] <- ajuste$estimate[[2]]
    }
    
    p_valores <- p.adjust(p_valores, method = "BH")
    
    # Encontra o índice aa melhor distribuição com base nos p-valores
    melhor_indice <- which.max(p_valores)
    
    # Agrupa o resultado em apenas um data frame
    resultados_lista[[coluna]] <- tibble::tibble(
      variavel = coluna,
      distribuicao = distribuicoes[melhor_indice],
      ks_estatistica = estatistica[melhor_indice],
      p_valor = p_valores[melhor_indice],
      param1 = param1[melhor_indice],
      param2 = param2[melhor_indice]
    )
  }
  
  return(do.call(rbind, resultados_lista))
}

#' Cria uma função de distribuição cumulativa (CDF) para cada variável em um dataframe
#'
#' Esta função cria uma função de distribuição cumulativa (CDF) para cada variável em um dataframe,
#' com base nas distribuições especificadas em outro dataframe.
#'
#' @param df Um dataframe cujas colunas representam diferentes variáveis.
#' @param dist Um dataframe que especifica a melhor distribuição para cada variável em 'df',
#' conforme determinado pela função 'melhor_distribuicao'. Este dataframe deve ter as seguintes colunas:
#' - 'variavel': o nome da variável (deve corresponder às colunas de 'df')
#' - 'distribuicao': o nome da distribuição a ser usada
#' - 'ks_estatistica': a estatística do teste Kolmogorov-Smirnov para a distribuição
#' - 'p_valor': o valor-p do teste Kolmogorov-Smirnov
#' - 'param1': o primeiro parâmetro da distribuição
#' - 'param2': o segundo parâmetro da distribuição (se aplicável)
#'
#' @return Um dataframe onde cada coluna é a CDF da variável correspondente em 'df', com as colunas nomeadas
#' de acordo com a coluna 'variavel' do dataframe 'dist'.
#' @export
create_cdf <- function(df, dist) {
  # Verifica se o número de colunas em df é igual ao número de linhas em dist
  if (ncol(df) != nrow(dist)) {
    stop("O número de variáveis em df é diferente do número de linhas em dist")
  }
  
  # Inicializa uma lista para armazenar os resultados
  cdf_list <- vector("list", length = nrow(dist))
  
  # Loop sobre as linhas de dist
  for (i in seq_len(nrow(dist))) {
    # Cria o nome da função de distribuição
    cdf_name <- paste0("p", dist[i, 2])
    
    # Obtém a função de distribuição
    cdf <- get(cdf_name)
    
    # Converte a série e os parâmetros para numérico
    serie <- as.numeric(df[[i]])
    param_1 <- as.numeric(dist[i, 5])
    param_2 <- as.numeric(dist[i, 6])
    
    # Verifica se os parâmetros são numéricos
    if (is.na(param_1)) {
      stop(paste("O primeiro parâmetro para a variável", i, "não é numérico"))
    }
    
    # Se param_2 não é NA e não é zero, usa a função de distribuição com dois parâmetros
    if (!is.na(param_2) && param_2 != 0) {
      cdf_list[[i]] <- cdf(serie, param_1, param_2)
    } else {
      # Se param_2 é NA ou zero, usa a função de distribuição com um parâmetro
      cdf_list[[i]] <- cdf(serie, param_1)
    }
  }
  
  # Usa purrr::set_names para nomear os elementos da lista
  cdf_list <- purrr::set_names(cdf_list, dist[[1]])
  
  # Usa dplyr::bind_cols para combinar os elementos da lista em um dataframe
  return(do.call(cbind, cdf_list) |> tibble::as_tibble())
}

#' Identifica a melhor família de cópulas para modelar a dependência entre variáveis.
#'
#' @param data Um data frame contendo os dados a serem analisados.
#' @param B Número de réplicas para o teste de bondade de ajuste.
#' @param criterio Critério de seleção para a família de cópulas (ex: "logLik", "AIC", "BIC").
#'
#' @return Um data frame com os resultados da seleção da família de cópulas para cada variável.
#'
#' @details
#' A função utiliza o pacote VineCopula para realizar a seleção de cópulas. Ela converte os dados
#' para pseudo-observações, verifica a validade dos argumentos de entrada e, em seguida, realiza
#' o teste de bondade de ajuste para selecionar a melhor família de cópulas para cada variável.
#'
#' @examples
#' \dontrun{
#' # Exemplo de uso da função
#' resultados <- find_family(dados, 100, "AIC")
#' }
#'
#' @export
find_family <- function(data, B, criterio) {
  # Define uma semente para reprodutibilidade
  set.seed(999)
  
  # Lista de famílias de cópulas originais
  original_copula_families <- c(1, 3, 4, 5, 6, 13, 14, 16, 23, 24, 26, 33, 34, 36)
  
  # Calcula o número de colunas de dados menos uma
  num_col <- ncol(data) - 1
  
  # Inicializa um vetor para armazenar os resultados
  results <- vector(mode = "list", length = num_col)
  
  # Converte os dados para pseudo-observações
  u <- VineCopula::pobs(data)
  
  # Verifica se 'data' é um data frame
  if (!is.data.frame(data)) {
    stop("O argumento 'data' deve ser um data frame.")
  }
  
  # Verifica se 'B' é um número inteiro positivo
  if (!is.numeric(B) || B <= 0 || B != round(B)) {
    stop("O argumento 'B' deve ser um número inteiro positivo.")
  }
  
  # Verifica se 'criterio' é uma string válida
  if (!is.character(criterio) || !(criterio %in% c("logLik", "AIC", "BIC"))) {
    stop("O argumento 'criterio' deve ser 'logLik', 'AIC' ou 'BIC'.")
  }
  
  # Loop para testar cada coluna de dados
  for (i in seq_len(num_col)) {
    copula_families <- original_copula_families
    
    # Seleciona a melhor família de cópulas para a coluna atual
    copula_selection <- VineCopula::BiCopSelect(
      u[, 1], u[, i + 1],
      familyset = copula_families,
      rotations = FALSE,
      selectioncrit = criterio
    )
    
    # Armazena os parâmetros estimados
    param1 <- copula_selection$par
    param2 <- ifelse(copula_selection$par2 == 0, 0, copula_selection$par2)
    
    # Realiza o teste de bondade de ajuste
    gof_test <- VineCopula::BiCopGofTest(
      u[, 1], u[, i + 1],
      family = copula_selection$family,
      par = param1,
      par2 = param2,
      method = ifelse(copula_selection$family == 2, "white", "kendall"),
      B = B
    )
    
    # Calcula o maior p-valor dos testes
    max_pvalue <- max(gof_test$p.value, gof_test$p.value.CvM)
    
    # Verifica se o p-valor é aceitável
    if (max_pvalue >= 0.05) {
      # Seleciona o número correspondente ao critério
      selec_number <- dplyr::case_when(
        criterio == "logLik" ~ 11,
        criterio == "AIC" ~ 12,
        criterio == "BIC" ~ 13
      )
      
      # Armazena os resultados da seleção
      family <- as.numeric(copula_selection$family)
      name <- copula_selection$familyname
      AIC <- copula_selection[[selec_number]]
      max_pvalue <- max_pvalue
    } else {
      # Se o p-valor não for aceitável, tenta outras famílias
      find_family <- function() {
        tryCatch(
          {
            while_iteration <- 0
            while (max_pvalue < 0.05) {
              while_iteration <- while_iteration + 1
              family_index <- which(copula_families == copula_selection$family)
              copula_families <<- copula_families[-family_index]
              if (length(copula_families) == 0) break
              copula_selection <- VineCopula::BiCopSelect(
                u[, 1], u[, i + 1],
                familyset = copula_families,
                rotations = FALSE,
                selectioncrit = criterio
              )
              param1 <- copula_selection$par
              param2 <- ifelse(copula_selection$par2 == 0, 0, copula_selection$par2)
              gof_test <- VineCopula::BiCopGofTest(
                u[, 1], u[, i + 1],
                family = copula_selection$family,
                par = param1,
                par2 = param2,
                method = ifelse(copula_selection$family == 2, "white", "kendall"),
                B = B
              )
              selec_number <- dplyr::case_when(
                criterio == "logLik" ~ 11,
                criterio == "AIC" ~ 12,
                criterio == "BIC" ~ 13
              )
              family <- as.numeric(copula_selection$family)
              name <- copula_selection$familyname
              AIC <- as.numeric(copula_selection[[selec_number]])
              max_pvalue <- as.numeric(max(gof_test$p.value, gof_test$p.value.CvM))
            }
            return(result_while <- cbind(family, name, AIC, max_pvalue))
          },
          error = function(e) {
            message("Ocorreu um erro em BiCopSelect: ", e$message)
            return(result_while <- cbind(NULL, NULL, NULL, NULL))
          }
        )
      }
      result_while <- find_family()
      family <- as.numeric(result_while[1])
      name <- result_while[2]
      AIC <- as.numeric(result_while[3])
      max_pvalue <- as.numeric(result_while[4])
    }
    
    # Define o nome do critério para a saída
    nome_criterio <- dplyr::case_when(
      criterio == "logLik" ~ "MLE",
      criterio == "AIC" ~ "AIC",
      criterio == "BIC" ~ "BIC"
    )
    
    # Cria um tibble com os resultados
    result <- tibble::tibble(
      var = colnames(u)[i + 1],
      family_number = family,
      family_name = name,
      valor = AIC,
      gof_pvalue = max_pvalue
    ) |>
      dplyr::rename(!!nome_criterio := valor)
    
    # Armazena o resultado para a coluna atual
    results[[i]] <- result
    # cat("\rFinalizada a copula", i, "de", num_col, "\n")
  }
  
  # Mensagem de conclusão do processo
  # message("Fundo finalizado")
  
  # Retorna os resultados combinados
  return(do.call(rbind, results))
}

#' Estima a copula para cada coluna de um conjunto de dados
#'
#' @param data Um data frame. Cada coluna será usada para estimar uma copula.
#' @param familia Um data frame que é o output da função `find_family()`.
#'        Este data frame contém o número da família da copula para cada coluna em `data`.
#'        Aqui está um exemplo de output da função `find_family()`:
#'        # A tibble: 5 × 5
#'        # var       family_number family_name              MLE gof_pvalue
#'        # <chr>             <dbl> <chr>                  <dbl>      <dbl>
#'        # ibov                 16 Survival Joe            4.29        0.4
#'        # ima.geral             2 t                       5.34        1
#'        # cds.5y               26 Rotated Joe 90 degrees  2.81        0.2
#'        # ibc.br               16 Survival Joe            2.83        0.4
#'        # cambio                2 t                       6.53        1
#' @return Uma lista de objetos `BiCop` para cada coluna em `data`.
#' @examples
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#' familia <- data.frame(family_number = c(0, 0))
#' estimate_copula(data, familia)
estimate_copula <- function(data, familia) {
  # Verifica se 'data' é um data frame
  if (!is.data.frame(data)) {
    stop("O argumento 'data' deve ser um data frame.")
  }
  
  # Verifica se 'familia' é um data frame
  if (!is.data.frame(familia)) {
    stop("O argumento 'familia' deve ser um data frame.")
  }
  
  # Transformação dos dados para uniformes marginais
  u <- VineCopula::pobs(data)
  
  num_col <- ncol(data) - 1
  
  # Inicialização do resultado como uma lista com nomes das colunas de 'data'
  resultado <- setNames(vector(mode = "list", length = num_col), names(data)[-1])
  
  # Loop para estimar a copula para cada coluna
  for (i in seq_len(num_col)) {
    # Obtenção do índice da família da copula
    familia_index <- familia$family_number[i]
    
    # Verifica se 'familia_index' é um número
    if (!is.numeric(familia_index)) {
      stop("Os índices da família devem ser numéricos.")
    }
    
    # Estimação dos parâmetros da copula
    estimate_param <- VineCopula::BiCopEst(u[, 1], u[, i + 1], family = familia_index)
    
    # Criação da copula com os parâmetros estimados
    copula <- VineCopula::BiCop(
      family = familia_index,
      par = estimate_param$par,
      par2 = estimate_param$par2
    )
    
    # Armazenamento da copula no resultado
    resultado[[i]] <- copula
  }
  
  return(resultado)
}

#' Função para plotar copulas
#'
#' Esta função itera sobre cada copula fornecida e gera um plot correspondente.
#' O tipo de plot (superfície ou contorno) é determinado pelo argumento `plot_type`.
#' A função usa a função `plot.BiCop` do pacote VineCopula para gerar os plots.
#'
#' @param copula Uma lista de objetos `BiCop` do pacote VineCopula.
#' @param plot_type Uma string indicando o tipo de plot a ser gerado. Deve ser "surface" ou "contour".
#' @return Nenhum retorno. A função imprime os plots diretamente.
#' @examples
#' # Suponha que `copula1` e `copula2` são objetos `BiCop`
#' plt_copula(list(copula1 = copula1, copula2 = copula2), plot_type = "surface")
plt_copula <- function(copula, plot_type = c("surface", "contour")) {
  # Verificar se o tipo de plot é válido
  if (!plot_type %in% c("surface", "contour")) {
    stop("Tipo do plot errado. Usar 'surface' ou 'contour'")
  }
  
  # Iterar sobre cada copula e gerar o plot correspondente
  for (i in seq_along(copula)) {
    fund_name <- names(copula)[i]
    for (j in seq_along(copula[[i]])) {
      var_name <- names(copula[[i]])[j]
      
      # Verificar se o objeto é um `BiCop`
      if (!inherits(copula[[i]][[j]], "BiCop")) {
        stop(paste("O objeto", var_name, "não é um objeto BiCop válido."))
      }
      
      # Gerar o plot com base no tipo escolhido
      if (plot_type == "surface") {
        VineCopula:::plot.BiCop(copula[[i]][[j]],
                                type = "surface",
                                main = paste(fund_name, "--", var_name)
        ) |> print()
      } else if (plot_type == "contour") {
        VineCopula:::plot.BiCop(copula[[i]][[j]],
                                type = "contour",
                                main = paste(fund_name, "--", var_name)
        )
      }
    }
  }
}

#' Extrai parâmetros de cópulas bivariadas
#'
#' @param data Uma lista de objetos BiCop da função VineCopula::BiCop()
#' @return Um tibble com os parâmetros de cada cópula
#' @examples
#' # Criação de um objeto BiCop
#' data <- list(VineCopula::BiCop(family = 1, par = 0.5, par2 = 0, tau = 0.5))
#' copula_parameters(data)
copula_parameters <- function(data) {
  # Verifica se o argumento 'data' é uma lista
  if (!is.list(data)) {
    stop("O argumento 'data' deve ser uma lista de objetos BiCop.")
  }
  
  # Inicializa a lista de resultados
  result <- list()
  
  # Obtém o número de cópulas
  num_copulas <- length(data)
  
  # Loop sobre cada cópula
  for (i in seq_len(num_copulas)) {
    # Extrai a cópula atual
    copula <- data[[i]]
    
    # Verifica se o objeto é uma cópula bivariada
    if (!inherits(copula, "BiCop")) {
      stop("Todos os elementos da lista 'data' devem ser objetos BiCop.")
    }
    
    # Extrai os parâmetros e o tau, arredondando para duas casas decimais
    param_1 <- round(copula[[2]], 2)
    param_2 <- round(copula[[3]], 2)
    tau <- round(copula[[6]], 2)
    
    # Cria um tibble com os resultados e adiciona à lista
    result[[i]] <- tibble::tibble(
      variavel = names(data[i]),
      param_1 = param_1,
      param_2 = param_2,
      tau = tau
    )
  }
  
  # Combina todos os tibbles em um único tibble
  return(do.call(rbind, result))
}

read_data <- function(file_path) {
  raw_data <- readxl::read_excel(file_path) |> janitor::clean_names()
  unique_names <- unique(names(raw_data))[2:3]
  mes_vec <- c(
    "jan", "fev", "mar", "abril", "maio",
    "jun", "jul", "ago", "set", "out", "nov", "dez"
  )
  df_list <- purrr::map(unique_names, function(name) {
    same_name_df <- raw_data |> dplyr::select(mes, dplyr::ends_with(name))  # Remove 'fase' da seleção
    split_by_mes <- same_name_df |>
      dplyr::group_by(mes) |>
      dplyr::group_split() |>
      purrr::set_names(mes_vec)
    return(split_by_mes)
  })
  names(df_list) <- unique_names
  return(df_list)
}



check_copula_ajust <- function(copulas, orig_data, melhor_dist, simulations, seed) {
  set.seed(seed)
  
  list_vokin_volta <- list()
  list_vokin_analise <- list()
  list_result <- list()
  teste_result <- list()
  
  for (j in seq_along(copulas)) {
    set.seed(seed)
    sim_teste <- VineCopula::BiCopSim(N = simulations, obj = copulas[[j]]) |>
      as.data.frame()
    
    names(sim_teste) <- c(names(orig_data)[1], names(orig_data)[j + 1])
    
    for (i in seq_len(ncol(sim_teste))) {
      distr_function <- get(paste0("q", melhor_dist[i, 2]))
      list_vokin_volta[[i]] <- distr_function(sim_teste[[i]], melhor_dist[[5]][i], melhor_dist[[6]][i])
    }
    
    df_vokin_volta <- purrr::set_names(list_vokin_volta, names(sim_teste)) |>
      dplyr::bind_cols()
    
    for (i in seq_len(ncol(df_vokin_volta))) {
      list_vokin_analise[[i]] <- tibble::tibble(
        name = names(sim_teste)[i],
        max_original = orig_data[[names(sim_teste)[i]]] |> max(),
        max_sim = df_vokin_volta[[i]] |> max(),
        mean_original = orig_data[[names(sim_teste)[i]]] |> mean(),
        mean_sim = df_vokin_volta[[i]] |> mean(),
        min_original = orig_data[[names(sim_teste)[i]]] |> min(),
        min_sim = df_vokin_volta[[i]] |> min(),
        mediana_original = orig_data[[names(sim_teste)[i]]] |> median(),
        mediana_sim = df_vokin_volta[[i]] |> median(),
        assimetria_original = orig_data[[names(sim_teste)[i]]] |> TSA::skewness(),
        assimetria_sim = df_vokin_volta[[i]] |> TSA::skewness(),
        curtose_original = orig_data[[names(sim_teste)[i]]] |> TSA::kurtosis(),
        curtose_sim = df_vokin_volta[[i]] |> TSA::kurtosis()
      )
    }
    
    list_result[[j]] <- dplyr::bind_rows(list_vokin_analise) |>
      tidyr::pivot_longer(cols = -name, names_to = "variable", values_to = "value") |>
      tidyr::pivot_wider(names_from = name, values_from = value)
  }
  
  teste_result <- list()
  
  for (i in seq_along(list_result)) {
    if (i == 1) {
      teste_result[[i]] <- list_result[[i]]
    } else {
      teste_result[[i]] <- list_result[[i]][, 3]
    }
  }
  
  teste_df <- dplyr::bind_cols(teste_result)
  return(teste_df)
}

copula_sim <- function(copulas, orig_data, melhor_dist, simulations, seed) {
  set.seed(seed)
  
  list_vokin_volta <- list()
  list_vokin_analise <- list()
  df_list <- list() # Create an empty list to store dataframes
  
  for (j in seq_along(copulas)) {
    set.seed(seed)
    sim_teste <- VineCopula::BiCopSim(N = simulations, obj = copulas[[j]]) |>
      as.data.frame()
    
    names(sim_teste) <- c(names(orig_data)[1], names(orig_data)[j + 1])
    
    for (i in seq_len(ncol(sim_teste))) {
      distr_function <- get(paste0("q", melhor_dist[i, 2]))
      list_vokin_volta[[i]] <- distr_function(sim_teste[[i]], melhor_dist[[5]][i], melhor_dist[[6]][i])
    }
    
    df_vokin_volta <- purrr::set_names(list_vokin_volta, names(sim_teste)) |>
      dplyr::bind_cols()
    
    df_list[[j]] <- df_vokin_volta # Store the dataframe in the list
  }
  teste_result <- list()
  
  for (i in seq_along(df_list)) {
    if (i == 1) {
      teste_result[[i]] <- df_list[[i]]
    } else {
      teste_result[[i]] <- df_list[[i]][, 2]
    }
  }
  
  teste_df <- dplyr::bind_cols(teste_result)
  return(teste_df)
}

#########
# Plots 
#########

#' Função para plotar o scatter plot entre os dados e as densidades, para dados simulados e observados.
#'
#' Esta função itera sobre cada fase e mês fornecida e gera um plot correspondente.
#' O tipo de plot marginal ("density", "histogram", "boxplot", "violin", "densigram") é determinado 
#' pelo argumento `tipo`.
#' A função usa o pacote ggplot2 e a função `ggMarginal` do pacote ggExtra para gerar os plots.
#'
#' @param original Uma lista de objetos contendo os dados observados.
#' @param simulado Uma lista de objetos contendo os dados simulados.
#' @param tipo Uma string indicando o tipo de marginal plot a ser gerado. Deve ser "density", 
#' "histogram", "boxplot", "violin" ou "densigram".
#' @param ree Parâmetro auxiliar para sabermos qual a REE mais próxima da usina - legenda.
#' @return Nenhum retorno. A função salva os plots diretamente num diretório chamado `plots` presente no
#' caminho indicado (deve-se criar a pasta antes de rodar o código).
#' @examples
#' # Suponha que `orig` e `sim` são listas contendo respectivamente os dados originais e simulados:
#' plt_disp(original = orig, simulado = sim, tipo = "density", ree = 1)
plt_disp <- function(original, simulado, tipo, ree) {
  # Get the length of the lists
  len_list_df2 <- length(simulado)
  len_list_df1 <- length(original)
  
  # Determine the number of iterations to perform
  num_iterations <- min(len_list_df1, len_list_df2)
  
  # Loop through each pair of lists
  for (j in 1:num_iterations) {
    # Get the current pair of lists
    list_df2 <- simulado[[j]]
    list_df1 <- original[[j]]
    
    # Get the length of the current pair of lists
    len_list_df2_inner <- length(list_df2)
    len_list_df1_inner <- length(list_df1)
    
    # Determine the number of iterations to perform for the inner loop
    num_iterations_inner <- min(len_list_df1_inner, len_list_df2_inner)
    
    # Loop through each pair of dataframes in the current pair of lists
    for (k in 1:num_iterations_inner) {
      df2 <- list_df2[[k]]
      df1 <- list_df1[[k]]
      
      # Get the number of columns in the dataframes
      num_cols_df2 <- ncol(df2)
      num_cols_df1 <- ncol(df1)
      
      # Determine the number of plots to create
      num_plots <- min(num_cols_df1, num_cols_df2) - 1
      
      # Loop through each pair of columns
      for (i in 2:(num_plots + 1)) {
        # Create a new dataframe combining df1 and df2
        df <- rbind(cbind(df2[, c(1, i)], Type = "Simulado"), cbind(df1[, c(1, i)], Type = "Observado"))
        colnames(df) <- c("Sim", "Obs", "Type")
        
        # Legend
        mes <- stringr::str_to_title(names(original[[j]])[k])
        #local <- stringr::str_to_title(names(df1))
        fase <- stringr::str_to_title(names(original)[j])
        
        fases <- c("Fase Neutra", "Fase El Niño", "Fase La Niña")
        meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto",
                   "Setembro", "Outubro", "Novembro", "Dezembro")
        REE <- c("Sudeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Sudeste", "Sudeste",
                 "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste",
                 "Nordeste", "Nordeste", "Sul")
        bacias_titulos <- c("Arinos", "Juazeiro", "Futuro", "Sertão", "São Pedro", "Pirapora", 
                            "Paracatu", "Dom Inocêncio", "Serra da Palmeira", "São Clemente", "Babilônia", "Santa Clara", 
                            "Tianguá", "ECHO RN", "ECHO BA", "Osório")
        bacias <- c("ARINOS", "JUAZEIRO", "FUTURO", "SERTAO", "PEDRO", "PIRAPORA", 
                    "PARACATU", "DIS", "SDP", "CLEMENTE", "BABILONIA", "CLARA", 
                    "TIANGUA", "ECHO_RN", "ECHO_BA", "OSORIO")
        
        local <- bacias_titulos[ree]
        local_save <- bacias[ree]
        
        # Create the scatter plot with swapped axes
        p <- ggplot2::ggplot(df, ggplot2::aes(x = Obs, y = Sim, color = Type)) +
          ggplot2::geom_point(ggplot2::aes(size = ifelse(Type == "Observado", 2, 1))) + # Adjust size based on Type
          ggplot2::scale_size_identity() + # Ensure that sizes are used as is
          ggplot2::labs(
            title = local,
            subtitle = paste(
              fases[j] |>
                stringr::str_replace("_", " "),
              "||",
              meses[k]
            ),
            x = paste("ENA REE", REE[ree], "(MWmed)"),
            y = paste("Fator de Capacidade", local, "(p.u.)") 
          ) +
          scale_color_manual(values=c("blue","lightskyblue")) +
          ggplot2::guides(color = ggplot2::guide_legend(title = ""))
        ggthemes::theme_few()
        
        p_final <- ggMarginal(p + theme(legend.position = "bottom"), type = tipo, size = 4,
                              groupColour = TRUE,
                              groupFill = TRUE,
                              xparams = list(size = 0),
                              yparams = list(size = 0))
        
        file_name <- paste0("./plots/", #coloque aqui seu caminho
                            "dist_",local_save,"_",mes,"_",fase,"_",tipo,".png"
        )
        
        ggsave(file_name, p_final, width = 20, height = 11, units = "cm")
      }
    }
  }
}

#' Função para plotar as densidades, para dados simulados e observados.
#'
#' Esta função itera sobre cada fase e mês fornecida e gera um plot correspondente.
#' A função usa o pacote ggplot2 para gerar os plots.
#'
#' @param original_list Uma lista de objetos contendo os dados observados.
#' @param simulado_list Uma lista de objetos contendo os dados simulados.
#' @param save TRUE ou FALSE. Se TRUE, salva o plot no diretório plots. Se FALSE, apenas mostra 
#' o gráfico na tela. Default: FALSE.
#' @return Nenhum retorno. A função salva os plots diretamente num diretório chamado `plots` presente no
#' caminho indicado (deve-se criar a pasta antes de rodar o código) ou mostra o plot na tela.
#' @examples
#' # Suponha que `orig` e `sim` são listas contendo respectivamente os dados originais e simulados:
#' density_simulation_plot(original = orig, simulado = sim)
density_simulation_plot <- function(original_list, simulado_list, save = FALSE) {
  for (j in seq_along(original_list)) {
    original <- original_list[[j]]
    simulado <- simulado_list[[j]]
    
    for (i in seq_along(simulado)) {
      for (name in names(simulado[[i]])) {
        df_dados_simulado <- simulado[[i]][name] |>
          dplyr::rename_with(~paste0(., "_simulado")) |>
          tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variavel", values_to = "value")
        
        df_original <- original[[i]][name]  |>
          dplyr::rename_with(~paste0(., "_original")) |>
          tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variavel", values_to = "value")
        
        mes <- names(simulado[i])
        
        var_name <- stringr::str_extract(df_original[[1]][[1]], "^[^_]*")
        
        df_combined <- rbind(df_original, df_dados_simulado)
        
        densidade <- density(df_combined$value)
        max_density_value <- max(densidade$x, densidade$y)
        
        plot <- df_combined |>
          ggplot2::ggplot(ggplot2::aes(x=value, fill=variavel)) +
          ggplot2::geom_density(alpha=0.5) +
          scale_fill_manual( values = c("blue","lightskyblue"), labels = c("Original", "Simulação")) + 
          ggplot2::scale_x_continuous(expand = c(0, 0)) +
          ggplot2::scale_y_continuous(expand = c(0, 0)) +
          ggplot2::labs(
            fill="",
            title = ifelse(
              stringr::str_detect(name, "ree"),
              paste(
                "REE",
                stringr::str_extract(name, pattern = "(?<=_)[^_]*$") |>
                  stringr::str_to_title()
              ),
              name |> stringr::str_to_title()
            ),
            subtitle = paste(
              "Fase:", stringr::str_extract(names(original_list)[j], pattern = "(?<=_)[^_]*$"),
              " || Mes:",
              stringr::str_to_title(mes)
            ),
            x = "",
            y = ""
          ) + 
          ggthemes::theme_few()
        
        if (save) {
          file_name <- paste0("./plots/", #coloque aqui seu caminho
                              stringr::str_extract(name, pattern = "(?<=_)[^_]*$"), "_",
                              names(original_list)[j] |> tolower(),
                              stringr::str_to_title(mes),
                              ".png"
          )
          ggplot2::ggsave(filename = file_name, plot = plot, device = "png", width = 8, height = 4, dpi = 300)
        } else {
          print(plot)
        }
        
      }
    }
  }
}

#' Função para plotar o scatter plot entre os dados simulados e observados.
#'
#' Esta função itera sobre cada fase e mês fornecida e gera um plot correspondente.
#' A função usa o pacote ggplot2 para gerar os plots.
#'
#' @param original Uma lista de objetos contendo os dados observados.
#' @param simulado Uma lista de objetos contendo os dados simulados, já contendo as informações sobre os clusters.
#' @param clust Uma lista de objetos contendo as infomrações sobre os clusters dos dados originais.
#' @param r Um número que indica qual objeto da lista de dados dos clusters será utilizado.
#' @param ree Parâmetro auxiliar para sabermos qual a REE mais próxima da usina - legenda.
#' @return Nenhum retorno. A função salva os plots diretamente num diretório chamado `plots` presente no
#' caminho indicado (deve-se criar a pasta antes de rodar o código).
#' @examples
#' # Suponha que `orig`, `sim` e `clusters` são listas contendo respectivamente os dados 
#' originais, simulados e os clusters dos dados originais:
#' plt_cluster(original = orig, simulado = sim, clust = clusters, r = 1, ree = 1)
plt_cluster <- function(original, simulado, clust, r, ree) {
  # Get the length of the lists
  len_list_df2 <- length(simulado)
  len_list_df1 <- length(original)
  
  # Determine the number of iterations to perform
  num_iterations <- min(len_list_df1, len_list_df2)
  
  # Loop through each pair of lists
  for (j in 1:num_iterations) {
    # Get the current pair of lists
    list_df2 <- simulado[[j]]
    list_df1 <- original[[j]]
    
    # Get the length of the current pair of lists
    len_list_df2_inner <- length(list_df2)
    len_list_df1_inner <- length(list_df1)
    
    # Determine the number of iterations to perform for the inner loop
    num_iterations_inner <- min(len_list_df1_inner, len_list_df2_inner)
    
    # Loop through each pair of dataframes in the current pair of lists
    for (k in 1:num_iterations_inner) {
      df2 <- list_df2[[k]]
      df1 <- list_df1[[k]]
      clusters_original <- clust[[r]][clust[[r]]$Mes == k, 6]
      clusters_simulados <- df2[,3]
      
      # Get the number of columns in the dataframes
      num_cols_df2 <- ncol(df2)
      num_cols_df1 <- ncol(df1)
      
      # Determine the number of plots to create
      num_plots <- min(num_cols_df1, num_cols_df2) - 1
      
      # Loop through each pair of columns
      for (i in 2:(num_plots + 1)) {
        # Create a new dataframe combining df1 and df2
        df <- rbind(cbind(df2[, c(1, i)], cluster = clusters_simulados, Type = "Simulado"), 
                    cbind(df1[, c(1, i)], cluster = clusters_original, Type = "Observado"))
        colnames(df) <- c("usina", "ree", "cluster", "Type")
        
        myColors <- c("coral", "deeppink", "green", "royalblue", "cyan")
        names(myColors) <- c("1", "2", "3", "4")
        custom_colors <- scale_colour_manual(name = "Cluster", values = myColors)
        
        # Legend
        mes <- stringr::str_to_title(names(original[[j]])[k])
        #local <- stringr::str_to_title(names(df1))
        fase <- stringr::str_to_title(names(original)[j])
        
        fases <- c("Fase Neutra", "Fase El Niño", "Fase La Niña")
        meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto",
                   "Setembro", "Outubro", "Novembro", "Dezembro")
        REE <- c("Sudeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Sudeste", "Sudeste",
                 "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste",
                 "Nordeste", "Nordeste", "Sul")
        bacias_titulos <- c("Arinos", "Juazeiro", "Futuro", "Sertão", "São Pedro", "Pirapora", 
                            "Paracatu", "Dom Inocêncio", "Serra da Palmeira", "São Clemente", "Babilônia", "Santa Clara", 
                            "Tianguá", "ECHO RN", "ECHO BA", "Osório")
        bacias <- c("ARINOS", "JUAZEIRO", "FUTURO", "SERTAO", "PEDRO", "PIRAPORA", 
                    "PARACATU", "DIS", "SDP", "CLEMENTE", "BABILONIA", "CLARA", 
                    "TIANGUA", "ECHO_RN", "ECHO_BA", "OSORIO")
        
        local <- bacias_titulos[ree]
        local_save <- bacias[ree]
        
        # Create the scatter plot with swapped axes
        p <- ggplot2::ggplot(df, ggplot2::aes(x = ree, y = usina)) +
          ggplot2::geom_point(ggplot2::aes(size = ifelse(Type == "Observado", 2, 1),
                                           alpha = ifelse(Type == "Observado", 1, 0.9),
                                           color = cluster, shape = Type)) + # Adjust size based on Type
          ggplot2::scale_size_identity() + # Ensure that sizes are used as is
          ggplot2::labs(
            title = local,
            subtitle = paste(
              fases[j] |>
                stringr::str_replace("_", " "),
              "||",
              meses[k]
            ),
            x = paste("ENA REE", REE[ree], "(MWmed)"),
            y = paste("Fator de Capacidade", local, "(p.u.)") 
          ) +
          custom_colors +
          ggplot2::guides(color = ggplot2::guide_legend(title = "")) +
          ggthemes::theme_few() + 
          guides(alpha = FALSE, color = guide_legend(title = "Cluster"), shape = guide_legend(title = "Dado")) +
          theme(legend.position = "bottom")
        
        file_name <- paste0("./plots/", #coloque aqui seu caminho
                            "cluster_",local_save,"_",mes,"_",fase,".png"
        )
        
        ggsave(file_name, p, width = 20, height = 11, units = "cm")
      }
    }
    r = r + 1
  }
}

#' Função para plotar o scatter plot entre os dados e as densidades de cada cluster, 
#' considerando apenas os dados simulados.
#'
#' Esta função itera sobre cada fase e mês fornecida e gera um plot correspondente.
#' O tipo de plot marginal é fixo ("density").
#' A função usa o pacote ggplot2 e a função `ggMarginal` do pacote ggExtra para gerar os plots.
#'
#' @param simulado Uma lista de objetos contendo os dados simulados, já contendo as informações sobre os clusters.
#' @param ree Parâmetro auxiliar para sabermos qual a REE mais próxima da usina - legenda.
#' @return Nenhum retorno. A função salva os plots diretamente num diretório chamado `plots` presente no
#' caminho indicado (deve-se criar a pasta antes de rodar o código).
#' @examples
#' # Suponha que `sim` é a lista contendo os dados simulados:
#' plt_cluster_marginal(simulado = sim, ree = 1)
plt_cluster_marginal <- function(simulado, ree) {
  # Get the length of the lists
  len_list_df2 <- length(simulado)
  
  # Determine the number of iterations to perform
  num_iterations <- len_list_df2
  
  # Loop through each pair of lists
  for (j in 1:num_iterations) {
    # Get the current pair of lists
    list_df2 <- simulado[[j]]
    
    # Get the length of the current pair of lists
    len_list_df2_inner <- length(list_df2)
    
    # Determine the number of iterations to perform for the inner loop
    num_iterations_inner <- len_list_df2_inner
    
    # Loop through each pair of dataframes in the current pair of lists
    for (k in 1:num_iterations_inner) {
      df2 <- list_df2[[k]]
      clusters_simulados <- df2[,3]
      
      # Get the number of columns in the dataframes
      num_cols_df2 <- ncol(df2)
      
      # Determine the number of plots to create
      num_plots <- num_cols_df2 - 1
      
      # Loop through each pair of columns
      for (i in 2:(num_plots)) {
        # Create a new dataframe with df2
        df <- cbind(df2[, c(1, i)], cluster = clusters_simulados)
        colnames(df) <- c("usina", "ree", "cluster")
        
        myColors <- c("coral", "deeppink", "green", "royalblue", "cyan")
        names(myColors) <- c("1", "2", "3", "4")
        custom_colors <- scale_colour_manual(name = "Cluster", values = myColors)
        
        # Legend
        mes <- stringr::str_to_title(names(simulado[[j]])[k])
        #local <- stringr::str_to_title(names(df2)[1])
        fase <- stringr::str_to_title(names(simulado)[j])
        
        fases <- c("Fase Neutra", "Fase El Niño", "Fase La Niña")
        meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto",
                   "Setembro", "Outubro", "Novembro", "Dezembro")
        REE <- c("Sudeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Sudeste", "Sudeste",
                 "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste",
                 "Nordeste", "Nordeste", "Sul")
        bacias_titulos <- c("Arinos", "Juazeiro", "Futuro", "Sertão", "São Pedro", "Pirapora", 
                            "Paracatu", "Dom Inocêncio", "Serra da Palmeira", "São Clemente", "Babilônia", "Santa Clara", 
                            "Tianguá", "ECHO RN", "ECHO BA", "Osório")
        bacias <- c("ARINOS", "JUAZEIRO", "FUTURO", "SERTAO", "PEDRO", "PIRAPORA", 
                    "PARACATU", "DIS", "SDP", "CLEMENTE", "BABILONIA", "CLARA", 
                    "TIANGUA", "ECHO_RN", "ECHO_BA", "OSORIO")
        
        local <- bacias_titulos[ree]
        local_save <- bacias[ree]
        
        # Create the scatter plot with swapped axes
        p <- ggplot2::ggplot(df, ggplot2::aes(x = ree, y = usina)) +
          ggplot2::geom_point(ggplot2::aes(color = cluster)) + # Adjust size based on Type
          ggplot2::scale_size_identity() + # Ensure that sizes are used as is
          ggplot2::labs(
            title = local,
            subtitle = paste(
              fases[j] |>
                stringr::str_replace("_", " "),
              "||",
              meses[k]
            ),
            x = paste("ENA REE", REE[ree], "(MWmed)"),
            y = paste("Fator de Capacidade", local, "(p.u.)") 
          ) +
          custom_colors +
          ggplot2::guides(color = ggplot2::guide_legend(title = "")) +
          ggthemes::theme_few() + 
          guides(color = guide_legend(title = "Cluster"))
        
        p_final <- ggMarginal(p + theme(legend.position = "bottom"), type = "density", size = 4,
                              groupColour = TRUE,
                              groupFill = TRUE,
                              xparams = list(size = 0),
                              yparams = list(size = 0))
        
        file_name <- paste0("./plots/", #coloque aqui seu caminho
                            "density_cluster_",local_save,"_",mes,"_",fase,".png"
        )
        
        ggsave(file_name, p_final, width = 20, height = 11, units = "cm")
      }
    }
    #r = r + 1
  }
}
