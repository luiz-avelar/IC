library(rstan)
library(tidyverse)
library(glue)
library(gridExtra)

source("geracao_dados.R")

# Parametros poisson
home <- 0.21
mu_att <- 0.1
mu_def <- 0
sigma_att <- 0.25
sigma_def <- 0.25
bn_size <- 5.756
dist <- 'binomial_negativa'

# Numero de simulacoes
n_simulacoes <- 1000

# Vetores de parametros
home_vec      <- vector("double", n_simulacoes)
mu_att_vec    <- vector("double", n_simulacoes)
mu_def_vec    <- vector("double", n_simulacoes)
sigma_att_vec <- vector("double", n_simulacoes)
sigma_def_vec <- vector("double", n_simulacoes)

# Definicao do modelo stan
prg <- stan_model(file = "modelo-poisson-misto.stan")

# Funcao para criar os histomagras
make_histogram <- function(parameter, parameter_vec) {
  plot <- ggplot() +
    geom_histogram(aes(x = parameter_vec)) + 
    geom_vline(xintercept = parameter, color = "red") + 
    labs(
      y = 'Count',
      x = deparse(substitute(parameter))
    )
  
  return(plot) 
}

# Geracao dos dados para todas as simulacoes a serem feitas
dados_all <- replicate(
  n_simulacoes,
  geracao(home, mu_att, mu_def, sigma_att, sigma_def, dist, bn_size)
)

# Simulacoes
for(i in 1:n_simulacoes) {
  
  print(glue('Simulacao: ', i))
  
  dados <- dados_all[, i]
  
  dados_list <- list(G = 760,
                     T = 20,
                     h = dados$id_h,
                     a = dados$id_v,
                     y1 = dados$y_h,
                     y2 = dados$y_v
  )
  
  fit_sim <- sampling(prg, data = dados_list, chains = 1, iter = 4000, cores = 1, refresh = 1000)
  
  home_vec[i]      <- round(summary(fit_sim)[["summary"]]["home", 1], 2)
  mu_att_vec[i]    <- round(summary(fit_sim)[["summary"]]["mu_att", 1], 2)
  mu_def_vec[i]    <- round(summary(fit_sim)[["summary"]]["mu_def", 1], 2)
  sigma_att_vec[i] <- round(summary(fit_sim)[["summary"]]["sigma_att", 1], 2)
  sigma_def_vec[i] <- round(summary(fit_sim)[["summary"]]["sigma_def", 1], 2)
}

# Plot dos parametros
plot_home      <- make_histogram(home, home_vec)
plot_mu_att    <- make_histogram(mu_att, mu_att_vec)
plot_mu_def    <- make_histogram(mu_def, mu_def_vec)
plot_sigma_att <- make_histogram(sigma_att, sigma_att_vec)
plot_sigma_def <- make_histogram(sigma_def, sigma_def_vec)

grid.arrange(plot_home, plot_mu_att, plot_mu_def, plot_sigma_att, plot_sigma_def)