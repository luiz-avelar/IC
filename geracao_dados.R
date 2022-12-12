geracao <- function(home, mu_att, mu_def, sigma_att, sigma_def, dist, bn_size = NULL){
  
  T = 20 # Total de times
  aux <- cbind(rep(1:20, each = 40), rep(1:20, times = 40))
  aux <- aux[aux[,1] != aux[,2],]
  
  id_h <- aux[,1]
  id_v <- aux[,2]
  
  J = length(id_h)# total de jogos
  
  ## Geracao dos efeitos aleatorios
  att <- rnorm(T - 1, mu_att, sigma_att)
  def <- rnorm(T - 1, mu_def, sigma_def)
  att[T] <- 0
  def[T] <- 0
  
  ## Taxas
  theta_h <- exp(home + att[id_h] + def[id_v])
  theta_v <- exp(att[id_v] + def[id_h])
  
  if(dist == 'poisson') {
    y_h <- rpois(J, theta_h)
    y_v <- rpois(J, theta_v)
  } 
  else if (dist == 'binomial_negativa') {
    y_h <- rnbinom(J, mu = theta_h, size = bn_size)
    y_v <- rnbinom(J, mu = theta_v, size = bn_size)
  }

  return(data.frame(id_jogo = 1:J, id_h = id_h, id_v = id_v, y_h = y_h, y_v = y_v))
}