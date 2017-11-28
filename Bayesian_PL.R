########################################################
# Bayesian inference for Placket-Luce ranking models
########################################################

# Creating some data 
x_vec <- seq(1,10, 0.5)
N <- 10
K <- 10
Data_mat <- matrix(0, N, K)
test <- sample(10,10)
for (i in 1:N){
  Data_mat[i,] <- test
}

Q <- matrix(0, K, length(x_vec))
for (k in 1:K){
  Q[k, ] <- dgamma(x_vec, 3, 2)
}

# Auxilliary functions
ugam <- function(x, alpha = 3 ,beta=2){
  return(x^(alpha-1)*exp(-beta*x))
}

# Computing the message
comp_message <- function(ar_message, i, n, k, length_xvec = 100){
  message <- rep(0, length_xvec)
  print('jshdflksafasjfhalksfjhslkjfhjhal')
  for (temp in 1:length_xvec){
    prod_temp <- prod(ar_message[,,i,temp])
    message[temp] <- prod_temp/ar_message[n,k,i, temp]
  }
  print('done')
  return(message)
}

# Computing the projections q'
case_1 <- function(k, K){
  c <- 1 * (K-k)*3/2
  d <- 1
  return(c(c,d))
}

case_2 <- function(k, K){
  c <- 1 + 3* (K-k-1)*2/3
  d <- 3/2
  return(c(c,d))
}

q_prime_params <- function(c,d, a=2, b=2){
  E <- (c*a + d*(a+1))/(b*(c+d))
  E_2 <- (c*a*(a+1)+d*(a+1)*(a+2))/(b^2*(c+d))
  return(c(E^2/(E_2-E^2)), E/(E_2-E^2))
}

# Initializing the vectors
ar_message <- array(1, c(N, K, K, length(x_vec)))
n_int <- 10
vec1 <- sample(n_int,N, replace = TRUE)
vec2 <- sample(n_int,K, replace = TRUE)

a <- cbind(vec1, vec2)
for (iteration in 1:n_int){
  n <- a[iteration,][1]
  k <- a[iteration,][2]
  w <- order(Data_mat[n,])
  for (i in 1:K){
    if (i == w[k]){
      mess <- comp_message(ar_message = ar_message, i = i, n = n, k = k)
      c<- case_1(k = k, K = K)[1]
      d<- case_1(k = k, K = K)[2]
    }
  else{
    mess <- comp_message(ar_message = ar_message, i = i, n = n, k = k)
    c<- case_1(k = k, K = K)[1]
    d<- case_1(k = k, K = K)[2]
  }
  alpha <- q_prime_params(c = c, d = d)[1]
  beta <- q_prime_params(c = c, d = d)[2]
  q_temp <- ugam(x_vec, alpha, beta)
  Q[i, ] <- Q[i, ]^2/q_temp
  ar_message[n,k,i,] <- Q[i, ]/mess
  }
}
