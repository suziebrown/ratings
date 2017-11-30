########################################################
# Bayesian inference for Placket-Luce ranking models
########################################################
library(PLMIX)
data(d_nascar)
head(d_nascar)

# new_nascar <- matrix(0, 36, 87)
# for (j in 1:43){
#   for (i in 1:36){
#     new_nascar[i, d_nascar[i,j]] <- j 
#   }
# }
# K <- 87
# N <- 36
# Data_mat[Data_mat == 0] = 1000
# Data_mat <- new_nascar

# Creating some data 
x_vec <- seq(1,10, 1)
N <- 5
K <- 3
Data_mat <- matrix(0, N, K)
test <- 1:K#sample(K,K)
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
comp_message <- function(arr, i, n, k, length_xvec = length(x_vec)){
  message <- rep(0, length_xvec)
  for (temp in 1:length_xvec){
    prod_temp <- prod(arr[,,i,temp])
    message[temp] <- prod_temp/arr[n,k,i,temp]
  }
  return(message)
}

# Computing the projections q'
case_1 <- function(gamma, delta, k, K){
  c <- delta/(gamma-1) * (K-k)*gamma/delta
  d <- 1
  return(c(c,d))
}

case_2 <- function(gamma, delta, k, K){
  c <- 1 + delta/(gamma-1) * (K-k-1)*gamma/delta
  d <- delta/(gamma-1)*gamma/delta
  return(c(c,d))
}

q_prime_params <- function(c, d, a=2, b=2){
  E <- (c*a + d*(a+1))/(b*(c+d))
  E_2 <- (c*a*(a+1)+d*(a+1)*(a+2))/(b^2*(c+d))
  return(c(E^2/(E_2-E^2), E/(E_2-E^2)))
}

# Initializing the vectors
ar_message <- array(1, c(N, K, K, length(x_vec)))
n_int <- 7
#vec1 <- sample(N, n_int+100, replace = TRUE)
#vec2 <- sample(K, n_int+100, replace = TRUE)
#iteration = 1
#a <- cbind(vec1, vec2)
gamma = 3
delta = 2
iteration <-1
while(iteration<n_int){
  n <- sample(N,1)
  k <- sample(K,1)
  if (Data_mat[n,k] != 1000){
    w <- order(Data_mat[n,])
    for (i in 1:K){
      if (i == w[k]){
        #print('WE ARE IN THE FIRST CASE')
        mess <- comp_message(arr = ar_message, i = i, n = n, k = k)
        c<- case_1(gamma = gamma, delta = delta, k = k, K = K)[1]
        d<- case_1(gamma = gamma, delta = delta, k = k, K = K)[2]
      }
      else{
        #prnt('WE ARE IN THE SECOND CASE')
        mess <- comp_message(arr = ar_message, i = i, n = n, k = k)
        c<- case_2(gamma = gamma, delta = delta, k = k, K = K)[1]
        d<- case_2(gamma = gamma, delta = delta, k = k, K = K)[2]
      }
    alpha <- q_prime_params(c = c, d = d, a=(gamma-1), b=delta)[1]
    beta <- q_prime_params(c = c, d = d, a=(gamma-1), b=delta)[2]
    q_temp <- dgamma(x_vec, alpha, beta)
    print(mess)
    # print(alpha)
    # print(beta)
    #Q <- Q / rowSums(Q)
    Q[i, ] <- Q[i, ]^2/q_temp
    ar_message[n,k,i,] <- Q[i,]/mess
    }
    iteration <- iteration + 1
  }
  else{
    print('got a weird one with just zeros')
  }
  print(iteration)
  print(Q)
}


q_f <- matrix(0, K,length(x_vec))
for (i in 1:K){
  for (j in 1:length(x_vec)){
    q_f[i,j] <- prod(ar_message[,,i,j])
  }
}

q_f <- q_f / rowSums(q_f)
plot(x_vec,q_f[1,], ylim = c(0,1))
for (s in 2:K){
  lines(x_vec,q_f[s,], col = s)
}

# Q <- Q / rowSums(Q)
# plot(x_vec,Q[1,], ylim = c(0,1))
# for (s in 2:K){
#   lines(x_vec,Q[s,], col = s)
# }
# K
