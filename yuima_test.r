# 先にpackageをインストールしておく
install.packages("yuima")

# 以下2つは今回使用していない
# install.packages("tidyverse")
# install.packages("quantmod")

# Reference::  
# 今回simulationしかしていないが，yuimaを使えば様々なことができる
# 以下公式document
# https://cran.r-project.org/web/packages/yuima/yuima.pdf


# Preparation -------------------------------------------------------------
# load packages
require(yuima)
# Fix RN
set.seed(123)



# AR(1) & OU ---------------------------------------------------------------
N <- 100
K <- 100

# AR(1)
x <- rep(0, N)
x[1] <- 0
for (i in 2:length(x)) {
  x[i] <- 0.8 * x[i - 1] + rnorm(1, mean = 0, sd = 1) 
}
plot(x, type = 'l')


# Ornstein-Uhlenbeck
# dX_t = -theta*X_t dt + dZ_t
OU <- setModel(drift = "-theta*x", diffusion = 1)
# simulation
y <- simulate(OU, true.par=list(theta=0.2), Initial=0, Terminal=N, n=K)
plot(y)
# N=Kなら以下の二つのモデルはコンピューター上で同一




# ODE and SDE -------------------------------------------------------------
# drift parameter
a <- 1.0 # mu
# initial value
c <- 0.5 # X_0 = c


# ODE: dX_t = mu*X_t dt
curve(exp(a*x))


# SDE: dX_t = mu*X_t dt + sigma X_t dZ_t
gBm <- setModel(drift="mu*x", diffusion="sigma*x")
# simulation
y <- simulate(gBm, true.par=list(mu=a, sigma=0.3), xinit=c)
plot(y)


# ODEとSDEを重ね書きしてみる
# curve(exp(mu*x))
par(new=T) # 重ね書きする
plot(y)




# 以下代表的なSDE
# Model1:Standard Brownian Motion ------------------------------------------------------------------
# dX_t = dZ_t
Bm <- setModel(drift=0, diffusion = 1)
# simulation
x <- simulate(Bm)
plot(x)


# Model2: Ornstein-Uhlenbeck ------------------------------------------------------------------
# dX_t = -theta*X_t dt + dZ_t
OU <- setModel(drift = "-theta*x*t", diffusion = 1)
# simulation
x <- simulate(OU, true.par=list(theta=0.2), xinit=0.6)
plot(x)


# Model3: Geometric Brownian Motion ---------------------------------------
gBm <- setModel(drift="mu*x", diffusion="sigma*x")
# simulation
x <- simulate(gBm, true.par=list(mu=1, sigma=0.5), xinit=0.5)
plot(x)

# Model4: Vasicek ---------------------------------------------------------
vasicek <- setModel(drift="theta1-theta2*x", diffusion="theta3")
# simulation
x <- simulate(vasicek, true.par=list(theta1=0.6, theta2=0.2, theta3=0.5), xinit=0.01)
plot(x)


# Model5: CIR -------------------------------------------------------------
# vasicekと異なり確率1で正の値をとる
CIR= setModel(drift="theta1-theta2*x", diffusion="theta3*sqrt(x)")
# simulation
x <- simulate(CIR, true.par=list(theta1=0.2, theta2=0.2, theta3=0.5), xinit=0.01)
plot(x)



