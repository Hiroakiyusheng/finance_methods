require(yuima)
require(purrr)
require(dplyr)
require(ggplot2)
require(ggfortify)
require(rlist)
require(latex2exp)


#######################################################################################
# Monte Carlo Method ------------------------------------------------------------------
#######################################################################################


# サイコロ投げの期待値 ------------------------------------------------------------------
# set.seed(123)

## sample関数を使ってサイコロ投げを再現
N <- 1000 # number of sample
dice <- c(1, 2, 3, 4, 5, 6) # サイコロ
# sampling
dat <- sample(dice, size=N, replace=TRUE)
dat |> mean() # samplingしたものの平均を計算


## Nを増やすごとに平均値はどうなるか
p <- map_dbl(1:N, ~{mean(sample(dice, size=.x, replace=TRUE))}) |> 
  tibble(sample_mean = _, num = 1:N) |> 
  ggplot(aes(num, sample_mean)) + 
  geom_line(color='purple') + 
  labs(x = TeX(r'($N$)'), y = TeX(r'($\bar{X}_N$)'), title = TeX(r'(Convergence into  $\frac{7}{2}$)')) + 
  theme_light() + 
  theme(text = element_text(size=15, lineheight=.9, family="serif", face="bold.italic", colour="blue"), 
        plot.title = element_text(hjust=0.5), 
        legend.position = "none")
p
# ggsave('./dice.png', plot=p)



# オプション価格の期待値 ------------------------------------------------------------------
## 株価をsampling 
# number of sample
N <- 100 

# 株価のパラメーター
S0 <- 100
r <- 0.06
sigma <- 0.03

# オプションのパラメーター
K <- 100 # 行使価格
t <- 1 # 権利行使日

# 株価を大量に取得
St <- S0*exp((r-1/2*sigma^2)*t + sigma*sqrt(t)*rnorm(N))


## オプション価格の計算
# 各株価ごとにオプションの価値を計算
Ct <- map_dbl(St, ~max(.x-K, 0))

# 0期の価格の平均
C0 <- mean(exp(-r*t) * Ct)




## Nを増やすごとに平均値はどうなるか
# オプション価格のサンプリング関数
Call_option <- function(N, S0, r, sigma, K, t){
  # 株価をsampling 
  St <- S0*exp((r-1/2*sigma^2)*t + sigma*sqrt(t)*rnorm(N))
  
  ##オプション価格の計算
  ### 各株価ごとにオプションの価値を計算
  Ct <- map_dbl(St, ~max(.x-K, 0))
  
  ### 0期の価格の平均
  C0 <- mean(exp(-r*t) * Ct)
  
  return(C0)
}

## Nを増やすごとに平均値はどうなるか
p <- map_dbl(1:N, ~Call_option(.x, S0, r, sigma, K, t)) |> 
  tibble(sample_mean = _, num = 1:N) |> 
  ggplot(aes(num, sample_mean)) + 
  geom_line(color='purple') + 
  labs(x = TeX(r'($N$)'), y = TeX(r'($C_0^{(N)}$)'), title = TeX(r'(Convergence of Option Price$)')) +
  theme_bw() + 
  theme(text = element_text(size=15, lineheight=.9, family="serif", face="bold.italic", colour="brown"), 
        plot.title = element_text(hjust=0.5), 
        legend.position = "none")


# ggsave('./Call_option.png', plot=p)




#######################################################################################
# Simulating Stochastic Process ------------------------------------------------------------------
#######################################################################################

# Standard Brownian Motion ------------------------------------------------------------------
## X_t = Z_t
Bm <- setModel(drift=0, diffusion = 1)

### sample pathの作図
x <- simulate(Bm)
### 作図
x@data@original.data |> 
  autoplot(size=1) + 
  labs(x = 'time', y = TeX(r'($w(t)$)'), title = 'Brownian Motion') + 
  theme_light() + 
  theme(text = element_text(size=20, lineheight=.9, family="serif", face="bold.italic", colour="brown"), 
        plot.title = element_text(hjust=0.5), 
        axis.title.x = element_text(size=16, family="mono"), 
        legend.position = "none")


## 複数回シミュレーション
N <- 100 # シミュレーション回数

# シミュレート
Z <- map(1:N, ~{simulate(Bm, seed=.x)@data@original.data}) |> 
  Reduce(ts.union, x = _)

# 作図
p <- autoplot(Z, facets=FALSE, size=1) + 
  labs(x = 'time', y = TeX(r'($w(t)$)'), title = 'Brownian Motion') + 
  theme_bw() + 
  theme(text = element_text(size=20, lineheight=.9, family="serif", face="bold.italic", colour="brown"), 
        plot.title = element_text(hjust=0.5), 
        axis.title.x = element_text(size=16, family="mono"), 
        legend.position = "none")
p
# 図の保存
# ggsave('./BM.png', plot=p)







