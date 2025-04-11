library(readr)
library(quantmod)
library(xts)

sp500ticket<-read.csv("C:/Users/WINDOWS 10/Desktop/progetto laboratorio/Cartel1.csv")
sym<-sp500ticket$Symbol
sym3<-c("BAC","GOOGL","ADP","AAPL")
getSymbols(sym3, src = "yahoo", from = "2018-01-02", to = "2024-06-28")
dim(BAC)
par(mfrow=c(2,2))
plot.xts(BAC$BAC.Adjusted, main="BAC")
plot.xts(GOOGL$GOOGL.Adjusted, main="GOOGL")
plot.xts(ADP$ADP.Adjusted, main="ADP")
plot.xts(AAPL$AAPL.Adjusted, main="AAPL")
Bac_sam<-BAC[index(BAC) <= "2021-12-31", ]
Googl_sam<-GOOGL[index(GOOGL) <= "2021-12-31", ]
Adp_sam<-ADP[index(ADP) <= "2021-12-31", ]
Aapl_sam<-AAPL[index(AAPL) <= "2021-12-31", ]
dim(Bac_sam)
r<-function(x) { p<-as.numeric(x)
 y<-diff(log(p))}
r_Bac<-r(Bac_sam$BAC.Adjusted)
r_Googl<-r(Googl_sam$GOOGL.Adjusted)
r_Adp<-r(Adp_sam$ADP.Adjusted)
r_Aapl<-r(Aapl_sam$AAPL.Adjusted)
t<-index(Bac_sam) [2:length(index(Bac_sam))]
r2_Bac<-xts(r_Bac,order.by = t)
r2_Googl<-xts(r_Googl,order.by = t)
r2_Adp<-xts(r_Adp,order.by = t)
r2_Aapl<-xts(r_Aapl,order.by = t)
plot(r2_Bac, main="BAC")
plot(r2_Googl, main="GOOGL")
plot(r2_Adp, main="ADP")
plot(r2_Aapl, main="AAPL")
hist(r_Bac, main="BAC")
hist(r_Googl, main="BAC")
hist(r_Adp, main="BAC")
hist(r_Aapl, main="BAC")
r_tab<-cbind(r_Bac,r_Googl,r_Adp,r_Aapl)
library(quadprog)
mu<-colMeans(r_tab)
sigma<-cov(r_tab)
n<-dim(r_tab)
H<-2*sigma
f<-rep(0,4)
A<-cbind(mu,rep(1,4))
b<-c(0.001,1)
opt<-solve.QP(H,f,A,b,meq =2)
x_opt<-opt$solution
r_p<-r_tab %*% x_opt
alpha=0.05
VaR_hist<-quantile(r_p,probs = alpha)
VaR_hist
mu_p<-apply(r_tab, 2,mean)%*%x_opt
sigma_p<-t(x_opt) %*% sigma %*% x_opt
par(mfrow=c(1,1))
plot(sigma_p, mu_p, pch=19, col="blue", main="Mean-Variance Plot")
VaR_par<-qnorm(alpha, mean = mu_p,sd=sqrt(sigma_p))
VaR_par
sqrt(sigma_p)
set.seed(8)
r_p_MC<-rnorm(10000, mean=mu_p,sd=sqrt(sigma_p))
VaR_Mc<-quantile(r_p_MC,alpha)
VaR_Mc
### stress test
win<-20
vol_Bac<-NA
for (i in 1:(length(r_Bac)-win)){
  vol_Bac[i]<-sd(r_Bac[i:(i+win )])
}
max_vBac<-max(vol_Bac)


vol_Go<-NA
for (i in 1:(length(r_Googl)-win)){
  vol_Go[i]<-sd(r_Googl[i:(i+win )])
}
max_vGo<-max(vol_Go)

vol_Adp<-NA
for (i in 1:(length(r_Adp)-win)){
  vol_Adp[i]<-sd(r_Adp[i:(i+win )])
}
max_vAdp<-max(vol_Adp)

vol_Aapl<-NA
for (i in 1:(length(r_Aapl)-win)){
  vol_Aapl[i]<-sd(r_Aapl[i:(i+win )])
}
max_vAapl<-max(vol_Aapl)

cov_BAC_GOOGL <- rep(NA, length(r_Bac) - win)
for (i in 1:(length(r_Bac) - win)) {
  cov_BAC_GOOGL[i] <- cov(r_Bac[i:(i+win)], r_Googl[i:(i+win)])
}
max_cov_BAC_GOOGL <- max(cov_BAC_GOOGL)


cov_BAC_ADP <- rep(NA, length(r_Bac) - win)
for (i in 1:(length(r_Bac) - win)) {
  cov_BAC_ADP[i] <- cov(r_Bac[i:(i+win)], r_Adp[i:(i+win)])
}
max_cov_BAC_ADP <- max(cov_BAC_ADP)


cov_BAC_AAPL <- rep(NA, length(r_Bac) - win)
for (i in 1:(length(r_Bac) - win)) {
  cov_BAC_AAPL[i] <- cov(r_Bac[i:(i+win)], r_Aapl[i:(i+win)])
}
max_cov_BAC_AAPL <- max(cov_BAC_AAPL)


cov_GOOGL_ADP <- rep(NA, length(r_Googl) - win)
for (i in 1:(length(r_Googl) - win)) {
  cov_GOOGL_ADP[i] <- cov(r_Googl[i:(i+win)], r_Adp[i:(i+win)])
}
max_cov_GOOGL_ADP <- max(cov_GOOGL_ADP)


cov_GOOGL_AAPL <- rep(NA, length(r_Googl) - win)
for (i in 1:(length(r_Googl) - win)) {
  cov_GOOGL_AAPL[i] <- cov(r_Googl[i:(i+win)], r_Aapl[i:(i+win)])
}
max_cov_GOOGL_AAPL <- max(cov_GOOGL_AAPL)


cov_ADP_AAPL <- rep(NA, length(r_Adp) - win)
for (i in 1:(length(r_Adp) - win)) {
  cov_ADP_AAPL[i] <- cov(r_Adp[i:(i+win)], r_Aapl[i:(i+win)])
}
max_cov_ADP_AAPL <- max(cov_ADP_AAPL)

#stress scnario 1#
cova<-cov(r_tab)
cova_stress<-cova
diag(cova_stress)<-c(max_vBac^2,max_vGo^2,max_vAdp^2,max_vAapl^2)
variance_p_stress<-t(x_opt) %*% cova_stress %*% x_opt
VaR_stress<-qnorm(alpha, mean=mu_p,sd=sqrt(variance_p_stress))
VaR_stress
#stress scenario 2
cova_stress2<-cova
cova_stress2[2,4]<-cova_stress2[4,2]<-max_cov_GOOGL_AAPL
variance_p_stress2<-t(x_opt) %*% cova_stress2 %*% x_opt
VaR_stress2<-qnorm(alpha, mean=mu_p,sd=sqrt(variance_p_stress2))
VaR_stress2
#stress scenario 3
corr_eq<-matrix(1,nrow=nrow(cova), ncol = ncol(cova))
D <- diag(sqrt(diag(cova)))
cova_equi <- D %*% corr_eq %*% D
lambda <- 0.7
cova_stress3 <- (1-lambda)*cova+lambda*cova_equi
variance_p_stress3<-t(x_opt) %*% cova_stress3 %*% x_opt
VaR_stress3<-qnorm(alpha, mean=mu_p,sd=sqrt(variance_p_stress3))
VaR_stress3

#backtesting
testset<- as.data.frame(cbind(BAC$BAC.Adjusted[index(BAC$BAC.Adjusted)>"2021-12-31"],
                               GOOGL$GOOGL.Adjusted[index(GOOGL$GOOGL.Adjusted)>"2021-12-31"],
                               ADP$ADP.Adjusted[index(ADP$ADP.Adjusted)>"2021-12-31"],
                               AAPL$AAPL.Adjusted[index(AAPL$AAPL.Adjusted)>"2021-12-31"]))
r_test<-apply(testset,2,r)
r_p_test<-r_test %*% x_opt
t_test<- index(BAC) [(length(index(Bac_sam))+2):length(index(BAC))]
r_p_test_xts<-xts(r_p_test,order.by = t_test)
par(mfrow=c(1,1))
plot(r_p_test_xts, main='Portfolio VaR violations', col='red')
VaR_line<-xts(rep(VaR_Mc, length(r_p_test_xts)),order.by = t_test)
lines(VaR_line, col='blue')
violation <- NA
for (i in 1:nrow(r_p_test)){
  violation [i] <- ifelse(r_p_test[i] < VaR_Mc, 1, 0)
}

T <- nrow(r_test)
n_viol <- sum(violation)

binom.test(n_viol, T, p = 0.05,
           alternative = "two.sided",
           conf.level = 0.95)

z_stat <- (n_viol-T*alpha)/sqrt(T*alpha*(1-alpha))
crit_val <- abs(qnorm(0.025,0,1)) 
pvalue <- pnorm(z_stat)*2
z_stat
crit_val
pvalue
#expected shortfall
q<-quantile(r_p,alpha)
ES_hist<-mean(r_p[r_p<=q])
ES_hist
q_par<-qnorm(alpha,mean=mu_p,sd=sigma_p)
tail_mean_pf <- integrate(function(x) 
  x * dnorm(x, mean=mu_p, sd=sqrt(sigma_p)), 
  -Inf, q_par)$value / alpha
ES_par<-tail_mean_pf
ES_par
ES_mc<- mean(r_p_MC[r_p_MC<VaR_Mc])
ES_mc

  
 
