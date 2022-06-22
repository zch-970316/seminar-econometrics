##  This file generates Figures 19.5 and 19.6
##  Wage/Experience
#### Uses data file cps09mar.txt ####


dt_file <- here::here("data/cps09mar.dta")
#dat <- read.table(dt_file)
dat <- haven::read_dta(dt_file)

####data set 1 ####
## subsample of black men with 12 years of education
bf <- (dat[,11]==2)&(dat[,2]==1)&(dat[,4]==12)
dat1 <- dat[bf,]
## earning per hours
y <- as.matrix(log(dat1[,5]/(dat1[,6]*dat1[,7])))
x <- as.matrix(dat1[,1]-dat1[,4]-6)
n <- length(y)

dt_cps1 <-  tibble(X=x, Y=y)

#### basic plot ####
p0 <- ggplot() +
  geom_point(aes(X, Y),data = dt_cps1, pch=21) +
  labs(x= "X职业年数", y ="log(Y)时均工资的对数") +
  scale_x_continuous(breaks = seq(0,70,10), limits = c(0,65)) +
  scale_y_continuous(breaks = seq(0,4,1), limits = c(0,4)) +
  theme_bw()


#### Reference Rule ####
## window range
sx1 <- 0
sx2 <- 40
x1 <- matrix(1,n,1)
zz <- cbind(x1,x,x^2,x^3,x^4)
beta <- solve((t(zz)%*%zz),(t(zz)%*%y))
xtrim <- (x<=sx2)*(x>=sx1)
b <- mean(((beta[3]+x*3*beta[4]+(x^2)*6*beta[5])^2)*xtrim)
e <- y - zz%*%beta
sig <- (sum(e^2))/(n-5)
hrot <- 0.58*(((sx2-sx1)*sig/n/b)^.2)


#### CV Bandwidth Selection ####
g <- 200
h1 <- hrot/3
h2 <- 3*hrot
hh <- seq(h1,h2,(h2-h1)/g)
hn <- length(hh)

LL <- matrix(0,n,hn)
for (i in 1:hn){
  hi <- hh[i]
  for (j in 1:n){
    xj <- x-x[j]
    k <- dnorm(xj/hi)
    k[j] <- 0
    z <- cbind(x1,xj)
    zk <- z*(k%*%cbind(1,1))
    beta <- solve(t(zk)%*%z,t(zk)%*%y)
    LL[j,i] <- (y[j]-beta[1])^2
  }
}

LL2 <- LL*(xtrim%*%matrix(1,1,hn))
cvLL <- colMeans(LL2)
i <- which.min(cvLL)
hLL <- hh[i]
hLL_tex <-paste0("$h_{CV}=$",number(hLL,0.0001),"")
LLmin <- min(cvLL)

#### plot 19.5-a ####

tb_cvc <- tibble(
  h_tune = hh, 
  #cv_NW = cvnw,
  cv_LL = cvLL)

lwd <- 0.8
lwadd <- 0.2

p_cvc <- ggplot(aes(x = h_tune),data = tb_cvc) +
  geom_line(aes(y = cv_LL),
            lty = "dashed", color = "blue",
            lwd = lwd) +
  labs(x= "谱宽h", y ="交叉验证准则函数值CV(h)") +
  scale_x_continuous(#expand = expansion(add = c(0, .06)),
    breaks = seq(1,12,1), 
    limits = c(1,12)) +
  scale_y_continuous(expand = expansion(add = c(0,0)),
                     breaks = seq(0.2290,0.2310,0.0005), 
                     limits = c(0.2290,0.2310),
                     labels = scales::number_format(accuracy = 0.0001)
                     ) +
  geom_segment(aes(x=hLL, xend = hLL,
                   y= LLmin, yend = c(0.2290)),
               arrow = arrow(length = unit(0.1,"cm"),
                             type = "closed"),
               lty = "dashed", color= "gray", lwd=lwd) +
  geom_text(aes(x = hLL+0.5, y=LLmin-0.0004), 
            label =latex2exp::TeX(hLL_tex),
            #parse=TRUE,
            color= "blue", size=4)  +
  theme_bw() 


#### Regression Estimation ####
g <- 201
xg <- seq(sx1,sx2,(sx2-sx1)/(g-1))
m1 <- matrix(0,g,1)
m2 <- matrix(0,g,1)
for (j in 1:g){
  xj <- x-xg[j]
  z <- cbind(x1,xj)
  z1 <- z*(dnorm(xj/hrot)%*%cbind(1,1))
  z2 <- z*(dnorm(xj/hLL)%*%cbind(1,1))
  beta1 <- solve(t(z1)%*%z,t(z1)%*%y)
  beta2 <- solve(t(z2)%*%z,t(z2)%*%y)
  m1[j,1] <- beta1[1]
  m2[j,1] <- beta2[1]
}

#### plot 19.5-b ####

tb_mxh <- tibble(xg = xg,
                 mx1=as.vector(m1), 
                 mx2=as.vector(m2))

p00 <- ggplot() +
  geom_point(aes(X, Y),data = dt_cps1, pch=21,alpha =0.1) +
  labs(x= "X职业年数", y ="log(Y)时均工资的对数") +
  scale_x_continuous(breaks = seq(0,40,5), limits = c(0,40)) +
  scale_y_continuous(breaks = seq(2,2.7,0.1), limits = c(2,2.7)) +
  theme_bw()

p_mxh1 <- p00 +
  geom_line(aes(x = xg, y = m1, 
                color="m1", lty="m1"),
            lwd = lwd+lwadd,
            data = tb_mxh) +
  theme_bw() +
  scale_color_manual(
    name="",
    breaks = c("m1"),
    labels = c(expression(m(x):h[ROT])),
    values=c("green"))+
  scale_linetype_manual(
    name="",
    breaks = c("m1"),
    labels = c(expression(m(x):h[ROT])),
    values=c("dotted"))+
  theme(legend.position = "right")

p_mxh2 <- p00 +
 geom_line(aes(x = xg, y = m2, 
                color="m2", lty="m2"),
            lwd = lwd+lwadd,
            data = tb_mxh) +
  scale_color_manual(
    name="",
    breaks = c("m2"),
    labels = c(expression(m(x):h[CV])),
    values=c("blue"))+
  scale_linetype_manual(
    name="",
    breaks = c("m2"),
    labels = c(expression(m(x):h[CV])),
    values=c("dashed"))+
  theme(legend.position = "right")

p_mxh <- p00 +
  geom_line(aes(x = xg, y = m1, 
                color="m1", lty="m1"),
            lwd = lwd+lwadd,
            data = tb_mxh) +
  geom_line(aes(x = xg, y = m2, 
                color="m2", lty="m2"), 
            lwd = lwd+lwadd,
            data = tb_mxh) +
  scale_color_manual(
    name="",
    breaks = c("m1", "m2"),
    labels = c(expression(m(x):h[ROT]),expression(m(x):h[CV])),
    values=c("green", "blue"))+
  scale_linetype_manual(
    name="",
    breaks = c("m1", "m2"),
    labels = c(expression(m(x):h[ROT]),expression(m(x):h[CV])),
    values=c("dotted", "dashed"))+
  theme(legend.position = "right")


#### ROT Residual Estimation ####
h <- hrot
e2 <- matrix(0,n,1)
for (j in 1:n){
  xj <- x-x[j]
  k <- dnorm(xj/h)
  k[j] <- 0
  z <- cbind(x1,xj)
  zk <- z*(k%*%cbind(1,1))
  beta <- solve(t(zk)%*%z,t(zk)%*%y)
  e2[j,1] <- (y[j]-beta[1])^2
}

####  Variance ROT Bandwidth ####
beta <- solve((t(zz)%*%zz),(t(zz)%*%e2))
b <- mean(((beta[3]+x*3*beta[4]+(x^2)*6*beta[5])^2)*xtrim)
e <- e2 - zz%*%beta
sig <- (sum(e^2))/(n-5)
hrotv <- 0.58*(((sx2-sx1)*sig/n/b)^.2)


#### Variance CV Bandwidth Selection ####
h1 <- 2
h2 <- 40
hh <- seq(h1,h2,(h2-h1)/g)
hn <- length(hh)
LLV <- matrix(0,n,hn)
NWV <- matrix(0,n,hn)
for (i in 1:hn){
  hi <- hh[i]
  for (j in 1:n){
    xj <- x-x[j]
    k <- dnorm(xj/hi)
    k[j] <- 0
    mu <- (t(k)%*%e2)/sum(k)
    NWV[j,i] <- (e2[j]-mu)^2
    z <- cbind(x1,xj)
    zk <- z*(k%*%cbind(1,1))
    beta <- solve(t(zk)%*%z,t(zk)%*%e2)
    LLV[j,i] <- (e2[j]-beta[1])^2
  }
}
cvNWV <- colMeans(NWV*(xtrim%*%matrix(1,1,hn)))
cvLLV <- colMeans(LLV*(xtrim%*%matrix(1,1,hn)))
i1 <- which.min(cvNWV)
i2 <- which.min(cvLLV)
hNWV <- hh[i1]
hLLV <- hh[i2]
hNWV_tex <-paste0("$h_{NW}=$",number(hNWV,0.1),"")
hLLV_tex <-paste0("$h_{CV}=$",number(hLLV,0.1),"")

NWminV <- min(cvNWV)
LLminV <- min(cvLLV)



#### plot 19.6-a ####
## Cross-Validation for Conditional Variance

tbl_cvcv <- tibble(
  h_tune = hh, 
  cv_NW = cvNWV,
  cv_LL = cvLLV)

p_cvcv <- ggplot(aes(x = h_tune),data = tbl_cvcv) +
  geom_line(aes(y = cv_NW, lty = "CV_NW", color = "CV_NW"),
            lwd = lwd) +
  geom_line(aes(y = cv_LL, lty = "CV_LL", color = "CV_LL"),
            lwd = lwd) +
  labs(x= "谱宽h", y ="交叉验证准则函数值CV(h)") +
  scale_x_continuous(#expand = expansion(add = c(0, .06)),
    breaks = seq(0,40,5), 
    limits = c(0,40)) +
  scale_y_continuous(expand = expansion(add = c(0,0)),
                     breaks = seq(1.200, 1.216,0.004), 
                     limits = c(1.200, 1.216),
                     labels = scales::number_format(accuracy = 0.001)
                     ) +
  theme_bw() +
  geom_segment(aes(x=hNWV, xend = hNWV,
                   y= NWminV, yend = 1.2),
               arrow = arrow(length = unit(0.1,"cm"),
                             type = "closed"),
               lty = "dotted", color= "gray", lwd=lwd)  +
  geom_text(aes(x = hNWV-2, y=NWminV-0.002), 
            label =latex2exp::TeX(hNWV_tex),
            color= "orange", size=3) +
  geom_segment(aes(x=hLLV, xend = hLLV,
                   y= LLminV, yend = 1.2),
               arrow = arrow(length = unit(0.1,"cm"),
                             type = "closed"),
               lty = "dashed", color= "gray", lwd=lwd) +
  geom_text(aes(x = hLLV-2, y=LLminV-0.002), 
            label =latex2exp::TeX(hLLV_tex),
            color= "blue", size=3)  +
  scale_color_manual(
    name="",
    breaks = c("CV_NW", "CV_LL"),
    values=c("orange", "blue"))+
  scale_linetype_manual(
    name="",
    breaks = c("CV_NW", "CV_LL"),
    values=c("dotted", "dashed"))+
  theme(legend.position = "right")

#### Variance Estimation ####
se <- matrix(0,g,1)
u <- cbind(e2,e2)
for (j in 1:g){
  xj <- x-xg[j]
  z <- cbind(x1,xj)
  K <- dnorm(xj/hrot)
  z1 <- z*cbind(K,K)
  ZKZ <- solve(t(z1)%*%z)
  ZK2 <- t(z1)%*%(z1*u)
  V1 <- ZKZ%*%ZK2%*%ZKZ
  se[j,1] = sqrt(V1[1,1])
}

L1 <- m1-1.96*se
U1 <- m1+1.96*se

#### Plot 19.6-b Confidence Bands ####

tbl_band <- tibble(
  xg =xg,
  mx = m1,
  lwr = L1,
  upr = U1
)


p_band <- p00 +
  geom_line(aes(x = xg, y = mx),
            data = tbl_band,
            color = "blue", lty = "solid", lwd = lwd) +
  geom_ribbon(aes(x=xg,ymin = lwr, ymax = upr),
              data = tbl_band,
              alpha = 0.2)


