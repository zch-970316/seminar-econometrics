##  This file generates Figure 19.4

####  Bandwidth Selection, Regression Estimation ####

## simulate data 
n <- 100
set.seed(180)
xm = 10
x <- runif(n,0,xm)
a <- pi/4
y <- sin((x-2)*a)/((x-2)*a) + rnorm(n)/4

dt <- tibble(X=x, Y=y)

## basic plot
p0 <- ggplot() +
  geom_point(aes(X, Y),data = dt, pch=21) +
  labs(x= "自变量X", y ="因变量Y") +
  scale_x_continuous(breaks = seq(0,10,2), limits = c(0,10)) +
  scale_y_continuous(breaks = seq(-1,2,0.5), limits = c(-1,2)) +
  theme_bw()

#### Reference Rule ####

z <- cbind(matrix(1,n,1),x,x^2,x^3,x^4)
beta <- solve((t(z)%*%z),(t(z)%*%y))
mx <- z%*%beta
mx_q2 <- beta[3]+x*3*beta[4]+(x^2)*6*beta[5]
b <- mean((mx_q2)^2)
e <- y - z%*%beta
sig <- (sum(e^2))/(n-5)
hrot <- 0.58*((xm*sig/n/b)^.2)


#### Cross-Validation Calculation ####

## grid value for h
g <- 200
h1 <- hrot/3
h2 <- 3*hrot
hh <- seq(h1,h2,(h2-h1)/g)
hn <- length(hh)

nw <- matrix(0,hn,n)
LL <- matrix(0,hn,n)
x1 <- matrix(1,n,1)

for (i in 1:hn){
  hi <- hh[i]
  for (j in 1:n){
    xj <- x-x[j]
    k <- dnorm(xj/hi)
    k[j] <- 0
    nw[i,j] <- (y[j]-(t(k)%*%y)/sum(k))^2
    z <- cbind(x1,xj)
    zk <- z*k
    beta <- solve(t(zk)%*%z,t(zk)%*%y)
    LL[i,j] <- (y[j]-beta[1])^2
  }
}
cvnw <- rowMeans(nw)
cvLL <- rowMeans(LL)
hnw <- hh[which.min(cvnw)]
hLL <- hh[which.min(cvLL)]
nwmin <- min(cvnw)
LLmin <- min(cvLL)



#### plot 19.4-a ####

tbl_cvc <- tibble(
  h_tune = hh, 
  cv_NW = cvnw,
  cv_LL = cvLL)

lwd <- 0.8

p_cvc <- ggplot(aes(x = h_tune),data = tbl_cvc) +
  geom_line(aes(y = cv_NW, lty = "CV_NW", color = "CV_NW"),
            lwd = lwd) +
  geom_line(aes(y = cv_LL, lty = "CV_LL", color = "CV_LL"),
            lwd = lwd) +
  geom_segment(aes(x=hnw, xend = hnw,
                     y= nwmin, yend = 0.060),
               arrow = arrow(length = unit(0.1,"cm"),
                             type = "closed"),
               lty = "dotted", color= "gray", lwd=lwd)  +
  geom_text(aes(x = hnw+0.05, y=nwmin-0.009), 
            label = number(hnw,0.0001),
            color= "orange", size=4) +
  geom_segment(aes(x=hLL, xend = hLL,
                   y= LLmin, yend = 0.060),
               arrow = arrow(length = unit(0.1,"cm"),
                             type = "closed"),
               lty = "dashed", color= "gray", lwd=lwd) +
  geom_text(aes(x = hLL-0.05, y=LLmin-0.009), 
            label = number(hLL,0.0001),
            color= "blue", size=4)  +
  labs(x= "谱宽h", y ="交叉验证准则函数值CV(h)") +
  scale_x_continuous(#expand = expansion(add = c(0, .06)),
                     breaks = seq(0,1.8,0.2), 
                     limits = c(0,1.8)) +
  scale_y_continuous(expand = expansion(add = c(0,0)),
                     breaks = seq(0.066, 0.078,0.004), 
                     limits = c(0.060,0.078)) +
  theme_bw() +
  scale_color_manual(
    name="",
    breaks = c("CV_NW", "CV_LL"),
    values=c("orange", "blue"))+
  scale_linetype_manual(
    name="",
    breaks = c("CV_NW", "CV_LL"),
    values=c("dotted", "dashed"))+
  theme(legend.position = "right")


#### Regression Estimation Using Two Bandwidths ####

g <- 201
xg <- seq(0,10,10/(g-1))
m <- sin((xg-2)*a)/((xg-2)*a)
m1 <- matrix(0,g,1)
m2 <- matrix(0,g,1)

lwadd <- 0.1

for (j in 1:g){
  xj <- x-xg[j]
  z <- cbind(x1,xj)
  z1 <- z*dnorm(xj/hrot)
  z2 <- z*dnorm(xj/hLL)
  beta1 <- solve(t(z1)%*%z,t(z1)%*%y)
  beta2 <- solve(t(z2)%*%z,t(z2)%*%y)
  m1[j,1] <- beta1[1]
  m2[j,1] <- beta2[1]
}


#### plot 19.4-b ####

tbl_mxh <- tibble(xg = xg, 
                  mx1=as.vector(m1), 
                  mx2=as.vector(m2))

p_true <- p0 +
  stat_function(
    fun = function(x,a=pi/4) sin((x-2)*a)/((x-2)*a),
    geom = "line",
    xlim = c(0,10) ,
    aes(color = "red", lty="solid"),
    lwd = lwd
  ) +
  theme(legend.position = "")

p_mxh1 <- p0 +
  stat_function(
    fun = function(x,a=pi/4) {sin((x-2)*a)/((x-2)*a)},
    geom = "line",
    xlim = c(0,10) ,
    aes(color = "CEF", lty="CEF"),
    lwd = lwd ) +
  geom_line(aes(x = xg, y = m1, 
                color="m1", lty="m1"),
            lwd = lwd+lwadd,
            data = tbl_mxh) +
  theme_bw() +
  scale_color_manual(
    name="",
    breaks = c("CEF","m1"),
    labels = c(expression(m(x):true),expression(m(x):h[ROT])),
    values=c("red","green"))+
  scale_linetype_manual(
    name="",
    breaks = c("CEF","m1"),
    labels = c(expression(m(x):true),expression(m(x):h[ROT])),
    values=c("solid","dotted"))+
  theme(legend.position = "right")

p_mxh2 <- p0 +
  stat_function(
    fun = function(x,a=pi/4) {sin((x-2)*a)/((x-2)*a)},
    geom = "line",
    xlim = c(0,10) ,
    aes(color = "CEF", lty="CEF"),
    lwd = lwd ) +
  geom_line(aes(x = xg, y = m2, 
                color="m2", lty="m2"),
            lwd = lwd+lwadd,
            data = tbl_mxh) +
  theme_bw() +
  scale_color_manual(
    name="",
    breaks = c("CEF","m2"),
    labels = c(expression(m(x):true),expression(m(x):h[CV])),
    values=c("red","blue"))+
  scale_linetype_manual(
    name="",
    breaks = c("CEF","m2"),
    labels = c(expression(m(x):true),expression(m(x):h[CV])),
    values=c("solid","dashed"))+
  theme(legend.position = "right")

p_mxh <- p0 +
  stat_function(
    fun = function(x,a=pi/4) {sin((x-2)*a)/((x-2)*a)},
    geom = "line",
    xlim = c(0,10) ,
    aes(color = "CEF", lty="CEF"),
    lwd = lwd ) +
  geom_line(aes(x = xg, y = m1, 
                color="m1", lty="m1"),
            lwd = lwd+lwadd,
            data = tbl_mxh) +
  geom_line(aes(x = xg, y = m2, 
                color="m2", lty="m2"), 
            lwd = lwd+lwadd,
            data = tbl_mxh) +
  theme_bw() +
  scale_color_manual(
    name="",
    breaks = c("CEF","m1", "m2"),
    labels = c(expression(m(x):true),expression(m(x):h[ROT]),expression(m(x):h[CV])),
    values=c("red","green", "blue"))+
  scale_linetype_manual(
    name="",
    breaks = c("CEF","m1", "m2"),
    labels = c(expression(m(x):true),expression(m(x):h[ROT]),expression(m(x):h[CV])),
    values=c("solid","dotted", "dashed"))+
  theme(legend.position = "right")
