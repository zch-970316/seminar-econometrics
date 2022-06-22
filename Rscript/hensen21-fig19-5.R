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
## window range
sx1 <- 0
sx2 <- 40

#### Reference Rule ####
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
hLL_tex <-paste0("$h_\\{CV\\}=",number(hLL,0.0001),"$")
LLmin <- min(cvLL)

#### plot 19.5-a ####

tb_cvc <- tibble(
  h_tune = hh, 
  #cv_NW = cvnw,
  cv_LL = cvLL)

lwd <- 0.8

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
  geom_text(aes(x = hLL-0.3, y=LLmin-0.0004), 
            label =latex2exp::TeX(hLL_d4),
            color= "blue", size=4)  +
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

pdf("HANSEN19-5a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(hh,cvLL,type="l",lty=1,ylab="Cross-Validation Criterion",xlab="Bandwidth",xaxs="i",yaxs="i",ylim=c(.2292,.231),xlim=c(2,11),xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(2,12,1),lwd=wd)
axis(side=2,seq(.229,.231,.0005),lwd=wd)
points(hLL,LLmin,pch=19,cex=.75)
text(hLL,.2294,expression(h[CV]))
dev.off()

postscript("HANSEN19-5a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(hh,cvLL,type="l",lty=1,ylab="Cross-Validation Criterion",xlab="Bandwidth",xaxs="i",yaxs="i",ylim=c(.2292,.231),xlim=c(2,11),xaxt="n",bty="n",lwd=wd)
axis(side=1,seq(2,12,1),lwd=wd)
axis(side=2,seq(.229,.231,.0005),lwd=wd)
points(hLL,LLmin,pch=19,cex=.75)
text(hLL,.2294,expression(h[CV]))
dev.off()


# Regression Estimation
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

pdf("HANSEN19-5b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(xg,m1,type="l",lty=1,ylab="Log Wage",xlab="Experience (Years)",xaxs="i",yaxs="i",xlim=c(sx1,sx2),ylim=c(2,2.7),xaxt="n",bty="n",lwd=wd)
lines(xg,m2,lty=5,lwd=wd)
legend("topleft",legend=c(expression(h[ROT]),expression(h[CV])),lty=c(1,5),bg="white",bty="n",lwd=wd)
axis(side=1,seq(0,40,5),lwd=wd)
axis(side=2,lwd=wd)
dev.off()

postscript("HANSEN19-5b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(xg,m1,type="l",lty=1,ylab="Log Wage",xlab="Experience (Years)",xaxs="i",yaxs="i",xlim=c(sx1,sx2),ylim=c(2,2.7),xaxt="n",bty="n",lwd=wd)
lines(xg,m2,lty=5,lwd=wd)
legend("topleft",legend=c(expression(h[ROT]),expression(h[CV])),lty=c(1,5),bg="white",bty="n",lwd=wd)
axis(side=1,seq(0,40,5),lwd=wd)
axis(side=2,lwd=wd)
dev.off()

h <- hrot

# Residual Estimation
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
# Variance ROT Bandwidth
beta <- solve((t(zz)%*%zz),(t(zz)%*%e2))
b <- mean(((beta[3]+x*3*beta[4]+(x^2)*6*beta[5])^2)*xtrim)
e <- e2 - zz%*%beta
sig <- (sum(e^2))/(n-5)
hrotv <- 0.58*(((sx2-sx1)*sig/n/b)^.2)
cat("Reference Variance Bandwidth")
print(hrotv)

# Variance CV Bandwidth Selection
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
NWminV <- min(cvNWV)
LLminV <- min(cvLLV)
cat("Variance CV Bandwidth for NW")
print(hNWV)
cat("Variance CV Bandwidth for LL")
print(hLLV)
cat("NW min CV")
print(NWminV)
cat("LL min CV")
print(LLminV)

pdf("HANSEN19-6a.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(hh,cvNWV,type="l",lty=1,ylab="Cross-Validation Criterion",xlab="Bandwidth",xaxs="i",yaxs="i",bty="n",lwd=wd)
axis(side=1,seq(0,40,5),lwd=wd)
axis(side=2,seq(1.2,1.216,.004),lwd=wd)
lines(hh,cvLLV,lwd=wd)
text(20,1.206,"Nadaraya-Watson")
text(30,1.21,"Local Linear")
dev.off()

postscript("HANSEN19-6a.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(hh,cvNWV,type="l",lty=1,ylab="Cross-Validation Criterion",xlab="Bandwidth",xaxs="i",yaxs="i",bty="n",lwd=wd)
axis(side=1,seq(0,40,5),lwd=wd)
axis(side=2,seq(1.2,1.216,.004),lwd=wd)
lines(hh,cvLLV,lwd=wd)
text(20,1.206,"Nadaraya-Watson")
text(30,1.21,"Local Linear")
dev.off()

# Variance Estimation
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

# Plot Confidence Bands
L1 <- m1-1.96*se
U1 <- m1+1.96*se

pdf("HANSEN19-6b.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot(xg,m1,type="l",lty=1,ylab="Log Wage",xlab="Experience (Years)",xaxs="i",yaxs="i",xlim=c(sx1,sx2),ylim=c(2,2.7),xaxt="n",bty="n",lwd=wd)
polygon(c(xg,rev(xg)),c(L1,rev(U1)),border=NA,col=grey(.8),lwd=wd)
lines(xg,m1,lty=1,lwd=wd)
axis(side=1,seq(0,40,5),lwd=wd)
axis(side=2,lwd=wd)
dev.off()

postscript("HANSEN19-6b.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot(xg,m1,type="l",lty=1,ylab="Log Wage",xlab="Experience (Years)",xaxs="i",yaxs="i",xlim=c(sx1,sx2),ylim=c(2,2.7),xaxt="n",bty="n",lwd=wd)
polygon(c(xg,rev(xg)),c(L1,rev(U1)),border=NA,col=grey(.8),lwd=wd)
lines(xg,m1,lty=1,lwd=wd)
axis(side=1,seq(0,40,5),lwd=wd)
axis(side=2,lwd=wd)
dev.off()

