##  This file generates Figure 19.2

####Asymptotic Smoothing Bias####

x <- seq(0,10,.01)
xn <- length(x)
xi <- x-2
i0 <- (xi == 0)
xi <- xi + i0
i1 <- 1-i0
a <- pi/4
xp <- xi*a
sx = sin(xp)
cx = cos(xp)
m <- (sx/xp)*i1 + i0
d <- ((-(sx/xp) - 2*cx/(xp^2) + 2*sx/(xp^3))*i1 - i0/3)*(a^2)

h1 <- 1/2
h2 <- 1
h3 <- 3/2
m1 <- m + (d/2)*(h1^2)
m2 <- m + (d/2)*(h2^2)
m3 <- m + (d/2)*(h3^2)

#### draw plot ####
tbl_result <- tibble(x = x, m = m,
                     m1= m1, m2=m2, m3= m3)

lwd <- 1
p_bias <- ggplot(aes(x=x), data = tbl_result) +
  geom_line(aes(y = m, color="m", lty="m"), 
            lwd=0.8) +
  geom_line(aes(y = m1, color="m1", lty="m1") ,
            lwd=lwd) +
  geom_line(aes(y = m2, color="m2", lty="m2"), 
            lwd=lwd)+
  geom_line(aes(y = m3, color="m3", lty="m3"), 
            lwd=lwd) +
  labs(x= "自变量X", y ="因变量Y") +
  scale_x_continuous(breaks = seq(0,10,1), limits = c(0,10)) +
  scale_y_continuous(breaks = seq(-1,2,0.5), limits = c(-1,2)) +
  scale_color_manual(
    name="",
    #breaks = c("m", "m1","m2","m3"),
    labels = c("m(x)", "h=1/2","h2=1","h=3/2"),
    values=c("red", "blue","green","orange"))+
  scale_linetype_manual(
    name="",
    #breaks = c("m", "m1","m2","m3"),
    labels = c("m(x)", "h=1/2","h2=1","h=3/2"),
    values=c("solid", "dotted","dashed","dotdash")) +
  theme_bw()+
  theme(legend.position = "right")

