# Hansen Bruce 2021
#  This file generates Figure 19.1a
#  Nadaraya-Watson Regression


n <- 100
xm <- 10

# generate X and Y data
set.seed(180)
xdat <- runif(n,0,xm)
a <- pi/4
m <- sin((xdat-2)*a)/((xdat-2)*a)
ydat <- m + rnorm(n)/4

dt <- tibble::tibble(X= xdat, Y=ydat)

# set h
h1 <- 1          # for binned
h2 <- h1/sqrt(3) # for NW

# helper function
## calculate bins
calc_bins <- function(dt, x,h, type){
  if (type=="binned") { 
    out <- dt %>%
      mutate(
        Xx=abs(X-x), 
        K = Xx<=h,              # for binned or rolling
        KY= K*Y)
  } else if(type=="NW"){ 
    out <- dt %>%
      mutate(
        Xx = abs(X-x),
        K=dnorm((abs(X-x))/h), # for Nadaraya-Watson
        KY= K*Y)
  }
  return(out)
  
}

# helper function
## summarize bins
sum_bins <- function(dt){
  out <- dt %>%
    summarize(sum_KY = sum(KY),
              sum_K = sum(K)) %>%
    mutate(mx = sum_KY/sum_K)
}

# helper function
## summarize bins

get_result <- function(dt, x, h, type ){
  out <- tibble(x=x) %>%
    mutate(dt = map(x, ~dt)) %>%
    # now calculate bins
    mutate(dt= map2(.x = dt, .y = x,
                    .f = calc_bins, 
                    h =h, type=type) # options of function
    ) %>%
    # summarize
    mutate(m = map(dt, .f = sum_bins))%>%
    unnest(m)
  return(out)
}

# now get estimator on CEF m(x)
x_binned <- seq(1,9,2)
x_grid <- seq(0,xm,.01)
tbl_binned <- get_result(dt = dt, x = x_binned,
                         h = h1, type = "binned")
tbl_rolling <- get_result(dt = dt, x = x_grid,
                          h=h1, type = "binned")
tbl_NW <- get_result(dt = dt, x = x_grid,
                     h=h2, type = "NW")


##### draw plot ####

p0 <- ggplot() +
  geom_point(aes(X, Y),data = dt, pch=21) +
  labs(x= "自变量X", y ="因变量Y") +
  scale_x_continuous(breaks = seq(0,10,1), limits = c(0,10)) +
  scale_y_continuous(breaks = seq(-1,2,0.5), limits = c(-1,2)) +
  theme_bw()

## binned  estimator
x0 <- seq(2,10,2) # bins break
dt_tar1 <- tbl_binned %>% 
  mutate(dt = map(dt, ~filter(.x,K==TRUE))) %>%
  unnest(dt) %>%
  select(x, X,Y,mx) %>%
  mutate(lwr = x-h1, upr=x+h1) 
  

p00 <- p0 +
  geom_vline(xintercept = x0, lty="dashed") +
  geom_point(aes(X, Y, color=as.factor(x)),
             data = dt_tar1, pch=21) +
  geom_rect(aes(ymin=-1,ymax=2,
                xmin= lwr, xmax = upr, 
                fill = as.factor(x)),
            data = dt_tar1 %>% select(x, lwr, upr) %>% unique,
            alpha = 0.05, inherit.aes = FALSE) +
  theme(legend.position = "none")

p1 <- p00 +
  geom_point(aes(x, mx),
             data = dt_tar1, 
             pch=15, color="black") +
  geom_line(aes(X, mx, color=as.factor(x)),
             data = dt_tar1, 
             lty="dashed", lwd=0.8) +
  theme(legend.position = "none")
  
## scrolling  estimator
x1 <- seq(0,10,0.01) # bins break
dt_tar2 <- tbl_rolling %>% 
  mutate(dt = map(dt, ~filter(.x,K==TRUE))) %>%
  unnest(dt) %>%
  select(x, X,Y,mx) %>%
  mutate(cut = cut_interval(x,5))


p2 <- p00 +
  geom_line(aes(x, mx, color=as.factor(cut)),
            data = dt_tar2 %>% select(x, mx, cut) %>% unique, 
            lty="dashed", lwd=0.8) +
  theme(legend.position = "none")

## Nadaraya-Watson  estimator

dt_tar3 <- tbl_NW %>% 
  #mutate(dt = map(dt, ~filter(.x,K==TRUE))) %>%
  unnest(dt) %>%
  select(x, X,Y,mx) %>%
  mutate(cut = cut_interval(x,5))


p3 <- p00 +
  geom_line(aes(x, mx, color=as.factor(cut)),
            data = dt_tar3 %>% select(x, mx, cut) %>% unique, 
            lty="solid", lwd=0.8) +
  theme(legend.position = "none")

## all three plot

p_all <- p0 +
  geom_vline(xintercept = x0, lty="dashed")  +
  geom_point(aes(x, mx, color="point"),
             data = dt_tar1%>% select(x, mx) %>% unique(), 
             pch=15) +
  geom_segment(aes(x=lwr, xend=upr, y=mx, yend= mx, 
                   color = "binned"),
            data = dt_tar1%>% select(lwr,upr,mx) %>% unique(), 
            lty="solid", lwd=0.8) +
  geom_line(aes(x, mx, color = "rolling"),
            data = dt_tar2 %>% select(x, mx, cut) %>% unique(), 
            lty="solid", lwd=0.8)  +
  geom_line(aes(x, mx, color = "NW"),
            data = dt_tar3 %>% select(x, mx, cut) %>% unique(), 
            lty="solid", lwd=0.8) +
  scale_color_manual(
    name="mx",
    breaks = c("point","binned", "rolling","NW"),
    values=c("green","black", "red","blue"))+
  theme(legend.position = "right")
