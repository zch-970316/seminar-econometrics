# Hansen Bruce 2021
#  This file generates Figure 19.1a 19.1b

##### bin Mean estimator #####

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
        K = Xx<=h,
        wt = 1,             # we will not use this     
        KY= K*Y)  
  } else if(type=="rolling"){ 
    out <- dt %>%
      mutate(
        Xx=abs(X-x), 
        K = Xx<=h,
        wt = ifelse(Xx<=h, 1,0), 
        KY= K*Y)
    }
  else if(type=="NW"){ 
    out <- dt %>%
      mutate(
        Xx=abs(X-x), 
        K = dnorm(Xx/h),
        wt = K,                  
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
## get estimator

get_result <- function(dt, x, h, type ){
  out <- tibble(x=x) %>%
    mutate(dt = map(x, ~dt)) %>%
    # now calculate bins
    mutate(dt= map2(.x = dt, .y = x,
                    .f = calc_bins,  # f
                    h =h, type=type) # options of function
    ) %>%
    # summarize
    mutate(m = map(dt, .f = sum_bins))%>% # f
    unnest(m)
  return(out)
}

# now get estimator on CEF m(x)
x_binned <- seq(1,9,2)
x_grid <- seq(0,xm,.01)
tbl_binned <- get_result(dt = dt, x = x_binned,
                         h = h1, type = "binned")
tbl_rolling <- get_result(dt = dt, x = x_grid,
                          h=h1, type = "rolling")
tbl_NW <- get_result(dt = dt, x = x_grid,
                     h=h2, type = "NW")

##### Local Linear regression #####

## helper function
### LM fit

fit_lm <- function(df, mod, type){
  if (type=="rolling" | type == "LL"){
    fits <- lm(formula = mod, data = df,weights = wt)  # weighted LS
    smry <- summary(fits)
    m_hat <- smry$coefficients[1]
  } else if (type =="binned"){
    fits <- lm(formula = mod, data = df)  # OLS
    smry <- summary(fits)
    m_hat <- fits$fitted.values
  }
  
  return(m_hat)
  
}

mod_binned <- "Y~X"
mod_rolling <- "Y~Xx" # the same model but different kernel
mod_LL <- "Y~Xx"

#fit_lm(df = as_tibble(tbl_NW$dt[[1]]), mod = mod_LL, type = "LL")
#fit_lm(df = as_tibble(tbl_rolling$dt[[1]]), mod = mod_binned, type = "binned")

get_mx <- function(df, mod, type){
  if (type=="binned" ){
    df_clean <- df %>% 
      mutate(dt = map(dt, ~filter(.x,K=="TRUE")))
  } else if(type=="LL"| type == "rolling") {
    df_clean <- df
  }
  
  out <- df_clean %>%
    mutate(
      fit = map(.x = dt,
                .f = fit_lm, # function lm
                mod=mod,type=type)
    )  %>%
    unnest(fit) %>%
    mutate( dt= map2(.x = dt, .y = fit,
                     ~.x %>% mutate(fit=.y))  # match X with predicts
    )
}


mx_binned <- get_mx(df = tbl_binned, 
                    mod = mod_binned, 
                    type = "binned")

mx_rolling <- get_mx(df = tbl_rolling, 
                     mod = mod_rolling, 
                     type = "rolling")

mx_LL<- get_mx(df = tbl_NW, 
               mod = mod_LL, 
               type = "LL")

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
  
### basic plot
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

### binned plot
p1 <- p00 +
  geom_point(aes(x, mx),
             data = dt_tar1, 
             pch=15, color="black") +
  geom_line(aes(X, mx, color=as.factor(x)),
             data = dt_tar1, 
             lty="dashed", lwd=0.8) +
  theme(legend.position = "none")
  
## scrolling  plot
x1 <- seq(0,10,0.01) # bins break
dt_tar2 <- tbl_rolling %>% 
  mutate(dt = map(dt, ~filter(.x,K==TRUE))) %>%
  unnest(dt) %>%
  select(x, X,Y,mx) %>%
  mutate(cut = cut_interval(x,5))


p2 <- p0 +
  geom_point(aes(X, Y, color=as.factor(cut)),
             data = dt_tar2, pch=21) +
  geom_line(aes(x, mx, color=as.factor(cut)),
            data = dt_tar2 %>% select(x, mx, cut) %>% unique, 
            lty="solid", lwd=0.8) +
  theme(legend.position = "none")

## Nadaraya-Watson  plot

dt_tar3 <- tbl_NW %>% 
  #mutate(dt = map(dt, ~filter(.x,K==TRUE))) %>%
  unnest(dt) %>%
  select(x, X,Y,mx) %>%
  mutate(cut = cut_interval(x,5))


p3 <- p0 +
  geom_point(aes(X, Y, color=as.factor(cut)),
             data = dt_tar3, pch=21) +
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
  theme(legend.position = "top")



##### draw plot ####

## binned  estimator
x0 <- seq(2,10,2) # bins break
X_new <- mx_binned %>%
  select(x,dt) %>%
  mutate(X = map(dt,~select(.x,X)),
         mx= map(dt, ~mean(.x$Y))) %>%
  select(-dt) %>%
  unique() %>%
  unnest(X) 
  
dt_tar1 <- mx_binned %>% 
  #mutate(dt = map(dt, ~filter(.x,K==TRUE))) %>%
  select(x, fit) %>%
  mutate(X=X_new$X,
         mx = unlist(X_new$mx),
         lwr=x-h1, upr=x+h1) 


### binned plot
p11 <- p00 +
  geom_point(aes(x, mx),
             data = dt_tar1, 
             pch=15, color="black") +
  geom_line(aes(X, fit, color=as.factor(x)),
            data = dt_tar1, 
            lty="solid", lwd=0.8) +
  theme(legend.position = "none")

## scrolling  plot
x1 <- seq(0,10,0.01) # bins break
dt_tar2 <- mx_rolling %>% 
  select(x, fit) 


p22 <- p0 +
  geom_line(aes(x, fit),
            data = dt_tar2 , 
            lty="solid", lwd=0.8, color="blue") +
  theme(legend.position = "none")

## Nadaraya-Watson  plot

dt_tar3 <- mx_LL %>% 
  select(x,fit) 


p33 <- p0 +
  geom_line(aes(x, fit),
            data = dt_tar3, 
            lty="solid", lwd=0.8, color = "green") +
  theme(legend.position = "none")

## all three plot

p_all_LL <- p0 +
  geom_vline(xintercept = x0, lty="dashed")  +
  geom_point(aes(x, mx, color="point"),
             data = dt_tar1%>% select(x, mx) %>% unique(), 
             pch=15) +
  geom_line(aes(X, fit, color="binned", group=x),
            data = dt_tar1, 
            lty="solid", lwd=0.8) +
  geom_line(aes(x, fit, color = "rolling"),
            data = dt_tar2 , 
            lty="solid", lwd=0.8)  +
  geom_line(aes(x, fit, color = "NW"),
            data = dt_tar3 , 
            lty="solid", lwd=0.8) +
  scale_color_manual(
    name="mx",
    breaks = c("point","binned", "rolling","NW"),
    values=c("green","black", "red","blue"))+
  theme(legend.position = "top")