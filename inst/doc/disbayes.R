## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(fig.width=7)

## ----show=FALSE,message=FALSE-------------------------------------------------
library(disbayes)
library(dplyr,quietly = TRUE)
ihdbristol <- ihdengland %>% filter(area=="Bristol", gender=="Male")
ihdbristol %>% filter(between(age, 50, 55))

## ----eval=TRUE----------------------------------------------------------------
ihdbristol[ihdbristol$age %in% 50:55, ]

## -----------------------------------------------------------------------------
dat <- data.frame(agefrom=seq(0,20,5), ageto=seq(4,24,5),
                  measure=c(15,20,24,35,29))
dat

## -----------------------------------------------------------------------------
if (requireNamespace("tempdisagg")) { 
   disagg_crude <- rep(dat$measure/5, each=5)
   disagg_smooth <- predict(tempdisagg::td(dat$measure ~ 1, to=5, method="fast"))
   ageyr <- 0:24
   plot(ageyr, disagg_crude, type="l", xlab="Age", ylab="Measure", ylim=c(0,8))
   lines(ageyr, disagg_smooth, col="blue")
   agegroup <- cut(ageyr, seq(0,25,5), right = FALSE)
   tapply(disagg_smooth, agegroup, sum)
}

## ---- eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE---------------------
dbres <- disbayes(data = ihdbristol, age = "age",
                 inc_num = "inc_num", inc_denom = "inc_denom", 
                 prev_num = "prev_num", prev_denom = "prev_denom",
                 mort_num = "mort_num", mort_denom = "mort_denom",
                 eqage = 40)

## ---- eval=TRUE, cache=TRUE, warning=FALSE, message=FALSE---------------------
dbresu <- disbayes(data = ihdbristol, age = "age",
                 inc_num = "inc_num", inc_denom = "inc_denom", 
                 prev_num = "prev_num", prev_denom = "prev_denom",
                 mort_num = "mort_num", mort_denom = "mort_denom",
                 cf_model = "indep", 
                 eqage = 40)

## ----eval=FALSE---------------------------------------------------------------
#  options(mc.cores = parallel::detectCores())
#  dbresm <- disbayes(data = ihdbristol, age = "age",
#                   inc_num = "inc_num", inc_denom = "inc_denom",
#                   prev_num = "prev_num", prev_denom = "prev_denom",
#                   mort_num = "mort_num", mort_denom = "mort_denom",
#                   method="mcmc", chains=2, iter=1000,
#                   eqage = 40)

## ----eval=FALSE---------------------------------------------------------------
#  rstan::traceplot(dbres$fit, pars=paste0("cf[", 60:65, "]"))

## ---- eval=TRUE---------------------------------------------------------------
summ <- tidy(dbres) 

## ----eval=TRUE----------------------------------------------------------------
library(dplyr,quietly=TRUE)
summ %>% 
  filter(var=="cf", between(age,60,65)) %>%
  select(age, `25%`, `50%`, `75%`)

## ---- eval=TRUE, warning=FALSE------------------------------------------------
library(ggplot2)
plot(dbres) +  ylab("Case fatality") + xlab("Age") 

## ----warning=FALSE------------------------------------------------------------
summs <- summ %>% filter(var=="cf")
summu <- tidy(dbresu) %>% filter(var=="cf")
ggplot(summu, aes(x=age)) + 
  geom_pointrange(aes(y=`50%`, ymin=`2.5%`, ymax=`97.5%`),
                  data=summu, col="blue", alpha=0.5) +
  geom_pointrange(aes(y=`50%`, ymin=`2.5%`, ymax=`97.5%`),
                  data=summ, col="black", alpha=0.5)

