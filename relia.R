library(tidyverse)
library(irr)
library(psych)
library(effsize)
library(ggsci)
library(knitr)
library(pander)
classify <- function(x){
  ret <- vector(mode="character",length = NROW(x))
  for(i in 1:NROW(x)){
    if(any(unlist(x[i,])))ret[i] <- colnames(x)[unlist(x[i,])]
    else ret[i] <- "other"
  }
  ret
}

rez_U <- read_csv("Rezeption_U.csv")
rez_P <- read_csv("Rezeption_P.csv")
rez_all <- rbind(cbind(rez_P,rater="P",object="rez"),cbind(rez_U,rater="U",object="rez"))

rez_taet <- names(rez_all)[9:14]
DIAMONDS <- names(rez_P)[1:8]
rez_all[rez_taet] <- rez_all[rez_taet] %>% mutate_all(function(x)ifelse(is.na(x),0,x) %>% as.logical())
rez_all <- rez_all %>% mutate(class=classify(rez_all[rez_taet]))
rez_taet <- c(rez_taet,"class")
pairwise_rez_taet <- rez_taet %>% map(function(x)cbind(filter(rez_all,rater=="U")[x],filter(rez_all,rater=="P")[x]))
names(pairwise_rez_taet) <- rez_taet
rez_teat_kappa <- pairwise_rez_taet$class %>% kappam.light() %>% .$value

pairwise_diamond_rez <- DIAMONDS %>% map(function(x)cbind(filter(rez_all,rater=="U")[x],filter(rez_all,rater=="P")[x]))
diamond_kendall_rez <- pairwise_diamond_rez %>% map(function(x)x %>% map(ordered) %>% bind_cols() %>% kendall(correct = T))
relia_rez_diamonds <- pairwise_diamond_rez %>% map(function(x)x %>% map_dbl(var)) %>% do.call(rbind,.) %>% cbind(.,mean=rowMeans(.),kendall=diamond_kendall_rez %>% map_dbl(function(x)x$value)) %>% round(2)
colnames(relia_rez_diamonds) <- c("Varianz Beobachter I","Varianz Beobachter II","Varianz Mittel","Kendall's W")
row.names(relia_rez_diamonds) <- DIAMONDS
knitr::kable(relia_rez_diamonds)

bar_U <- read_csv("Bar_U.csv")
bar_A <- read_csv("Bar_A.csv")
bar_all <- rbind(cbind(bar_A,rater="A",object="bar"),cbind(bar_U,rater="U",object="bar"))
pairwise_diamond_bar <- DIAMONDS %>% map(function(x)cbind(filter(bar_all,rater=="U")[x],filter(bar_all,rater=="A")[x]))
diamond_kendall_bar <- pairwise_diamond_bar %>% map(function(x)x %>% map(ordered) %>% bind_cols() %>% kendall(correct = T))
relia_bar_diamonds <- pairwise_diamond_bar %>% map(function(x)x %>% map_dbl(var)) %>% do.call(rbind,.) %>% cbind(.,mean=rowMeans(.),kendall=diamond_kendall_bar %>% map_dbl(function(x)x$value)) %>% round(2)
colnames(relia_bar_diamonds) <- c("Varianz Beobachter I","Varianz Beobachter II","Varianz Mittel","Kendall's W")
row.names(relia_bar_diamonds) <- DIAMONDS
knitr::kable(relia_bar_diamonds)

bar_taet <- names(bar_all)[9:14]
bar_all[bar_taet] <- bar_all[bar_taet] %>% mutate_all(function(x)ifelse(is.na(x),0,x) %>% as.logical())
bar_all <- bar_all %>% mutate(class=classify(bar_all[bar_taet]))
bar_taet <- c(bar_taet,"class")
pairwise_bar_taet <- bar_taet %>% map(function(x)cbind(filter(bar_all,rater=="U")[x],filter(bar_all,rater=="A")[x]))
names(pairwise_bar_taet) <- bar_taet
bar_teat_kappa <- pairwise_bar_taet$class %>% kappam.light() %>% .$value

pasta_C <- read_csv("Pasta_C.csv")
pasta_A <- read_csv("Pasta_A.csv")
pasta_all <- rbind(cbind(pasta_A,rater="A",object="pasta"),cbind(pasta_C,rater="C",object="pasta"))
pairwise_diamond_pasta <- DIAMONDS %>% map(function(x)cbind(filter(pasta_all,rater=="A")[x],filter(pasta_all,rater=="C")[x]))
diamond_kendall_pasta <- pairwise_diamond_pasta %>% map(function(x)x %>% map(ordered) %>% bind_cols() %>% kendall(correct = T))
relia_pasta_diamonds <- pairwise_diamond_pasta %>% map(function(x)x %>% map_dbl(var)) %>% do.call(rbind,.) %>% cbind(.,mean=rowMeans(.),kendall=diamond_kendall_pasta %>% map_dbl(function(x)x$value)) %>% round(2)
colnames(relia_pasta_diamonds) <- c("Varianz Beobachter I","Varianz Beobachter II","Varianz Mittel","Kendall's W")
row.names(relia_pasta_diamonds) <- DIAMONDS
knitr::kable(relia_pasta_diamonds)

pasta_taet <- names(pasta_all)[9:14]
pasta_all[pasta_taet] <- pasta_all[pasta_taet] %>% mutate_all(function(x)ifelse(is.na(x),0,x) %>% as.logical())
pasta_all <- pasta_all %>% mutate(class=classify(pasta_all[pasta_taet]))
pasta_taet <- c(pasta_taet,"class")
pairwise_pasta_taet <- pasta_taet %>% map(function(x)cbind(filter(pasta_all,rater=="C")[x],filter(pasta_all,rater=="A")[x]))
names(pairwise_pasta_taet) <- pasta_taet
pasta_teat_kappa <- pairwise_pasta_taet$class %>% kappam.light() %>% .$value

t.test(Duty~customer,rez_all)
cohen.d.formula(Duty~customer,rez_all)$estimate
t.test(Duty~customer,pasta_all)
cohen.d.formula(Duty~customer,pasta_all)

ggplot(pasta_all, aes(Duty, customer)) + geom_boxplot()

all <- rbind(pasta_all[c(DIAMONDS,"class","object")],bar_all[c(DIAMONDS,"class","object")],rez_all[c(DIAMONDS,"class","object")])
summary(aov(Duty~object,all))
