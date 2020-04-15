# Zajêcia 6 ####

# Zadanie 1 ---------------------------------------------------------------
load(url("http://ls.home.amu.edu.pl/data_sets/Centrala.RData"))

# sugerujê rozk³ad Poissona, zatem funkcja to epois

library(EnvStats)

b_conf_int <- function(x, conf_level = 0.95) {
  u <- epois(
    x, 
    ci = TRUE, 
    ci.method = "pearson",
    conf.level = conf_level
  )$interval$limits
  
  return(c(u))
}

d_conf_int <- function(x, conf_level = 0.95) {
  u <- epois(
    x, 
    ci = TRUE, 
    ci.method = "pearson.hartley.approx",
    conf.level = conf_level
  )$interval$limits
  
  return(c(u))
}


c_conf_int <- function(x, conf_level = 0.95) {
  u <- epois(
    x, 
    ci = TRUE, 
    ci.method = "normal.approx",
    conf.level = conf_level
  )$interval$limits
  
  return(c(u))
}

b_conf_int(Centrala$Liczba)
d_conf_int(Centrala$Liczba)
c_conf_int(Centrala$Liczba)

# Zadanie 2 ---------------------------------------------------------------
Awaria = as.numeric(unlist(read.table("awarie.txt")))

# sugerujê rozk³ad wyk³adniczy

# zgodnie z tabel¹ https://pl.wikipedia.org/wiki/Rozk%C5%82ad_wyk%C5%82adniczy
# wariancja to parametr^-2, wartoœæ oczekiwana to par^-1

wariancja <- function(x, conf_level = 0.95) {
  
  u <- eexp(
    x,
    ci = TRUE, 
    ci.method = "exact",
    conf.level = conf_level
  )
  
  temp = u$interval$limits ** -2
  
  return(temp)
}

expected_value <- function(x, conf_level = 0.95) {
  
  u <- eexp(
    x,
    ci = TRUE, 
    ci.method = "exact",
    conf.level = conf_level
  )
  
  temp = u$interval$limits ** -1
  
  return(temp)
}

wariancja(Awaria)
expected_value(Awaria)

# Zadanie 3 ---------------------------------------------------------------
# 1
median_cint <- function(x, conf_level = 0.95){
  
  n = length(x)
  z = qnorm(1 - conf_level / 2)
  LCL = sqrt(ln(2) * mean(x ** 2) * (1 - z / sqrt(n)))
  UCL = sqrt(ln(2) * mean(x ** 2) * (1 + z / sqrt(n)))
  ENW = mean(LCL, UCL)
  
  return(
    confint(
      title: "mediana", 
      est: ENW, 
      l: LCL, 
      r: UCL, 
      conf_level: conf_level
    )
  )
}

# 2
wiatr = c(0.9, 6.2, 2.1, 4.1, 7.3, 1.0, 4.6, 6.4, 3.8, 5.0, 2.7, 9.2, 5.9, 7.4, 3.0, 4.9, 8.2, 5.0, 1.2, 10.1, 12.2, 2.8, 5.9, 8.2, 0.5)

median_cint(wiatr)
  

# Zadanie 4 ---------------------------------------------------------------
granica_przedzialu_ufnoœci <- function(x, conf_level = 0.95) {
  n = length(x)
  LCL = max(x) / nthroot(1 - conf_level/2, n)
  UCL = max(x) / nthroot(conf_level/2, n)
  
  return(c(LCL, UCL))
}

