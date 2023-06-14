install.packages("rio")
library("rio")


data_creation <- function(cout_moyen_non_nul) {

  #create data that follows different distributions
  #Exponential Distribution :
  fit_exponential = fitdistr(cout_moyen_non_nul, "exponential")
  exponential_data <- rexp(n =N,rate= fit_exponential["rate"])
  
  
  #Log normal Distribution :
  fit_lognormal = fitdistr(cout_moyen_non_nul, "lognormal")
  lognormal_data <- rlnorm(n=N, meanlog = fit_lognormal["meanlog"], sdlog =fit_lognormal["sdlog"])
  
  #Gamma Distribution :
  fit_gamma = fitdistr(cout_moyen_non_nul, "gamma")
  gamma_data <- rgamma(n = N)

  return(fit_exponential,exponential_data,fit_lognormal,lognormal_data)
}

##########  Tests de normalité #############

#Test de Kolmogorov-Smirnov
test_ks <- function(cout_moyen_non_nul) {
  
  ###Exponential Distribution
  
  ks.test(cout_moyen_non_nul, "pexp", data_creation(cout_moyen_non_nul )[0]["rate"], exact=FALSE)
  
  ###Log normal Distribution :
  
  ks.test(cout_moyen_non_nul, "plnorm",  data_creation(cout_moyen_non_nul )[2]["meanlog"],data_creation(cout_moyen_non_nul )[2]["sdlog"], exact=FALSE)
  
  ###Gamma Distribution :
  
  ks.test(cout_moyen_non_nul, "pgamma",  data_creation(cout_moyen_non_nul )[4]["meanlog"],data_creation(cout_moyen_non_nul )[4]["sdlog"], exact=FALSE)
  
}


#Test de Cramer_Von-Mises$
test_cvm <- function(cout_moyen_non_nul) {
  
  ###Exponential Distribution
  
  cvm.test(cout_moyen_non_nul, "pexp", data_creation(cout_moyen_non_nul )[0]["rate"], exact=FALSE)
  
  ###Log normal Distribution :
  
  cvm.test(cout_moyen_non_nul, "plnorm",  data_creation(cout_moyen_non_nul )[2]["meanlog"],data_creation(cout_moyen_non_nul )[2]["sdlog"], exact=FALSE)
  
  ###Gamma Distribution :
  
  cvm.test(cout_moyen_non_nul, "pgamma",  data_creation(cout_moyen_non_nul )[4]["meanlog"],data_creation(cout_moyen_non_nul )[4]["sdlog"], exact=FALSE)
  
}

#Test de Anderson Darling
test_ad <- function(cout_moyen_non_nul) {
  
  ###Exponential Distribution
  
  ad.test(cout_moyen_non_nul, "pexp", data_creation(cout_moyen_non_nul )[0]["rate"], exact=FALSE)
  
  ###Log normal Distribution :
  
  ad.test(cout_moyen_non_nul, "plnorm",  data_creation(cout_moyen_non_nul )[2]["meanlog"],data_creation(cout_moyen_non_nul )[2]["sdlog"], exact=FALSE)
  
  ###Gamma Distribution :
  
  ad.test(cout_moyen_non_nul, "pgamma",  data_creation(cout_moyen_non_nul )[4]["meanlog"],data_creation(cout_moyen_non_nul )[4]["sdlog"], exact=FALSE)
  
}



Q_Q_plot <- function(exponential_data,lognormal_data,gamma_data){
  
  ###Exponential Distribution
  
  qqnorm(exponential_data, main='Q-Q plot du cout moyen')
  qqline(exponential_data)
  
  ###Log normal Distribution
  
  qqnorm(lognormal_data, main='Q-Q plot du cout moyen')
  qqline(lognormal_data)
  
  ##Gamma Distribution
  
  qqnorm(gamma_data, main='Q-Q plot du cout moyen')
  qqline(gamma_data)
  
}
