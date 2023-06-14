#import
install.packages('rio', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(rio)
library(MASS)
if(!exists("foo", mode="function")) source("Module.R")


MASS = importr('MASS')#data
data <- read.csv("C:/Users/hamza.addi/Downloads/Untitled_Message/result.csv")
cout_moyen <- data["cout.moyen"]

#données

cout_moyen_non_nul <- matrix(t(cout_moyen)[t(cout_moyen) != 0], byrow = TRUE)
N <- length(cout_moyen_non_nul)


#create data that follows different distributions
data_creation(cout_moyen_non_nul)


#Test de Kolmogorov-Smirnov

test_ks(cout_moyen_non_nul)

#Test de Cramer_Von-Mises

test_cvm(cout_moyen_non_nul)

#Test de Anderson Darling

test_ad(cout_moyen_non_nul)

#Q-Q plot

Q_Q_plot(exponential_data,lognormal_data,gamma_data)

