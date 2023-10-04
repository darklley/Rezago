# This script is the continuation of the scripts presented on github https://github.com/CarlosRivera1212/rezago_aov

library(gstat)
library(writexl)
library(readxl)
library(reshape2)
library(ape)
library(clhs)
library(tidyverse)
# Import data
Tesis <- read_excel( "Datos.xlsx")
Tesis$trt = as.factor(Tesis$trt)
Tesis$blq = as.factor(Tesis$blq)
Tesis$.id = as.factor(Tesis$.id)
zm=read_rds("Z_matrix.rds")
xm=read_rds("X_matrix.rds")
alp=read_rds("alfas.rds")

# Anova of the simulations and extraction of p and f values

data = NULL
for (i in 1:length(unique(Tesis$.id))) {
  for (j in 7:1006) {
    datos = Tesis |> 
      dplyr::filter(.id == unique(Tesis$.id)[i])
    resaov = aov(data = datos,formula = unlist(datos[,j]) ~ blq + trt)
    fvt = summary(resaov)[[1]][4][2,1]
    fvb = summary(resaov)[[1]][4][1,1]
    pvt = summary(resaov)[[1]][5][2,1]
    pvb = summary(resaov)[[1]][5][1,1]
    datax = data.frame(id = unique(Tesis$.id)[i], F_vT = fvt , 
                       F_vB = fvb,P_vT = pvt , P_vT = pvb ,  sim = paste0("sim",j))
    data = rbind.data.frame(data,datax)
  }
}

# Generate matrix

generar_matrices <- function(datos, n) {
  num_filas <- nrow(datos)
  matrices <- vector("list")
  num_matrices <- ceiling(num_filas / n)
  for (i in 1:num_matrices) {
    inicio <- (i - 1) * n + 1
    fin <- min(i * n, num_filas)
    matriz <- datos[inicio:fin, c("x", "y")]
    matriz = as.matrix(dist(cbind(matriz$x,matriz$y)))
    matriz  = 1/matriz
    diag(matriz) <- 0
    matriz = matriz/sum(matriz)
    matrices[[i]] <- matriz
  }
  matrices
}
matrices_generadas <- generar_matrices(Tesis, 60)

data2 = NULL
for (i in 1:length(unique(Tesis$.id))) {
  for (j in 1:1000) {
    datos2 = Tesis |> 
      dplyr::filter(.id == unique(Tesis$.id)[i])
    dat = datos2[,-c(1:6,1007)]
    IM =  Moran.I(dat[[j]], 
                  matrices_generadas[[i]])$p.value
    datax2 = data.frame(id = unique(Tesis$.id)[i], 
                        IMorant =  IM, sim = paste0("sim",j))
    data2 = rbind.data.frame(data2,datax2)
  }
}

# Calculating Alpha

X0= xm %*% zm
Mo=  X0%*%solve(t(X0)%*%X0)%*%t(X0)
IDEN = matrix(nrow = 60,ncol = 60,data = rep(0,60))
diag(IDEN)<-1
apha <- (t(Tesis$sim781[1:60]) %*% Mo %*% matrices_generadas[[1]] %*% (IDEN - Mo) %*% Tesis$sim781[1:60]) /
  (t(Tesis$sim781[1:60]) %*% Mo %*% matrices_generadas[[1]] %*% matrices_generadas[[1]] %*% Mo %*% Tesis$sim781[1:60])
data3 <- NULL

for (i in 1:length(unique(Tesis$.id))) {
  for (j in 1:1000) {
    datos3 <- Tesis %>%
      dplyr::filter(.id == unique(Tesis$.id)[i])
    dat3 <- datos3[,-c(1:6,1007)]
    apha <- t(dat3[[j]]) %*% Mo %*% matrices_generadas[[i]] %*% (IDEN - Mo) %*% dat3[[j]] /
      t(dat3[[j]]) %*% Mo %*% matrices_generadas[[i]] %*% matrices_generadas[[i]] %*% Mo %*% dat3[[j]]
    datax3 <- data.frame(id = unique(Tesis$.id)[i], a = apha, sim = paste0("sim",j))
    data3 <- rbind.data.frame(data3, datax3)
  }
}
