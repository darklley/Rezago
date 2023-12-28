# This script is the continuation of the scripts presented on github https://github.com/CarlosRivera1212/rezago_aov

library(gstat)
library(writexl)
library(readxl)
library(reshape2)
library(ape)
library(clhs)
library(tidyverse)
# Import data
Data1 <- read_excel("Data.xlsx")
Data1$trt = as.factor(Data1$trt)
Data1$blq = as.factor(Data1$blq)
Data1$.id = as.factor(Data1$.id)
zm=read_rds("Z_matrix.rds")
xm=read_rds("X_matrix.rds")
alp=read_rds("alfas.rds")

# Anova of the simulations and extraction of p and f values

data = NULL
for (i in 1:length(unique(Data1$.id))) {
  for (j in 7:1006) {
    datos = Data1 |> 
      dplyr::filter(.id == unique(Data1$.id)[i])
    resaov = aov(data = datos,formula = unlist(datos[,j]) ~ blq + trt)
    fvt = summary(resaov)[[1]][4][2,1]
    fvb = summary(resaov)[[1]][4][1,1]
    pvt = summary(resaov)[[1]][5][2,1]
    pvb = summary(resaov)[[1]][5][1,1]
    datax = data.frame(id = unique(Data1$.id)[i], F_vT = fvt , 
                       F_vB = fvb,P_vT = pvt , P_vT = pvb ,  sim = paste0("sim",j))
    data = rbind.data.frame(data,datax)
  }
}

# Generate matrix

generate_matrix <- function(datos, n) {
  num_filas <- nrow(datos)
matrixs <- vector("list")
  num_matrices <- ceiling(num_filas / n)
  for (i in 1:num_matrices) {
    inicio <- (i - 1) * n + 1
    fin <- min(i * n, num_filas)
    matriz <- datos[inicio:fin, c("x", "y")]
    matriz = as.matrix(dist(cbind(matriz$x,matriz$y)))
    matriz  = 1/matriz
    diag(matriz) <- 0
    matriz = matriz/sum(matriz)
  matrixs[[i]] <- matriz
  }
matrixs
}
matrix_generate <- generate_matrix(Data1, 60)

data2 = NULL
for (i in 1:length(unique(Data1$.id))) {
  for (j in 1:1000) {
    datos2 = Data1 |> 
      dplyr::filter(.id == unique(Data1$.id)[i])
    dat = datos2[,-c(1:6,1007)]
    IM =  Moran.I(dat[[j]], 
                  matrix_generate[[i]])$p.value
    datax2 = data.frame(id = unique(Data1$.id)[i], 
                        IMorant =  IM, sim = paste0("sim",j))
    data2 = rbind.data.frame(data2,datax2)
  }
}

# Calculating Alpha

X0= xm %*% zm
Mo=  X0%*%solve(t(X0)%*%X0)%*%t(X0)
IDEN = matrix(nrow = 60,ncol = 60,data = rep(0,60))
diag(IDEN)<-1
apha <- (t(Data1$sim781[1:60]) %*% Mo %*% matrix_generate[[1]] %*% (IDEN - Mo) %*% Data1$sim781[1:60]) /
  (t(Data1$sim781[1:60]) %*% Mo %*% matrix_generate[[1]] %*% matrix_generate[[1]] %*% Mo %*% Data1$sim781[1:60])
data3 <- NULL

for (i in 1:length(unique(Data1$.id))) {
  for (j in 1:1000) {
    datos3 <- Data1 %>%
      dplyr::filter(.id == unique(Data1$.id)[i])
    dat3 <- datos3[,-c(1:6,1007)]
    apha <- t(dat3[[j]]) %*% Mo %*% matrix_generate[[i]] %*% (IDEN - Mo) %*% dat3[[j]] /
      t(dat3[[j]]) %*% Mo %*% matrix_generate[[i]] %*% matrix_generate[[i]] %*% Mo %*% dat3[[j]]
    datax3 <- data.frame(id = unique(Data1$.id)[i], a = apha, sim = paste0("sim",j))
    data3 <- rbind.data.frame(data3, datax3)
  }
}

