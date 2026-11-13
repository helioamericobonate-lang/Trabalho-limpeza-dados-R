
#Helio A. Bonate (ISCET | Ciencias de dados e IA)

# Instacao e Carregamento de Pacotes --------------------------------------

install.packages("readr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# Importacao e Conhecimento de Dados --------------------------------------

dados <- read.csv("C:/Users/BONATE/Downloads/dados_sujos_limpeza.csv")
head(dados) #Visualizar os primeiros registos
View(dados) #Visualizar a base de dados


# Identificacao de Valores Ausentes --------------------------

is.na(dados) #Verificar valores ausentes
sum(is.na(dados)) #Contar o total de valores ausentes
dados[!complete.cases(dados), ] #visualizar linhas incompletas

names(dados) <- trimws(names(dados)) #Remover espacos desnecessarios

dados[] <- lapply(dados, function(x){
  if (is.character(x)){
    x <- trimws(x)
    x[x == ""] <- NA 
    }
  return(x)
}) #Corrigir espacos nas celulas de texto

sum(is.na(dados)) #Contar o total de valores ausentes apos a correcao
dados[!complete.cases(dados), ] #Visualizar linhas incompletas 

#Tratamento de valores Nulos ----------------------

mediana_idade <- median(dados$idade, na.rm = TRUE) #Calculo da mediana
dados$idade[is.na(dados$idade)] <- mediana_idade #Substituir NA pela mediana
sum(is.na(dados$idade)) #Verificar Resultados
summary(dados$idade)


# Identificacao e Eliminacao de Registos Duplicados -----------------------

sum(duplicated(dados)) #contar duplicados
dados[duplicated(dados), ] #Visualizar dados duplicados
dados <- dados[!duplicated(dados), ] #Eliminar dados duplicados
sum(duplicated(dados)) #Verificar resultado


# Correcao de Espacos e letras maiusculas/minusculas ----------------------
dados$nome_cliente <- tools::toTitleCase(
  tolower(trimws(dados$nome_cliente))
) #Correcao de nome_cliente

dados$cidade <- tools::toTitleCase(
  tolower(trimws(dados$cidade))
) #Correcao de cidades

dados$produto <- tools::toTitleCase(
  tolower(trimws(dados$produto))
) #Correcao de produto

dados$metodo_pagamento <- tools::toTitleCase(
  tolower(trimws(dados$metodo_pagamento))
) #Correcao de metodo_pagamento

dados$enail <- tolower(trimws(dados$email)) #Email sempre em minusculas

head(dados) #Visualizar Resultados


# Identificacao e Tratamento de Outliers ----------------------------------

dados[dados$idade < 0 | dados$idade > 100, ] #Identificar idades invalidas (Outliers)
dados$idade[dados$idade < 0 | dados$idade > 100] <- NA #Corrigir idades invalidas
dados[dados$quantidade <= 0, ] #Verificar quanidades negativas ou igual a zero
dados$quantidade[dados$quantidade <= 0] <- NA

identificar_outliers <- function(x){
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  intervalo <- IQR(x, na.rm = TRUE)
  
  limite_inferior <- Q1-1.5 * intervalo
  limite_superior <- Q3 + 1.5 * intervalo
  x < limite_inferior | x > limite_superior
} #Funcao para identificar outliers

outliers_idade <- identificar_outliers(dados$idade) #identificar outliers na idade
outliers_quantidade <- identificar_outliers(dados$quantidade) #Outliers na quantidade
dados[outliers_idade, ] #Visualizar Resultado
dados[outliers_quantidade, ] #Visualizar Resultado

# Correcao de Tipos de Dados ----------------------------------------------

dados$id_venda <- as.character(dados$id_venda) #Converter id para texto
dados$nome_cliente <- as.character(dados$nome_cliente) #Converter nome_cliente para texto
dados$idade <- as.numeric(dados$idade) #Converter idade para numero
dados$cidade <- as.character(dados$cidade) #Converter cidade para texto
dados$produto <- as.character(dados$produto) #Converter produto para texto
dados$quantidade <- as.numeric(dados$quantidade) #Converter quantidade para numero
if (is.character(dados$preco_unitario_MZN)){
  dados$preco_unitario_MZN <- as.numeric(
    gsub(",",".", dados$preco_unitario_MZN)
  )
} #Converter preco para numerico
dados$data_venda <- as.Date(
  dados$data_venda,
  format = "%d/%m/%Y"
) #Converter data para formato de data
dados$email <- as.character(dados$email) #Converter email para texto
dados$metodo_pagamento <- as.character(dados$metodo_pagamento) #Metodo de pagamento para texto
dados$satisfacao <- as.numeric(dados$satisfacao) #Converter Satisfacao em texto
str(dados) #Verificar os tipos de dados

# Validacao de emails -----------------------------------------------------
validar_email <- function(email){
  grepl(
    "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
    email
  )
} #Criar funcao de validacao
dados$email_valido <- validar_email(dados$email) #validar emails
dados[dados$email_valido==FALSE, ] #Visualizar emails invalidos
sum(dados$email_valido, na.rm = TRUE) #Contar emails validos
sum(!dados$email_valido, na.rm = TRUE) #Contar emails invalidos

# CSV com Dados Limpos ----------------------------------------------------
getwd() #Ver caminho

dir.create("resultado", showWarnings = FALSE) #Criar a pasta
write.csv(dados, file = "resultado/dados_limpos.csv", row.names = FALSE,
          na = ""
          ) #Exportar o ficheiro

file.exists("resultado/dados_limpos.csv") #Confirmar se foi guardado
