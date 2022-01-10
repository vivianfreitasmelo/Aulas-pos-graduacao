# Covariância e Coeficiente de Correlação

setwd("D:/OneDrive/Unichristus/Praticas/Estatististica_Descritiva")
getwd()


# Exemplo: Analisar a covariância e correlação entre as variáveis massa muscular e i(idade) e m(massa muscular).

# Definindo x e y
i = c(71, 64, 43, 67, 56, 73, 68, 56, 76, 65, 45, 58, 45, 53, 49, 78, 73, 68)
m = c(82, 91, 100, 68, 87, 73, 78, 80, 65, 84, 116, 76, 97, 100, 105, 77, 73, 78)

# Covariância
?cov
cov(i, m)

# Correlação
?cor
cor(i, m)

plot(i,m)

require(xlsx)
dados <- read.xlsx("massacorp.xlsx", sheetName = "massacorp")
View(dados)

# Definindo x e y
x = dados$Idade
y = dados$Massa

cov(x, y)

cor(x, y)
  