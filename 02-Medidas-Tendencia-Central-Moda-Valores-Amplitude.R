# Medidas de Tendência Central - Moda, Valores Máximo e Mínimo e Amplitude


setwd("D:/OneDrive/Unichristus/Praticas/Estatististica_Descritiva")
getwd()


##### Moda #####


# Exemplo: Uma loja de calçados quer saber qual o tamanho mais comprado em um dia de vendas. 
# A partir dos dados coletados a seguir, determine o tamanho mais pedido. 
tamanhos = c(38, 38, 36, 37, 36, 36, 40, 39, 36, 35, 36)
mean(tamanhos)  
median(tamanhos)

moda = function(dados) {
  vetor = table(as.vector(dados))
  names(vetor)[vetor == max(vetor)]
}

moda(tamanhos)


##### Valores Máximo e Mínimo #####

# Representam os valores máximos e mínimos da distribuição de dados

# Exemplo: Quais são os valores máximo e mínimo dos tamanhos de sapatos do item anterior.
tamanhos = c(38, 38, 36, 37, 36, 36, 40, 39, 36, 35, 36)
max(tamanhos)
min(tamanhos)


##### Amplitude #####

# A amplitude é a diferença entre o maior e menor valor de um conjunto de dados qualquer.

# Exemplo: Bob quer aprender a voar com asa delta, e ele quer saber qual a amplitude máxima que um voo pode ter. 
# A partir dos dados de outros praticantes de voo livre, determine qual a amplitude. 
dados = c(28, 31, 45, 58, 22, 33, 42, 68, 24, 37)
range(dados)
diff(range(dados))






