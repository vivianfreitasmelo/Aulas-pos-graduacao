# Quartil e Percentil

setwd("D:/OneDrive/Unichristus/Praticas/Estatististica_Descritiva")
getwd()


##### Quartis #####

# Exemplo: O horário de funcionamento de um banco já está se esgotando, para adiantar o atendimento dos clientes o 
# gerente decide para de chamar individualmente e passa a chamar em grupos de 1/4 da quantidade total de clientes na fila. 
# A partir dos números das fichas dos clientes, determine os grupos das 4 chamadas.
num_fichas = c(54, 55, 56, 57, 58, 59, 60, 61, 62, 63)
?quantile
quantile(num_fichas)


##### Percentis #####

# Exemplo: Consoderando os dados do exemplos anterior, calcule o percentil 10, 80 e 98.
num_fichas = c(54, 55, 56, 57, 58, 59, 60, 61, 62, 63)
quantile(num_fichas, c(.10))
quantile(num_fichas, c(.80))
quantile(num_fichas, c(.98))

# Ou seja, o cliente que está com pouco mais do que 98% da fila a frente dele terá a ficha 63, 
# o que está com pouco menos do que 80% da fila a frente dele terá a ficha 61 
# e o que está com pouco mais do que 10% da fila a frente dele terá a ficha 55.






