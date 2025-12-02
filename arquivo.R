# ----------------------------------------------------------
# PROJETO MARKETPLACES — versão seguindo o código fornecido
# ----------------------------------------------------------

# Você deve salvar um arquivo .txt ou .csv com 3 colunas:
# from , to , weight
#
# Exemplo de linhas:
# Estevão, Mercado Livre, 1
# Joaquim, Mercado Livre, 1
# Matheus, Shein, 1
#

# Lê o arquivo

dados <- read.csv(file.choose(), header = TRUE, sep = ",")


# Visualiza os dados
dados


# ----------------------------------------------------------
# MATRIZ DE INCIDÊNCIA (usando xtabs)
# ----------------------------------------------------------

matriz_inc <- xtabs(weight ~ from + to, data = dados)

class(matriz_inc)        # "table"
matriz_inc <- unclass(matriz_inc)   # transforma em matriz
class(matriz_inc)

# Exibe a matriz de incidência
print(matriz_inc)


# ----------------------------------------------------------
# MATRIZ DE SIMILARIDADE (Aluno × Aluno)
# ----------------------------------------------------------

s <- matriz_inc %*% t(matriz_inc)

class(s)
diag(s) <- 0   # remove a diagonal
s


# ----------------------------------------------------------
# MATRIZ DE COOCORRÊNCIA (Marketplace × Marketplace)
# ----------------------------------------------------------

c <- t(matriz_inc) %*% matriz_inc

diag(c) <- 0
c


# ----------------------------------------------------------
# GRAFOS (igual ao modelo fornecido)
# ----------------------------------------------------------

library(igraph)

# Grafo de incidência — direcionado e ponderado
grafo_inc <- graph_from_incidence_matrix(
  matriz_inc,
  directed = TRUE,
  mode = "out",
  weighted = TRUE
)

# Grafo de similaridade entre alunos
grafo_sim <- graph_from_adjacency_matrix(
  s,
  weighted = TRUE,
  mode = "undirected"
)

# Grafo de coocorrência entre marketplaces
grafo_co <- graph_from_adjacency_matrix(
  c,
  weighted = TRUE,
  mode = "undirected"
)


# ----------------------------------------------------------
# PLOTS
# ----------------------------------------------------------

plot(grafo_inc, edge.width = E(grafo_inc)$weight, edge.arrow.size = 0.1)
plot(grafo_sim, edge.width = E(grafo_sim)$weight)
plot(grafo_co, edge.width = E(grafo_co)$weight)


# ----------------------------------------------------------
# MÉTRICAS TOPOLOGICAS (iguais ao modelo fornecido)
# ----------------------------------------------------------

# Vértices
V(grafo_sim)
length(V(grafo_sim))

# Arestas
E(grafo_sim)
ecount(grafo_sim)

# Grau
degree(grafo_sim)
mean(degree(grafo_sim))

# Pesos e força média
E(grafo_sim)$weight
mean(E(grafo_sim)$weight)

# Densidade da rede
edge_density(grafo_sim, loops = FALSE)

