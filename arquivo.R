# -----------------------------------------------------
# PROJETO DE MATRIZES E GRAFOS – MARKETPLACES
# -----------------------------------------------------

# Instalar pacotes caso não possua
# install.packages("igraph")

library(igraph)

# -----------------------------------------------------
# DADOS DA PESQUISA
# -----------------------------------------------------

marketplaces <- list(
  "Mercado Livre" = c("Estevão", "Joaquim", "Artur", "Jenie", "Gustavo", "Matheus", "Daniel"),
  "Amazon"        = c("Estevão", "Joaquim", "Gustavo", "Matheus"),
  "Shopee"        = c("Jenie", "Simone", "Estevão", "Vinicius"),
  "Shein"         = c("Matheus", "Simone", "Estevão", "Maisa"),
  "AliExpress"    = c("Daniel")
)

alunos <- sort(unique(unlist(marketplaces)))
mkt     <- names(marketplaces)

# -----------------------------------------------------
# MATRIZ DE INCIDÊNCIA (Alunos x Marketplaces)
# -----------------------------------------------------

incidencia <- matrix(0, nrow = length(alunos), ncol = length(mkt),
                     dimnames = list(alunos, mkt))

for (mp in mkt) {
  incidencia[marketplaces[[mp]], mp] <- 1
}

cat("\n MATRIZ DE INCIDÊNCIA:\n")
print(incidencia)

# -----------------------------------------------------
# MATRIZ DE SIMILARIDADE (Aluno ↔ Aluno)
# -----------------------------------------------------

similaridade <- incidencia %*% t(incidencia)

cat("\n MATRIZ DE SIMILARIDADE:\n")
print(similaridade)

# -----------------------------------------------------
# MATRIZ DE COOCORRÊNCIA (Marketplace ↔ Marketplace)
# -----------------------------------------------------

coocorrencia <- t(incidencia) %*% incidencia

cat("\n MATRIZ DE COOCORRÊNCIA:\n")
print(coocorrencia)

# -----------------------------------------------------
# GRAFO DE INCIDÊNCIA (bipartido)
# -----------------------------------------------------

g_inc <- graph_from_incidence_matrix(incidencia)

cat("\n--- MÉTRICAS GRAFO DE INCIDÊNCIA ---\n")
print(list(
  degree = degree(g_inc),
  betweenness = betweenness(g_inc),
  closeness = closeness(g_inc),
  eigenvector = eigen_centrality(g_inc)$vector,
  clustering = transitivity(g_inc, type = "localaverage")
))

# -----------------------------------------------------
# GRAFO DE SIMILARIDADE ENTRE ALUNOS
# -----------------------------------------------------

# Remove diagonal e liga apenas pares com similaridade > 0
similaridade_no_diag <- similaridade
diag(similaridade_no_diag) <- 0

g_sim <- graph_from_adjacency_matrix(similaridade_no_diag, mode = "undirected", weighted = TRUE)

cat("\n--- MÉTRICAS GRAFO DE SIMILARIDADE ---\n")
print(list(
  degree = degree(g_sim),
  betweenness = betweenness(g_sim),
  closeness = closeness(g_sim),
  eigenvector = eigen_centrality(g_sim)$vector,
  clustering = transitivity(g_sim, type = "localaverage")
))

# -----------------------------------------------------
# GRAFO DE COOCORRÊNCIA ENTRE MARKETPLACES
# -----------------------------------------------------

cooc_no_diag <- coocorrencia
diag(cooc_no_diag) <- 0

g_cooc <- graph_from_adjacency_matrix(cooc_no_diag, mode = "undirected", weighted = TRUE)

cat("\n--- MÉTRICAS GRAFO DE COOCORRÊNCIA ---\n")
print(list(
  degree = degree(g_cooc),
  betweenness = betweenness(g_cooc),
  closeness = closeness(g_cooc),
  eigenvector = eigen_centrality(g_cooc)$vector,
  clustering = transitivity(g_cooc, type = "localaverage")
))

# -----------------------------------------------------
# PLOTS DOS GRAFOS (opcional)
# -----------------------------------------------------
par(mfrow = c(1,3))

plot(g_inc,
     main = "Grafo de Incidência",
     vertex.color = ifelse(V(g_inc)$type, "lightblue", "orange"),
     vertex.size = 20)

plot(g_sim,
     main = "Grafo de Similaridade (Alunos)",
     vertex.color = "lightgreen",
     vertex.size = 20)


plot(g_cooc,
     main = "Grafo de Coocorrência (Marketplaces)",
     vertex.color = "salmon",
     vertex.size = 20)
