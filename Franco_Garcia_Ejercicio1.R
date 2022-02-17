#### Enrique Franco García - Ejercicio 1 - 17/02/22####
library(igraph)
red <- read.csv("Amigos.csv")
red
row.names (red) <- red [ ,1]
red <- red [ ,-1]
red <- red [-2, ]
red <- red [ ,-2] 
#Elimino todos los valores que marca como NA y no permiten que corra bien la base
#Lo hago sobre el mismo objeto para evitar tener varios
red <- as.matrix (red) #Lo convierto en una matriz
ared <- graph_from_adjacency_matrix(red, mode= "directed") #A partir de la matriz
#la convierto en matriz de adyacencia para de esta manera poder indicar que es una red dirigida
#1. Grafique la red
V(ared)$size <- degree(ared, mode = "in")
plot(ared, edge.arrow.size = .2, edge.curved =.8) #Aqui puedo modificar caracteristicas da la grafica como 
#el tamaño de sus componentes 

#2. Determine a las tres personas con más amigues
red_pop <- degree(ared, mode = "in") #Creo un nuevo objeto y uso la funcion degree
#además uso "in", esto le va a indicar a R que debe de tomar en cuenta unicamente las entradas
pop <- sort(red_pop, decreasing = T) [1:3] #Con esto voy a ordenar de mayor a menor e indicando 
#que unicamente quiero que selecione a los 3 primeros
pop #Lo imprimo y visualizo que los 3 con mayor cantidad son Carolina, Maria Fernanda y Mayela

#3. Determine a las tres personas que consideran que tiene más amigues
red_amigables <- degree(ared, mode = "out") #Lo hago igual que en el ejercicio 2 solo que aqui
#uso el modo "out" ya que quiero unicamente las salidas
amigables <- sort(red_amigables, decreasing = T) [1:3]
amigables
#Como lo había hecho anteriormente vuelvo a ordenar de mayor a menor y le digo a R que 
#me guarde en el objeto amigables las 3 personas que mas tienen.

#4. Las tres personas más importantes por tres medidas de centralidad
p <- sort(degree_distribution(ared), decreasing = T) [1:3] #Los ordeno mediante
#el degree distribution de mayor a menor para así tenerlos acomodados y solo le pido que 
#me selecione a los 3 primeros usando corchetes
p #Lo imprimo
p2 <- sort(degree(ared), decreasing = T) [1:3] #Los ordeno mediante el degree  
#de mayor a menor para así tenerlos acomodados y solo le pido que 
#me selecione a los 3 primeros usando corchetes
p2 #Lo imprimo
p3 <- sort(mean(ared), decreasing = T)[1:3]
p3 
#5. Clusteriza la red con al menos dos métodos y determine cuáles son los clústers.
cluster1 <- cluster_edge_betweenness(ared,directed = FALSE)
table(membership(cluster1))

plot(cluster1,ared)

cluster2 <- cluster_spinglass(ared)#Realizo el cluster 2 por este metodo
membership(cluster2)
table(membership(cluster2)) #Para visualizar la cantidad de individuos que tiene cada grupo del cluster

#6. Calcule el diámetro
diameter(ared) #Calculo el diametro con esta funcion
#7. La matriz de distancias y dibuje un heatmap
