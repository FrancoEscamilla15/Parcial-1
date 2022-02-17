####Enrique Franco Garcia - Problema 2 - 17/02/22

library(igraphdata)
data("karate")
karate
#1.Encuentre las tres personas más conectadas.
karate_nodos <- degree(karate, mode = "in") #Creo un nuevo objeto y uso la funcion degree
#además uso "in", esto le va a indicar a R que debe de tomar en cuenta unicamente las entradas
knodos <- sort(karate_nodos, decreasing = T) [1:3] #Con esto voy a ordenar de mayor a menor e indicando 
#que unicamente quiero que selecione a los 3 primeros
knodos #Lo imprimo y visualizo que los 3 con mayor cantidad son John A, Mr Hi y Actor 33

#2. La gráfica de la distribución de conectividades.
graf_dc <- degree_distribution(karate) #Genero un objeto que va a conetener la distribucion 
#de conectividades de la base de datos
plot(graf_dc) #genere una grafica a partir de lo anterior

#3. El diámetro de la red.
diameter(karate)

#4. El coeficiente de clusterización cada una de las 3 personas más conectadas
sort(transitivity(karate, type = "local"), decreasing = T) [1:3]
#Con esto estoy ordenando de mayor a menor usando la funcion transitivity de igraph que 
#me permite obtener el coeficiente de clusterización y entre los corchetes coloque 1:3 porque
#quero que me de solo los 3 primeros. Es importante poner "local" porque R lo convierte en global

#5. Encuentre si los hay, a los nodos con coeficiente de clusterización de 1. Discute su significado.
transitivity(karate, type = "local")
#Si hay 11, esto indica que estan sumamente conectados 

#6. El porcentaje de conexiones respecto al total.

#7. El promedio de conectividades.
promedio <- degree (karate) #Con este calculo 
mean (promedio)

#8. Encuentre QUIÉNES son las 3 personas más importantes con al menos 3 distintos métodos
m1 <- sort(degree_distribution(karate), decreasing = T) [1:3] #Los ordeno mediante degree distribution
#de mayor a menor y selecionando unicamente los 3 primeros
m1 #Lo imprimo
m2 <- sort(degree(karate), decreasing = T) [1:3] #Los ordeno mediante degree 
#de mayor a menor y selecionando unicamente los 3 primeros
m2 #Lo imprimo
m3 <- sort(closeness(karate), decreasing = T) [1:3] #Los ordeno mediante su cercania
#
m3
#9.Encuentre la trayectoria entre las personas más alejadas. 
farthest.nodes(karate)

#10. Clusteriza la red con al menos 4 métodos distintos y discute tu resultado sabiendo que ese grupo de personas se separo en dos clubes distintos con el tiempo.
clustera <- cluster_edge_betweenness(karate,directed = FALSE)
table(membership(clustera))
plot(clustera,karate)
---------
clusterb <- cluster_fast_greedy(karate)
membership(clusterb)
table(membership(clusterb))
plot(clusterb,karate)
----------
clusterc <- cluster_fast_greedy(karate)
membership(clusterc)
table(membership(clusterc))
plot(clusterc,karate)
----------
clusterd <- cluster_infomap(karate)
membership(clusterd)
table(membership(clusterd))
plot(clusterd,karate)
