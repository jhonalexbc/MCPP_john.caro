memory.size(max = TRUE)
memory.limit(size = NA)

### Instalación del paquete "igraph"###
#install.packages("igraph")
###

### Cargar la libreria "igraph" necesaria para realizar Network Analyisis ###
library(igraph)
###

### Lectura de la base de datos de "World Bank Group Finances" correspondiente a los prestamos hechos y tomados
### por países o entidades en el mundo ###
banco_mundial = read.csv("C:/Users/Usuario/Documents/Carpeta de metodos computacionales de politicas publicas/proyecto final/Major_Contract_Awards_LIMPIO_final.csv",
head=TRUE,sep=";",dec=",")
###

### Creación de las relaciones entre variables para realizar el Network Analyisis ###

### Relación de los países que toman un prestamo ###
### Región vs Tipo de adquisición ###
region_P_type = as.matrix(data.frame(banco_mundial$Region, banco_mundial$Procurement.Type))
### Región vs Categoría de Adquisiciones ###
region_P_category = as.matrix(data.frame(banco_mundial$Region, banco_mundial$Procurement.Category))
### Región vs Sector Principal ###
region_M_sector = as.matrix(data.frame(banco_mundial$Region, banco_mundial$Major.Sector))
### Tipo de adquisición vs Sector Principal ###
P_type_M_sector = as.matrix(data.frame(banco_mundial$Procurement.Type, banco_mundial$Major.Sector))
### Categoría de Adquisiciones vs Sector Principal ###
P_category_Major_Sector = as.matrix(data.frame(banco_mundial$Major.Sector, banco_mundial$Procurement.Category))
### País Prestatario vs País Prestatario ###
S_contry_B_contry = as.matrix(data.frame(banco_mundial$Supplier.Country, banco_mundial$Borrower.Country))
###

###
### Región que más han pedido prestado: África ###
### Tipo de Adquisición más frecuente : Management /Technical Advice ###
### Categoría de adquisiciones más frecuente : Consultant Services ###
### Sector Principal más frecuente: Public Administration ###
###

### Creación de los objetos del gráfico de Network Analyisis ###
### Inicio Región vs Tipo de adquisición ###
region_P_type_graph = graph.edgelist(region_P_type, directed=FALSE)
### Dar color algunos atributos importantes ###
V(region_P_type_graph)$color = "pink"
V(region_P_type_graph)$frame.color = 0
V(region_P_type_graph)$label.color = "black"

X11()
plot(region_P_type_graph, edge.curved=TRUE)
title(expression("Relación entre Región y Tipo de Adquisición"), col.main = "orange")

### top 5 Región vs top 5 Tipo de adquisición ###
region_P_type_LA = region_P_type[region_P_type[,1]=="Europe and Central Asia" | region_P_type[,1]=="Latin America and Caribbean" |
region_P_type[,1]=="East Asia and Pacific" | region_P_type[,1]=="Africa" | 
region_P_type[,1]=="South Asia",]

region_P_type_LA = region_P_type_LA[region_P_type_LA[,2]=="Management /Technical Advice" | region_P_type_LA[,2]=="Project Management" |
region_P_type_LA[,2]=="Works, Infrastructure" | region_P_type_LA[,2]=="Works, Maintenance and Rehabilitation" | 
region_P_type_LA[,2]=="Equipment Information Technology",]
region_P_type_LA_graph = graph.edgelist(region_P_type_LA, directed=FALSE)
### Dar color algunos atributos importantes ###
V(region_P_type_LA_graph)$color = "pink"
V(region_P_type_LA_graph)["Latin America and Caribbean"]$color <- "turquoise"
V(region_P_type_LA_graph)$frame.color = 0
V(region_P_type_LA_graph)$label.color = "black"

X11()
plot(region_P_type_LA_graph, edge.curved=TRUE)
title(expression("Relación entre top 5 Región y top 5 Tipo de Adquisició"), col.main = "orange")

### print the degree for each variable ###
degree_top_5_r_pt = sort(degree(region_P_type_LA_graph))

### print the betweenness for each variable ###
betweenness_top_5_r_pt = sort(round(betweenness(region_P_type_LA_graph),1))
### Fin Región vs Tipo de adquisición ###

##############################################################################
### Inicio Región vs Categoría de adquisiciones ###
region_P_category_graph = graph.edgelist(region_P_category, directed=FALSE)
### Dar color algunos atributos importantes ###
V(region_P_category_graph)$color = "pink"
V(region_P_category_graph)$frame.color = 0
V(region_P_category_graph)$label.color = "black"

X11()
plot(region_P_category_graph, edge.curved=TRUE)
title(expression("Relación entre Región y Categoría de adquisiciones"), col.main = "orange")

### top 5 Región vs Categoría de adquisiciones sin Not Assigned ###
region_P_category_LA = region_P_category
region_P_category_LA = region_P_category_LA[region_P_category_LA[,2]!="Not Assigned",]
region_P_category_LA_graph = graph.edgelist(region_P_category_LA, directed=FALSE)
### Dar color algunos atributos importantes ###
V(region_P_category_LA_graph)$color = "pink"
V(region_P_category_LA_graph)["Latin America and Caribbean"]$color <- "turquoise"
V(region_P_category_LA_graph)$frame.color = 0
V(region_P_category_LA_graph)$label.color = "black"

X11()
plot(region_P_category_LA_graph, edge.curved=TRUE)
title(expression("Relación entre Región y Categoría de adquisiciones sin Not Assigned"), col.main = "orange")

### print the degree for each variable ###
degree_top_5_r_pc = sort(degree(region_P_category_LA_graph))

### print the betweenness for each variable ###
betweenness_top_5_r_pc = sort(round(betweenness(region_P_category_LA_graph),1))
### Fin Región vs Categoría de adquisiciones ###

##############################################################################
### Inicio Región vs Sector Principal ###
region_M_sector_graph = graph.edgelist(region_M_sector, directed=FALSE)
### Dar color algunos atributos importantes ###
V(region_M_sector_graph)$color = "pink"
V(region_M_sector_graph)$frame.color = 0
V(region_M_sector_graph)$label.color = "black"

X11()
plot(region_M_sector_graph, edge.curved=TRUE)
title(expression("Relación entre Región y Sector Principal"), col.main = "orange")

### top 5 Región vs top 5 Sector Principal ###
region_M_sector_LA = region_M_sector[region_M_sector[,1]=="Europe and Central Asia" | region_M_sector[,1]=="Latin America and Caribbean" |
region_M_sector[,1]=="East Asia and Pacific" | region_M_sector[,1]=="Africa" | 
region_M_sector[,1]=="South Asia",]

region_M_sector_LA = region_M_sector_LA[region_M_sector_LA[,2]=="Public Administration" | region_M_sector_LA[,2]=="(Historic)Health and other social services" |
region_M_sector_LA[,2]=="Water, Sanitation and Waste Management" | region_M_sector_LA[,2]=="Agriculture, Fishing and Forestry" | 
region_M_sector_LA[,2]=="Transportation",]
region_M_sector_LA_graph = graph.edgelist(region_M_sector_LA, directed=FALSE)
### Dar color algunos atributos importantes ###
V(region_M_sector_LA_graph)$color = "pink"
V(region_M_sector_LA_graph)["Latin America and Caribbean"]$color <- "turquoise"
V(region_M_sector_LA_graph)$frame.color = 0
V(region_M_sector_LA_graph)$label.color = "black"

X11()
plot(region_M_sector_LA_graph, edge.curved=TRUE)
title(expression("Relación entre top 5 Región y top 5 Sector Principal"), col.main = "orange")
### print the degree for each variable ###
degree_top_5_r_ms = sort(degree(region_M_sector_LA_graph))

### print the betweenness for each variable ###
betweenness_top_5_r_ms = sort(round(betweenness(region_M_sector_LA_graph),1))
### Fin Región vs Sector Principal ###

##############################################################################
### Sector Principal vs Tipo de adquisición ###
### top 5 Sector Principal vs top 5 Tipo de adquisición ###
P_type_M_sector_LA = P_type_M_sector[P_type_M_sector[,1]=="Management /Technical Advice" | P_type_M_sector[,1]=="Project Management" |
P_type_M_sector[,1]=="Works, Infrastructure" | P_type_M_sector[,1]=="Works, Maintenance and Rehabilitation" | 
P_type_M_sector[,1]=="Equipment Information Technology",]

P_type_M_sector_LA = P_type_M_sector_LA[P_type_M_sector_LA[,2]=="Public Administration" | P_type_M_sector_LA[,2]=="(Historic)Health and other social services" |
P_type_M_sector_LA[,2]=="Water, Sanitation and Waste Management" | P_type_M_sector_LA[,2]=="Agriculture, Fishing and Forestry" | 
P_type_M_sector_LA[,2]=="Transportation",]
P_type_M_sector_LA_graph = graph.edgelist(P_type_M_sector_LA, directed=FALSE)
### Dar color algunos atributos importantes ###
V(P_type_M_sector_LA_graph)$color = "pink"
#V(P_type_M_sector_LA_graph)["Latin America and Caribbean"]$color <- "turquoise"
V(P_type_M_sector_LA_graph)$frame.color = 0
V(P_type_M_sector_LA_graph)$label.color = "black"

X11()
plot(P_type_M_sector_LA_graph, edge.curved=TRUE)
title(expression("Relación entre top 5 Tipo de Adquisición y top 5 Sector Principal"), col.main = "orange")

### print the degree for each variable ###
degree_top_5_pt_ms = sort(degree(P_type_M_sector_LA_graph))

### print the betweenness for each variable ###
betweenness_top_5_pt_ms = sort(round(betweenness(P_type_M_sector_LA_graph),1))
### Fin Sector Principal vs Tipo de adquisición ###

##############################################################################
### Categoría de adquisiciones vs Sector Principal ###
### top 5 Categoría de adquisiciones vs tp 5 Sector Principal ###
P_category_Major_Sector_LA = P_category_Major_Sector[P_category_Major_Sector[,1]=="Public Administration" | P_category_Major_Sector[,1]=="(Historic)Health and other social services" |
P_category_Major_Sector[,1]=="Water, Sanitation and Waste Management" | P_category_Major_Sector[,1]=="Agriculture, Fishing and Forestry" | 
P_category_Major_Sector[,1]=="Transportation",]

P_category_Major_Sector_LA = P_category_Major_Sector_LA[P_category_Major_Sector_LA[,2]!="Not Assigned",]
P_category_Major_Sector_LA_graph = graph.edgelist(P_category_Major_Sector_LA, directed=FALSE)
### Dar color algunos atributos importantes ###
V(P_category_Major_Sector_LA_graph)$color = "pink"
#V(P_category_Major_Sector_LA_graph)["Latin America and Caribbean"]$color <- "turquoise"
V(P_category_Major_Sector_LA_graph)$frame.color = 0
V(P_category_Major_Sector_LA_graph)$label.color = "black"

X11()
plot(P_category_Major_Sector_LA_graph, edge.curved=TRUE)
title(expression("Relación entre top 5 Sector Principal y Categoría de adquisiciones sin Not Assigned"), col.main = "orange")

### print the degree for each variable ###
degree_top_5_pc_ms = sort(degree(P_category_Major_Sector_LA_graph))

### print the betweenness for each variable ###
betweenness_top_5_pc_ms = sort(round(betweenness(P_category_Major_Sector_LA_graph),1))
### Fin Categoría de adquisiciones vs Sector Principal ###

##############################################################################
### Inicio Colombia presta vs países que reciben el prestamo ###
S_contry_B_contry_2 = S_contry_B_contry
S_contry_B_contry_2 = S_contry_B_contry_2[S_contry_B_contry_2[,1]=="Colombia" | S_contry_B_contry_2[,1]=="Panama" |
S_contry_B_contry_2[,1]=="Honduras" | S_contry_B_contry_2[,1]=="Peru" | S_contry_B_contry_2[,1]=="Nicaragua" | 
S_contry_B_contry_2[,1]=="El Salvador",]
#S_contry_B_contry_2[,1] = ifelse(S_contry_B_contry[,1]!="Colombia","Otro País","Colombia")
S_contry_B_contry_2[,1] = ifelse(S_contry_B_contry_2[,1]!="Colombia","Otro País","Colombia")
S_contry_B_contry_2 = S_contry_B_contry_2[S_contry_B_contry_2[,2]=="Colombia" | S_contry_B_contry_2[,2]=="Panama" |
S_contry_B_contry_2[,2]=="Honduras" | S_contry_B_contry_2[,2]=="Peru" | S_contry_B_contry_2[,2]=="Nicaragua" | 
S_contry_B_contry_2[,2]=="El Salvador",]

S_contry_B_contry_graph = graph.edgelist(S_contry_B_contry_2, directed=FALSE)
### Dar color algunos atributos importantes ###
V(S_contry_B_contry_graph)$color = "pink"
V(S_contry_B_contry_graph)["Colombia"]$color <- "turquoise"
V(S_contry_B_contry_graph)$frame.color = 0
V(S_contry_B_contry_graph)$label.color = "black"

X11()
plot(S_contry_B_contry_graph, edge.curved=TRUE)
title(expression("Relación entre Colombia cuando presta y países que toman prestado"), col.main = "orange")

### print the degree for each variable ###
degree_top_5_cp_ptp = sort(degree(S_contry_B_contry_graph))

### print the betweenness for each variable ###
betweenness_top_5_cp_ptp = sort(round(betweenness(S_contry_B_contry_graph),1))
### Fin Colombia presta vs países que reciben el prestamo ###

##############################################################################
### Inicio Colombia toma prestado vs países que dan prestamos ###
S_contry_B_contry_2_p = S_contry_B_contry
S_contry_B_contry_2_p = S_contry_B_contry_2_p[S_contry_B_contry_2_p[,1]=="Colombia" | S_contry_B_contry_2_p[,1]=="Spain" |
S_contry_B_contry_2_p[,1]=="Israel" | S_contry_B_contry_2_p[,1]=="United Kingdom" | S_contry_B_contry_2_p[,1]=="United States" | 
S_contry_B_contry_2_p[,1]=="Guatemala",]
#S_contry_B_contry_2_p[,1] = ifelse(S_contry_B_contry[,1]!="Colombia","Otro País","Colombia")
S_contry_B_contry_2_p = S_contry_B_contry_2_p[S_contry_B_contry_2_p[,2]=="Colombia" | S_contry_B_contry_2_p[,2]=="Spain" |
S_contry_B_contry_2_p[,2]=="Israel" | S_contry_B_contry_2_p[,2]=="United Kingdom" | S_contry_B_contry_2_p[,2]=="United States" | 
S_contry_B_contry_2_p[,2]=="Guatemala",]
S_contry_B_contry_2_p[,2] = ifelse(S_contry_B_contry_2_p[,2]!="Colombia","Otro País","Colombia")

S_contry_B_contry_graph_p= graph.edgelist(S_contry_B_contry_2_p, directed=FALSE)
### Dar color algunos atributos importantes ###
V(S_contry_B_contry_graph_p)$color = "pink"
V(S_contry_B_contry_graph_p)["Colombia"]$color <- "turquoise"
V(S_contry_B_contry_graph_p)$frame.color = 0
V(S_contry_B_contry_graph_p)$label.color = "black"

X11()
plot(S_contry_B_contry_graph_p, edge.curved=TRUE)
title(expression("Relación entre Colombia cuando pide prestado y países que dan prestado"), col.main = "orange")
### print the degree for each variable ###
degree_top_5_ctp_pp = sort(degree(S_contry_B_contry_graph_p))

### print the betweenness for each variable ###
betweenness_top_5_ctp_pp = sort(round(betweenness(S_contry_B_contry_graph_p),1))
### Fin Colombia toma prestado vs países que dan prestamos ###
