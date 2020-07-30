library(phytools)
library(mapdata)

#Archivo de coordenadas
gps<-read.csv("gps_data.example.dat",header=TRUE)
rownames(gps) <- gps$ID

#Para leer distintos formatos de de arboles filogeneticos
#read.newick, read.nexus
nwk=read.newick("tree.example.nwk")

#Agrego los paises a graficar
#countries_data <- scan("~/workspace/covid-19/mapeo_arbol/countries_data.dat",what="\n")
#countries_data <- c(countries_data ,"Venezuela", "Guyana", "Suriname","French Guiana", "Bolivia", "Peru", "falkland")  
countries_data <- c("Argentina", "falkland")  
latitudes_uniques=unique(gps$Lat)
latitudes_uniques=round(latitudes_uniques, digits=3)

#Colores del mapa
library("RColorBrewer")
colores=brewer.pal(n = 5, name = 'Set1')

#Colores a mano
#colores=c('#ffed6f','#1a1a1a','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#fdb462','#e54aed','#b15928','#8dd3c7','#b5aa16','#bebada','#17e833','#80b1d3','#6a3d9a','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f')
#Transparente "#00000000"
#colores[10]="#00000000"
#colores[1]="#00000000"
#colores[13]="#a60355"
#colores[11]="#fefc32"
#colores[7]="#1a1a1a"
#colores[1]="#ebcd00"
#colores[3]="#7719a2"

names(colores)=latitudes_uniques

# Mapeo de coordenadas con los colores
latitudes_id=gps[,1:2]
ids=gps$ID
latitudes=round(gps$Lat, digits=3)
names(latitudes)=gps$ID

colores_ids=c()
colores_vector=c()
id_vector=c()
for (i in (1:dim(latitudes_id)[1])) {
  color=(colores[toString(latitudes[i])])
  colores_vector[i]=color
  id=names(latitudes[i])
  id_vector[i]=id
}
names(colores_vector)=id_vector

# Grafico la figura
obj<-phylo.to.map(compute.brlen(nwk),gps[1:29,2:3],fsize=0.001,lwd=0.8,psize=0.8,lty="solid",colors=colores_vector,
                  direction="rightwards",database="worldHires",  pts=TRUE,
                  regions=countries_data)

#Mas opciones del plot
#pts=FALSE
#rotate=FALSE
#split=c(0.6,0.4)
#ftype="off"
#direction
#psize
#compute.brlen

