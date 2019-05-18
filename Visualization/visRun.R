#requires packages tidyverse, sf, IceCast, and rbst from github
#to download from github you will need devtools package
#install from github:
#install.packages("devtools")
#devtools::install_github("tarakc02/rbst")

#may need to change this path
source("visibility.R")

#create two SF polygon objects
ns.sf <- st_polygon(list(matrix(c(63,78,21,78,27,25,70,12,63,78),ncol=2,byrow=T)))
ns1.sf <- st_polygon(list(matrix(c(118,73,174,48,109,18,132,48,118,73),ncol=2,byrow=T)))

#create an sf multipolygon from a list of polygons
square.multi <- st_multipolygon(list(ns.sf,ns1.sf))

#visibility edges from a single vertex
pt <- c(63,78)#this vertex does not have to be the vertex of a polygon
vv <- visibleVertices(pt,square.multi)
vv1 <- lapply(vv, function(x) st_linestring(matrix(c(pt,x[1],x[2]),ncol=2,byrow=T)))
vv1 <- vv1 %>% st_multilinestring()


ggplot()+
  geom_sf(data=square.multi,aes(fill=T))+
  geom_point(aes(x=pt[1],y=pt[2]),color="red")+
  geom_sf(data=vv1,color="blue")

#all visibility edges of square.multi
vv1 <- vis.graph(square.multi)

ggplot()+
  geom_sf(data=square.multi,aes(fill=T))+
  geom_sf(data=vv1,color="blue")

#if you are interested in debugging print out from the source code, 
# replace all '#print(' in it with 'print('
# (There will be a ton of stuff printed out)
