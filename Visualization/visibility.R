library(tidyverse)
library(IceCast)
library(rbst)
library(sf)

polydf <- function(pts){
  mat <- matrix(pts,ncol=2,byrow=T)
  
  data.frame(x=mat[,1],y=mat[,2])
}

#finds the counterclockwise angle between the segment from p1 to p2 and the horizontal line from p1.
angle <- function(p1,p2){
  vec <- as.numeric(p2-p1)
  if(p1[1]==p2[1] && p1[2]==p2[2]){
    #print("minus 10")
    return(-10)
  }
  theta <- atan2(vec[2],vec[1])
  if(theta<=0){
    return(round(abs(theta),5))
  }
  return(round(2*pi-theta,5))
}

#orders a dataframe of points, p.df, by angle of each from p and the horizontal.
order.by.angle <- function(p,p.df){
  angles <- numeric(nrow(p.df))
  dis <- numeric(nrow(p.df))
  #angles <- apply(as.matrix(p.df),1,angle,p2=p)
  for(i in 1:nrow(p.df)){
    ###print(p)
    ###print(p.df[i,1:2])
    angles[i] <- angle(p,p.df[i,1:2])
    dis[i] <- distance(p,p.df[i,1:2])
  }
  #print(cbind(p.df,angles,dis))
  p.df[order(angles,dis,decreasing=F),]
}

#generate list of edges from point dataframe
#these are stored in a list
#each element of the list is a list that contains two vertices
gen.edges <- function(p.df){
  edges <- list()
  for(i in 2:nrow(p.df)-1){
    edges <- c(edges,list(list(p.df[i,],p.df[i+1,])))
  }
  #edges <- c(edges,list(list(p.df[nrow(p.df),],p.df[1,])))
  edges
}

#checkIntersect wrapper function
#should probably convert this to use sf
intersect <- function(pt,e.df,ptx=310){
  int.edge <- c()
  ###print(e.df)
  for(i in 1:(nrow(e.df)-1)){
    ##print(pt)
    ##print(c(ptx,pt[2]))
    ##print(e.df[i,])
    ##print(e.df[i+1,])
    if(checkIntersect(c(pt[1]+0.0001,pt[2]),c(ptx,pt[2]),e.df[i,],e.df[i+1,])){
      ##print("true!")
      int.edge <- c(int.edge,list(list(e.df[i,],e.df[i+1,])))
    }
  }
  
  int.edge
}

#euclidean  distance between two points
distance <- function(p1,p2){
  sqrt(sum((p1-p2)^2))
}


#the same as those used in class
signed.area <- function(a,b,c){
  sa <- ((b[1]-a[1])*(c[2]-a[2])-(b[2]-a[2])*(c[1]-a[1]))/2
  return(sa)
}

#taken from what we did in class in mathematica
leftOf <- function(a,b,c){
  signed.area(a,b,c)>=0
}

#given two edges, it should determine if the second is 
#clockwise of or counterclockwise of the first.
#THEY MUST SHARE A MIDDLE VERTEX
#returns true for counterclockwise (left)
#false for clockwise (right)
edge.direction <- function(e1,e2){
  #e1 and e2 will share a middle vertex
  #e2 will be "counterclockwise of e1" if the second vertex of e2 
  #is "leftOf" e1
  a <- e1[[1]]
  b <- e1[[2]]
  bprime <- e2[[1]]
  c <- e2[[2]]
  if(b[[1]]!=bprime[[1]] && b[[2]]!=bprime[[2]]){
    ##print("Error - edges do not share central vertex.")
    return(NA)
  }
  leftOf(a,b,c)
}

#this is the visible function as described in de Berg et. al.
#p is a point being testing from
#w is the current point
#wj is the previous point
#poly is the st_polygon of the current polygon
#visible edges is the current list of visible edges
#polylist is the list of Polygon objects (from SF package)
#all pts is the list of points in the entire set of polygons 
visible <- function(p,w,wj,poly,tree,visible_verts,polylist,allpts){
  #print("visible function")
  #print(p)
  #print(w)
  #print(c("wj",wj))
  ###print(visible_verts)
  ##print(tuplein(wj,visible_verts))
  ###print(poly)
  pw <- st_linestring(matrix((c(p,w)),ncol=2,byrow=T))
  #print(pw)
  #print(poly)
  #print(st_crosses(pw,poly,sparse=F))
  ##print("visibletest1")
  #crosses the boundary, or is contained within
  if((st_crosses(pw,poly,sparse=F))||st_within(pw,poly,sparse=F)){
    #print("visibletest2")
    #print(FALSE)
    return(F)
  }
  
  else if (is.null(wj) || !st_intersects(st_point(wj),pw,sparse=F)){
    #print("visibletest3")
    #find the leftmost leaf of the tree
    e <- NULL
    if(!is_empty(tree)){
      e <- retrieve(tree,min_key(tree))
      e <- lapply(e,as.numeric)
      lm.edge <- st_linestring(matrix(c(e[[1]],e[[2]]),ncol=2,byrow=T))
    }
    #print(e)
    #if it intersects, but doesn't cross, can be either or 
    if(!is.null(e) && st_crosses(pw,lm.edge,sparse=F)){
      #print('return f')
      return(F)
    } else if(!is.null(e) && st_intersects(pw,lm.edge,sparse=F)){
      ppoly <- NULL
      #print("testing crosses")
      #print(w)
      ##print(allpts)
      for(i in 1:length(allpts)){
        ##print(allpts[[i]])
        ##print(tuplein.mat(w,allpts[[i]]))
        if(tuplein.mat(wj,allpts[[i]])){
          #print("ppoly found")
          ppoly <- polylist[[i]]
          #print(ppoly)
          i <- length(allpts)+1
        }
        ##print("here")
        ##print(st_crosses(pw,ppoly,sparse=F))
      }
      if(!is.null(ppoly) && st_crosses(pw,ppoly,sparse=F)){
        #print("return f 1")
        return(F)
      } else{
        return(T)
      }
    } else{
      return(T)
    }
  }
    ##print("visibletest4")
  
  else if(!tuplein(wj,visible_verts)){
    #print("visibletest5")
    #print("return f")
    return(F)
  }
  
  else {
    #print("visibletest6")
    #search tree for an edge e that intersects w-wj
    e1 <- NULL
    for(key in keys(tree)){
      e <- retrieve(tree,key)
      e.sf <- st_linestring(matrix(c(e[[1]],e[[2]]),ncol=2,byrow=T))
      w.wj.sf <- st_linestring(matrix(c(w,wj),ncol=2,byrow=T))
      if(st_crosses(w.wj.sf,e.sf,sparse=F)){
        #print(c("intersecting edge: ",e))
        #print("return f")
        return(F)
      }
    }
    #print("return t")
    return(T)
  }
  
}

#is tuple (a 2d coordinate pair) in the list of pairs tuplist?
tuplein <- function(tuple,tuplist){
  if(length(tuplist)==0){
    return(F)
  }
  for(i in 1:length(tuplist)){
    if(((tuple[1]==tuplist[[i]][1]) && (tuple[2]==tuplist[[i]][2]))){
      return(T)
    }
  }
  return(F)
}

#is tuple (a 2d coordinate pair) in a matrix of coordinate pairs?
tuplein.mat <- function(tuple,tupmat){
  
  if(nrow(tupmat)==0){
    return(F)
  }
  for(i in 1:nrow(tupmat)){
    if(((tuple[1]==tupmat[i,1]) && (tuple[2]==tupmat[i,2]))){
      return(T)
    }
  }
  return(F)
}

#associates all vertices with their edges in a polygon
#takes in a dataframe of vertices
#return a dataframe with the same vertex information but four additional columns
#two for each edge with the x and y values of the attached vertexxzsxzsxx
edges.from.vert <- function(e.df){
  if(!all(e.df[1,]==e.df[nrow(e.df),])){
    e.df <- rbind(e.df,e.df[1,])
  }
  
  e1x <- numeric(nrow(e.df))
  e1y <- numeric(nrow(e.df))
  
  e2x <- numeric(nrow(e.df))
  e2y <- numeric(nrow(e.df))
  for(i in 1:(nrow(e.df)-1)){
    e1x[i] <- e.df[i+1,1]
    e1y[i] <- e.df[i+1,2]
  }
  
  for(i in 2:nrow(e.df)){
    e2x[i] <- e.df[i-1,1]
    e2y[i] <- e.df[i-1,2]
  }
  
  e1x[nrow(e.df)] <- e.df[1,1]
  e1y[nrow(e.df)] <- e.df[1,2]
  
  e2x[1] <- e.df[nrow(e.df)-1,1]
  e2y[1] <- e.df[nrow(e.df)-1,2]
  
  ve.df <- cbind(e.df,e1x,e1y,e2x,e2y)[-nrow(e.df),]
  ve.df
}

#edge as list to string
edge.list.to.string <- function(elist){
  a1 <- as.numeric(c(elist[[1]],elist[[2]])) %>% as.character() %>% paste(collapse="")
  a2 <- as.numeric(c(elist[[2]],elist[[1]])) %>% as.character() %>% paste(collapse="")
  
  c(a1,a2)
}

#edge as numeric to a string
edge.numeric.to.string <- function(n){
  a1 <- c(n[1:2],n[3:4]) %>% as.character() %>% paste(collapse="")
  a2 <- c(n[3:4],n[1:2]) %>% as.character() %>% paste(collapse="")
  
  c(a1,a2)
}

#given a point and an edge returns the lookup value for the edge in the current binary tree
#edges are indexed by distance from the point
#this can cause issues when two edges are exactly the same distance away
#however, this should never happen in the app
#since coords are plotted by clicking
keyval <- function(pt,edge){
  ##print(c("keyval pt: ",pt))
  ##print(c("keyval edge: ", edge))
  v <- round(distance(pt,midpt(edge[[1]],edge[[2]])),5)
  ##print(c("keyval distance: ",v))
  v
}

#AS described in de berg et. al.
#p is a point not interior to the polygons
#   typically this will be a vertex of a polygon
#ps is a sf MULTIPOLYGON
#returns all visible vertices within the list of polygons
#   visible from point p
visibleVertices <- function(p,ps){
  coords <- data.frame(st_coordinates(ps)) #dataframe polys differentiated on L2
  ppts <- split(coords,coords$L2) #split into list of pts
  withloop <- lapply(ppts, function(mat) as.matrix(mat[,1:2])) #first two columns of ppts in list of matrices
  #polyptlist <- lapply(ppts, function(mat) as.matrix(mat[-1*nrow(mat),1:2])) #same as above without last point (loop pt)
  polyptlist <- lapply(withloop,edges.from.vert)
  reppts <- do.call(rbind,polyptlist) # matrix of all pts without loop pts
  
  polygons <- lapply(withloop, function(mat) list(mat[,1:2]) %>% st_polygon()) #separate sf polygons in a list
  v.per.poly <- as.numeric(lapply(polyptlist,nrow)) # num vertices in each polygon
  
  #print(p)
  orderppts <- order.by.angle(p,data.frame(reppts)) #all pts ordered by angle
  int.edges <- list() 
  ##print("determining intersection")
  for(poly in withloop){#determines which edges intersect the horizontal line from p, adds them to int.edges
    #print(poly)
    ##print("INTERSECTION TIME")
    ie <- (intersect(p,poly))
    ##print(ie)
    int.edges <- append(int.edges,ie)
  }
  ##print("test1")
  ordered <- int.order(p,int.edges) #sorts int.edges by clockwise angle from horizontal line from p
  ###print(ordered)
  #need to change the keys to a vector of proper length
  keyhash <- new.env(hash=T)
  #ctr stores current edge num to be inserted next
  edge.ctr <- 1
  edges.tree <- bst()
  for(edge in ordered){
    estring <- edge.list.to.string(edge)
    ###print(edge)
    #print(estring)
    
    keyhash[[estring[[1]]]] <- edge.ctr
    keyhash[[estring[[2]]]] <- edge.ctr
    edges.tree <- insert(edges.tree,keyval(p,edge),edge)
    edge.ctr <- edge.ctr+1
  }
  

  v.verts <- list()
  prevpt <- NULL
  #print(orderppts)
  for(orow in 1:nrow(orderppts)){
    #print(c("prevpt: ",prevpt))
    pt <- as.numeric(orderppts[orow,1:2])
    if(p[1]==pt[1] && p[2]==pt[2]){
      #print("going to next")
      v.verts <- append(v.verts,list(pt))
      prevpt <- pt
      next
    }
    #print("in edge loop")
    ##print(pt)
    pt.sf <- st_point(pt)
    
    npoly <- NULL
    for(j in 1:length(polyptlist)){
      ##print(c("pt in here: ",pt))
      if(tuplein.mat(pt,polyptlist[[j]][,1:2])){
        ##print("tuple in poly")
        currpoly <- polygons[[j]]
        break
      }
    }
    
    if(visible(p,pt,prevpt,currpoly,edges.tree,v.verts,polygons,withloop)){
      ##print(c("is visible ",pt))
      v.verts <- append(v.verts,list(pt))
    }
    prevpt <- pt
    ##print(orderppts)
    #print(pt)
    ptdata <- matrix(orderppts[orow,],nrow=1)
    #print(c("ptdata: ",as.numeric(ptdata)))
    e1 <- as.numeric(ptdata[,3:4])
    e2 <- as.numeric(ptdata[,5:6])
    #sort them by dist from p
    d1 <- distance(p,midpt(pt,e1))
    d2 <- distance(p,midpt(pt,e2))
    #print(d1)
    #print(d2)
    e1.names <- edge.numeric.to.string(as.numeric(ptdata[,c(1:4)]))
    e2.names <- edge.numeric.to.string(as.numeric(ptdata[,c(1:2,5:6)]))

    if(d2<d1){
      temp <- e1
      e1 <- e2
      e2 <- temp
      e1.names <- edge.numeric.to.string(as.numeric(ptdata[,c(1:2,5:6)]))
      e2.names <- edge.numeric.to.string(as.numeric(ptdata[,1:4]))
    }
    #e1 is the edge closer to p
    
    e1.dir <- edge.direction(list(p,pt),list(pt,e1))
    e2.dir <- edge.direction(list(p,pt),list(pt,e2))
    #print(c("e1",e1,e1.dir))
    #print(c("e2",e2,e2.dir))
    
    ##print("to here")
    #insert e1
    if(!e1.dir){
      #print("insert e1")
      if(is.null(keyhash[[e1.names[[1]]]]) || !contains(edges.tree,keyval(p,list(pt,e1)))){
        keyhash[[e1.names[1]]] <- edge.ctr
        keyhash[[e1.names[2]]] <- edge.ctr
        #print(e1.names[[1]])
        #print(edge.ctr)
      
        edges.tree <- insert(edges.tree,keyval(p,list(pt,e1)),list(pt,e1))
        edge.ctr <- edge.ctr + 1
      }
    } else{
      #print("remove1")
      #remove e1 from edge.tree if it's in it
      #doesn't matter which hash lookup name we use, both are in the table
      if(contains(edges.tree,keyval(p,list(pt,e1)))){
        edges.tree <- (delete(edges.tree,keyval(p,list(pt,e1))))
      }

    }
    
    #same thing for e2
    if(!e2.dir){
      #print("insert e2")
      if(is.null(keyhash[[e2.names[[1]]]]) || !contains(edges.tree,keyval(p,list(pt,e2)))){
        keyhash[[e2.names[1]]] <- edge.ctr
        keyhash[[e2.names[2]]] <- edge.ctr
        #print(e2.names[[1]])
        #print(edge.ctr)
        
        edges.tree <- insert(edges.tree,keyval(p,list(pt,e2)),list(pt,e2))
        edge.ctr <- edge.ctr + 1
      }

    } else{
      #print("remove3")
      if(contains(edges.tree,keyval(p,list(pt,e2)))){
        edges.tree <- (delete(edges.tree,keyval(p,list(pt,e2))))
      }
    }
    ##print("test3")
    #print("")
  }
  
  v.verts
}

#finds the midpoint of two points
#(probably ends of a line segment)
midpt <- function(p1,p2){
  c((p1[1]+p2[1])/2,(p1[2]+p2[2])/2)
}

###OLD WAY OF FINDING WHICH EDGES TO ADD AND REMOVE FROM EDGES.TREE
#insert edges incident to pt on the clockwise side of p-pt into edges.tree
#remove edges on the counterclockwise side of the same segment.
#for(e in edges){
#    en <- st_linestring(matrix(c(e[[1]],e[[2]]),ncol=2,byrow=T))#is this edge on the 
#    
#    if(st_intersects(pt.sf,en,sparse=F)){
#      if(!edge.direction(list(pt,e[[1]]),e)){#false = clockwise
#        insert(edges.tree,size(edges.tree)+1,e)
#      } else{
#        #how to know the key of the edge to remove?
#        for(key in keys(edge.tree)){
#          if(edge.tree[key]==e){
#            delete(edge.tree,key)
#            break
#          }
#        }
#      }
#        
#    }
#}


#given a set of edges intedges that intersect the horizontal line segment from pt
#order them with those intersecting closer first
int.order <- function(pt,intedges,ptx=100){
  int.pts <- numeric(length(intedges))
  #e1 <- st_linestring(matrix(c(pt,c(ptx,pt[2])),ncol=2,byrow=T))
  ctr <- 1
  for(edge in intedges){
    #print(edge)
    
    #e2 <- st_linestring(matrix(c(as.numeric(edge[[1]]),as.numeric(edge[[2]])),ncol=2,byrow=T))
    #inter <- st_coordinates(st_intersection(e1,e2))
    ###print(inter)
    int.pts[ctr] <- distance(midpt(edge[[1]],edge[[2]]),pt)
    #print(int.pts[ctr])
    ctr <- ctr+1
  }
  ###print(int.pts)
  intedges[order(int.pts)]
}


#deprecated by the use of SF (created before I started using that)
#is c on the line segment between a and b
#returns true or false
pt.on.line <- function(a,b,c){
  if(a[1]==b[1] && a[2]==b[2]){
    ##print("A and B are the same point, this function should not be used in this manner.")
    return(FALSE)
  }
  
  if(a[1]!=b[1]){
    m=(b[2]-a[2])/(b[1]-a[1])
    bool <- (m*(c[1]-a[1])-(c[2]-a[2])==0)
  }
  else{
    bool <- (c[1]==a[1])
  }
  
  if(!bool){
    return(FALSE)
  }
  
  #boolean logic - 
  #if c is on the line determined by a and b
  #is it actually between the two points?
  
  #vertical line
  if(a[1]==b[1]){
    if(a[2]<b[2]){
      if(c[2]>a[2] && c[2]<b[2]){
        ###print("vert1")
        return(TRUE)
      }
    }else{
      if(c[2]<a[2] && c[2]>b[2]){
        ###print("vert2")
        return(TRUE)
      }
    }
  }
  #horizontal line
  else if(a[2]==b[2]){
    if(a[1]<b[1]){
      if(c[1]>a[1] && c[1]<b[1]){
        ###print("horiz1")
        return(TRUE)
      }
    }else{
      if(c[1]<a[1] && c[1]>b[1]){
        ###print("horiz2")
        return(TRUE)
      }
    }
  }
  else if(a[1]<b[1]){
    #Q1
    if(a[2]<b[2]){
      if(c[1]>a[1] && c[1]<b[1] && c[2]>a[2] && c[2]<b[2]){
        ###print("q1")
        return(TRUE)
      }
      #Q4
    }else{
      if(c[1]>a[1] && c[1]<b[1] && c[2]<a[2] && c[2]>b[2]){
        ###print("q4")
        return(TRUE)
      }
    }
    #Q2
  } else {
    if(a[2]<b[2]){
      if(c[1]<a[1] && c[1]>b[1] && c[2]>a[2] && c[2]<b[2]){
        ###print("q2")
        return(TRUE)
      }
      #Q3
    }else{
      if(c[1]<a[1] && c[1]>b[1] && c[2]<a[2] && c[2]>b[2]){
        ###print("q3")
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

#takes a multipolygon and returns the set of all visibility edges 
#Iterates on the vertices of the set
#passes relevant information to visibleVertices
vis.graph <- function(multip){
  coords1 <- data.frame(st_coordinates(multip)) 
  ppts1 <- split(coords1,coords1$L2) #split into list of pts
  withloop1 <- lapply(ppts1, function(mat) as.matrix(mat[,1:2])) #first two columns of ppts in list of matrices
  polyptlist1 <- lapply(withloop1,edges.from.vert)
  reppts1 <- do.call(rbind,polyptlist1)
  verts1 <- reppts1[,1:2] %>% as.matrix()
  
  vv2 <- list()
  for(i in 1:nrow(verts1)){
    ##print(verts[i,])
    vv11 <- visibleVertices(verts1[i,],multip)
    #print(vv)
    vv2 <- append(vv2, lapply(vv11, function(x) st_linestring(matrix(c(verts1[i,],x[1],x[2]),ncol=2,byrow=T))) )
  }
  ##print(vv1)
  vv2 <- vv2 %>% st_multilinestring()
  
  #gg1 <- ggplot()+
  ##  geom_sf(data=multip,aes(fill=T))+
  #  geom_sf(data=vv1,color="blue")+
  #  #lims(x = c(0, 100), y = c(0, 100)) +
  #  theme(legend.position = "bottom")+
  #  guides(fill=F)
  
  #gg1
  vv2
}