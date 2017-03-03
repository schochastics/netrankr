rank_analysis=function(P,method="bfs",names="",only.results=T,verbose=F){
  #' @title Rank Analysis of networks
  #' @description  Performes a complete Rank analysis of a network.
  #' Calculates Expected Rankings, (Relative) Rank Probabilities and number of possible rankings.
  #' Works best with small networks.
  #'
  #' @param P matrix representing a partial ranking
  #' @param method string indicating method to get all possible rankings. (Default should not be changed)
  #' @param names optional argument for names if P does not have row/column names
  #' @param only.results logical. wether only results (default) or additionally the ideal tree and lattice should be returned
  #' @param verbose logical. should diagnostics be printed. Defaults to F
  #' @details TODO
  #' @return 
  #' \item{n.ext}{Number of possible centrality rankings}
  #' \item{mse}{Array indicating equivalent nodes}
  #' \item{rank.prob}{Matrix containing rank probabilities: \code{rank.prob[i,k]} is the probability that i has rank k}
  #' \item{mutual.rank}{Matrix containing relative rank probabilities: \code{mutual.rank[i,j]} is the probability that i is ranked lower than j}
  #' \item{expected.rank}{Expected ranks of nodes in any centrality ranking}
  #' \item{rank.spread}{Variance of the ranking probabilities}
  #' \item{lattice}{igraph object. The lattice of ideals (if only.results=F)}
  #' \item{tree}{igraph object. The tree of ideals (if only.results=F)}
  #' @references \insertRef{ddd-elirp-06}{netrankr}
  #' @seealso [approx_rank_mutual], [approx_rank_expected]
  #' @examples
  #' ###TODO
  #' @export
  if(is.null(rownames(P)) & length(names)!=nrow(P)){
    rownames(P)=1:nrow(P)
  }
  else if(is.null(rownames(P)) & length(names)==nrow(P)){
    rownames(P)=names
  }
  n.full <- nrow(P)
  P.full <- P
  # Equivalence classes ------------------------------------------------
  MSE=which((P+t(P))==2,arr.ind=T)
  if(length(MSE)>=1){
    MSE<-t(apply(MSE,1,sort))
    MSE<-MSE[!duplicated(MSE),]
    g<-igraph::graph.empty()
    g<-igraph::add.vertices(g,nrow(P))
    g<-igraph::add.edges(g,c(t(MSE)))
    g<-igraph::as.undirected(g)
    MSE<-igraph::clusters(g)$membership
    equi<-which(duplicated(MSE))
    P<-P[-equi,-equi]
  }
  else{
    MSE <- 1:nrow(P)
  }
  names <- rownames(P)
  #number of Elements
  m <- length(names)
  # LATTICEOFIDEALS ----------------------------------------------------
  n<-nrow(P)
  topo.order<-P %>% 
    igraph::graph_from_adjacency_matrix("directed") %>% 
    igraph::topological.sort() %>% 
    as.vector()
  
  tree<-.tree_of_ideals(P,topo.order)
  if(verbose==T){
    print("tree of ideals built")
  }
  g.ideals<-.build_lattice(tree)
  if(verbose==T){
    print("lattice of ideals built")
  }
  ideals<-.ideal_listing(P,topo.order)
  ideals.integer<-sapply(ideals,function(x){strsplit(x," ") %>% unlist %>% as.integer},USE.NAMES = F)
  ideals.integer<-lapply(ideals.integer,function(x){topo.order[x]})
  if(verbose==T){
    print("ideals listed")
  }
  V(g.ideals)$name=ideals
  V(tree$graph)$name=ideals
  el=get.edgelist(g.ideals,names = F)
  E(g.ideals)$label=apply(el,1,function(x){a<-ideals.integer[[x[1]]];b<-ideals.integer[[x[2]]];union(setdiff(a,b),setdiff(b,a))})
  
  if(verbose==T){
    print("lattice determined")
    print(paste("No of ideals:",igraph::vcount(g.ideals)))
  }
  E(g.ideals)$color="grey"
  #number of ideals
  n=igraph::vcount(g.ideals)
  
  #TOPDOWN BFS ---------------------------------------------------------
  #find full ideal
  ideal.sink<-which.max(nchar(V(g.ideals)$name))
  
  visited<<-rep(0,n)
  lef.g<<-rep(0,n)
  e<-.assign_top_down(1,g.ideals,ideal.sink)
  lef<-lef.g
  rm(list=c("lef.g","visited"),pos = ".GlobalEnv")
  if(verbose==T){
    print("Top Down BFS finished")
    print(paste("Number of possible centrality rankings:",e))
  }
  #BottomUp---------------------------------------------------------
  visited<<-rep(0,n)
  lei<-.assign_bottom_up(g.ideals,m,method)
  rm(visited,pos = ".GlobalEnv")
  if(verbose==T){
    print("Bottom Up BFS finished")
  }
  #RankProb---------------------------------------------------------
  rp.g<<-matrix(0,m,m)
  visited<<-rep(0,n)
  .compute_rank_prob(g.ideals,1,1,lei,lef,e,ideal.sink)
  rp=rp.g
  rm(list=c("visited","rp.g"),pos = ".GlobalEnv")
  if(verbose==T){
    print("Rank Probabilites calculated")
  }
  #MutualRankProb----------------------------------------------------
  visited<<-rep(0,n)
  visitedElement<<-rep(0,n)
  mrp.g<<-matrix(0,m,m)
  .compute_mutual_rank_prob(g.ideals,1,1,lei,lef,e,m,ideal.sink)
  mrp=mrp.g
  rm(list=c("visited","visitedElement","mrp.g"),pos = ".GlobalEnv")
  if(verbose==T){
    print("Mutual Rank Probabilities calculated")
  }
  ###############################END
  expected=rp%*%1:m
  rank.spread=rowSums((matrix(rep(1:m,each=m),m,m)-c(expected))^2*rp)
  gc()
  expected=c(expected)
  ###############################
  # Insert equivalent nodes again
  rp.full=matrix(0,n.full,ncol(rp))
  mrp.full=matrix(0,n.full,n.full)
  expected.full=c(0,n.full)
  rank.spread.full=rep(0,n.full)
  for(i in sort(unique(MSE))){
    idx <- which(MSE==i)
    if(length(idx)>1){
      group.head <- i
      rp.full[idx,]  <- do.call(rbind, replicate(length(idx), rp[group.head,], simplify=FALSE))
      mrp.full[idx,] <- do.call(rbind, replicate(length(idx), mrp[group.head,MSE], simplify=FALSE))
      expected.full[idx] <- expected[group.head]
      rank.spread.full[idx] <- rank.spread[group.head]
    }
    else if(length(idx)==1){
      rp.full[idx,] <- rp[i,]
      
      mrp.full[idx,] <- mrp[i,MSE]
      expected.full[idx] <- expected[i]
      rank.spread.full[idx] <- rank.spread[i]
    }
  }
  ###############################
  if(only.results){
    return(list(lin.ext=e,
                names=rownames(P.full),
                mse=MSE,
                rank.prob=rp.full,
                mutual.rank.prob=t(mrp.full),
                expected.rank=expected.full,
                rank.spread=sqrt(rank.spread.full)))
  } else{
    return(list(lin.ext=e,
                names=rownames(P.full),
                mse=MSE,
                rank.prob=rp.full,
                mutual.rank.prob=t(mrp.full),
                expected.rank=expected.full,
                rank.spread=sqrt(rank.spread.full),
                lattice=g.ideals,
                tree=tree$graph))
  }
  ###############################
}
#HELPER FUNCTIONS----------------------------------------------------------------
.tree_of_ideals=function(P,topo.order){
  P<-P[topo.order,topo.order]
  n<-nrow(P)
  max.id=n+1
  topo.sort<-topo.order
  
  topo.order<-1:max.id
  g<-igraph::make_empty_graph() %>% 
    igraph::add_vertices(n+1,attr=list(name=topo.order)) %>% 
    igraph::add_edges(c(topo.order[1],rep(topo.order[2:n],each=2),n+1))
  for(i in 3:(n+1)){
    v=topo.order[i]
    v.prec=topo.order[i-1]
    id.v=which(igraph::V(g)$name==v)
    pd.check=which(igraph::V(g)$name==v.prec)[1]
    children<-neighborhood(g,1,pd.check,mode="in",mindist = 1) %>% unlist()
    focal=rep(id.v,length(children))
    while(length(children)>=1){
      child.name=igraph::V(g)$name[children[1]]
      child.id=children[1]
      target=focal[1]
      children=children[-1]
      focal=focal[-1]
      if(P[child.name,pd.check]!=1){
        max.id=max.id+1
        g<-g %>% igraph::add_vertices(1,attr=list(name=child.name)) %>% 
          igraph::add_edges(c(max.id,target))
        tmp<-igraph::neighborhood(g,1,child.id,mode="in",mindist = 1) %>% unlist
        if(length(tmp)>0){
          children= c(tmp,children)
          focal=c(rep(max.id,length(tmp)),focal)
        }
      }
    }
  }
  return(list(graph=g,topo.sort=topo.sort))
  
}

#####################################################################################
.build_lattice=function(tree){
  topo=tree$topo.sort
  tree=tree$graph
  el<-get.edgelist(tree,names=F)
  n=max(igraph::V(tree)$name)-1
  pred=vector("list",igraph::vcount(tree))
  sink=which(igraph::V(tree)$name==(n+1))
  pred[[sink]]=igraph::neighborhood(tree,1,sink,"in",1) %>% unlist
  Ek=map(1:n,function(x){which(igraph::V(tree)$name==x)})
  
  for(k in n:1){
    for(v in Ek[[k]]){
      parent.v=igraph::neighborhood(tree,1,v,"out",1) %>% unlist
      children.v=igraph::neighborhood(tree,1,v,"in",1) %>% unlist
      v.prime<-pred[[parent.v]][1]
      j=1
      while(v.prime!=v){
        children.v.prime=igraph::neighborhood(tree,1,v.prime,"in",1) %>% unlist
        v.star=children.v.prime[1]
        pred[[v]]=c(pred[[v]],v.star)
        j=j+1
        v.prime<-pred[[parent.v]][j]
      }
      pred[[v]]=c(pred[[v]],children.v)
    }
    if(length(el)>2){ #this is just a workaround for Rs stupidity to not recognize 1 row matrices
      del.edges<-which(el[,1]%in%Ek[[k]])
      tree<-tree %>% igraph::delete_edges(del.edges)
      el<-el[-del.edges,]
    } else{
      del.edges<-1
      tree<-tree %>% igraph::delete_edges(del.edges)
      el<-el[-c(1,2)]
    }
  }
  lattice=graph_from_adj_list(pred,mode="in")
  return(lattice)
}
#####################################################################################
.ideal_listing=function(P,topo.order){
  n=nrow(P)
  P<-P[topo.order,topo.order]
  P.work<-P
  I=1:n
  i<-1
  S<-list()
  S[[1]]=sort(which(rowSums(P)==0),decreasing=T)
  Code=numeric()
  ideals=I %>% sort %>% paste(collapse=" ")
  while(i>0){
    if(length(S[[i]]!=0)){
      x<-S[[i]][1]
      # Flag<-"BF"
      Code=c(Code,x)
      I=setdiff(I,x)
      S[[i]]<-setdiff(S[[i]],x)
      new.ideal<-I %>% sort %>% paste(collapse=" ")
      # print(new.ideal)
      if(!new.ideal%in%ideals){
        ideals=c(ideals,new.ideal)
      }
      P.tmp=P.work
      P.tmp[,x]=0
      S2=which(rowSums(P.work)!=0 & rowSums(P.tmp)==0)
      P.work=P.tmp
      S[[i+1]]=sort(c(S[[i]],S2),decreasing=T) #!!!!!!!!!!!!!
      i=i+1
    } 
    else{
      # if(Flag=="BT"){
      # if(!is.unsorted(I)){
      #   print(I)
      # }
      new.ideal<-I %>% sort %>% paste(collapse=" ")
      # print(new.ideal)
      if(!new.ideal%in%ideals){
        ideals=c(ideals,new.ideal)
      }
      # }
      x=Code[length(Code)]
      Code=Code[-length(Code)]
      I=c(I,x)
      # Flag="BT"
      P.work[,x]=P[,x]
      i=i-1
    }
  }
  idx<-which(ideals=="")
  ideals=ideals[c(idx:1,(idx+1):length(ideals))]
  return(ideals)
}

##################################################################################################

.assign_top_down=function(v,g,top){
  visited[v]<<-1
  e=0
  nv=igraph::neighborhood(g,1,v,"out",1)[[1]]
  for(w in nv){
    if(w==top){
      e=1
      lef.g[w]<<-e
    }
    else{
      if(visited[w]==0){
        e=e+.assign_top_down(w,g,top)
      }
      else{
        e=e+lef.g[w]
      }
    }
  }
  lef.g[v]<<-e
  return(e)
}
########################################################################################
.assign_bottom_up=function(g,m,method){
  if(method=="matrix"){
    A=igraph::get.adjacency(g,sparse=F)
    lei=A[1,]
    for(i in 2:m){
      lei=lei+(expm::`%^%`(A,i))[1,]
    }
    lei[1]=1
  }
  else if(method=="bfs"){
    n=igraph::vcount(g)
    lei=rep(0,n)
    lei[1]=1
    n1=igraph::neighborhood(g,1,1,"out",1)[[1]]
    L=list()
    inList=rep(0,n)
    for(w in n1){
      L=.lappend(L,w)
      lei[w]=1
      inList[w]=1
    }
    while(length(L)>=1){
      v=L[[1]]
      L[[1]]<-NULL
      visited[v]<<-1
      nv=igraph::neighborhood(g,1,v,"out",1)[[1]]
      for(w in nv){
        lei[w]=lei[w]+lei[v]
        
        if(visited[w]==0 & inList[w]==0){
          L=.lappend(L,w)
          inList[w]=1
          
        }
      }
    }
  }
  return(lei)
}
####################################################################################################
.compute_rank_prob=function(g,v,h,lei,lef,e,top){
  visited[v]<<-1
  nv=igraph::neighborhood(g,1,v,"out",1)[[1]]
  for(w in nv){
    idx=igraph::get.edge.ids(g,c(v,w))
    x=igraph::get.edge.attribute(g,"label",idx)
    rp.g[x,h]<<-rp.g[x,h]+(lei[v]*lef[w])/e
    if(w!=top & visited[w]==0){
      .compute_rank_prob(g,w,h+1,lei,lef,e,top)
    }
  }
}
####################################################################################################
.compute_mutual_rank_prob=function(g,v,h,lei,lef,e,m,top){
  visited[v]<<-1
  nv=neighborhood(g,1,v,"out",1)[[1]]
  for(w in nv){
    for(y in 1:m){
      if(visitedElement[y]==1){
        idx=igraph::get.edge.ids(g,c(v,w))
        x=igraph::get.edge.attribute(g,"label",idx)
        mrp.g[x,y]<<-mrp.g[x,y]+(lei[v]*lef[w])/e
      }
    }
    if(w!=top & visited[w]==0){
      idx=igraph::get.edge.ids(g,c(v,w))
      x=igraph::get.edge.attribute(g,"label",idx)
      visitedElement[x]<<-1
      .compute_mutual_rank_prob(g,w,h+1,lei,lef,e,m,top)
      visitedElement[x]<<-0
    }
  }
}
####################################################################################################
.lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}
################################################################################
