#' @title Plot Rank Intervals
#' @description Derive indirect relations like distances for a given network. 
#' @param P A partial ranking as matrix for which rank intervals should be determined.
#' @param names string. potential names of nodes used in the plot (optional)
#' @param cent.df a data frame containing centrality scores of indices (optional). See Details.

#' @details The 
#' 
#' @return a ggplot object.
#' @author David Schoch
#' @examples
#' require(igraph)
#' require(ggplot2)
#' g <- graph.empty(n=11,directed = FALSE)
#' g <- add_edges(g,c(1,11,2,4,3,5,3,11,4,8,5,9,5,11,6,7,6,8,
#'                    6,10,6,11,7,9,7,10,7,11,8,9,8,10,9,10))
#' P <- neighborhood_inclusion(g)
#' plot_rank_intervals(P)
#' 
#' #adding index based rankings
#' cent_scores <- data.frame(
#'   degree=degree(g),
#'   betweenness=round(betweenness(g),4),
#'   closeness=round(closeness(g),4),
#'   eigenvector=round(eigen_centrality(g)$vector,4))
#' plot_rank_intervals(P,cent.df=cent_scores)
#' @export

plot_rank_intervals <- function(P,names,cent.df){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  n <- nrow(P)
  if(missing(names)){
    names <- 1:nrow(P)
  }
  if(length(names)!=n){
    stop("names must have the same length as the number of nodes")
  }
  intervals <- netrankr::rank_intervals(P)
  intervals$node <- as.character(names)
  df <- 
    data.frame(interval=rep(c("max_rank","min_rank","mid_point"),each=n),
               node=rep(names,3),
               rank=c(intervals$max_rank,intervals$min_rank,intervals$mid_point),
               order=rep(rank(intervals$mid_point,ties.method = "first"),3),
               mid_point=rep(intervals$mid_point,3)
    )
  if(missing(cent.df)){
    ggplot2::ggplot(df,ggplot2::aes_(x=~stats::reorder(node,order),y=~rank,group=~node))+
      ggplot2::geom_line(col="#8F8F8F")+
      ggplot2::geom_point(col="#8F8F8F",
                          shape=ifelse(df$interval=="min_rank",24,
                                       ifelse(df$interval=="max_rank",25,3)),
                          fill="#8F8F8F")+
      ggplot2::scale_y_continuous(breaks=pretty(1:nrow(P)))+
      ggplot2::theme_bw()+
      ggplot2::theme(text = ggplot2::element_text(family="Times",size=14),
                     axis.text.x = ggplot2::element_text(angle=ifelse(any(nchar(names)>2),45,0),
                                                hjust=1),
                     legend.position="bottom",
                     panel.border = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank())+
      ggplot2::labs(x="",y="Rank",shape="",colour="")
  } else{
    no.indices <- ncol(cent.df)
    cent.df.long <- data.frame(node=rep(names,no.indices),
                               mid_point=rep(intervals$mid_point,no.indices),
                               index=rep(names(cent.df),each=n),
                               rank=c(apply(cent.df,2,rank)))
    ggplot2::ggplot(df,ggplot2::aes_(x=~stats::reorder(node,mid_point),y=~rank,group=~node))+
      ggplot2::geom_line(col="#8F8F8F")+
      ggplot2::geom_point(col="#8F8F8F",
                          shape=ifelse(df$interval=="min_rank",24,
                                       ifelse(df$interval=="max_rank",25,3)),
                          fill="#8F8F8F")+
      ggplot2::geom_jitter(data=cent.df.long,
                           ggplot2::aes_(x=~stats::reorder(node,mid_point),y=~rank,shape=~index),
                           size=2,width=0.1,height = 0)+
      ggplot2::scale_shape_manual(values=3:(3+no.indices))+
      ggplot2::scale_y_continuous(breaks=pretty(1:nrow(P)))+
      ggplot2::theme_bw()+
      ggplot2::theme(text = ggplot2::element_text(family="Times",size=14),
                     axis.text.x = ggplot2::element_text(angle=ifelse(any(nchar(names)>2),45,0),
                                                hjust=1),
                     legend.position="bottom",
                     panel.border = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank())+
      ggplot2::labs(x="",y="Rank",shape="",colour="")
  }
}
