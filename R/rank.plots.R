rank_plots=function(df,method="",legend.pos="right"){
  #' @title Visualize expected ranks and rank probabilities
  #' @description  Uses the output of
  #'  Works best with small networks
  #' @param df a dataframe as returned from rank.analysis
  #' @param method string, see details
  #' @param legend.pos string, position of legend. "none" to turn of legend
  #' @return ggplot object
  #' @details TODO
  #' @examples
  #' ###TODO
  #' @export
  if(method=="expected"){
    data.frame(id=df$names,e=df$expected.rank,sd=df$rank.spread) %>%
      ggplot2::ggplot(aes(x=reorder(id,e),y=e))+
      ggplot2::geom_col()+
      ggplot2::geom_hline(yintercept = pretty(1:length(df$names)),col="white")+
      ggplot2::scale_y_continuous(breaks=pretty(1:length(df$names)))+
      ggplot2::geom_errorbar(aes(ymin=e-sd,ymax=e+sd))+
      ggthemes::theme_tufte(ticks=F)+
      #ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45,hjust=1))+
      ggplot2::labs(x="",y="")+coord_flip()
  }
  else if(method=="mutual"){
    data.frame(idx=rep(df$names,length(df$names)),idy=rep(df$names,each=length(df$names)),p=c(df$mutual.rank.prob)) %>%
      ggplot2::ggplot(aes(y=idx,x=idy,fill=p))+
      ggplot2::geom_tile()+
      ggplot2::scale_fill_gradient(low="white",high="black")+
      ggthemes::theme_tufte(ticks=F)+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45,hjust=1),
                     legend.position=legend.pos)
  }
  else if(method=="prob"){
    data.frame(idx=rep(df$names,ncol(df$rank.prob)),rank=rep(1:ncol(df$rank.prob),each=length(df$names)),p=c(df$rank.prob)) %>%
      ggplot2::ggplot(aes(y=idx,x=rank,fill=p))+
      ggplot2::geom_tile()+
      ggplot2::scale_fill_gradient(low="white",high="black")+
      ggthemes::theme_tufte(ticks=F)+
      ggplot2::theme(legend.position=legend.pos)
  }
  else{
    stop("method must be one of expected, mutual or prob")
  }

}
