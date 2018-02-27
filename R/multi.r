ggMultiplot <- function(
  ..., 
  plotlist=NULL, 
  file, 
  cols=1, 
  layout=NULL
  ){
  
  # Package
  require(grid)
  
  # Explain list(...)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  
  if(is.null(layout)){
    layout <- matrix(seq(1, cols*ceiling(numPlots/cols)),
                     ncol = cols,nrow = ceiling(numPlots/cols))
  }
  if(numPlots==1) {
    print(plots[[1]])
  }else{
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plotMulti <- function(
  ...,
  plotlist=NULL,
  nrow = 3,
  ncol = 2,
  byrow = TRUE
){
  # Explain list(...)
  plots <- c(list(...), plotlist)
  
  opar <- par(no.readonly=TRUE)
  if(byrow){
    par(mfrow=c(nrow,ncol))
  }else{
    par(mfcol=c(nrow,ncol))
  }
  
  for(p in plots){
    plot(p)
  }
  par(opar)
  return(opar)
}

splitCol <- function(
  split, # Add a Formula as :: "A ~ B" or "~ B"
  ncol=NULL,
  nrow=NULL,
  scales="fixed"
){
  
  split = facet_wrap(split, ncol=ncol, nrow=nrow, scales=scales)
  
  return(split)
}
