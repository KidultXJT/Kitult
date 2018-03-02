require(pheatmap)
require(methods)

brwCol <- function(){
  return("HEATMAP Color, Please Try This: Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral")
}

phm <- function(
  matrix,
  filename=NULL,           # = "heatmap.pdf",
  brwCol = "RdYlBu",  # select From RColorBrewer::Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd 
                                                # Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd BrBG PiYG PRGn PuOr
                                                # RdBu RdGy RdYlBu RdYlGn Spectral
  border_color = "white", # Kidult Style
  width=8,
  height=12,
  dismethod = "euclidean",
  clustermethod = "complete",
  scale = "none"
){
  # Package 
  require(pheatmap)
  
  if(is.null(filename)){
    pheatmap(matrix,
             #filename=filename,
             silent=FALSE,
             scale=scale,
             color = colorRampPalette(rev(brewer.pal(n = 7, name = brwCol)))(100),
             border_color = border_color,
             width=width, 
             height=height,
             clustering_distance_rows = dismethod,
             clustering_distance_rols = dismethod,
             clustering_method = clustermethod)
  }else{
    pheatmap(matrix,
             filename=filename,
             silent=FALSE,
             scale=scale,
             color = colorRampPalette(rev(brewer.pal(n = 7, name = brwCol)))(100),
             border_color = border_color,
             width=width, 
             height=height,
             clustering_distance_rows = dismethod,
             clustering_distance_rols = dismethod,
             clustering_method = clustermethod
    )
  }
  
}
