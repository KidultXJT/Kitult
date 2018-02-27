#ggAdcanceHeatmap  <- function(
#  Col="RdYlGn",     # When Use One Color
#  size=.3,
#  Trans=F,  # When make a Basic Graph
#  df=NULL,  # When Use This, Try To make a Basic Graph; Else, To make a Point Graph LAYER !!! 
#  num=F,
#  limit,    # c()
#  breaks,   # c()
#  x=1,
#  y=3,
#  color=2,
#  label=2
#){
#  # Description:
#  # A point Graph for Basic Graph or A Layer
#  
#  # Package:
#  require(ggplot2)
#  
#  if(num){heatmap = geom_tile(color="white",size=size)}else{heatmap = geom_tile(color="white",size=size) + geom_text(color="white")}
#  if(!is.null(limit)){if(!is.null(breaks)){
#    heatmap = scale_fill_distiller(palette = Col,guide = guide_colorbar(nbin=100,draw.ulim = FALSE,draw.llim = FALSE))
#  }else{heatmap = scale_fill_distiller(palette = Col,guide = guide_colorbar(nbin=100,draw.ulim = FALSE,draw.llim = FALSE),breaks=breaks)}
#  }else{if(!is.null(breaks)){
#    heatmap = scale_fill_distiller(palette = Col,guide = guide_colorbar(nbin=100,draw.ulim = FALSE,draw.llim = FALSE),limit=limit)
#    }else{heatmap = scale_fill_distiller(palette = Col,guide = guide_colorbar(nbin=100,draw.ulim = FALSE,draw.llim = FALSE),limit=limit,breaks=breaks)}}
#  
#  if(!is.null(df)){
#    
#    df$x = factor(df$x)
#    heatmap = ggplot(data = df,
#                     aes(x=df[,x],
#                         y=df[,y],
#                         fill = df[,color],
#                         label= df[,label])) + heatmap
#    
#    if(Trans){
#      heatmap =  heatmap + coord_flip()
#    }
#  }
#  
#  return(heatmap)
#}