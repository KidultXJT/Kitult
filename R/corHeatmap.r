## --------------------------- ##
##   Title: cor::Heatmap       ##
##  Author: Kidult             ##
##    Date: 2017/9/27          ##
## --------------------------- ##
corHeatmap <- function(
  X, # Table X::sp, with more var
  Y, # Table Y::env
  outDir = getwd(),   # outDir
  method = "pearson", # Correlation Method::pearson or kandall or spearman. Can Be a list::"pearson,kendall,spearman"
  brwCol = "RdYlBu",  # select From RColorBrewer::Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd 
                                                # Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd BrBG PiYG PRGn PuOr
                                                # RdBu RdGy RdYlBu RdYlGn Spectral
  width  = 30,
  height = 30,
  free   = TRUE
  ){
  # Description:
  #
  # cor compute the variance of x and the covariance or correlation of x 
  # and y if these are vectors. If x and y are matrices then the 
  # correlations between the columns of x and the columns of y are computed.
  # 
  # Package:
  require(pheatmap)
  require(RColorBrewer)

  methods = c(strsplit(method,split = ",")[[1]])
  for(i in methods){
    corTable = cor(X,Y,method = paste(i))
    write.table(corTable,paste(path.expand(outDir),"/cor.",i,".xls",sep = ""),sep = "\t")
    pheatmap(corTable,
             filename=paste(path.expand(outDir),"/cor.",i,".heatmap.png",sep = ""),
             width=8,
             height=8,
             silent=FALSE,
             color = colorRampPalette(rev(brewer.pal(n = 7, name = brwCol)))(100),
             border_color = "white"
             )
    pheatmap(corTable,
             filename=paste(path.expand(outDir),"/cor.",i,".heatmap.pdf",sep = ""),
             width=width,
             height=height,
             silent=FALSE,
             color = colorRampPalette(rev(brewer.pal(n = 7, name = brwCol)))(100),
             border_color = "white"
             )
    
    if(free){
      print("We Don't Like the Free Project !!!")
    }else{
      require(corrgram)
      png(filename = paste(path.expand(outDir),"/cor.",i,".halfheatmap.png",sep = ""),
          width = width*20,
          height = height*20,
          units = "px")
      corrgram(corTable,
               main="Correlation Matrix",
               order=NULL, 
               lower.panel=panel.shade,
               upper.panel=NULL, 
               text.panel=panel.txt,
               col.regions = colorRampPalette(c(rev(brewer.pal(n = 7, name = brwCol)))),
               cor.method = paste(i)
               )
      dev.off()
    }
  }
  return(corTable)
}
