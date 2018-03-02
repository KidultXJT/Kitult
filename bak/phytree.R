#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################

phytree <- function(
  input,
  tableType = "tre",
  output,
  outputFormat = "png",
  layout = "rectangular",
  treecolour = "grey30",
  labcolour = "grey30",
  treesize = if(layout == "rectangular") 0.4 else 0.2,
  lablesize = 2,
  linetype = 1,
  ladderize = F,
  right = F,
  branch.len = 1
){
  
  # require(ggplot2)
  # require(ggtree)
  # require(phytools)
  
  try(if(tableType != "tre") stop("Not ready yet!"))
    # raxml <- read.raxml(raxml_file)
  
  if(tableType == "tre"){
    
    tree = read.newick(file(input))
    num <- sum(tree$Nnode)
  }
  
  treeplot <-
        ggtree(tree,
               layout = layout,
               color  = treecolour, 
               #aes(color=group, 
               #   linetype=group),
               size   = treesize,
               linetype = linetype,
               ladderize = ladderize, 
               right = right, 
               branch.length = branch.len) +
        geom_treescale() +
        geom_tiplab(
          size  = lablesize, 
          color = labcolour, 
          vjust = 0.03,
          hjust  = 0.02
        ) +
        geom_text2(
          aes(subset=!isTip,label=node), 
          size  = 2.5,
          hjust =-.3,
          color = "grey30") + 
        scale_colour_hue(h = c(0, 200))
      
      #################################################    
  
  
  Format   = strsplit(outputFormat,",")[[1]]
  sapply(Format,function(x){
    if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = 9, height = log10(num) + 9)")
    else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = 9*72, height = (log10(num) + 9)*72, type = \'cairo\')")
    eval(parse(text = temp))
    # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
    plot(treeplot)
    dev.off()
    
  })
  return(treeplot)
  
      # setwd(output_dir)
        # ggsave(output,
        #        treeplot,
        #        width  = 9,
        #        height = log10(num) + 9)
               # device = output_format
       # unlink(output)
}


