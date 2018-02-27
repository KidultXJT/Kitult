#########################################
###### Author:Katharine Lee #############
############## 2017.08 ##################
#########################################
sankey <- function(
  input,
  output,
  outputFormat = "png",
  start = 1,
  end = 7,
  textcex = 0.8,
  textcol = "grey20",
  texangle = 0,  
  width  = 16,
  height = 10
){
  # library(riverplot)
  # library(dplyr)
  
  # input = "~/total_tax_otus_all.xls"
  
  df = read.table(file = input,header = T)
  tax = df[,c(-1:-10)]
  tax = data.frame(apply(tax,2,as.character))
  names = colnames(tax)
  tax[which(tax[,1]== "k__"),1] = "Other"
  tax[which(tax[,2]== "p__"),2] = "Other"
  tax[which(tax[,3]== "c__"),3] = "Other"
  tax[which(tax[,4]== "o__"),4] = "Other"
  tax[which(tax[,5]== "f__"),5] = "Other"
  tax[which(tax[,6]== "g__"),6] = "Other"
  tax[which(tax[,7]== "s__"),7] = "Other"
  tax = data.frame(apply(tax,2,gsub,pattern = "\\[", replacement = ""))
  tax = data.frame(apply(tax,2,gsub,pattern = "\\]", replacement = ""))
  
  colours = c("#F4BD6D","#87CCC5","olivedrab3","goldenrod1","maroon2","mediumpurple4",
              "cadetblue4","hotpink3","indianred1",
              "slategray3","mediumorchid3","paleturquoise1",
              "orangered4","royalblue4","antiquewhite3")
  colours = rep(colours,100)
  
  nodes = data.frame()
  edges = character()
  
  for(i in (start+1):end){
    expr = paste0("dplyr::summarise(group_by(tax,",names[i-1],",",names[i],"), n = n())")
    p = eval(parse(text = expr))
    
    p = p[which(p[,1]!= "Other"),]
    p = p[which(p[,2]!= "Other"),]
    # p = p[which(p$n>=3),]
    
    # A =  eval(parse(text = paste0("as.character(p$",names[i-1],")")))
    # if(length(grep(pattern = "\\[", x = A))!=0) p = p[-grep(pattern = "\\[", x = A),]
    # B =  eval(parse(text = paste0("as.character(p$",names[i],")")))
    # if(length(grep(pattern = "\\[", x = B))!=0) p = p[-grep(pattern = "\\[", x = B),]
    # 
    if(nrow(p)!=0){
      p[,1] =  eval(parse(text = paste0("gsub(\'-\',\'_\',as.character(p$",names[i-1],"))")))
      p[,2] =  eval(parse(text = paste0("gsub(\'-\',\'_\',as.character(p$",names[i],"))")))
      
      p = arrange(p,n)
      expr = paste0("dplyr::summarise(group_by(p,",names[i-1],"), t = paste0(",names[i]," ,\'=\ ', n,collapse = \',\'))")
      expr = eval(parse(text = expr))
      expr = apply(expr,1,function(x) paste0(x[1],"=list(",x[2],")"))
      expr = paste0(expr,collapse = ",")
      edges = ifelse(i ==(start+1), expr ,paste0(edges,",",expr))
      # edges = eval(parse(text = paste0("list(",expr,")")))
      
      A =  unique(eval(parse(text = paste0("as.character(p$",names[i-1],")"))))
      A = gsub("-","_",A)
      B = unique(eval(parse(text = paste0("as.character(p$",names[i],")"))))
      B = gsub("-","_",B)
      Nodes = data.frame(ID = c(A,B),
                         x = c(rep((i-1),length(A)),rep(i,length(B))),
                         # col = colours[1:(length(A)+length(B))],
                         lables = c(A,B),
                         stringsAsFactors = F)
      nodes = data.frame(rbind(nodes,Nodes))
    }
    # p = p[which(p[,2]!= "p__" & p[,1]!= "c__" & p[,1]!= "o__" & p[,1]!= "f__"& p[,1]!= "g__"& p[,1]!= "s__"),]
    
  }
  
  edges <- eval(parse(text = paste0("list(",edges,")")))
  nodes = unique(nodes)
  nodes$col = colours[1:nrow(nodes)]
  r <- makeRiver(nodes, edges)
  
  Format   = strsplit(outputFormat,",")[[1]]
  sapply(Format,function(x){
    if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = width, height = height)")
    else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
    eval(parse(text = temp))
    # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
    plot(r,srt=texangle,textcol = textcol,textcex = textcex)
    dev.off()
    
  })
  
  return(plot(r,srt=texangle,textcol = textcol,textcex = textcex))
  
}

