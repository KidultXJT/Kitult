#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################
clustertree <- function(
  input,
  output_hc,
  output_pv = NULL,
  outputFormat = "png",
  method = "average",
  nboot = 1000,
  select = NULL,
  display = "all",
  width = 800,
  height = 400,
  log = TRUE,
  parallel = 30
){
  # library(pheatmap)
  # library(ggplot2)
  # # library(cluster)
  # library(pvclust)
  # library(dplyr)
  # library(RColorBrewer)
  # # library(readxl)
  # # library(xlsx)
  # library(gtable)
  # input = "/Bio/User/liyewei/test/05.Taxonomy/total/abundance/total_L7_tax.xls"
  # input = "/Bio/User/liyewei/a.txt"
  
  t = read.table(input,header = T, sep = "\t", comment.char = "@")
  id = which(sapply(1:ncol(t),function(i) is.numeric(t[,i])))
  
  if(display!="all"){
    try(if(!display %in% colnames(t)) stop("No this display! Please input:Kingdom,Phylum,Class,Order,Family,Genus,Species"))
    expr = paste0("group_by(t,",display,")")
    t = eval(parse(text = expr))
    expr = paste0(colnames(t[,id]),"=sum(",colnames(t[,id]),")",collapse = ",")
    expr = paste0("summarise(t,",expr,")")
    t = as.data.frame(eval(parse(text = expr)))
  }
  
  in_table <- as.data.frame(t[,2:(dim(t)[2])])
  rownames(in_table) <- t[,1]
  
  in_table = in_table[,id-1]
  # colSums(in_table)
  in_table2 <- data.frame(in_table,rowSums(in_table))
  # colnames(in_table2) = c(rownames(in_table2),"rowSums.in_table.")
  in_table_order_by_row_sum <- in_table2[order(-in_table2$rowSums.in_table.),]
  
  if(dim(in_table_order_by_row_sum)[2]==2){
    in_table = as.data.frame(in_table_order_by_row_sum[,1])
    rownames(in_table) = rownames(in_table_order_by_row_sum)
    colnames(in_table) = colnames(t)[2]
  }else{
    in_table = in_table_order_by_row_sum[,1:(dim(in_table_order_by_row_sum)[2]-1)]
    
  }
  
  if(!is.null(select)){
    if(select >= dim(in_table)[1]){
      in_table = in_table
    }else{
      in_table = in_table[1:as.numeric(select),]
    }
  }
  
  if(log){
    in_table = log(in_table+0.001)
    ind <- apply(in_table, 1, sd) == 0
    if(ncol(in_table) != 1) in_table <- in_table[!ind,]
    ind <- apply(in_table, 2, sd) == 0
    if(nrow(in_table) != 1) in_table <- in_table[,!ind]
    
  } 
  
  names = colnames(read.table(input,header = T,sep = "\t", comment.char = "@",check.names = F))
  names = names[-1]
  colnames(in_table) = names
  
  
  h = hclust(dist(t(as.matrix(in_table))))
  
  if(is.numeric(parallel)) parallel <- as.integer(parallel)
  intable_pv <- try(pvclust(in_table, nboot=nboot, parallel=parallel,quiet=T),silent = T)
  
    Format   = strsplit(outputFormat,",")[[1]]
    for(x in Format){
      if(ncol(in_table) != 1){
        if(nrow(in_table) != 1){
          if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output_hc,\'.\',x), bg = \'transparent\', width = width/72, height = height/72)")
          else temp = paste0(x,"(file = paste0(output_hc,\'.\',x), bg = \'transparent\', width = width, height = height, type = \'cairo\')")
          eval(parse(text = temp))
          # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
          plot(h)
          dev.off()
          # print(x)

          if(!is.null(output_pv)){
              if ('try-error' %in% class(intable_pv)) next
              else{
                if(x == "pdf") temp <- paste0("cairo_pdf(file = paste0(output_pv,\'.\',x), bg = \'transparent\', width = width/72, height = height/72)")
                else temp <- paste0(x,"(file = paste0(output_pv,\'.\',x), bg = \'transparent\', width = width, height = height, type = \'cairo\')")
                eval(parse(text = temp))
                # png(file = paste0(output_pv,".",outputFormat), bg = "transparent", width = width, height = height)
                plot(intable_pv)
                dev.off()
              }
              
            }
        }
      }
    }
  
    return(plot(h))


}