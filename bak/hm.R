#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################
hm <- function(
  input,
  output,
  outputFormat = "png",
  output_table = NA,
  title = "",
  select = NULL,
  max_otu_number = 20000,
  display = "all",
  kmeans_num = NA,
  cell_width = NA,
  cell_height = NA,
  colour = "RdYlBu",
  border_color = "white",
  log = TRUE,
  tree = "both",
  scale = "column",
  cluster_rows = TRUE,
  cluster_cols = TRUE, 
  c_dis_m = "euclidean",
  r_dis_m = "euclidean",
  cluster_m = "complete",
  legend_labels = NA,
  width = 35/5,
  height = 70/5,
  fontsize = 12,
  display_numbers = F,
  fontsize_number = 0.8 * fontsize, 
  gaps_row = NULL, 
  gaps_col = NULL,
  number_color = "white",
  annotation_row = NA,
  annotation_col = NA,
  annotation = NA,
  annotation_colors = "grey", 
  annotation_legend = TRUE,
  annotation_names_row = TRUE, 
  annotation_names_col = TRUE
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
  # input = "/Bio/User/liyewei/test/05.Taxonomy/total/relative_abundance/total_otu_table_L3.txt"
  # input = "/Bio/User/liyewei/a.txt"
  
  if(length(grep(pattern = "xls",input)) != 0) t = read.table(input, sep = "\t",comment.char = "@",header = T) else t = read.table(input,header = T, sep = "\t", skip = 1,comment.char = "@")
  if(nrow(t) >= max_otu_number) select = max_otu_number
  
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
   
    # intable_pv <- pvclust(intable, nboot=1000, parallel=FALSE,method.hclust = )  # nboot set at 1000 for real
    
    if(nrow(in_table) == 1)  {cluster_rows = F;scale = "none"}
    if(ncol(in_table) == 1)  {cluster_rows = F;scale = "none"}
  

    if(length(grep(pattern = "xls",input)) != 0) names = colnames(read.table(input, sep = "\t",comment.char = "@",header = T,check.names = F)) else names = colnames(read.table(input,header = T, sep = "\t", skip = 1,comment.char = "@",check.names = F))
    names = names[-1]
    colnames(in_table) = names
  
    Format   = strsplit(outputFormat,",")[[1]]
    sapply(Format,function(x){
          output = paste0(output,".",x)
          # if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = width, height = height)")
          # else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
          # eval(parse(text = temp))
          pheatmap(in_table, 
                       color = colorRampPalette(rev(brewer.pal(n = 7, name = colour)))(100),
                       # color = colour,
                       kmeans_k = kmeans_num, 
                       breaks = NA,
                       border_color = border_color,
                       cellwidth = cell_width, 
                       cellheight = cell_height,
                       scale = scale,
                       cluster_rows = cluster_rows,
                       cluster_cols = cluster_cols, 
                       clustering_distance_rows = r_dis_m,
                       clustering_distance_cols = c_dis_m, 
                       clustering_method = cluster_m,
                       
                       # clustering_callback = identity2,
                       cutree_rows = NA,
                       cutree_cols = NA,
                       treeheight_row = ifelse((class(cluster_rows) == "hclust") || cluster_rows,
                                               50, 0),
                       treeheight_col = ifelse((class(cluster_cols) == "hclust") ||
                                                 cluster_cols, 50, 0), legend = TRUE, legend_breaks = NA,
                       
                       legend_labels = legend_labels, 
                       annotation_row = annotation_row,
                       annotation_col = annotation_col,
                       annotation = annotation,
                       annotation_colors = annotation_colors, 
                       annotation_legend = annotation_legend,
                       annotation_names_row = annotation_names_row, 
                       annotation_names_col = annotation_names_col,
                       
                       drop_levels = TRUE, 
                       show_rownames = T, 
                       show_colnames = T, 
                       main = title,
                       
                       
                       fontsize_row = ceiling(fontsize/(nrow(in_table)^(1/10))), 
                       fontsize_col = ceiling(fontsize/(nrow(in_table)^(1/10))),
                       
                       display_numbers = display_numbers, 
                       number_format = "%.2f", 
                       number_color = number_color,
                       fontsize_number = fontsize_number,
                       gaps_row = gaps_row, gaps_col = gaps_col,
                       labels_row = NULL, labels_col = NULL, 
                       filename = output,
                       width = width,
                       height = height, 
                       silent = FALSE)
          # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
          # dev.off()
          # print(x)
    })
    if(display != "all") write.table(in_table,output_table,row.names = F,quote = F)
}
  

