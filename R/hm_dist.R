#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################

hm_dist <- function(
  input,
  output,
  outputFormat = "png",
  group = NULL,
  group_colour = NULL,
  seed = 27,
  title = "",
  # select = NULL,
  # display = "all",
  # kmeans_num = NA,
  cell_width = NA,
  cell_height = NA,
  colour = "YlGnBu",
  border_color = "white",
  log = TRUE,
  # tree = "both",
  scale = "none",
  # cluster_rows = F,
  # cluster_cols = F, 
  # c_dis_m = "euclidean",
  # r_dis_m = "euclidean",
  # cluster_m = "complete",
  legend_labels = NA,
  width = 10,
  height = 10,
  fontsize = 12,
  display_numbers = T,
  fontsize_number = 0.8 * fontsize, 
  gaps_row = NULL, 
  gaps_col = NULL,
  number_color = "white"
  # annotation_row = NA,
  # annotation_col = NA,
  # annotation = NA,
  # # annotation_colors = "grey", 
  # annotation_legend = TRUE,
  # annotation_names_row = TRUE, 
  # annotation_names_col = TRUE
){
  
  # library(pheatmap)
  # library(ggplot2)
  # # library(cluster)
  # library(pvclust)
  # library(dplyr)
  # library(RColorBrewer)
  # library(readxl)
  # library(xlsx)
  # library(gtable)
  
  # input = "/Bio/User/liyewei/test100/04.Beta/total/unweighted_unifrac_div.xls"
  #group = "case,case,case,case,case,case,control,case,case,case,control,control,case,control,case,control,case,control,case,control,case,case,control,case,control,case,case,control,case,case,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,control,control,case,control,case,case,case,case,control,case,control,control,case,control,case,case,control,control,control,control,control,case,case,control"
  t <- read.table(input,header = T,check.names = F)
  in_table <- as.data.frame(t[,2:(dim(t)[2])])
  rownames(in_table) <- t[,1]
  colnames(in_table) <- t[,1]
  
  
  if(!is.null(group)) {
    group <- unlist(strsplit(group,","))
    annotation_col = data.frame(Groups = factor(group))
    rownames(annotation_col) = colnames(in_table)

    colours <- colorRampPalette(rev(brewer.pal(n = 7, name =  "Dark2" )))(100)
    
    if(is.null(group_colour)) {
      set.seed(seed)
      ann_colors = sample(colours,length(unique(group)))
    }else{
    ann_colors = unlist(strsplit(group_colour,","))
      
    
    row_names = dplyr::arrange(data.frame(sample = rownames(in_table),group = group),group)$sample
    new_row_names = paste0("\'",row_names[1],"\'")
    for(i in 2:length(row_names)) new_row_names = paste0(new_row_names,",\'",row_names[i],"\'")
    new_row_names = paste0("c(",new_row_names,")")
    
    temp = paste0("in_table = in_table[,",new_row_names,"]")
    eval(parse(text = temp))
    
    temp = paste0("in_table = in_table[",new_row_names,",]")
    eval(parse(text = temp))
    
    # in_table = in_table[,new_row_names]
    # in_table = in_table[new_row_names,]
    # 
    temp = paste0("\'",unique(group)[1],"\'=\'",ann_colors[1],"\'")
    for(i in 2:length(unique(group))) temp = paste0(temp,",",paste0("\'",unique(group)[i],"\'=\'",ann_colors[i],"\'"))
    temp = paste0("ann_colors = list(Groups = c(",temp,"))")
    eval(parse(text = temp))
    
    Format   = strsplit(outputFormat,",")[[1]]
    sapply(Format,function(x){
      output = paste0(output,".",x)
      # if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = width, height = height)")
      # else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
      # eval(parse(text = temp))
      
      pheatmap(in_table, 
               color = colorRampPalette(rev(brewer.pal(n = 7, name = colour)))(100),
               # color = colour,
               # kmeans_k = kmeans_num, 
               breaks = NA,
               border_color = border_color,
               cellwidth = cell_width, 
               cellheight = cell_height,
               scale = "none",
               cluster_rows = F,
               cluster_cols = F, 
               # clustering_distance_rows = r_dis_m,
               # clustering_distance_cols = c_dis_m, 
               # clustering_method = cluster_m,
               
               # clustering_callback = identity2,
               cutree_rows = NA,
               cutree_cols = NA,
               # treeheight_row = ifelse((class(cluster_rows) == "hclust") || cluster_rows,
               # 50, 0),
               # treeheight_col = ifelse((class(cluster_cols) == "hclust") ||
               # cluster_cols, 50, 0), legend = TRUE, legend_breaks = NA,
               legend = F,
               legend_labels = NA, 
               annotation_col = annotation_col,
               annotation_names_col = F,
               annotation_names_row = F,
               annotation_row = annotation_col,
               # annotation = annotation,
               annotation_colors = ann_colors,
               # annotation_legend = annotation_legend,
               # annotation_names_row = annotation_names_row, 
               # annotation_names_col = annotation_names_col,
               
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
      
      # dev.off()
      # print(x)
    })

  }else{
    
    Format   = strsplit(outputFormat,",")[[1]]
    sapply(Format,function(x){
      output = paste0(output,".",x)
      # if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = width, height = height)")
      # else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
      # eval(parse(text = temp))
      pheatmap(in_table, 
               color = colorRampPalette(rev(brewer.pal(n = 7, name = colour)))(100),
               # color = colour,
               # kmeans_k = kmeans_num, 
               breaks = NA,
               border_color = border_color,
               cellwidth = cell_width, 
               cellheight = cell_height,
               scale = "none",
               cluster_rows = F,
               cluster_cols = F, 
               # clustering_distance_rows = r_dis_m,
               # clustering_distance_cols = c_dis_m, 
               # clustering_method = cluster_m,
               
               # clustering_callback = identity2,
               cutree_rows = NA,
               cutree_cols = NA,
               # treeheight_row = ifelse((class(cluster_rows) == "hclust") || cluster_rows,
               # 50, 0),
               # treeheight_col = ifelse((class(cluster_cols) == "hclust") ||
               # cluster_cols, 50, 0), legend = TRUE, legend_breaks = NA,
               legend = F,
               legend_labels = legend_labels, 
               # annotation_row = annotation_row,
               # annotation_col = annotation_col,
               annotation = NA,
               # annotation_colors = ann_colors, 
               # annotation_legend = annotation_legend,
               # annotation_names_row = annotation_names_row, 
               # annotation_names_col = annotation_names_col,
               
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
      
      # dev.off()
    
    
  })
    
}
}
