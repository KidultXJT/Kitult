#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################

metaca <- function(
  input,
  output,
  output_sp = NULL,
  output_spt = NULL,
  outputFormat = "png",
  center = F,
  scale = F,
  main = NULL,
  group = NULL,
  group_colour = NULL,
  select = NULL,
  point_size  = 8,
  point_alpha = 1,
  point_text_size = 4,
  sp_colour   = "royalblue4", 
  sp_size    = 4,
  width  = 9,
  height = 8
){
  
  # library(ggplot2)
  # library(vegan)
  # library(xlsx)
  # library(readxl)
  
  # input = "/Bio/User/liyewei/total_otu_table_L3.txt"
  t = read.table(input,header = T, skip = 1,sep = "\t",comment.char = "@",check.names = F)
  
  if(length(unique(t[,1]))!=length(t[,1])){
    id = which(sapply(1:ncol(t),function(i) is.numeric(t[,i])))
    colnames(t)[1] = "otuid"
    t[,1] = as.character(t[,1])
    expr = paste0(colnames(t[,id]),"=sum(",colnames(t[,id]),")",collapse = ",")
    expr = paste0("dplyr::summarise(group_by(t,otuid),",expr,")")
    t = as.data.frame(eval(parse(text = expr)))
  }
  
  in_table <- as.data.frame(t[,2:(dim(t)[2])])
  rownames(in_table) <- t[,1]
  
  # colSums(in_table)
  in_table2 <- data.frame(in_table,rowSums(in_table))
  colnames(in_table2) = c(rownames(in_table2),"rowSums.in_table.")
  in_table_order_by_row_sum <- in_table2[order(-in_table2$rowSums.in_table.),]
  
  in_table = in_table_order_by_row_sum[,1:(dim(in_table_order_by_row_sum)[2]-1)]
  
  if(!is.null(select)){
    if(select >= dim(in_table)[1]){
      in_table = in_table
    }else{
      in_table = in_table[1:as.numeric(select),]
    }
  }
  
  ## --------------------- CA analysis ---------------------- ##
  ca <- cca(t(in_table),center=center,scale = scale)
  # summary
  sum_ca <- summary(ca)
  (eigv_table <- sum_ca$cont$importance)
  
  sp_df <- data.frame(sum_ca$species)
  sites_df <-  data.frame(sum_ca$sites)
  
  
  # Eigenvalue
  (eigv_ca_1 <- as.numeric(sprintf("%.3f",eigv_table[2,1]))*100)
  (eigv_ca_2 <- as.numeric(sprintf("%.3f",eigv_table[2,2]))*100)
  
  
  name <- row.names(sites_df)
  sites_df$SitesID = name
  # sites_df$Groups = groups
  
  name <- row.names(sp_df)
  sp_df$SpID = name
  
  
  if(!is.null(group)) {groups <- unlist(strsplit(group,","))} else groups = group
  sites_df$Groups = groups
  
  # spname <- row.names(sp_df)
  # sp_df$name = spname
  
  # draw pca plot figure
  xlab=paste("CA1(",eigv_ca_1,"%)",sep="")
  ylab=paste("CA2(",eigv_ca_2,"%)",sep="")
  
  # CA
  ggca <- ggplot(sites_df,aes(CA1,CA2))
  
  ggca = ggca +
    xlab(xlab)
  ggca = ggca +
    ylab(ylab)
  
  if(!is.null(main)) ggca <- ggca + labs(title = main)
  
  ggca <- ggca +
    geom_hline(yintercept=0,linetype=2,color="grey",size=.5) + 
    geom_vline(xintercept=0,linetype=2,color="grey",size=.5)
  
  if(!is.null(group)){
    ggca <- ggca +
      geom_point(data = sites_df,size=point_size,alpha = point_alpha,aes(color=Groups))
  }else{
    ggca <- ggca +
      geom_point(data = sites_df,size=point_size,alpha = point_alpha)
  }

    ggca = ggca + geom_text(data  = sites_df,
              size  = point_text_size,
              aes(label = SitesID),
              color = "grey56")
              # nudge_x = .02,
              # nudge_y = .02)
  
  # if(input$sites){
  #   ggca <- ggca + 
  #     geom_text(data = sites_df, aes(label=SitesID, color=Groups),
  #               family="Times", size = point_size,
  #               nudge_x=.3,nudge_y=.1)
  # }else{
  #   ggca <- ggca
  # }
  # 
  colours <- c("#F4BD6D","#87CCC5","olivedrab3","brown2","goldenrod1","deepskyblue3","maroon2","mediumpurple4",
               "cadetblue4","darkblue","hotpink3","indianred1","gray59",
               "darkorange1","slategray3","grey95","mediumorchid3","paleturquoise1",
               "orangered4","grey64","gray62","royalblue4","antiquewhite3")
  cols <- colours[1:dim(as.data.frame(table(sites_df$Groups)))[1]]
  
  ggca <- ggca + 
    scale_colour_manual(values = cols)
  
  if(!is.null(group_colour)) {col <- unlist(strsplit(group_colour,",")); ggca = ggca + scale_colour_manual(values = col)}
  
  ggca <- ggca + theme_bw()
  
  ggca <- ggca + 
    theme(legend.title = element_text(angle = 0,  
                                      hjust =.95, 
                                      vjust =.95,
                                      size  = 12),
          legend.position   = "right",
          legend.background = element_rect(
            colour = "white", 
            fill = "white", 
            size = 1, 
            linetype = 1),#'dashed'
          legend.key.size   = unit(.5, "cm"),
          legend.key.height = unit(.5, "cm"),
          legend.key.width  = unit(.5, "cm"),
          legend.text       = element_text(angle = 0,  
                                           hjust =.95, 
                                           vjust =.95,
                                           size  = 10),
          legend.justification = "top",
          panel.background = element_rect(fill = "white",colour = "white",size = 1.5))

  ##########################################################################
  
  ggcasp <- ggca +
    geom_point(data   = sp_df,
               color  ="royalblue4", 
               shape  = 0,
               alpha  = .6,
               size   = 4)
  
  ggcaspt <- ggca +
    geom_text(data    = sp_df,
              aes(label = SpID),
              color   = sp_colour, 
              alpha   = .7,
              size    = sp_size)
  
  ##########################################################################
  
  Format   = strsplit(outputFormat,",")[[1]]
  sapply(Format,function(x){
    if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = width, height = height)")
    else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
    eval(parse(text = temp))
    # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
    plot(ggca)
    dev.off()
    
    if(!is.null(output_sp)){
      if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output_sp,\'.\',x), bg = \'transparent\', width = width, height = height)")
      else temp = paste0(x,"(file = paste0(output_sp,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
      eval(parse(text = temp))
      # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
      plot(ggcasp)
      dev.off()
    }
    
    if(!is.null(output_spt)){
      if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output_spt,\'.\',x), bg = \'transparent\', width = width, height = height)")
      else temp = paste0(x,"(file = paste0(output_spt,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
      eval(parse(text = temp))
      # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
      plot(ggcaspt)
      dev.off()
    }
    
  })
  
  return(ggca)
  
  # if(!is.null(output)) ggsave(output,
  #        ggca,
  #        width  = 9,
  #        height = 8)
  # 
  # if(!is.null(output_sp)) ggsave(output_sp,
  #                                ggcasp,
  #                                width  = 9,
  #                                height = 8)
  # 
  # if(!is.null(output_spt)) ggsave(output_spt,
  #                                 ggcaspt,
  #                                 width  = 9,
  #                                 height = 8)
}
