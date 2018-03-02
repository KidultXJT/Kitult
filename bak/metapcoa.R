#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################

metapcoa <- function(
  input,
  output,
  outputFormat = "png",
  main = NULL,
  group = NULL,
  group_colour = NULL,
  select = NULL,
  point_size  = 8,
  point_alpha = 1,
  point_text_size = 4,
  width  = 9,
  height = 8
){
  
  # library(ggplot2)
  # library(ape)
  # # library(xlsx)
  # # library(readxl)
  
  # input = "/Bio/User/liyewei/unweighted_unifrac_div.xls"
  t <- read.table(input,header = T,check.names = F)
  in_table <- as.data.frame(t[,2:(dim(t)[2])])
  rownames(in_table) <- t[,1]
  
  ## --------------------- pcoa analysis ---------------------- ##
  pcoa <- pcoa(in_table)
  # eigv_table <- pcoa$cont$importance
  # species
  # sum_cca$species
  # sp_df <- data.frame(pcoa$vectors)
  #write.table("sp.txt",sp_df)
  # sites
  # sum_cca$sites
  sites_df <-  data.frame(pcoa$vectors)
  
  # Eigenvalue
  (eigv_pcoa_1 <- as.numeric(sprintf("%.3f",pcoa$values$Cumul_eig[1]))*100)
  (eigv_pcoa_2 <- as.numeric(sprintf("%.3f",pcoa$values$Cumul_eig[2]))*100)
  
  name <- row.names(sites_df)
  sites_df$name = name
  
  if(!is.null(group)) {groups <- unlist(strsplit(group,","))} else groups = group
  sites_df$Groups = groups
  
  # spname <- row.names(sp_df)
  # sp_df$name = spname
  
  # draw pcoa plot figure
  xlab=paste("PC1(",eigv_pcoa_1,"%)",sep="")
  ylab=paste("PC2(",eigv_pcoa_2,"%)",sep="")
  
  # pcoa
  ggpcoa = ggplot(sites_df,aes(Axis.1,Axis.2))
  
  if(!is.null(main)) ggpcoa <- ggpcoa + labs(title = main)
  
  ggpcoa = ggpcoa +
    xlab(xlab)
  ggpcoa = ggpcoa +
    ylab(ylab)
  
  ggpcoa = ggpcoa +
    geom_hline(yintercept=0,linetype=2,color="grey",size=.5) + 
    geom_vline(xintercept=0,linetype=2,color="grey",size=.5)
  
  if(!is.null(group)) {ggpcoa <- ggpcoa +
    geom_point(data  = sites_df,
               size  =  point_size,
               alpha =  point_alpha, 
               aes(color=Groups)) + 
    geom_text(data  = sites_df,
              size  = point_text_size,
              aes(label = name),
              color = "grey56")}else{
              # nudge_x = .02, 
              # nudge_y = .02
                
                ggpcoa <- ggpcoa +
                  geom_point(data  = sites_df,
                             # colour = colour,
                             size  = point_size,
                             alpha = point_alpha) + 
                  geom_text(data  = sites_df,
                            size  = point_text_size,
                            aes(label = name),
                            color = "grey56")
                            # nudge_x = .02, 
                            # nudge_y = .02)
              }
  
  
  colours <- c("#F4BD6D","#87CCC5","olivedrab3","brown2","goldenrod1","deepskyblue3","maroon2","mediumpurple4",
               "cadetblue4","darkblue","hotpink3","indianred1","gray59",
               "darkorange1","slategray3","grey95","mediumorchid3","paleturquoise1",
               "orangered4","grey64","gray62","royalblue4","antiquewhite3")
  cols <- colours[1:dim(as.data.frame(table(sites_df$Groups)))[1]]
  ggpcoa <- ggpcoa + 
    scale_colour_manual(values = cols)
  
  if(!is.null(group_colour)) {col <- unlist(strsplit(group_colour,",")); ggpcoa = ggpcoa + scale_colour_manual(values = col)}
  
  ggpcoa <- ggpcoa + theme_bw()
  
  ggpcoa <- ggpcoa + theme(
    legend.title = element_text(angle = 0,  
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
  # legend.justification = "top")
  
  ##########################################################################
  
  # ggpcoasp <- ggpcoa +
  #   geom_point(data   = sp_df,
  #              color  ="royalblue4", 
  #              shape  = 0,
  #              alpha  = .6,
  #              size   = 4)
  # 
  # ggpcoaspt <- ggpcoa +
  #   geom_text(data    = sp_df,
  #             aes(label = name),
  #             color   = sp_colour, 
  #             alpha   = .7,
  #             size    = sp_size)
  
  
  Format   = strsplit(outputFormat,",")[[1]]
  sapply(Format,function(x){
    if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = width, height = height)")
    else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
    eval(parse(text = temp))
    # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
    plot(ggpcoa)
    dev.off()
    
  })
  return(ggpcoa)
  # if(!is.null(output)) ggsave(output,
  #                             ggpcoa,
  #                             width  = 9,
  #                             height = 8)
}

