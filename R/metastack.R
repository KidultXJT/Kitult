#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################

metastack <- function(
  input,
  output,
  outputFormat = "png",
  group = NULL,
  size =.2,
  title = "",
  xlab  = "Samples",
  ylab  = "Abundance",
  fill  = NULL,
  facet_grid = NULL,
  lncol      = 1,
  width  = 15,
  height = 9
){
  # library(phyloseq)
  # library(ggplot2)
  # library(ape)
  # library(dplyr)
  
  
  # input =  "/Bio/User/liyewei/test100/05.Taxonomy/total/relative_abundance/Dominant/total_L7_tax.xls"
  t <- read.table(input, sep = "\t",comment.char = "@",header = T,check.names = F)
  colnames(t)[1] = "Taxonomy"
  
  Others = t(as.matrix(c("Others",1 - colSums(t[2:ncol(t)]))))
  colnames(Others)[1] = "Taxonomy"
  combind <- rbind(t,Others)
  combind$Taxonomy <- factor(combind$Taxonomy, levels=unique(combind$Taxonomy))
  
  if(nrow(t) == 0){
    df = data.frame(Name = rep("Others",ncol(combind)-1),
                    values = rep(1,ncol(combind)-1),
                    ind = colnames(combind)[2:ncol(combind)])
  }else{
    df = data.frame(Name = combind[,1],stack(combind[2:ncol(combind)]))
  }
  
  colnames(df)[1] = "Name"
  df$values = as.numeric(as.character(df$values))
  if(length(grep("-",group))!=0) df$ind = chartr(df$ind,old = ".",new = "-")
  
  # levelnames = c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
  # levels(levelnames) = c(1,2,3,4,5,6,7)
  # 
  # try(if(!select %in% levels(levelnames)) stop("No this select! Please input:Kingdom(1),Phylum(2),Class(3),Order(4),Family(5),Genus(6),Species(7)"))
  # 
  # title = levelnames[as.numeric(select)]  
  
  
  if(!is.null(group)) {
    group <- unlist(strsplit(group,","))
    row_names = dplyr::arrange(data.frame(sample = colnames(t)[-1], group = group),group)$sample
    df$ind = factor(df$ind,levels = row_names)
  }
  
  
  colours <- c("#2DCB70","#1CA29B","olivedrab3","brown2","goldenrod1","deepskyblue3",
               "maroon2","mediumpurple4","cadetblue4","darkolivegreen3","darkblue","hotpink3",
               "indianred1","gray59","darkorange1","slategray3","grey95","mediumorchid3",
               "paleturquoise1","orangered4","grey64","gray62","royalblue4","antiquewhite3",
               "#0E1555","#FF5722","#51710A","#4E1184","#083358","#F87D09","#C5D200",
               "#932B77","#0D63A5","#F9C535","#9BDF46","#FD367E","#228896","#F8DA5B","#83CC61",
               "#E84A5F","#3D84A8","#F8C957","#B7E576","#FF847C","#46CDCF","#99CDA9","#86D27B",
               "#43496E","#544D7E","#65589C","#3D3551","#884EA2","#3EC280","#D15400","#D0527E",
               "#E16A6B","#C0392B","#EA9432","#36D6B5","#68C2A1","#AAB6B6","#2474A8","#3597DA",
               "#E57E22","#81CEE0","#D54542","#D24D58","#51B2D7","#F5AA36","#AEA7D1","#00B069",
               "#EE4836","#E77E04","#F6C917","#1F3A92","#2ABA99","#169F85","#F5D66D","#BDC2C6",
               "#F17835","#1E8AC2","#F44746","#A0DDCE","#66CB99","#59AAE2","#903D87","#D81E17",
               "#8C44AC","#4ECCC3","#F22613","#DA0A5B","#9959B4","#95281B","#03C8A7","#F1F0EE",
               "#BEBEBE","#F62459","#94A4A4","#C7F5C4","#D1D6D3","#F89306","#EA964E","#65C5BA",
               "#5C96BE","#23A6F0","#4182D6","#D9DEE1","#4DAE7B","#EBEFF0","#E74C3D","#89C3F3",
               "#6D7A89","#EBEBEB","#85E1D4","#1E824C","#019775","#FECEA8","#CE0010","#EDEDEE",
               "#F4B151","#3B529A","#26C180","#F3CF3E","#674172","#663397","#F86A0E","#FCE2A7",
               "#ABEDD8","#D1E9D2","#FFD6A4","#A7CDCC","#E5F4E7","#FDE9DF","#D6E6F2","#F1FDF3",
               "#029273","#03A579","#F29B11","#9912B3","#34495E","#E3F1FE","#2D3E50","#6CB8F0",
               "#F1A89F","#F1774B","#1ABB9B","#C3EEF5","#446CB1","#26A55B","#8FC594","#DF8182",
               "#BE55EC","#BD90D3","#4B77BD","#19B4FE","#213140","#336F7A","#F8BE3A","#DCC5DF")
  colours <- rep(colours,100)
  
  p = ggplot(data = df, 
             aes(x    = ind))
  p = p + geom_bar(data = df, 
                   aes(x    = ind, 
                       y    = values, 
                       fill = factor(Name)
                       
                   ),
                   stat="identity", 
                   #position=position_dodge(),
                   width  = .8,
                   # size = .4
                   size   = size,
                   alpha  = .8,
                   colour = "grey20"
  )
  p = p + 
    labs(x    = xlab,
         y    = ylab,
         fill = "")
  
  theme =  theme(
    # Axis
    axis.text.x      = element_text(angle = 45, 
                                    hjust =.95, 
                                    vjust =.95, 
                                    size  = ceiling(12/(nrow(df)^(1/10))),
                                    color = "grey30"),
    axis.ticks.x     = element_blank(),
    axis.line.x      = element_blank(),
    
    axis.text.y      = element_text(angle = 0, 
                                    hjust =.5, 
                                    vjust =.5, 
                                    size  = 12,
                                    color = "grey30"),
    axis.ticks.y     = element_blank(),
    axis.line.y      = element_blank(),
    
    axis.title.x     = element_text(angle = 0, 
                                    hjust =.5, 
                                    vjust =.95, 
                                    size  = 14,
                                    color = "grey20"),
    axis.title.y     = element_text(angle = 90,
                                    hjust =.5, 
                                    vjust =.95, 
                                    size  = 14,
                                    color = "grey20"),
    
    # Legend          
    legend.title = element_text(angle = 0,  
                                hjust =.95, 
                                vjust =.95,
                                size  = 8,
                                color ="grey56"),
    legend.position   = "right",
    legend.justification = "top",
    legend.background = element_rect(
      colour = "white", 
      fill = "white", 
      size = .1, 
      linetype = .1),#'dashed'
    
    legend.key.size   = unit(.5, "cm"),
    legend.key.height = unit(.5, "cm"),
    legend.key.width  = unit(.5, "cm"),
    legend.text       = element_text(angle = 0,  
                                     hjust =.95, 
                                     vjust =.95,
                                     size  = 10,
                                     colour="grey56")#,
    
    #panel.grid       = element_line(colour = "black")
    #panel.border     = element_blank(),
    #panel.grid.minor = element_line(colour = "black")
    #panel.spacing.x  = element_blank(),
    #panel.spacing.y  = element_blank()
    
    #panel.background = element_rect(
    #  colour = "grey20", 
    #  fill = "white", 
    #  size = .2, 
    #  linetype = 1) 
    #panel.background = element_blank()
  ) 
  p = p + theme_light()
  p = p + theme
  
  p = p + scale_x_discrete(expand = c(0,0)) +  scale_y_continuous(expand = c(0,0))
  
  p = p + guides(fill = guide_legend(ncol = lncol))
  
  ###############################################################
  cols <- c()
  nums <- 1:length(levels(df$Name))
  #nums <- 1:length(levels(df$Samples))
  for(i in nums){
    colour = colours[i]
    i = levels(df$Name)[i]
    #i = levels(df$Samples)[i]
    i = colour
    cols <- c(cols, i)
  }
  
  p = p + scale_fill_manual(values = cols)
  
  ##############################################################
  
  
  if (!is.null(facet_grid)) {
    p <- p + facet_grid(facet_grid)
  }
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }

  Format   = strsplit(outputFormat,",")[[1]]
  sapply(Format,function(x){
    if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = width, height = height)")
    else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
    eval(parse(text = temp))
    # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
    plot(p)
    dev.off()
    
  })
  
  # ggsave(output,
  #        p,
  #        width  = 15,
  #        height = 9)
 return(p)
}