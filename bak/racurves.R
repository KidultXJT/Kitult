#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################

racurves <- function(
  input,
  output,
  outputFormat = "png",
  group = NULL,
  group_colour = NULL,
  themes = "pale",
  blank = F,
  titlesize = 12,
  xsize = 12,
  xtitlesize = 12,
  ysize = 12,
  ytitlesize = 12,
  width  = 8,
  height = 6
){
  # library(ggplot2)
  # library(dplyr)
  # # library(readxl)
  # # library(xlsx)
  # library(gtable)
  # library(ggthemr)
  
  # input = "/Bio/User/liyewei/0.01_otus_tax_taxonomy_re_format.txt"
  # input = "/Bio/User/liyewei/test/02.OTU/de_novo/total/total_tax_otus_all.xls"
  # t = read.csv(input)
  
  t = read.table(input, header = T, sep = "\t",check.names = F)
  id = which(sapply(1:ncol(t),function(i) is.numeric(t[,i])))
  t = t[,c(1,id)]
  
  in_table <- as.data.frame(t[,2:(dim(t)[2])])
  rownames(in_table) <- t[,1]
  
  in_table = apply(in_table,2,function(i) i/sum(i))
  
  rank_table = apply(in_table,2,function(i) nrow(in_table)-rank(i,ties.method= "first")+1)
  
  # df = data.frame(ra = in_table[,1], rank = rank_table[,1], Samples = rep(colnames(in_table)[1],nrow(in_table)))
  # 
  # 
  # for(i in 2:(ncol(t)-1)) df  = data.frame(rbind(df,data.frame(ra = in_table[,i], rank = rank_table[,i], Samples = rep(colnames(in_table)[i],nrow(in_table)))))
  # df = arrange(group_by(df,Samples),desc(ra))
  
  
  if(!is.null(group)) {groups <- unlist(strsplit(group,","))} else groups = colnames(in_table)
  df = data.frame(ra = in_table[,1], rank = rank_table[,1], Samples = rep(colnames(in_table)[1],nrow(in_table)), Groups = rep(groups[1],nrow(in_table)))
  
  
  for(i in 2:(ncol(t)-1)) df  = data.frame(rbind(df,data.frame(ra = in_table[,i], rank = rank_table[,i], Samples = rep(colnames(in_table)[i],nrow(in_table)), Groups = rep(groups[i],nrow(in_table)))))
  df = arrange(group_by(df,Groups),desc(ra))
  
  df = df[which(df$ra!=0),]
  
  # colours <- c("deepskyblue3","maroon2","mediumpurple4",
  #              "cadetblue4","darkolivegreen3","darkblue","hotpink3","indianred1","gray59",
  #              "darkorange1","slategray3","grey95","mediumorchid3","paleturquoise1",
  #              "orangered4","grey64","gray62","royalblue4","antiquewhite3")
  # 
  # colours <- rep(colours,100)
  


  
  ggthemr(themes)
  t = ggthemr(themes)
  colour = as.character(t$palette$swatch)
  colours = c("#F4BD6D","#87CCC5","olivedrab3","brown2","goldenrod1","deepskyblue3","maroon2","mediumpurple4",
              "cadetblue4","darkblue","hotpink3","indianred1",
              "darkorange1","slategray3","grey95","mediumorchid3","paleturquoise1",
              "orangered4","grey10","royalblue4","antiquewhite3")
  swatch_colours = union(colour,colours)
  set_swatch(swatch_colours)
  

  ggra = ggplot()+
    # geom_boxplot(data   = df,
    #              colour = "white",
    #              fill   = "transparent",
    #              alpha  = .01,
    #              width  = .3,
    #              size   = .00000001)+
    # stat_boxplot( data = df,
    #               aes(colour = Samples),
    #               fill   = "transparent",
    #               alpha  = .4,
    #               width  = .25,
  #               size   = .1)+
  geom_line(data = df,
               aes(x     = rank,
                   y     = ra,
                   group = Samples,
                   colour=  Groups),
               alpha  = .2,
               size   =.5,
               linetype   = 2)+
    stat_summary(data = df,
                 aes(x     = rank,
                     y     = ra,
                     group = Groups,
                     colour= Groups),
                 fun.y  = mean,
                 # alpha  = .9,
                 geom   ="line",
                 size   =.5,
                 linetype   = 1)+
    labs(x = "OTU Rank",
         y = "Relative Abundance")
  
  if(length(unique(df$Groups))> length(swatch_colours)) ggra = ggra + 
    scale_colour_manual(values = colours[1:length(unique(df$Groups))])+
    guides(colour = guide_legend(ncol = 2))
  
  
  ggra <- ggra + theme(
    #        ##AES 
    axis.text.x      = element_text(hjust=.5, vjust=.5,
                                    size=xsize),
    axis.line.x      = element_line(colour= "#d1d1d1",size = 0.5,linetype=1),
    
    axis.text.y      = element_text(hjust=.95, vjust=.5,
                                    size=ysize),
    axis.line.y      = element_line(colour="#d1d1d1",size = 0.5,linetype=1),
    #                          
    axis.title.x     = element_text(size=xtitlesize),
    axis.title.y     = element_text(size=ytitlesize),
    title     = element_text(size=titlesize),
    
    ## Background
    # panel.background = element_rect(fill = input$bgcolours, colour = "white",size = 1.5),
    panel.border = element_rect(fill = NA, colour = "#d1d1d1",size = 0.5,linetype = 1),
    
    
    #        ## Legend
    legend.position   = "right",
    legend.justification = "top",
    # legend.title= element_blank(),
    legend.text = element_text(size = 12)
  )
  
  ggra = ggra + 
    scale_colour_manual(values = colours[1:length(unique(df$Groups))]) 
  
  ggra = ggra + theme(legend.key.size=unit(0.5,'cm'))
  
  if(blank) ggra = ggra + theme(panel.grid =  element_blank())
  
  ggra = ggra + 
    scale_y_continuous(trans = "log10",breaks = c(10e-5,10e-4,10e-3,10e-2,10e-1),
                       labels = c("0.0001",10e-4,10e-3,10e-2,10e-1))
  

  if(!is.null(group_colour)) {col <- unlist(strsplit(group_colour,",")); ggra = ggra + scale_colour_manual(values = col)}
  
  
  Format   = strsplit(outputFormat,",")[[1]]
  sapply(Format,function(x){
    if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = width, height = height)")
    else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
    eval(parse(text = temp))
    # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
    plot(ggra)
    dev.off()
    
  })
  return(ggra)
  
  # ggsave(output,
  #        ggra,
  #        width  = 8,
  #        height = 6)
  
}
