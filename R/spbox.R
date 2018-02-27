#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################

spbox <- function(
  input,
  output,
  outputFormat = "png",
  group,
  group_colour = NULL,
  xlab = "Species(select)",
  ylab = "Abundance",
  fill       = NULL,
  title      = "", 
  facet_grid = NULL,
  xtextsize  = 12,
  width  = 15,
  height = 9
){
  # library(ggplot2)
  
  # input = "/Bio/User/liyewei/testsmall/05.Taxonomy/total/abundance/Dominant/total_L5_tax.xls"
  t <- read.table(input, sep = "\t",comment.char = "@",header = T,check.names = F)
  
  t.mat <- t(t[,2:ncol(t)] )
  colnames(t.mat) <- t[,1]
  t.df <- data.frame(t.mat)
  
  groups <- unlist(strsplit(group,","))
  
  
  if(mean(t.mat)==1 | ncol(t.df)==0){
    print("There is no result Here")
    p <- ggplot()
  }else{
    
  if(ncol(t.df)==1){
      tdd <- t.df
      df = data.frame(Name = groups,values = tdd, ind = rownames(tdd))
      colnames(df) = c("Name","values","ind")
      df$ind = rep(colnames(tdd)[1],nrow(df))
  }else{
      tdd <- t.df[,1:ncol(t.df)]
      df = data.frame(Name = groups,stack(tdd))
      if(length(grep("-",group))!=0) df$ind = chartr(df$ind,old = ".",new = "-")
  }
    
    colnames(df)[1] = "Name"
    df$values = as.numeric(as.character(df$values))
    colours <- c("#F4BD6D","#87CCC5","olivedrab3","brown2","goldenrod1","deepskyblue3","maroon2","mediumpurple4",
                 "cadetblue4","darkblue","hotpink3","indianred1","gray59",
                 "darkorange1","slategray3","grey95","mediumorchid3","paleturquoise1",
                 "orangered4","grey64","gray62","royalblue4","antiquewhite3")
    
    colours <- rep(colours,5000)
    colours <- colours[1:length(unique(groups))]
    
    p = ggplot()
    p = p + 
      geom_violin(data=df,aes(x    = ind, 
                              y    = values,
                              color = Name),
                  fill = "transparent",
                  color = "grey90") +
      geom_boxplot(data=df, 
                   aes(x    = ind, 
                       y    = values,
                       fill  = Name),
                   color = "grey56",
                   size  = .4)
    p = p + 
      labs(x    = xlab,
           y    = ylab,
           fill = "")
    
    p = p + 
      theme(
        # Axis
        axis.text.x      = element_text(angle = 45,  hjust=.95, vjust=.95,
                                        size  = xtextsize,colour="grey56"),
        axis.ticks.x     = element_line(colour= "grey"),
        axis.line.x      = element_line(colour= "grey90",linetype=1),
        
        axis.text.y      = element_text(angle = 0,  hjust=.95, vjust=.5,
                                        size  = 12,colour="grey56"),
        axis.ticks.y     = element_line(colour="grey"),
        axis.line.y      = element_line(colour="grey90",linetype=1),
        
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
                                    size  = 12,
                                    color ="grey56"),
        legend.position   = "right",
        legend.justification = "top",
        
        legend.key.size   = unit(.6, "cm"),
        legend.key.height = unit(.6, "cm"),
        legend.key.width  = unit(.6, "cm"),
        legend.text       = element_text(angle = 0,  
                                         hjust =.95, 
                                         vjust =.95,
                                         size  = 10,
                                         colour="grey56"),
        
        panel.grid       = element_blank(),
        #panel.border     = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.spacing.x  = element_blank(),
        panel.spacing.y  = element_blank(),
        
        panel.background = element_rect(
          colour = "grey90", 
          fill = "white", 
          size = .2, 
          linetype = 1) 
        #panel.background = element_blank()
      ) 
    
    #p = p + scale_x_discrete(expand = c(0,0)) +  scale_y_continuous(expand = c(0,0))
    
    #p = p + ylim(c(0,100))
    #p = p + guides(fill = guide_legend(ncol = lncol))
    #p = p + scale_color_manual(values = colours[1:dim(data.frame(table(df$Name)))[1]])
    p = p + scale_fill_manual(values = colours[1:dim(data.frame(table(df$Name)))[1]])
    
    if(!is.null(group_colour)) {col <- unlist(strsplit(group_colour,",")); p = p + scale_fill_manual(values = col)}
    
    if (!is.null(facet_grid)) {
      p <- p + facet_grid(facet_grid)
    }
    
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
  return(p)
}

