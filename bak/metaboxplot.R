#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################

metaboxplot <- function(
  input,
  output,
  outputFormat = "png",
  group = NULL,
  xlab = "Samples",
  ylab = "Abundance",
  facet_grid = NULL,
  type = 3,
  xtextsize  = 12,
  width  = 16,
  height = 8
){
  # library(ggplot2)
  # library(dplyr)
  
  #input = '~/test/02.OTU/de_novo/0.001/0.001_tax_otus.xls'
  #input = '~/test100/02.OTU/de_novo/0.001/0.001_tax_otus.xls'
  #group = 'case,case,case,case,case,case,control,case,case,case,control,control,case,control,case,control,case,control,case,control,case,case,control,case,control,case,case,control,case,case,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,case,control,control,control,case,control,case,case,case,case,control,case,control,control,case,control,case,case,control,control,control,control,control,case,case,control'
  
  head(t <- read.table(input, sep = "\t",header = T,comment.char = "@",check.names = F)) 
  
  # otu Profile
  otuma <- t[1:dim(t)[1],2:(dim(t)[2])]
  otuma  <- lapply(otuma,FUN = as.character)
  otuma  <- as.matrix(data.frame(lapply(otuma,FUN = as.numeric)))
  otumat <- as.matrix(otuma)
  rownames(otumat) <- t[,1]
  # rownames(otumat) <- trowName

  OTU  <- data.frame(otumat)
  Name <- rownames(OTU)
  df   <- data.frame(Name = Name, stack(OTU))
  if(length(grep("-",group))!=0) df$ind = chartr(df$ind,old = ".",new = "-")
  
  if(!is.null(group)) {
    group <- unlist(strsplit(group,","))
    row_names = dplyr::arrange(data.frame(sample = colnames(t)[-1], group = group),group)$sample
    df$ind = factor(df$ind,levels = row_names)
  }
  
  
  table <- data.frame(table(df[,type]))
  nt    <- length(table[,1])
  
  p = ggplot()
  
  ## 22 colours
  colours <- c("olivedrab3","brown2","goldenrod1","deepskyblue3","maroon2","mediumpurple4",
               "cadetblue4","darkblue","hotpink3","indianred1","gray59",
               "darkorange1","slategray3","grey95","mediumorchid3","paleturquoise1",
               "orangered4","grey64","gray62","royalblue4","antiquewhite3")
  
  colours <- rep(colours,5000)
  colours <- colours[1:nt]
  
  
  p = p + geom_boxplot(data=df, 
                       aes(x    = df[,type], 
                           y    = values#,
                           #color= factor(tax.df[,type]),
                           #fill = factor(tax.df[,type])
                       ),
                       width  = .8,
                       size   = .4,
                       alpha  = .75,
                       colour = colours,
                       fill   = colours
  )
  p = p + 
    labs(x    = xlab,
         y    = ylab,
         fill = "")
  
  p = p + 
    theme(
      # Axis
      axis.text.x      = element_text(angle = 45,  hjust=.95, vjust=.95,
                                      size  = ceiling(xtextsize/(nrow(df)^(1/10))),colour="grey56"),
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
      legend.position   = "none",
      
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
  
  if (!is.null(facet_grid)) {
    p <- p + facet_grid(facet_grid)
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
  # ggsave(output,
  #        p,
  #        width  = 16,
  #        height = 8)
  
}