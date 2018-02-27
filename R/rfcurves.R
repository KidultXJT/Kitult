#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################
rfcurves <- function(
  input,
  output,
  outputFormat = "png",
  ytitle = "",
  group = NULL,
  group_colour = NULL,
  themes = "pale",
  blank = F,
  titlesize = 12,
  xsize = 12,
  xtitlesize = 12,
  ysize = 12,
  ytitlesize = 12,
  width  = 16,
  height = 8
){
  
  # library(ggplot2)
  # library(ggthemr)
  # 
  # input = "/Bio/Project/SGM/SGM1002-Homo_sapiens-Bone_Marrow-28-meta16S-v3v4-30000/03.Alpha/total/alpha_div_collated/observed_otus.txt"
  # input = "/Bio/User/liyewei/TEST/03.Alpha/total/alpha_div_collated/observed_otus.txt"
  
  t <- read.table(input, header = T, sep = "\t",check.names = F)
  
  # x = round(max(t[,2])/1000)*1000
  
  Name  = t[,2]

  colname = as.vector(colnames(t))[4:dim(t)[2]]
  Table   = as.matrix(t[,4:dim(t)[2]])
  Table[Table == "n/a"] = "100000000"
  Table = as.data.frame(matrix(as.numeric(Table),
                 nrow = dim(Table)[1]))
  colnames(Table) = colname
  
  df <- data.frame(Name =Name, stack(Table))
  if(length(grep("-",group))!=0) df$ind = chartr(df$ind,old = ".",new = "-")
  if(is.null(group)) {groups = colnames(Table)}else groups   = strsplit(group,",")[[1]]
  # groups = rep(c("D","N"),each  = 14)
  groups   = as.vector(rep(groups, each = length(Name)))
  # levels(df$Name) = ceiling(t[,2]/1000)*1000
  
  df$group = groups
  df <- df[df$values != 100000000,] 
  colnames(df) = c("Name","values","Samples","Groups")
  
  # colours <- c("deepskyblue3","maroon2","mediumpurple4",
  #              "cadetblue4","darkolivegreen3","darkblue","hotpink3","indianred1","gray59",
  #              "darkorange1","slategray3","grey95","mediumorchid3","paleturquoise1",
  #              "orangered4","grey64","gray62","royalblue4","antiquewhite3")
  # 
  # colours <- rep(colours,100)
  
  ggthemr(themes)
  tt = ggthemr(themes)
  colour = as.character(tt$palette$swatch)
  colours = c("#F4BD6D","#87CCC5","olivedrab3","brown2","goldenrod1","deepskyblue3","maroon2","mediumpurple4",
              "cadetblue4","darkblue","hotpink3","indianred1",
              "darkorange1","slategray3","grey95","mediumorchid3","paleturquoise1",
              "orangered4","grey10","royalblue4","antiquewhite3")
  swatch_colours = union(colour,colours)
  set_swatch(swatch_colours)
  

  
  if(is.null(group)){
    alphabox = ggplot(data = df,
                      aes(x = Name,
                          y = values))+
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
    stat_summary(data = df,
                 aes(x     = Name,
                     y     = values,
                     group = Samples,
                     colour= Samples),
                 fun.y  = mean,
                 # alpha  = .9,
                 geom   ="line",
                 size   =.5,
                 linetype   = 1)+ 
      labs(x = "Sequence per Sample",
           y = ytitle)
  }else{
    alphabox = ggplot(data = df,
                      aes(x = Name,
                          y = values))+
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
    stat_summary(data = df,
                 aes(x     = Name,
                     y     = values,
                     group = Groups,
                     colour= Groups),
                 fun.y  = mean,
                 # alpha  = .9,
                 geom   ="line",
                 size   =.5,
                 linetype   = 1)+ 
      labs(x = "Sequence per Sample",
           y = ytitle)
  }
  
  if(length(unique(df$Groups))> length(swatch_colours)) alphabox = alphabox + 
    scale_colour_manual(values = colours[1:length(unique(df$Groups))])+
    guides(colour = guide_legend(ncol = 2))
  
   alphabox = alphabox + 
     theme(
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
  
   alphabox = alphabox + 
     scale_colour_manual(values = colours[1:length(unique(df$Groups))]) 
     # guides(colour = guide_legend(ncol = 2))
   
   alphabox = alphabox + theme(legend.key.size=unit(0.5,'cm'))
   
   if(blank) alphabox = alphabox + theme(panel.grid =  element_blank())
   
   alphabox = alphabox + scale_x_continuous(breaks = seq(0,ceiling(max(t[,2])/10000)*10000,10000),
                                            labels = c(seq(0,ceiling(max(t[,2])/10000)*10000,10000)))
              # scale_y_continuous(breaks = seq(0,ceiling(max(df[,2])/100)*100,100),
              #                    labels = c(seq(0,ceiling(max(df[,2])/100)*100,100)))
              # 
   if(!is.null(group)) alphabox = alphabox + stat_summary(data = df,
                                                          aes(x     = Name,
                                                              y     = values,
                                                              group = Samples,
                                                              colour= Groups),
                                                          fun.y  = mean,
                                                          alpha  = .2,
                                                          geom   ="line",
                                                          size   =.5,
                                                          linetype   = 2) 
   
   if(!is.null(group_colour)) {col <- unlist(strsplit(group_colour,",")); alphabox = alphabox + scale_colour_manual(values = col)}
   
   # if(is.null(group)) alphabox = alphabox + guides(fill=guide_legend(title="Samples"))
  
  # rm(alphabox)
   
   
   
   Format   = strsplit(outputFormat,",")[[1]]
   sapply(Format,function(x){
     if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = width, height = height)")
     else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
     eval(parse(text = temp))
     # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
     plot(alphabox)
     dev.off()
     
   })
   return(alphabox)
   
   # ggsave(output,
   #        alphabox,
   #        width  = 16,
   #        height = 8)
   # device = output_format
   # unlink(output)
}
