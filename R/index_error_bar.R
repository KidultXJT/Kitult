#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################

index_error_bar <- function(
  input,
  output,
  outputFormat = "png",
  caculate = T,
  scale = F,
  group = NULL,
  group_colour = NULL,
  select = "ace",
  error = "se",
  pvalue = 0.05,
  main = "",
  xlab = "",
  ylab = "",
  themes = "pale",
  blank = F,
  titlesize = 12,
  xsize = if(caculate) 12 else 10,
  xtitlesize = 12,
  ysize = 12,
  ytitlesize = 12,
  error_size = 0.1,
  width  = if(caculate) 10 else 12,
  height = if(caculate) 8 else 9
){
  # library(ggplot2)
  # library(dplyr)
  # library(RColorBrewer)
  # library(ggthemr)
  # library(dplyr)
  # library(plyr)
  # input = "/Bio/User/liyewei/test/06.Diff/metastats/example"
  # group = "a,b,a,b,a,b,a,b,a,b"
  # input = "/Bio/User/liyewei/test/03.Alpha/total/total_alpha_indices.xls"
  
  if(caculate == T){
    in_table <- read.table(input, header = T,check.names = F)
    group <- unlist(strsplit(group,","))
    in_table$Groups = group
    
    summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                          conf.interval=.95, .drop=TRUE) {
      
      # New version of length which can handle NA's: if na.rm==T, don't count them
      length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
      }
      
      # This does the summary. For each group's data frame, return a vector with
      # N, mean, and sd
      datac <- plyr::ddply(data, groupvars, .drop=.drop,
                     .fun = function(xx, col) {
                       c(N    = length2(xx[[col]], na.rm=na.rm),
                         mean = mean   (xx[[col]], na.rm=na.rm),
                         sd   = sd     (xx[[col]], na.rm=na.rm)
                       )
                     },
                     measurevar
      )
      
      # Rename the "mean" column    
      datac <- plyr::rename(datac, c("mean" = measurevar))
      
      datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
      
      # Confidence interval multiplier for standard error
      # Calculate t-statistic for confidence interval: 
      # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
      ciMult <- qt(conf.interval/2 + .5, datac$N-1)
      datac$ci <- datac$se * ciMult
      
      return(datac)
    }
    
    # var = c("ace","chao1","shannon","simpson")
    df = summarySE(in_table, measurevar=select, groupvars= "Groups")
    
    expr = paste0("ggindex = ggplot(data = df,aes(x  =","Groups",",y  = ",select,",fill = ","Groups","))")
    eval(parse(text = expr))
    
    ggindex = ggindex + geom_bar(position=position_dodge(),
                                 stat="identity",
                                 width  = .75,
                                 # alpha  = 0.85,
                                 colour = "white")
    
    expr = paste0("ggindex = ggindex + geom_errorbar(aes(ymin=",select,"-",error,", ymax=",select,"+",error,"),width=",as.character(error_size), ", colour = \'grey50\',position=position_dodge(.75))")
    eval(parse(text = expr))
    
    ggindex = ggindex + 
      theme(axis.text.x  = element_text(hjust=.5, vjust=.5,
                                        size=xsize))
  }
  
  if(caculate == F){
    in_table = read.table(input, header = T, sep = "\t")
    in_table = in_table[which(in_table[,2]!=""),]
    in_table = in_table[which(in_table[,ncol(in_table)]<=pvalue),]
    in_table = in_table[which(in_table[,ncol(in_table)]!=0),]
    if(nrow(in_table)!=0){
      in_table = in_table[,c(-1,-9)]
      
      df = in_table[,1:4]
      colnames(df) = c("Species","mean","sd","se")
      df$Groups = unlist(strsplit(colnames(in_table)[2],"[.]"))[2]
      
      tmp = data.frame(in_table[,c(1,5:7)])
      colnames(tmp) = c("Species","mean","sd","se")
      tmp$Groups = unlist(strsplit(colnames(in_table)[5],"[.]"))[2]
      df = rbind(df,tmp)
      
      df$sd = sqrt(df$sd)
      df$mean[which(df$mean==0)] = 1e-16
      
      if(scale){
        expr = paste0("ggindex = ggplot(data = df,aes(x  =","Species",",y  = ","mean",",fill = ","Groups","))")
        eval(parse(text = expr))
        
        ggindex = ggindex + geom_bar(position=position_dodge(),
                                     stat="identity",
                                     width  = .75,
                                     # alpha  = 0.85,
                                     colour = "white")
        
        expr = paste0("ggindex = ggindex + geom_errorbar(aes(ymin=","mean","-",error,", ymax=","mean","+",error,"),width=",as.character(error_size), ", colour = \'grey50\',position=position_dodge(.8))")
        eval(parse(text = expr))
      
        ggindex = ggindex + scale_y_continuous(trans = "log10")
        # expr = paste0("ggindex = ggindex + ylim(min(log(df$mean,base = 0.1)),max(log(df$mean+df$",error,",base = 0.1)))")
        # eval(parse(text = expr))
        # print(max(-log(df$mean,base = 0.1)))

      }else{
        expr = paste0("ggindex = ggplot(data = df,aes(x  =","Species",",y  = ","mean",",fill = ","Groups","))")
        eval(parse(text = expr))
        
        ggindex = ggindex + geom_bar(position=position_dodge(),
                                     stat="identity",
                                     width  = .75,
                                     # alpha  = 0.85,
                                     colour = "white")
        
        expr = paste0("ggindex = ggindex + geom_errorbar(aes(ymin=","mean","-",error,", ymax=","mean","+",error,"),width=",as.character(error_size), ", colour = \'grey50\',position=position_dodge(.8))")
        eval(parse(text = expr))
      }
     
      
      ggindex = ggindex + 
        theme(axis.text.x  = element_text(hjust=.5, vjust=.5,
                                          size=xsize, angle = 45))
    }
    
  }
  
  if(nrow(in_table)!=0){
    ggthemr(themes)
    t = ggthemr(themes)
    
    colour = as.character(t$palette$swatch)
    colours = c("#F4BD6D","#87CCC5","olivedrab3","brown2","goldenrod1","deepskyblue3","maroon2","mediumpurple4",
                "cadetblue4","darkblue","hotpink3","indianred1",
                "darkorange1","slategray3","grey95","mediumorchid3","paleturquoise1",
                "orangered4","grey10","royalblue4","antiquewhite3")
    swatch_colours = union(colour,colours)
    set_swatch(swatch_colours)

    ggindex <- ggindex + labs(
      title = main, 
      x = xlab,
      y = ylab)
    
    ggindex = ggindex + 
      theme(
        #        ##AES 
        # axis.text.x      = element_text(hjust=.5, vjust=.5,
        # size=xsize),
        axis.text.x=element_text(hjust=.5, vjust=.5,angle=90,size=12),
        
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
    
    # ggindex = ggindex + 
      # scale_colour_manual(values = colours[1:length(unique(df$Groups))]) +
      # guides(colour = guide_legend(ncol = 2))
    
    ggindex = ggindex + theme(legend.key.size=unit(0.5,'cm'))
    
    if(blank) ggindex = ggindex + theme(panel.grid =  element_blank())
    
    ggindex = ggindex + 
      scale_fill_manual(values = colours[1:length(unique(df$Groups))])
    
    if(!is.null(group_colour)) {col <- unlist(strsplit(group_colour,",")); ggindex = ggindex + scale_fill_manual(values = col)}
    # print(col)
   
    Format   = strsplit(outputFormat,",")[[1]]
    sapply(Format,function(x){
      if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = width, height = height)")
      else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
      eval(parse(text = temp))
      # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
      plot(ggindex)
      dev.off()
    })
    
  }
  return(ggindex)
}