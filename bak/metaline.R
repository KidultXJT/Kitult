#########################################
###### Author:Katharine Lee #############
############## 2017.08 ##################
#########################################
metaline <- function(
  input,
  output,
  outputFormat = "png",
  split = NULL,
  split_colour = NULL,
  themes = "pale",
  main = "",
  xlab = 'Position',
  ylab = 'Reads',
  linesize = 1,
  xstart = 0,
  xend = 100,
  xstep = 20,
  titlesize = 12,
  xsize = 12,
  xtitlesize = 12,
  ysize = 12,
  ytitlesize = 12,
  width  = 10,
  height = 6
){
  # library(ggplot2)
  # library(dplyr)
  # # library(readxl)
  # # library(xlsx)
  # library(gtable)
  # library(ggthemr)
  
  # df = data.frame(y = runif(n = 100, min = 50, max = 500),x = seq(1,100,1))
  
  df = read.table(input, sep = "\t",comment.char = "@",header = T,check.names = F)
  colnames(df) = c("x","y")
  
  ggthemr(themes)
  t = ggthemr(themes)
  colour = as.character(t$palette$swatch)
  colours = c("#F4BD6D","#87CCC5","olivedrab3","brown2","goldenrod1","deepskyblue3","maroon2","mediumpurple4",
              "cadetblue4","darkblue","hotpink3","indianred1",
              "darkorange1","slategray3","grey95","mediumorchid3","paleturquoise1",
              "orangered4","grey10","royalblue4","antiquewhite3")
  swatch_colours = union(colour,colours)
  set_swatch(swatch_colours)
  
  # split=c(50,150,250,350,500)
  if(is.null(split)) split = as.numeric(round(quantile(df$y))) else split = as.numeric(unlist(strsplit(split,",")))
  
  newdf = NULL
  for(i in 2:nrow(df)){
    idx = (split > min(df$y[i:(i-1)])) & (split <= max(df$y[i:(i-1)]))
    if(any(idx)){
      if(df$y[i]>df$y[i-1]){
        addy = split[idx]
      }else if(df$y[i]<df$y[i-1]){
        addy = rev(split[idx])
      }
      addx = seq(from=df$x[i-1],to=df$x[i],length.out=sum(idx)+2)[-c(1,sum(idx)+2)]
      add = data.frame(y=addy,x=addx)
      add = rbind(df[i-1,],add)
      newdf = rbind(newdf, add)
    }else{
      newdf = rbind(newdf,df[i-1,])
    }
  }
  newdf = rbind(newdf,df[nrow(df),])
  
  
  cut_low = split[1:(length(split)-1)]
  cut_up = split[2:length(split)]
  aa = c()
  for(i in 2:nrow(newdf)){
    idx1 = cut_low <= newdf$y[i] & cut_up >= newdf$y[i] 
    idx2 = cut_low <= newdf$y[i-1] & cut_up >= newdf$y[i-1]
    
    if(length(which(idx1&idx2))!=0){
      aa = c(aa,min(which(idx1&idx2)))}else print(i)
  }
  newdf$Groups=c(aa,aa[length(aa)])
  
  newdf$Groups= as.character(newdf$Groups)
  
  ggline = ggplot()+
    geom_line(data = newdf, aes(x = x, y = y,colour = Groups,group = 1),size = linesize)+
    scale_colour_manual(name = NULL, 
                        label = paste0(cut_low,"~",cut_up), 
                        values = swatch_colours[1:(length(split)-1)])+
    # theme(plot.title = element_text(lineheight=3, face="bold", color="black",size=24)) +
    theme(
      #        ##AES 
      axis.text.x      = element_text(hjust=.5, vjust=.5,
                                      size=xsize),
      # axis.line.x      = element_line(colour= "#d1d1d1",size = 0.5,linetype=1),
      
      axis.text.y      = element_text(hjust=.95, vjust=.5,
                                      size=ysize),
      # axis.line.y      = element_line(colour="#d1d1d1",size = 0.5,linetype=1),
      #                          
      axis.title.x     = element_text(size=xtitlesize),
      axis.title.y     = element_text(size=ytitlesize),
      title     = element_text(size=titlesize),
      
      ## Background
      # panel.background = element_rect(fill = input$bgcolours, colour = "white",size = 1.5),
      panel.border = element_rect(fill = NA, colour = "#d1d1d1",size = 0.5,linetype = 1),
      
      
      #        ## Legend
      legend.justification = "top",
      legend.position  =  "right",    #c(.8, .3),
      legend.title= element_blank(),
      legend.key.size=unit(1,'cm'),
      legend.text = element_text(size = 12)
    )
  s = ifelse(xstart >= min(as.numeric(as.character(df$x))-5),xstart,min(as.numeric(as.character(df$x))))
  e = ifelse(xend <= max(as.numeric(as.character(df$x))+5),xend,max(as.numeric(as.character(df$x))))
  ggline = ggline + scale_y_continuous(breaks = split) + 
    scale_x_continuous(limits = c(s, e), breaks=seq(s,e,xstep))
  ggline = ggline + labs(
    title = main,
    x = xlab,
    y = ylab)
 
  if(!is.null(split_colour)) {
    col <- unlist(strsplit(split_colour,","))
    ggline <- ggline + scale_colour_manual(name = NULL, 
                                        label = paste0(cut_low,"~",cut_up), 
                                        values = col)
  }
  
  Format   = strsplit(outputFormat,",")[[1]]
  sapply(Format,function(x){
    if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = width, height = height)")
    else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
    eval(parse(text = temp))
    # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
    plot(ggline)
    dev.off()
    
  })
  return(ggline)
}
