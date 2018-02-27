#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################
metaroc <-function(
  input,
  output,
  outputFormat = "png",
  zero = F,
  blank = T,
  percent = F,
  themes = "pale",
  main = "ROC Curve",
  titlesize = 12,
  xsize = 12,
  xtitlesize = 12,
  ysize = 12,
  ytitlesize = 12,
  linesize = 1,
  width  = 16,
  height = 12
){
  
  # library(ggplot2)
  # library(pROC)
  # library(readxl)
  #install.packages("xlsx")   for write the formula excel.xlsx filt
  # library(xlsx)
  #install.packages("latex2exp")
  # library(latex2exp)
  #install.packages("pROC")
  # library(pROC)
  #install.packages("plotROC")
  # library(plotROC)
  # # require(devtools)
  # # devtools::install_github('cttobin/ggthemr')
  # library(ggthemr)
  # library(dplyr)
  
  # input = "/Bio/User/liyewei/Example.xlsx"
  df <- read_excel(input, 1)
  df <- as.data.frame(df)
  
  df.roc <- data.frame()
  df.auc <- data.frame()
  
  for(n in 2:dim(df)[2]){
    if(percent){
      roc  <- assign(paste("roc", n, sep = "_"),
                     roc(df[,1], df[,n], percent=T, direction = "auto",ci = T))
    }else{
      roc  <- assign(paste("roc", n, sep = "_"),
                     roc(df[,1], df[,n], direction = "auto",ci = T))
    }
    
    auc  <- assign(paste("auc",n,sep = "_"),
                   c(round(roc$ci[2],3),paste0("(",round(roc$ci[1],3),", ",round(roc$ci[3],3),")")))
    # direction <- assign(paste("direction",n,sep = "_"),
    #                     roc$direction)
    
    #scale_y_reverse()
    # roc$thresholds
    sens <- assign(paste("sens",n, sep = "_"), sort(roc$sensitivities, decreasing=T) )
    if(percent){sps <- assign(paste("sps",n, sep = "_"), 100- roc$specificities)}else{
      sps <- assign(paste("sps",n, sep = "_"), 1- roc$specificities)
    }
    
    
    df.s <- data.frame(Sensitivities = sens,
                       Specificities = sps,
                       Variables  =  paste0(colnames(df)[n]," (Auc = ",auc[1],")"))
    
    df.roc <- rbind(df.roc,df.s)
    
    df.auc <- c(df.auc, auc)
    
    ##(ggroc <- ggroc + geom_line(data=df.s, aes(x=Specificities, y=Sensitivities)))
  }
  
  df.auc = unlist(df.auc)
  
  # levels(df.auc) = colnames(df)[-1]
  # colnames(df.auc) = c("Auc","Condifence Interval")
  
  ggthemr(themes)
  t = ggthemr(themes)
  colour = as.character(t$palette$swatch)
  swatch_colours = union(colour,c("olivedrab3","brown2","goldenrod1","deepskyblue3","maroon2","mediumpurple4",
                                  "cadetblue4","darkblue","hotpink3","indianred1",
                                  "darkorange1","slategray3","grey95","mediumorchid3","paleturquoise1",
                                  "orangered4","grey10","royalblue4","antiquewhite3"))
  set_swatch(swatch_colours)
  
  ggroc <- ggplot()
  ggroc <- ggroc +
    geom_path(data = df.roc,
              aes(x = Specificities,y = Sensitivities, colour = Variables))
  
  if(percent){ ggroc <- ggroc + labs(title = main, x = "False Positive Rate (100-Specificity)", 
                                     y = "True Positive Rate (Sensitivity)")}else{
                                       ggroc <- ggroc + labs(title = main, x = "False Positive Rate (1-Specificity)", 
                                                             y = "True Positive Rate (Sensitivity)")     
                                     }
  # scale_colour_brewer(type = "qual") +
  ggroc <- ggroc + theme(
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
    legend.text = element_text(size = 12)
  )

  
  ggroc <- ggroc + geom_abline(slope = 1,color = "#d1d1d1",size = 0.5,linetype = 1)
  
  
  ggroc = ggroc + theme(legend.key.size=unit(1,'cm'))
  
  if(!blank) ggroc = ggroc + theme(panel.grid =  element_blank())
  
  if(zero) ggroc = ggroc +  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))
  
  # method = "delong"
  # alternative = "two.sided"
  # expr = paste0("roc.test(roc_2,roc_3,method =\'",method,"\',alternative = \'",alternative,"\')")
  # t = eval(parse(text = expr))
  # t = as.numeric(t$p.value)
  # if(t<0.0001) {t = "far more less than 0.0001"} else t = round(t,6)
  # 
  # ggroc = ggroc + geom_text(aes(x = 0.7, y = 0.3),
  #                           label = paste0("pvalue is ",t),
  #                           colour = "grey20",
  #                           size = 4.8)
  
  Format   = strsplit(outputFormat,",")[[1]]
  sapply(Format,function(x){
    if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = width, height = height)")
    else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
    eval(parse(text = temp))
    # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
    plot(ggroc)
    dev.off()
  })
  
  # ggsave(output,
  #        ggroc,
  #        width  = 16,
  #        height = 12)
  return(ggroc)
}






# plot(ggroc)

# auc = data.frame()
# auc[1:length(colnames(df)[-1]),1] = colnames(df)[-1]
# for(i in 1:(length(df.auc)/2)){
#   auc[i,2] = df.auc[2*i-1]
#   auc[i,3] = df.auc[2*i]
# }
# colnames(auc) = c("Variables","AUC","Confidence Interval")
# auc = arrange(auc,desc(AUC))
