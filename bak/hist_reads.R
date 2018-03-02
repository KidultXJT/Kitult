#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################
hist_reads <- function(
  input,
  output,
  outputFormat = "png",
  output_table,
  line_colour = "grey20",
  text_colour = "grey35",
  width  = 10,
  height = 6
){
  # library(ggplot2)
  # library(dplyr)
  # library(ggpubr)
  # input = "/Bio/User/liyewei/mix001/04.SplitLib/count_len.txt"
  id = character()
  num = numeric()
  con = file(input,"r")
  line=readLines(con,n=1,warn = F)
  line=readLines(con,n=1,warn = F)
  while(length(line)!= 0){
    if(line!="" & line!="--"){
      id = c(id,as.numeric(unlist(strsplit(line,"\t")))[1])
      num = c(num,as.numeric(unlist(strsplit(line,"\t"))[2]))
    }
    line=readLines(con,n=1,warn = F)
  }
  close(con)
  
  df = data.frame(Length = as.numeric(id), Tags=as.numeric(num))
  df = na.omit(df)
  df$Length = floor(df$Length/10)*10
  df = summarise(group_by(df,Length),Tags = sum(Tags))
  
  
  485646
  min = min(round(df$Length/100+0.1)*100)
  max = max(round(df$Length/100+0.1)*100)
  
  # dff = data.frame(Length = rep(df$Length,df$Tags))
  # 
  # ggdensity(dff, x = "Length", fill ="skyblue2",add = "mean")+
  #   scale_x_continuous(limits = c(min,max-20),breaks = seq(min,max,10),labels = seq(min,max,10))+
  #   theme_bw()
  
  p = ggplot(data = df)+
    geom_line(aes(x = Length, y = Tags/sum(Tags)),size = 1,colour = line_colour)+
    geom_point(aes(x = Length, y = Tags/sum(Tags)),size = 1,colour = line_colour)+
    geom_text(aes(x = Length, y = Tags/sum(Tags)+0.03),label = df$Tags,colour = text_colour)+
    scale_x_continuous(breaks = seq(min,max,10),labels = seq(min,max,10))+ ylab("Ratio")+
    theme_bw()
  
  Format   = strsplit(outputFormat,",")[[1]]
  sapply(Format,function(x){
    if(x == "pdf") temp = paste0("cairo_pdf(file = paste0(output,\'.\',x), bg = \'transparent\', width = width, height = height)")
    else temp = paste0(x,"(file = paste0(output,\'.\',x), bg = \'transparent\', width = width*72, height = height*72, type = \'cairo\')")
    eval(parse(text = temp))
    # png(file = paste0(output_hc,".",x), bg = "transparent", width = width, height = height)
    plot(p)
    dev.off()
  })
  
  ans = t(df)
  write.table(ans,col.names = F,file = output_table,sep = "\t",row.names = T,quote=F)
}

