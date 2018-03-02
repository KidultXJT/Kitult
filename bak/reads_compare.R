#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################

reads_compare = function(
  input,
  output,
  outputFormat = "png",
  group = NULL,
  # themes = "pale","grey70",
  # names = NULL,
  thredhold = NULL,
  # bar_colours = NULL,
  width  = 16,
  height = 8
){
  # library(ggplot2)
  # library(scales)
  # library(gtable)
  # library(dplyr)
  # 
  # input = system.file("example", "total_otu_stat.xls", package = "metaplot")
  # input = "~/mix004.xls"
  
  t = read.table(input,header = T,sep = "\t",check.names = F)[,-1]
  
  if(nrow(t)==2){
    df = data.frame(Samples = colnames(t),otu_num = t(t[1,]),count_num = t(t[2,]))[-1:-5,]
    colnames(df) = c("Samples","OTUs","Tags")
    df$OTUs = as.numeric(df$OTUs)
    df$Tags = as.numeric(df$Tags)
    df$name = as.character(1)
    
    if(!is.null(group)) {
      group <- unlist(strsplit(group,","))
      row_names = dplyr::arrange(data.frame(sample = df$Samples, group = group),group)$sample
      df$Samples = factor(df$Samples,levels = row_names)
    }
    
    AXIS1_MIN = min(df$Tags)
    AXIS1_MAX = max(df$Tags)
    AXIS2_MIN = min(df$OTUs)
    AXIS2_MAX = max(df$OTUs)
    
    scale_to_value1 <- function(values) rescale(values, to = c(AXIS1_MIN, AXIS1_MAX))
    scale_to_value2 <- function(values) rescale(values, to = c(AXIS2_MIN, AXIS2_MAX))
    
    p = ggplot() +
      geom_bar(aes(x = Samples,y = Tags,fill =name) ,position = "dodge",data = df ,stat="identity",size=1) +
      geom_line(aes(x = c(1:nrow(df)),y = scale_to_value1(OTUs),colour = name),data = df,size=1) +
      geom_point(aes(x = c(1:nrow(df)),y = scale_to_value1(OTUs)),data = df,size=3,colour = 'olivedrab3')+
      scale_y_continuous(
        breaks=pretty_breaks(n=7),
        # limits=c(AXIS1_MIN, AXIS1_MAX),
        # expand = c(0,0),
        sec.axis = sec_axis( ~ scale_to_value2(.),
                             breaks=pretty_breaks(n=7),
                             name = "Number of OTUs"))+
      scale_colour_manual(name = NULL, 
                          label = "OTUs", 
                          values = "olivedrab3")+
      scale_fill_manual(name = NULL,
                        label = "Tags",
                        values = "grey85") +
      xlab('Samples')+ylab('Number of Tags')+
      # theme(plot.title = element_text(lineheight=3, face="bold", color="black",size=24)) +
      theme(legend.text=element_text(size=14),
            legend.title=element_text(size=14))+
      theme_bw()+
      # theme(axis.text.x=element_text(angle=90))+
      theme(legend.position = "bottom")+
      theme(axis.text.x=element_text(hjust=.5, vjust=.5,angle=90,colour="grey20",size=12),
            axis.text.y=element_text(hjust=.5, vjust=.5,colour="grey20",size=12),
            axis.title.x=element_text(colour="grey20",size=12),
            axis.title.y=element_text(colour="grey20",size=12),
            legend.text = element_text(size = 12))
    
    if(!is.null(thredhold)) p = p + geom_hline(yintercept = thredhold,colour="tomato", linetype="dashed")
    
    # if(!is.null(colour)) {col <- unlist(strsplit(colour,","))} else col = c("grey50","grey70")
  }
  
  if(nrow(t)==3){
    reads = data.frame(Samples = colnames(t),count_num = t(t[2,]))[-1:-5,]
    colnames(reads) = c("Samples","num")
    reads$Samples = unlist(strsplit(as.character(reads$Samples),":"))
    reads$type = "clean"
    
    raw = data.frame(Samples = colnames(t),raw = t(t[3,]))[-1:-5,]
    colnames(raw) = c("Samples","num")
    raw = raw[which(raw$Samples %in% reads$Samples),]
    raw$type = "raw"
    
    if(!is.null(group)) {
      group <- unlist(strsplit(group,","))
      row_names = dplyr::arrange(data.frame(sample = raw$Samples, group = group),group)$sample
      raw$Samples = factor(raw$Samples,levels = row_names)
      reads$Samples = factor(reads$Samples,levels = row_names)
    }
    
    df = data.frame(rbind(raw,reads))
    df$type = factor(df$type,levels = c("raw","clean"))
    
    otus = data.frame(Samples = colnames(t),otu_num = t(t[1,]))[-1:-5,]
    colnames(otus) = c("Samples","otu_num")
    otus$Samples = unlist(strsplit(as.character(otus$Samples),":"))
    otus$otu_num = as.numeric(otus$otu_num)
    rownames(otus) = otus$Samples
    otus = otus[which(otus$Samples == unique(df$Samples)),]
    otus$name = as.character(1)
    otus = otus[levels(df$Samples),]
    
    AXIS1_MIN = min(df$num)
    AXIS1_MAX = max(df$num)
    AXIS2_MIN = min(otus$otu_num)
    AXIS2_MAX = max(otus$otu_num)
    
    scale_to_value1 <- function(values) rescale(values, to = c(AXIS1_MIN, AXIS1_MAX))
    scale_to_value2 <- function(values) rescale(values, to = c(AXIS2_MIN, AXIS2_MAX))
    
    p = ggplot() +
      geom_bar(aes(x = Samples,y = num,fill = type) ,position = "dodge",data = df ,stat="identity",size=1) +
      geom_line(aes(x = c(1:nrow(otus)),y = scale_to_value1(otu_num),colour = name),data = otus,size=1) +
      geom_point(aes(x = c(1:nrow(otus)),y = scale_to_value1(otu_num)),data = otus,size=3,colour = 'olivedrab3')+
      scale_y_continuous(
        breaks=pretty_breaks(n=7),
        # limits=c(AXIS1_MIN, AXIS1_MAX),
        # expand = c(0,0),
        sec.axis = sec_axis( ~ scale_to_value2(.),
                             breaks=pretty_breaks(n=7),
                             name = "Number of OTUs"))+
      scale_colour_manual(name = NULL, 
                          label = "OTUs", 
                          values = "olivedrab3")+
      scale_fill_manual(name = NULL,
                        label = c("Valid Tags","Non-chimera Tags"),
                        values = c("grey65","grey85")) +
      # theme(axis.text.x=element_text(angle=45,colour="grey20",face="bold",size=12),
      #       axis.text.y=element_text(colour="grey20",face="bold",hjust=1,vjust=0.8,size=15),
      #       axis.title.x=element_text(colour="grey20",face="bold",size=16),
      #       axis.title.y=element_text(colour="grey20",face="bold",size=16)) +
      xlab('Samples')+ylab('Number of Tags')+
      # theme(plot.title = element_text(lineheight=3, face="bold", color="black",size=24)) +
      theme(legend.text=element_text(size=14),
            legend.title=element_text(size=14))+
      theme_bw()+
      # theme(axis.text.x=element_text(angle=90))+
      theme(legend.position = "bottom")+
      theme(axis.text.x=element_text(hjust=.5, vjust=.5,angle=90,colour="grey20",size=12),
            axis.text.y=element_text(colour="grey20",size=12),
            axis.title.x=element_text(colour="grey20",size=12),
            axis.title.y=element_text(colour="grey20",size=12),
            legend.text = element_text(size = 12))
    
    if(!is.null(thredhold)) p = p + geom_hline(yintercept = thredhold,colour="tomato", linetype="dashed")
    
    # if(!is.null(colour)) {col <- unlist(strsplit(colour,","))} else col = c("grey50","grey70")
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
