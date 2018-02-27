#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################
format_otu <- function(
  input,
  output,
  withTax = T
){
  # input = "/Bio/User/liyewei/test/02.OTU/de_novo/0.001/0.001_otu_table.txt"
  
  # library(stringr)
  
  t = read.table(input,header = T, sep = "\t", skip = 1, comment.char = "@",check.names = F)
  colnames(t)[1] = "OTU.ID"
  write.table(t[,-ncol(t)],file = output,sep = "\t",row.names = F,quote=F)
  
  if(withTax){
    tax = t[,ncol(t)]
    
    tmp = strsplit(as.character(tax),";")
    
    newtax = t(sapply(tmp,function(tmp){
      if(length(tmp)!=7) result = c(tmp,rep("Other",7-length(tmp)))
      else result = tmp
      result                                   
    }))
    colnames(newtax) = c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
    result = cbind(t[,-ncol(t)],newtax)
    
    output = paste0(unlist(strsplit(output,".xls")),"_all.xls")
    write.table(result,file = output,sep = "\t",row.names = F,quote=F)
  }
  
}


