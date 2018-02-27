#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################
format_sp <- function(
  input,
  output,
  select = 7,
  rm = T
){
  # input = "/Bio/User/liyewei/total_otu_table_L3.txt"
  # input = "/Bio/Project/SGM/SGM1002-Homo_sapiens-Bone_Marrow-28-meta16S-v3v4-30000/05.Taxonomy/total/relative_abundance/Dominant/total_otu_table_L3.txt"
  # input = '~/test100/05.Taxonomy/total/relative_abundance/Dominant/total_otu_table_L1.txt'
  # library(stringr)
  # library(dplyr)
  
  t = read.table(input, header = T, sep = "\t", skip = 1, comment.char = "@")
  tax = t[,1]
  
  tmp = strsplit(as.character(tax),";")
  
  newtax = t(sapply(tmp,function(tmp){
    if(length(tmp)!=7) result = c(tmp,rep("Other",7-length(tmp)))
    else result = tmp
    result                                   
  }))
  colnames(newtax) = c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
  
  levels(colnames(newtax)) = c(1,2,3,4,5,6,7)
  
  try(if(!select %in% levels(colnames(newtax))) stop("No this select! Please input:Kingdom(1),Phylum(2),Class(3),Order(4),Family(5),Genus(6),Species(7)"))
  
  
  
  
  if(select != 1 ) {
    result = cbind(newtax[,c(which(select == levels(colnames(newtax)))-1,which(select == levels(colnames(newtax))))],t[,-1])
    
    id = which(sapply(1:ncol(result),function(i) is.numeric(result[,i])))
    expr = paste0("group_by(result,",colnames(result)[2],",",colnames(result)[1],")")
    result = eval(parse(text = expr))
    expr = paste0(colnames(result[,id]),"=sum(",colnames(result[,id]),")",collapse = ",")
    expr = paste0("dplyr::summarise(result,",expr,")")
    result = as.data.frame(eval(parse(text = expr)))
    
    names = list("Unknown",'Other','Ambiguous','uncultured','Unclassified','unidentified','_sp') ##the undentified key word
    id = sort(unlist(sapply(names,function(word) grep(word,result[,1], ignore.case = T))))
    id = c(id,which(result[,1] %in% paste0(c("k","p","c","o","f","g","s"),"__")))
    result[,1] = as.character(result[,1])
    result[id,1] = "Unidentified"
    
    tmp = result[-id,]
    if(nrow(tmp)!=0) {
      names = unique(tmp[,1])
      names = names[unlist(sapply(1:length(names), function(i) if(length(which(tmp[,1]==names[i]))!=1) return(i)))]
      if(length(names)!=0) {
        
        for(i in 1:length(names)){
          id_tmp = which(tmp[,1] == names[i])
          tmp[id_tmp,1] = paste0(names[i],"_",1:length(id_tmp))
        }
        
        result[-id,] = tmp
      }
    }
  
    }else{
      result = cbind(newtax[,which(select == levels(colnames(newtax)))],t[,-1])
      colnames(result)[1] = colnames(newtax)[select]
    }
  
  
  id = which(sapply(1:ncol(result),function(i) is.numeric(result[,i])))
  expr = paste0("group_by(result,",colnames(newtax)[select],")")
  result = eval(parse(text = expr))
  expr = paste0(colnames(result[,id]),"=sum(",colnames(result[,id]),")",collapse = ",")
  expr = paste0("dplyr::summarise(result,",expr,")")
  result = as.data.frame(eval(parse(text = expr)))
  
  names = colnames(read.table(input, header = T, sep = "\t", skip = 1, comment.char = "@",check.names = F))[-1]
  colnames(result)[-1] = names
  if(rm) write.table(result[which(result[,1]!="Unidentified"),],file = output,sep = "\t",row.names = F,quote=F)
  else write.table(result ,file = output,sep = "\t",row.names = F,quote=F)
}


