#########################################
###### Author:Katharine Lee #############
############## 2017.07 ##################
#########################################

format_compare = function(
  input_clean,
  input_otu,
  input_raw = NULL,
  sample,
  output
){
  
  # library(dplyr)
  
  # input_otu = system.file("example", "otu.txt", package = "metaplot")
  # input_clean = system.file("example", "count.txt", package = "metaplot")
  # input_raw = system.file("example", "split_library_log.txt", package = "metaplot")
  # sample = "A1,A2,B1,B2,B3,B4,B5,B6,C1"
  
  sample = unlist(strsplit(sample,","))
  
  readfile = function(input,sample,sep = ":"){
    id = character()
    num = numeric()
    con = file(input,"r")
    line=readLines(con,n=1,warn = F)
    while(length(line)!= 0){
      if(unlist(strsplit(line,sep))[1] %in% sample) {
        id = c(id,unlist(strsplit(line,sep))[1])
        num = c(num,as.numeric(unlist(strsplit(line,sep))[2]))
        # print(line)
      }
      line=readLines(con,n=1,warn = F)
    }
    close(con)
    return(data.frame(id = id,num=as.numeric(num)))
  }
  
  otu = readfile(input_otu,sample)
  clean = readfile(input_clean,sample)
  
  result = merge(otu,clean,by = "id")
  colnames(result) = c("id","OTUs","Clean Tags")
  
  if(!is.null(input_raw)){
    raw = readfile(input_raw,sample,sep = "\t")
    result = merge(result,raw,by = "id")
    colnames(result) = c("id","OTUs","Non-chimera Tags","Valid Tags")
  }
  
  colnames(result)[1] = "Samples"
  ans = data.frame(t(result[,-1]))
  colnames(ans) = result$Samples 
  
  Min = as.numeric(apply(ans,1,min))
  Max = as.numeric(apply(ans,1,max))
  Median =as.numeric(apply(ans,1,median))
  Mean = round(as.numeric(apply(ans,1,mean)),2)
  Sd = round(as.numeric(apply(ans,1,sd)),2)
  stat = cbind(Min,Max,Median,Mean,Sd)
  
  ans = cbind(rownames(ans),stat,ans)
  colnames(ans)[1] = "Samples"
  
  write.table(ans,file = output,sep = "\t",row.names = F,quote=F)
}
