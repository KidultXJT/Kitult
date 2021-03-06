require(ggplot2)

ggBasicPoint <- function(
  Col=NULL, # When Use One Color
  size=4,
  alpha=.7,
  df=NULL,  # When Use This, Try To make a Basic Graph; Else, To make a Point Graph LAYER !!! 
  x=1,
  y=2
){
  # Description:
  # A point Graph for Basic Graph or A Layer
  
  # Package:
  require(ggplot2)
  
  if(is.null(Col)){
    point = geom_point(size = size, alpha = alpha)
  }else{
    point = geom_point(size = size, alpha = alpha, color = Col)
  }
  
  if(!is.null(df)){
    point = ggplot(data = df,
                   aes(x=df[,x],
                       y=df[,y])) + point
  }
  
  return(point)
}

ggBasicText <- function(
  Col=NULL, # When Use One Color
  size=4,
  df=NULL,  # When Use This, Try To make a Basic Graph; Else, To make a Text Graph LAYER !!! 
  x=1,
  y=2,
  label=3
){
  # Description:
  # A TEXT Graph for Basic Graph or A Layer
  # Package:
  require(ggplot2)
  
  if(is.null(Col)){
    text = geom_text(size = size)
  }else{
    text = geom_text(size = size,
                     color = Col)
  }
  
  if(!is.null(df)){
    
    df$x = factor(df$x)
    text = ggplot(data = df,
                  aes(x=df[,x],
                      y=df[,y],
                      label=df[,label])) + 
      text
  }
  
  return(text)
}

ggBasicFreqBar <- function(
  Col=NULL,     # When Use One Color
  Trans=T,  # When make a Basic Graph
  width=.4,
  alpha=.7,
  df=NULL,  # When Use This, Try To make a Basic Graph; Else, To make a Point Graph LAYER !!! 
  x=1,
  y=2
){
  # Description:
  # A point Graph for Basic Graph or A Layer
  
  # Package:
  require(ggplot2)
  
  if(is.null(Col)){
    bar = geom_bar(width = width, 
                   alpha = alpha)
    
  }else{
    bar = geom_bar(width = width, 
                   alpha = alpha, 
                   color = Col)
  }
  
  if(!is.null(df)){
    bar = ggplot(data = df,
                 aes(x=df[,x])) + bar
    
    if(Trans){
      bar =  bar + coord_flip()
    }
  }
  
  return(bar)
}

ggBasicBar <- function(
  Col=NULL,     # When Use One Color
  Trans=T,  # When make a Basic Graph
  width=.4,
  alpha=.7,
  df=NULL,  # When Use This, Try To make a Basic Graph; Else, To make a Point Graph LAYER !!! 
  x=1,
  y=2
){
  # Description:
  # A point Graph for Basic Graph or A Layer
  
  # Package:
  require(ggplot2)
  
  if(is.null(Col)){
    bar = geom_bar(stat="identity",
                   position=position_dodge(),
                   width = width, 
                   alpha = alpha)
    
  }else{
    bar = geom_bar(stat="identity",
                   position=position_dodge(),
                   width = width, 
                   alpha = alpha, 
                   color = Col)
  }
  
  if(!is.null(df)){
    df$x = factor(df$x)
    bar = ggplot(data = df,
                 aes(x=df[,x],
                     y=df[,y])) + bar
    
    if(Trans){
      bar =  bar + coord_flip()
    }
  }
  
  return(bar)
}

ggBasicStack <- function(
  Col=NULL,     # When Use One Color
  Trans=T,  # When make a Basic Graph
  width=.4,
  alpha=.7,
  df=NULL,  # When Use This, Try To make a Basic Graph; Else, To make a Point Graph LAYER !!! 
  x=1,
  y=2,
  color=3
){
  # Description:
  # A point Graph for Basic Graph or A Layer
  
  # Package:
  require(ggplot2)
  
  if(is.null(Col)){
    stack = geom_bar(stat="identity",
                     width = width, 
                     alpha = alpha)
    
  }else{
    stack = geom_bar(stat="identity",
                     width = width, 
                     alpha = alpha, 
                     color = Col)
  }
  
  if(!is.null(df)){
    df$x = factor(df$x)
    stack = ggplot(data = df,
                   aes(x=df[,x],
                       y=df[,y],
                       fill = df[,color]),
                   color="black") + stack
    
    if(Trans){
      stack =  stack + coord_flip()
    }
  }
  
  return(stack)
}

ggBasicBox  <- function(
  Col=NULL,     # When Use One Color
  Trans=F,  # When make a Basic Graph
  width=.4,
  alpha=.7,
  df=NULL,  # When Use This, Try To make a Basic Graph; Else, To make a Point Graph LAYER !!! 
  x=1,
  y=2,
  color=3
){
  # Description:
  # A point Graph for Basic Graph or A Layer
  
  # Package:
  require(ggplot2)
  
  if(is.null(Col)){
    box =  geom_boxplot(width  = width,
                        alpha  = alpha
    )
    
  }else{
    box = geom_boxplot(width = width, 
                       alpha = alpha, 
                       color = Col)
  }
  
  if(!is.null(df)){
    
    df$x = factor(df$x)
    box = ggplot(data = df,
                 aes(x=df[,x],
                     y=df[,y],
                     fill = df[,color]),
                 color="black") + box
    
    if(Trans){
      box =  box + coord_flip()
    }
  }
  
  return(box)
}

ggBasicDensity  <- function(
  Col=NULL,     # When Use One Color
  Trans=F,  # When make a Basic Graph
  alpha=.1,
  df=NULL,  # When Use This, Try To make a Basic Graph; Else, To make a Point Graph LAYER !!! 
  x=1,
  color=2
){
  # Description:
  # A point Graph for Basic Graph or A Layer
  
  # Package:
  require(ggplot2)
  
  if(is.null(Col)){
    density =  geom_density(alpha=alpha)
    
  }else{
    density = geom_density(alpha=alpha,
                           color=Col)
  }
  
  if(!is.null(df)){
    density = ggplot(data = df,
                     aes(x=df[,x],
                         fill = df[,color]),
                     color="grey20") + density
    
    if(Trans){
      density =  density + coord_flip()
    }
  }
  
  return(density)
}

ggBasicLine  <- function(
  Col=NULL,     # When Use One Color
  Trans=T,  # When make a Basic Graph
  df=NULL,  # When Use This, Try To make a Basic Graph; Else, To make a Point Graph LAYER !!! 
  x=1,
  y=2,
  color=3
){
  # Description:
  # A point Graph for Basic Graph or A Layer
  
  # Package:
  require(ggplot2)
  
  if(is.null(Col)){
    line =  geom_line()
    
  }else{
    line = geom_line(color=Col)
  }
  
  if(!is.null(df)){
    
    df$x = factor(df$x)
    line = ggplot(df,
                  aes(x=df[,x],
                      y=df[,y],
                      #color=df[,color],
                      group=df[,color])) + line
    
    if(Trans){
      line =  line + coord_flip()
    }
  }
  
  return(line)
}