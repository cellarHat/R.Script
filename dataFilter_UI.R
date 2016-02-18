library(magrittr)
library(dplyr)
library(tcltk)
library(svDialogs)
##
## use this  when the package almost down
dataSourcePath = tk_choose.dir(caption = "Please Select The Folder of Source Data")
dataSavePath = tk_choose.dir(caption = "Please Select The Folder of Data Export")
keyWord = dlgInput(message = "Input the keyword you want. Seperate by ','.",default = "?,?") %>% .$res

dataFilter = 
  function(x = data.frame(), dropS = T,
           keepC = vector(), keepAtt = character(),
           dropC = vector(), dropAtt = character())
  {
    regN = function(nameVector = vector(), patDir = "both"){
      #### edit:  take the regular expression in direction depend
      rightD = ""; leftD = "";
      if (patDir == "both") {rightD = "+";leftD = "+"}else{
        if(patDir == "left") leftD = "+"
        if(patDir == "right") rightD = "+"
      }
      regN = paste(nameVector,collapse = "|") %>% paste(leftD,.,rightD,sep = "")
      return(regN)
    }
    
    cleanData = data.frame()
    if (dropS){
      eval(parse( text = paste('cleanData = x[which((!(grepl("',regN(dropC),'",dataF$',
                               dropAtt,'))&(grepl("',regN(keepC),'",dataF$',
                               keepAtt,")))),]",sep = "")))
    }else{
      eval(parse( text = paste('cleanData = x[which(grepl("',regN(keepC),'",dataF$',
                               keepAtt,")),]",sep = "")))
    }
    return(cleanData)
  }

fileNameStack = function(path = dir(), format = "csv") 
  {
    nameList = dir(path) %>% .[grep(paste(".",format,sep = ""),.)]
    return(nameList)
  }

rwPath = function(path = dir(),fileN = "test.csv",rw = 'r')
  {
    if (rw == 'r'){
      rwpath = path %>% paste("/",fileN,sep = "")
    }else if (rw == 'w'){
      rwpath = path %>% paste("/",'clean',fileN,sep = "")
    }else{print('No such file.')}
    return(rwpath)
  }

####### main code  #######
keepN = c('?','?')
for(dataPath in fileNameStack(dataSourcePath)){
#   dataSourceFile = dataSourcePath %>% paste("/",dataPath,sep = "")
#   dataSaveFile = dataSavePath %>% paste("/",'clean',dataPath,sep = "")
  dataF = read.csv(file = rwPath(dataSourcePath,dataPath,'r'),check.names = F)
  
  datKeep = dataFilter(x = dataF,dropS = T,
                       keepC = keepN,keepAtt = "頨思遢?",
                       dropC = "????",dropAtt = "??鈭箏???")
  write.csv(datKeep,rwPath(dataSavePath,dataPath,'w'))
}
