dataSourcePath = "/Users/jackbaska/Documents/work.space.sync/Data_of_Professor/TEJ資料/輸出TEJ/董監事資料/TransCSV/"
dataSavePath = "/Users/jackbaska/Documents/work.space.sync/Data_of_Professor/TEJ資料/輸出TEJ/董監事資料/TransCSV/Test/第一次DataClean"
dataDropPath = "/Users/jackbaska/Documents/work.space.sync/Data_of_Professor/TEJ資料/輸出TEJ/董監事資料/TransCSV/Test/第一次DataClean排除"
excludFile = "/Users/jackbaska/Documents/work.space.sync/Data_of_Professor/TEJ資料/輸出TEJ/董監事資料/TransCSV/Test/ExcludeFile/考量特殊情形.csv"

library(magrittr)
library(dplyr)

dataFilter = 
  function(x = data.frame(), dropS = T,
           keepC = vector(), keepAtt = character(),
           dropC = vector(), dropAtt = character())
  {
    regN = function(nameVector = vector(), patDir = "both"){
      #### edit:  take the regular expression in direction depend
      rightD = ""; leftD = "";
      if (logicOR) operator = "|" else operator = "&"
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

fileNameStack = function(path = dir(), format = "csv") {
  nameList = dir(path) %>% .[grep(paste(".",format,sep = ""),.)]
}

####### code  #######
keepN = c('董','監')
for(dataPath in fileNameStack(dataSourcePath)){
  dataSourceFile = dataSourcePath %>% paste("/",dataPath,sep = "")
  dataDropFile = dataDropPath %>% paste("/",'drop.',dataPath,sep = "")
  dataSaveFile = dataSavePath %>% paste("/",'clean',dataPath,sep = "")
  dataF = read.csv(file = dataSourceFile,check.names = F)
  
  datKeep = dataFilter(x = dataF,dropS = T,
                       keepC = keepN,keepAtt = "身份別",
                       dropC = "合計",dropAtt = "持股人姓名")

  write.csv(datKeep,dataSaveFile)
  write.csv(datDrop,dataDropFile)
}
