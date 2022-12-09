library(tidyverse)

## read in data
# test<- read.csv("day7_test.csv")
# test<-test$output 
# 
# input<- read.csv("day7_input.csv")
# input<-input$output 
# input %>% head()

## internet solution

input <- read.csv("day7_input.txt")
eof <- nrow(input)

fileCount <- 0
fileList <- list()
path <- "/"

for (i in 1:eof){
  
  #tree <- tree$ls 
  line <- input[i,]
  splitLine <- strsplit(line," ")
  splitLine <- splitLine[[1]]
  print(splitLine)
  # if the line is a directory make a path
  if (splitLine[1] == "dir")
  {
    newpath <- paste(path, splitLine[2], "/", sep="")
    print(paste0("new path =",newpath))
    
  }else if (splitLine[1] == "$") ## if it's a command
  {
    # if it's changing directory
    if(splitLine[2] == "cd")
    {
      if(splitLine[3] == "..") # changing up
      {       
        # chop the last directory off path
        splitPath <- strsplit(path,"/")
        splitPath <- splitPath[[1]][-length(splitPath[[1]])]  
        splitPath <- splitPath[-1]
        path <- "/"
        for (folder in splitPath)
        {
          #print(folder)
          path <- paste(path, folder, "/", sep="")
          print(paste0("path =",path))
        }
      }else{
        path <- paste(path, splitLine[3], "/", sep="")
        print(paste0("path =",path))
      }
    }
    
  }else
  {   # this is a file
    fileCount <- fileCount+1
    fileList[[fileCount]] <- data.frame(fileName = splitLine[2], fileSize = splitLine[1], filePath = path )
    
  }

  
}

fileList <- do.call(rbind, fileList)
expandedList <- fileList
startCell <- 1

while (startCell > 0){
  fileList <- expandedList[c(startCell:nrow(expandedList)),]
  startCell <- 0
  splitPath <- strsplit(fileList$filePath, "/")
  longestPath <- 0

  fileCount <- 0
  moreFiles <- 0
  for (i in splitPath)
  {

    fileCount <- fileCount+1
    initialLength <- length(strsplit(fileList[fileCount,]$filePath, "/"))

    length(i)
    if (length(i) > 1)
    {
      subdir <- i[-length(i)]
      subdir <- subdir[-1]

      path <- "/"
      for (folder in subdir)
      {
        path <- paste(path, folder, "/", sep="")
      }
      print(path)

      endLength <- length(strsplit(path, "/"))

      newLine <- fileList[fileCount,]

      newLine$filePath <- path
      expandedList <- rbind(expandedList, newLine)
      if (startCell == 0) startCell <- nrow(expandedList)
    }

  }
}
# 
# # Part 1 solution
expandedList <- expandedList[order(expandedList$filePath ),]
Folders <- split(expandedList, rleid(expandedList$filePath))

bigFolderThreshold <- 100000
biggestFolder <- 0
sumSmallFolders <- 0
for(folder in Folders)
{
  if (sum(as.numeric(folder$fileSize) ) <= bigFolderThreshold)
  {
    sumSmallFolders <- sumSmallFolders+(sum(as.numeric(folder$fileSize) ))
  }

}
print(sumSmallFolders)

# Part 2 solution
outerDirectory <- Folders[[1]]
TotalDiskUsage <- sum(as.numeric(outerDirectory$fileSize))
FreeSpace <- 70000000 - TotalDiskUsage
NeedToDelete <- 30000000 - FreeSpace

bigFolderThreshold <- NeedToDelete # 30000000
biggestFolder <- 0
sumSmallFolders <- 0
count <- 0
bigFolders <- list()
for(folder in Folders)
{

  if (sum(as.numeric(folder$fileSize) ) >= bigFolderThreshold)
  {
    count <- count+1
    bigFolders[[count]] <- sum(as.numeric(folder$fileSize) )
  }

}
print(min(unlist(bigFolders)))

