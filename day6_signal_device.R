library(tidyverse)

## The input was manually split into two files
signal<- names(read.csv("day6_input.csv"))
signal 

### task1 find the first time 4 characters that have no repeats are seen
## task 2 find the first time 14 characters that have no repeats are seen
signal_length<-substring(signal, 1:nchar(signal), 1:nchar(signal)) %>% length()

for(x in 1:signal_length) {
  
  sub <- substr(signal, x, x + 13)## chunk into 4 characters
  print(sub)
  
  ## if there are 4 unqique letters we've found it
  n<-substring(sub, 1:nchar(sub), 1:nchar(sub)) %>% unique() %>% length()
  if (n ==14) {
    print("four unique")
    print(paste0("position = ",x+13)) ## need the end position, x is the start of the chain
    break
  }
  
}
