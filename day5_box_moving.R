library(tidyverse)

## The input was manually split into two files
boxes<-read.csv("day5_input_a.csv")
boxes 
steps<-read.csv("day5_input_b.csv", blank.lines.skip=FALSE)
steps %>% head()

## boxes
# X1  X2  X3  X4  X5  X6  X7  X8  X9
# 1 [B]                     [N]     [H]
# 2 [V]         [P] [T]     [V]     [P]
# 3 [W]     [C] [T] [S]     [H]     [N]
# 4 [T]     [J] [Z] [M] [N] [F]     [L]
# 5 [Q]     [W] [N] [J] [T] [Q] [R] [B]
# 6 [N] [B] [Q] [R] [V] [F] [D] [F] [M]
# 7 [H] [W] [S] [J] [P] [W] [L] [P] [S]
# 8 [D] [D] [T] [F] [G] [B] [B] [H] [Z]


## the steps say how boxes many from a column should be moved, from which column and to where
## the boxes must be moved individually so e.g if you take 2 from x1 and move them to x2 you will
## take [B] then [V] so column x2 will read [D],[W],[B],[B],[V] from the bottom up

## split the steps into something usable
steps <- steps %>% separate(steps, into = letters[1:6], ' ') %>%
  rename(num_boxes = b,from = d,to = f) %>%
  select(-a, -c, -e) %>% 
  mutate_if(is.character,as.numeric)

steps %>% head() 

### make the columns of boxes into lists that can be altered
## reverse the boxes so you're adding/removing from the end of the list not top
names(colnames(boxes)) <- colnames(boxes)
rev_boxes <-boxes %>% arrange(-row_number()) 
box_lists <- lapply(colnames(boxes), function(x){rev_boxes[,x,drop = TRUE]})
box_lists

## turn list of lists back into df to look at
do.call(cbind, box_lists)
boxes

steps %>% head()

## function to loop over n times, removing the box from it's column and adding to the new column
complete_step <- function(row) {
  
  n <- steps$num_boxes[row]
  from <- steps$from[row]
  to <- steps$to[row]
  print(do.call(cbind, box_lists)) ##original
  
  for (x in 1:n) {
    ### find position of box that needs to move
    box_position <-tail(which(box_lists[[from]] != ''), 1)

    ## find position of empty spot to move it into, if the whole column is 0 set to 1
    if(identical(tail(which(box_lists[[to]] != '')), integer(0)) ) { 
      new_position <- 1
      }else{
      new_position <- tail(which(box_lists[[to]] != ''), 1)+1
      } 
    
    print(box_lists[[from]][[box_position]]) ## get element to move
    
    ## move element
    box_lists[[to]][[new_position]] <<-box_lists[[from]][[box_position]]
    ## remove element from original position
    box_lists[[from]][[box_position]] <<- ''
    
    
    ## this is for visual checking that it's working
    ### to bind boxes back together they need to be the same length
    
    ## if one column is longer than the others, even them out
    if(sapply(box_lists, length) %>% unique() %>% length()>1) {
      
      max_length <- max(sapply(box_lists, length))
      
      ## add blanks to make each list the same size
      for (i in 1:length(box_lists)) {
        if (length(box_lists[[i]]) < max_length) {
          box_lists[[i]] <<- box_lists[[i]] %>% append('')
        }
      }
    }
    
  
  }
  ## have a look
 print(do.call(cbind, box_lists))
}
steps %>% head()

## loop over every step
for(row in 1: nrow(steps)){
  print(paste0("step ",row))
  print(steps[row,])
  complete_step(row)
}


## find the letters at the end of each list
## these correspond to the top of the box piles
top_letter<-as.character()
for(x in 1:9) {
  box = box_lists[[x]][[tail(which(box_lists[[x]] != ''), 1)]]
  print(box)
  clean_letter =  gsub('\\]','', gsub('\\[','',box))
  print(clean_letter)
  top_letter <- paste0(top_letter,clean_letter)
  
  
}
top_letter

#PSNRGBTFT
