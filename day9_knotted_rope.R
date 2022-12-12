library(tidyverse)

## the head of the rope will move in steps of 1, the tail must follow to keep adjacent 
## if the tail is in the same row or column it moves up/down/left/right to keep adjacent
## if they're not in the same row or column, and they're not diagonally adjacent, the tail will move diagonally

## task is to find every x,y position tail visited and count them

find_valid_pos<-function(x,y){
  ## check if the surrounding matrix elements exist (i.e not outside the dimensions)
  valid_pos <- c(tail[x,y])
  if(x+1 <= nrow(tail)){ valid_pos<-valid_pos %>% append(tail[x+1,y])}## one below
  if(x-1 >0){ valid_pos<-valid_pos %>% append(tail[x-1,y])}## one above
  if(y+1 <= ncol(tail)){ valid_pos<-valid_pos %>% append(tail[x,y+1])}## one right
  if(y-1 > 0){ valid_pos<-valid_pos %>% append(tail[x,y-1])}## one left
  if(x+1 <= nrow(tail) & y+1 <= ncol(tail)){ valid_pos<-valid_pos %>% append(tail[x+1,y+1])}## diagonal down right
  if(x+1 <= nrow(tail) & y-1 > 0){ valid_pos<-valid_pos %>% append(tail[x+1,y+-1])}## diagonal down left
  if(x-1 >0 & y+1 <= ncol(tail)){ valid_pos<-valid_pos %>% append(tail[x-1,y+1])}## diagonal up right
  if(x-1 >0 & y-1 >0){ valid_pos<-valid_pos %>% append(tail[x-1,y-1])}## diagonal up left
  
  return(valid_pos)
}

##step 1 move H right 4 (y+1)
### steps
steps_df <-
  readLines("day9_input.csv")  %>%
  data.frame() %>%
  separate('.', c('direction', 'n_steps'), ' ') %>%
  mutate(n_steps = as.numeric(n_steps))
steps_df %>% head()
# turn this into a sequence of steps
steps<-steps_df[rep(seq_len(dim(steps_df)[1]), steps_df$n_steps), 1, drop = FALSE]
steps %>% head()
#write.csv(steps, 'day9_input_clean.csv', row.names = FALSE)

## set up matrix
## the matrix seems to be infinite in size
head<-matrix(nrow = 1200, ncol = 1200)
tail<-matrix(nrow = 1200, ncol = 1200)

#starting points
x =500
y =500
head[x,y]<-'H'
tail[x,y]<-'T'

head
tail

#starting position for the tail
tail_pos<- which(!is.na(tail), TRUE)
print(Sys.time())
## loop over every step and move the tail to follow the head
for(n in 1:nrow(steps)){
  print(n)
  ## find the position of the H in the matrix
  x <- which(!is.na(head), TRUE)[1]
  y <- which(!is.na(head), TRUE)[2]


  print(paste0("step is ", steps[[1]][n]))
  if(steps[[1]][n] == 'R') {head[x, y + 1] <- 'H'
  }else if (steps[[1]][n] == 'L') {head[x, y - 1] <- 'H'
  }else if (steps[[1]][n] == 'U') {head[x - 1, y] <- 'H'
  }else if (steps[[1]][n] == 'D') {head[x + 1, y] <- 'H'}

  head[x,y]<-NA #set old position to NA
  #head %>% print()
  #tail %>% print()

  # ## visualise the steps for the example
  # head2<-head
  # tail2<-tail
  # head2[!is.na(tail2)] <- tail2[!is.na(tail2)]
  # head2[is.na(head2)] <-""
  # print(head2)

  ## x and y have now changed
  x <- which(!is.na(head), TRUE)[1]
  y <- which(!is.na(head), TRUE)[2]
  ## check if the surrounding matrix elements exist (i.e not outside the dimensions)
  valid_pos <- find_valid_pos(x,y)

  ## if they're all NA then move the tail
  if(all(is.na(valid_pos))){
    print('move')
    tail_x <- which(!is.na(tail), TRUE)[1]
    tail_y <- which(!is.na(tail), TRUE)[2]

    if(tail_x - x ==0 ){new_tail_x= tail_x } else if(x>tail_x){new_tail_x<-tail_x+1} else{new_tail_x<-tail_x-1}
    if(tail_y - y ==0 ){new_tail_y= tail_y } else if(y>tail_y){new_tail_y<-tail_y+1} else{new_tail_y<-tail_y-1}

    tail[new_tail_x,new_tail_y]<-'T'
    tail[tail_x,tail_y]<-NA
    #tail %>% print()
  }else {print('still adjacent')
      }


  tail_pos<- tail_pos %>% rbind(which(!is.na(tail), TRUE))

  ## visualise the steps for the example
  # head2<-head
  # tail2<-tail
  # head2[!is.na(tail2)] <- tail2[!is.na(tail2)]
  # head2[is.na(head2)] <-""
  # print(head2)
}
head %>% print()
tail %>% print()



## now find out how many places the tail visited
tail_pos<-data.frame(tail_pos)%>% unique()
total_pos<- tail_pos %>% unique() %>% nrow()
print(total_pos)

## visualise
tail_positions<- matrix(nrow = 1200, ncol =1200)

for(i in 1:total_pos){
  row = tail_pos$row[i]
  col = tail_pos$col[i]


  tail_positions[row,col]<-1
}
#tail_positions
print(total_pos) ##6266

tail_positions[is.na(tail_positions)]<-0
write.csv(tail_positions, 'day9_tail_positions.csv', row.names = FALSE)


