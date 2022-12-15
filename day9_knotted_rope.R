library(tidyverse)

## the head of the rope will move in steps of 1, the tail must follow to keep adjacent 
## if the tail is in the same row or column it moves up/down/left/right to keep adjacent
## if they're not in the same row or column, and they're not diagonally adjacent, the tail will move diagonally

## task is to find every x,y position tail visited and count them

find_valid_pos<-function(df, x,y){
  ## check if the surrounding matrix elements exist (i.e not outside the dimensions)
  valid_pos <- c(df[x,y])
  if(x+1 <= nrow(df)){ valid_pos<-valid_pos %>% append(df[x+1,y])}## one below
  if(x-1 >0){ valid_pos<-valid_pos %>% append(df[x-1,y])}## one above
  if(y+1 <= ncol(df)){ valid_pos<-valid_pos %>% append(df[x,y+1])}## one right
  if(y-1 > 0){ valid_pos<-valid_pos %>% append(df[x,y-1])}## one left
  if(x+1 <= nrow(df) & y+1 <= ncol(df)){ valid_pos<-valid_pos %>% append(df[x+1,y+1])}## diagonal down right
  if(x+1 <= nrow(df) & y-1 > 0){ valid_pos<-valid_pos %>% append(df[x+1,y+-1])}## diagonal down left
  if(x-1 >0 & y+1 <= ncol(df)){ valid_pos<-valid_pos %>% append(df[x-1,y+1])}## diagonal up right
  if(x-1 >0 & y-1 >0){ valid_pos<-valid_pos %>% append(df[x-1,y-1])}## diagonal up left
  
  return(valid_pos)
}

##step 1 move H right 4 (y+1)
### steps
steps_df <-
  readLines("day9_input.csv")  %>%
  #readLines("day9_example2.csv")  %>%
  data.frame() %>%
  separate('.', c('direction', 'n_steps'), ' ') %>%
  mutate(n_steps = as.numeric(n_steps))
steps_df %>% head()
# turn this into a sequence of steps
steps<-steps_df[rep(seq_len(dim(steps_df)[1]), steps_df$n_steps), 1, drop = FALSE]
steps %>% head()
#write.csv(steps, 'day9_input_clean.csv', row.names = FALSE)



#### part 2 - the length of the chain H-T is much longer - H+9

move_element<-function(df, value,comparison_df, comparison_value){
  ## x and y have now changed
  x <- which(comparison_df == comparison_value, TRUE)[1]
  y <- which(comparison_df == comparison_value, TRUE)[2]

  ## check if the surrounding matrix elements exist (i.e not outside the dimensions)
  valid_pos <- find_valid_pos(df,x,y)
  
  ## if the current tail is not adjecent to the head then move
  if(all(valid_pos == '')){
    #print(paste0('move ',value))
    element_x <- which(df == value, TRUE)[1]
    element_y <- which(df == value, TRUE)[2]
    
    if(element_x - x ==0 ){new_element_x= element_x } else if(x>element_x){new_element_x<-element_x+1} else{new_element_x<-element_x-1}
    if(element_y - y ==0 ){new_element_y= element_y } else if(y>element_y){new_element_y<-element_y+1} else{new_element_y<-element_y-1}
    
    df[new_element_x,new_element_y]<-value
    df[element_x,element_y]<-''
    
  }else {#print(paste0('value ',value,' still adjacent'))
  }
  return(df)
}

## set up matrix
## the matrix seems to be infinite in size
#head<-matrix(nrow = 15, ncol = 15) %>% replace(is.na(.), '')
for(n in 0:9){assign(paste0('mat',n), matrix(nrow = 1200, ncol = 1200) %>% replace(is.na(.), ''))}
mat_set<-Filter(function(x) is(x, "matrix"), mget(ls()))

#starting points
x =500
y =500
for(n in 1:10){
  mat_set[[n]][x,y]<-as.character(n-1)
  
}

view_rope <- function() {
  head2 <- mat_set[[1]]
  for (x in 2:length(mat_set)) {
    mat <- mat_set[[x]]
    head2[mat != ''] <- mat[mat != '']
  }
  #print(head2)
}
view_rope()

# Now there are 9 pieces following the head
# H123456789

#starting position for the tail
tail_pos<- which(mat_set[[10]] =='9', TRUE)

## loop over every step and move the tail to follow the head
for(n in 1:nrow(steps)){
  
  print(n)
  ## find the position of the H in the matrix
  x <- which(mat_set[[1]] == '0', TRUE)[1]
  y <- which(mat_set[[1]] == '0', TRUE)[2]
  
  print(paste0("step is ", steps[[1]][n]))
  if(steps[[1]][n] == 'R') {mat_set[[1]][x, y + 1] <- '0'
  }else if (steps[[1]][n] == 'L') {mat_set[[1]][x, y - 1] <- '0'
  }else if (steps[[1]][n] == 'U') {mat_set[[1]][x - 1, y] <- '0'
  }else if (steps[[1]][n] == 'D') {mat_set[[1]][x + 1, y] <- '0'}
  
  mat_set[[1]][x,y]<-'' #set old position to
  
  ## visualise the steps for the example
  
  for (n in 2:length(mat_set)) {

    mat_set[[n]] <- move_element(
      df = mat_set[[n]],
      value = as.character(n-1),
      comparison_df = mat_set[[n-1]],
      comparison_value = as.character(n-2)
    )
  }
  
  
  tail_pos<- tail_pos %>% rbind(which(mat_set[[10]] =='9', TRUE))
  
  ## visualise the steps for the example
  #view_rope()
}
# head %>% print()
# tail %>% print()


tail_pos %>% unique() %>% nrow() ##2369






