library(tidyverse)
library(naniar)
options(scipen=999) ## to stop it reading in standard form
## read in data
#input_raw<- read.csv("day8_test.csv")
input_raw<- read.csv("day8_input.csv")
input_raw %>% head()

input <-
  readLines("day8_input.csv") %>%
  str_split("") %>%
  unlist() %>%
  as.numeric() %>%
  matrix(nrow =99, byrow = T)

input %>% head()

write.csv(input,'day8_input_clean.csv', row.names = FALSE)
#### Task 1 - find all the elements where every element above,left, right below 
### has at least one element of the same or bigger size
visible_test<- matrix(nrow=99, ncol=99)

visible_test

#input[row,col]
# visible = 1
# not visible = 0

### loop across every row and column
for(row in 1:99) {
  for (col in 1:99) {
    tree<-input[row, col]
    
    if (is.na(tree)) {
      #visible_test[row, col] <- 0 ## doesn't exist so isn't visible
    }
    ## if it's an edge than visible
    else if (row == 1 | row == 99 |
             col == 1 | col == 99) {
      #print('edge value')
      visible_test[row, col] <- 1  ## visible
    }
    ## if there is anything same or bigger, than not visible
    ##check the columns/row above,below, left & right
    else if (any(input[1:row - 1, col] >= tree)&##above
                 any(input[(row + 1):99, col] >= tree)&##below
                 any(input[row, 1:col - 1] >= tree)& #left
                 any(input[row, (col + 1):99] >= tree) #right
                 ) {
                 ## all of these must hold true
                 visible_test[row, col] <- 0 #not visible
                 
  } else{
    visible_test[row, col] <- 1 ## visible
  }
  
}
}
visible_test

## then find how many positions are not visible, and how many visible
##total positions
n_pos = ncol(visible_test)*nrow(visible_test) 
n_pos

## how many visible? -- the answer required
visible_test[!is.na(visible_test)] %>% sum()
##1859

## how many not visible?
n_pos-visible_test[!is.na(visible_test)] %>% sum()


### part 2
### you need to find how many trees are between the given tree and the first of the same height or more



