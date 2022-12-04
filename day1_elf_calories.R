library(tidyverse)
library(hutilscpp)


input<-read.csv("day1_input.csv", blank.lines.skip=FALSE)
input %>% head(n=10)
input %>% filter(is.na(cal) ) %>% nrow() ## 224 elves

### task 1 - which elf is carrying the most calories
input$dummy[1] <- 1

cal_per_elf<-
  input %>% 
  mutate(calsum = cumsum_reset(!is.na(cal),cal)) %>% ##cumsum but reset at na value
  filter(lead(is.na(cal))) %>% ##only keep the largest value for each elf
  rowid_to_column('elf') %>% ## set elf number
  select(-cal) %>% 
  arrange( desc(calsum))
  
cal_per_elf %>% head()

### task 2 -  find the total calories from the top 3 elves







