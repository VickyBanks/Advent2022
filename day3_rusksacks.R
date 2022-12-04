library(tidyverse)
input<-read.csv("day3_input.csv", blank.lines.skip=FALSE)
input %>% head(n=10)

### task1 - there are items placed in a rucksack (given by letters)
#The bag is split in two, with the first half of the letters in part 1 and the second in part 2
#You need to identify bags with the same item in both parts

##step 1 split into two parts
bag<- input %>% #head(n=10) %>% 
  mutate(n_items = nchar(bag)) %>% 
  mutate(part1 = substr(bag,1, n_items/2),
         part2 = substr(bag, n_items/2+1,n_items )) %>% 
  select(-bag,-n_items)

## step 2 find item in both parts

## split the characters and count how many of each letter in each bag. 
# Join the result to find the letters in common.

## for this pass there is only one in common
bag$dup_let <-NA
bag
for (row in 1:nrow(bag)) {
  letter <-
    data.frame(x = table(str_split(bag$part1[row], "", simplify = T)[1,])) %>%
    inner_join(
      data.frame(x = table(str_split(bag$part2[row], "", simplify = T)[1,])), by = "x.Var1") %>%
    rename(letter = x.Var1) %>%
    select(letter) %>% unlist() %>% as.character()
  
  ## check there is at least one match
  if(!identical(letter, character(0))){ bag$dup_let[row] <- letter}
}

bag %>% head(n=10)


### each letter has a value, lower and upper are distinct
## how many points for the dup letters
bag %>% #head(n=10) %>% 
  left_join(data.frame(letter = c(letters, LETTERS), value = 1:52),
            by = c('dup_let'= 'letter')) %>% 
  summarise(total = sum(value)) #8139


## part 2
### each three rows correspond to three elves who must all have an item in common

elves<- input %>% #head(n=15) %>% 
  rowid_to_column() %>% 
  mutate(elf_group = as.integer(rowid/3-0.01)+1 ) %>% 
  select(-rowid) %>% 
  cbind(elf_num = rep(letters[1:3], times=nrow(elves)/3, each=1)) %>% 
  group_by(elf_group) %>% spread(key = elf_num, value = bag)


elves %>% head()
elves$common_let <-NA
for (row in 1:nrow(elves)) {
  letter <-
    data.frame(x = table(str_split(elves$a[row], "", simplify = T)[1,])) %>%
    inner_join(
      data.frame(x = table(str_split(elves$b[row], "", simplify = T)[1,])), by = "x.Var1") %>%
    inner_join(
      data.frame(x = table(str_split(elves$c[row], "", simplify = T)[1,])), by = "x.Var1") %>%
    rename(letter = x.Var1) %>%
    select(letter) %>% unlist() %>% as.character()
  
  print(letter)
  ## check there is at least one match
  if(!identical(letter, character(0))){ elves$common_let[row] <- letter}
}

elves %>% head()

elves %>%   
  left_join(data.frame(letter = c(letters, LETTERS), value = 1:52),
                      by = c('common_let' = 'letter')) %>%
  ungroup %>% 
  summarise(total = sum(value))







