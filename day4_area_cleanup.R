library(tidyverse)
input<-read.csv("day4_input.csv", blank.lines.skip=FALSE)
input %>% head(n=10)

# The elves are given areas to clean e.g 3-6 would clean areas 3,4,5,6
# The elves are paired up but in some cases the area overlaps e.g 3-6 and 4-6 would both be cleaning 4,5,6
# In some examples one elf's areas are completely contained by the other e.g 4-5 would be contained by 3-6

### Task 1 - How many elf pairs have one of their areas contained by the other's

x<-input %>% head(n=6) %>%
  separate(area_pairs, letters[1:4],',|-') %>%
  mutate_if(is.character,as.numeric) %>% 
  mutate(contained = case_when(a<=c & b>=d | a >= c & b<=d ~TRUE  )) %>%
  mutate(area_cd_in_ab = case_when(a <= c & b >= d ~TRUE),
         area_ab_in_cd = case_when(a >= c & b <= d ~TRUE))
x %>% head()
x %>% group_by(contained) %>%  count() #487
x %>% group_by(area_cd_in_ab,area_ab_in_cd) %>%  count() #487

### task 2 - how many elves overlap at all

y <- input %>% #head(n = 10) %>%
  separate(area_pairs, letters[1:4], ',|-') %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(overlap = case_when(a <= c & b >= c |c <= a & d >= a ~ TRUE))

y %>% group_by(overlap) %>% count() ##849













