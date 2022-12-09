library(tidyverse)

## read in data
test<- read.csv("day7_test.csv")
test<-test$output 

input<- read.csv("day7_input.csv")
input<-input$output 
input %>% head()
x<-list()

for(i in 1:length(input)){
  
  if(grepl('\\$ ls', input[i]) ){
    dir <- paste0('dir ',gsub('\\$ cd ', '',input[i-1]))
    
    if(dir == 'dir /'){ dir = 'dir top'} ## just to make life easier
    
    print(dir)
    for(j in 1:(length(input)-i) ){
      if(grepl('\\$', input[i+j]) ){break}
      print(paste0("inside  ", dir,"/", input[i+j]))
      val =  str_split(input[i+j],' ',2) %>% unlist()
      
      print(c(dir,val))
     x<- x %>% append(paste0(dir,"/", input[i+j]))
      
      }
  } 
}

## make an empty df to hold directories
all_dir_sizes<-data.frame()

## make df from the list x with the top dir, and the files within it, and their size
df <- data.frame('x' = x %>% unlist()) %>% 
  separate(x, c('dir', 'file'), '/') %>%
  mutate(size = as.numeric(gsub("([a-z])|([.])", '', file))) %>%
  mutate(file = gsub("([0-9])", '', file)) %>% 
  replace(is.na(.),0) %>% 
  arrange(dir)

df %>%head(n=10)


n=1

has_sub_dir<-'' #
## look for directories that have sub directories within them
## whilst there is something keep doing this
while(!identical(has_sub_dir, character(0))) {
 print(n)
  ## find if the directory contains another dir
  has_sub_dir <- df$dir[grepl('dir', df$file)] %>% unique()
  #has_sub_dir %>% head() %>% print()
  
  ## group any dir don't contain others i.e in their base form
  ## and find their size
  total_dir_size <-
    df %>% filter(!dir %in% has_sub_dir) %>%
    group_by(dir) %>%
    summarise(size = sum(size))
  
  total_dir_size %>% arrange() %>% head()%>% print()
  
  print(paste0("number found = ",total_dir_size %>% nrow()))
  
  ## add their sizes to this list
  all_dir_sizes <- all_dir_sizes %>% rbind(total_dir_size)
  print(paste0("total dir = ",all_dir_sizes %>% nrow()))
  
  ## now join back into the df giving the sub-directory it's size
  ## rename the sub directory now we've delt with it
  df <- df %>% filter(dir %in% has_sub_dir) %>%
    left_join(total_dir_size, by = c('file' = 'dir')) %>%
    replace(is.na(.), 0) %>%
    mutate(file = case_when(size.y > 0 ~ str_replace(file, 'dir', ''),
                            size.y <= 0 ~ file)) %>%
    mutate(size = size.x + size.y) %>%
    select(dir, file, size)
  
  #df  %>% head() %>% print()
  ## show all the directories and their sizes
  #all_dir_sizes %>% arrange(dir) %>% print()
  n=n+1
  if(total_dir_size %>% nrow() ==0){break}

}


write.csv(df, "output.csv")

data.frame(y = input) %>% #head() %>% 
  mutate(lag_y =lag(y),
         lead_y = lead(y)
         ) %>% 
  select(lag_y, y, lead_y) %>% 
  filter(y == '$ ls') %>% 
  head()




