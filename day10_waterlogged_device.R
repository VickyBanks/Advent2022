library(tidyverse)

input<- 
  readLines("day10_input.csv")  %>%
  data.frame() %>%
  separate('.', c('signal', 'value'), ' ') %>%
  mutate(value = as.numeric(value)) %>% 
  replace(is.na(.),0) %>% 
  mutate(x =1, n_cycle = 0) #%>% head(n=10)

input %>% head(n=10)

data<- rbind(data.frame(signal = 'start', value = 0, x =1, n_cycle = 0),input)
data %>% head()
cycle = 0

## if the signal is addx it takes two cycles and AFTER the second cycle 
## the value of X is increased by that value
## i.e if value = 4 and x = 1, then cycle 1: x=1, cycle 2:x=1, cycle 3: x=1+4

## step 1 loop over the input setting the cycles and the value of x
for(i in 2:nrow(data)){
  if(data$signal[i] == 'addx'){cycle<-cycle+2} else{cycle<-cycle+1}
  data$n_cycle[i]<-cycle
  data$x[i]<-data$x[i-1]+data$value[i]

}
data %>% head(n=10)

total_n_cycles<-
data %>% filter(signal=='addx') %>% nrow()*2 +
data %>% filter(signal!='addx') %>% nrow()
total_n_cycles

## add in every cycle number
filled_data<-
  data.frame(n_cycle = c(0,seq(1:total_n_cycles))) %>% 
  full_join(data, by = 'n_cycle') %>% 
  fill(x, .direction ='down') %>% 
  replace(is.na(.),'') %>% 
  mutate(x = case_when(signal == 'addx' ~ lag(x),
                       signal !='addx' ~x) ) %>% ## x doesn't change until AFTER the cycle so change that here
  mutate(signal_strength = x*n_cycle)

## look at the desired rows
filled_data %>% filter(n_cycle %in% c(20,60,100,140,180,220) )
## what is the sum of the signal strengths at these points
filled_data %>% filter(n_cycle %in% c(20,60,100,140,180,220) ) %>% summarise(sum(signal_strength))
## for test data=13140




