library(tidyverse)
input<-read.csv("day2_input.csv", blank.lines.skip=FALSE)
input %>% head(n=10)

######### rock paper scissor game #########

# opponent
# A = rock = 1 point
# B = paper = 2 points
# C = scissor = 3 points
# 
# your play
# X = rock = 1 point
# Y = paper = 2 points
# Z = scissor = 3 points

# outcome score
# win = 6
# draw = 3
# lose = 0
# 
# total score = shape point + outcome

## rules
play <- data.frame(shape = c('A', 'B', 'C'),
                   shape_points = c(1, 2, 3),
                   win = c('C', 'A', 'B'),
                   draw = c('A', 'B', 'C'),
                   lose = c('B','C','A')
                   )
## make all play into ABC rather than XYZ
input <- input %>%
  mutate(shape = case_when(you == 'X' ~ 'A',
                         you == 'Y' ~ 'B',
                         you == 'Z' ~ 'C',))

input %>% head()
#### task 1 - if you play according to this list, how many points will you have?
input %>% 
  full_join(play, by = 'shape') %>%
  mutate(outcome = case_when(opposition == win ~ 6,
                             opposition == draw ~ 3,
                             opposition == lose ~ 0,)) %>% 
  mutate(points = shape_points+outcome) %>% 
  summarise(total = sum(points))


##### task 2 - find the shape you should play, and sum score
# Now the round must end with XZY where
#
# X = lose
# Y = draw
# Z = win

input<-read.csv("day2_input.csv", blank.lines.skip=FALSE) %>% rename(outcome = you)
input %>% head(n=10)

## joining the win/lose/draw to THEIR move
## so for you to win the lose/win needs to switch from last time
play2 <- data.frame(shape = c('A', 'B', 'C'),
                   shape_points = c(1, 2, 3),
                   lose = c('C', 'A', 'B'),
                   draw = c('A', 'B', 'C'),
                   win = c('B','C','A')
)


game<-input %>%
  full_join(play2 %>% select(-shape_points), by = c('opposition' = 'shape')) %>%
  mutate(your_play  = case_when(outcome == 'X' ~ lose,
                                outcome == 'Y' ~ draw,
                                outcome == 'Z' ~ win)) %>% 
  mutate(outcome_points  = case_when(outcome == 'X' ~ 0,
                                outcome == 'Y' ~ 3,
                                outcome == 'Z' ~ 6)) %>% 
  #select(opposition, outcome, your_play, outcome_points) %>% 
  full_join(play2 %>% select(shape,shape_points), by = c('your_play' = 'shape')) %>% 
  mutate(points = outcome_points + shape_points)

  
  
game %>% summarise(total  = sum(points))

### for ease of checking
game[ game == 'A']<-'rock'
game[ game == 'B']<-'paper'
game[ game == 'C']<-'scissor'

game[ game == 'X']<-'lose'
game[ game == 'Y']<-'draw'
game[ game == 'Z']<-'win'
game %>% head()





















