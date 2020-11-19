#### input: what RCT week are we on?####
weekno <- 1
########################################

library(tidyverse)

#### set-up ####
members_list <- "data/rct_members.csv"
pairings_list <- "data/old_pairings.csv"

reset_loop <- function(){
  p1 <- NULL 
  rct <- members_list$email
  pairings <- c()
}

if (!file.exists(pairings_list)) {
  old_pairings <- c()
} else {
    old_pairings <- read.csv(pairings_list)
}

members_list <- read.csv(members_list)

if (length(members_list$email)%%2 == 1) { members_list <- rbind(members_list, c("Sally","sally.andrews@ntu.ac.uk")) }

rct <- members_list$email
pairings <- c()

#### pair RCT members ####
while (length(rct) > 1) {
  p1 <- sample(rct, 2, replace = FALSE)
  p1 <- data.frame(name_1 = p1[1], name_2 = p1[2], week = weekno)
  
  if (TRUE %in% (old_pairings$name_1==p1$name_1&old_pairings$name_2==p1$name_2)) { reset_loop }
  if (TRUE %in% (old_pairings$name_1==p1$name_2&old_pairings$name_2==p1$name_1)) { reset_loop }
  
  rct <- rct[rct %in% p1 == FALSE] 
  pairings <- rbind(pairings, p1)
}

#### format and save ####
current_pair <- members_list %>% 
  left_join(pairings, by = c("email" = "name_1")) %>% 
  left_join(pairings, by = c("email" = "name_2")) %>% 
  mutate(pair = paste0(name_1, name_2)) %>% 
  mutate(pair = str_replace(pair, "NA", "")) %>% 
  select(name, email, pair)

current_pair <- current_pair %>% 
  left_join(members_list, by = c("pair" = "email")) %>% 
  rename(pair_name = name.y,
         name = name.x)
write.csv(current_pair, "data/current_pairs.csv", row.names = FALSE)
  
old_pairings <- rbind(old_pairings, pairings)
write.csv(old_pairings, pairings_list, row.names = FALSE)


