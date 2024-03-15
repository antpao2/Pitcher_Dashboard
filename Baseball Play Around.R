#Play around with baseball savant

library(baseballr)
library(tidyverse)
library(gtExtras)
library(stringr)
library(lubridate)


#Spring Training Stats for Today

today <- statcast_search('2024-02-20', player_type='pitcher') %>% filter(away_team == 'NYY' | home_team == 'NYY')
#Start with stroman
stroman <- today %>% filter(pitcher== '573186')
names(stroman)
#Pitch Chart
ggplot(stroman, aes(x=plate_x, y=plate_z, color = pitch_type)) + geom_point() + geom_rect(aes(xmin=-.85, xmax=.85, ymin = mean(sz_bot, na.rm=T),
   
                                                                                              
                                                                                                                                                                                                                 ymax = mean(sz_top, na.rm=T)))

#Scout Function
pitch <- function(date1, date2, name){
  #Split Name
  ab <- unlist(str_split(name, ' '))
  #Get Proper Format
  ab[1] <- str_to_title(str_to_lower(ab[1]))
  ab[2] <- str_to_title(str_to_lower(ab[2]))
  
  #Getting ID
  k <- playerid_lookup(ab[2], ab[1])
  k <- k$mlbam_id
  
  #Pulling Player Query
  t <- statcast_search_pitchers(mdy(date1), mdy(date2), pitcherid = k)
  
  #My Beautiful Table
  final <- t %>% group_by(pitch_name) %>% summarise(Count= n(), #go over this
                                                    Avg_Velo = round(mean(release_speed, na.rm=T), 1), 
                                                    Avg_Spin = round(mean(release_spin_rate, na.rm = T),1),
                                                    Balls = length(type[type == 'B']),
                                                    Strikes = length(type[type == 'X' | type == 'S']),
                                                    BIP = length(type[type == 'X']),
                                                    xWOBA = round( (sum(estimated_woba_using_speedangle, na.rm=T) 
                                                    + (.7*sum(woba_denom[events == 'walk']))
                                                    + (.7*sum(woba_denom[events == 'hit_by_pitch']))) / (sum(woba_denom, na.rm=T)),3)) %>% mutate(
                                                      Per = paste(round((Count/sum(Count) *100),2),'%')
                                                    ) %>% arrange(desc(Avg_Velo)) %>% gt() %>% gt_theme_538(quiet = T)  %>% cols_label(
                                                      pitch_name = 'Pitch') %>% tab_header(title = paste(ab[1], ab[2], 'Pitch Breakdown'),
                                                                                           subtitle = (paste('From', date1, 'to', date2)))

#Possibly Add another loop for L/R
  plots = list()
  for (i in unique(t$pitch_name)){
    u <- t %>% group_by(zone) %>% filter(pitch_name == i) %>% summarise(xWOBA = round( (sum(estimated_woba_using_speedangle, na.rm=T) 
                                                                                        + (.7*sum(woba_denom[events == 'walk']))
                                                                                        + (.7*sum(woba_denom[events == 'hit_by_pitch'])))
                                                                                       / (sum(woba_denom, na.rm=T)),3)) %>% replace(is.na(.), 0) %>% filter(zone < 10) %>% add_row(zone = 1:9, xWOBA = 0) %>% filter(!(duplicated(zone))) %>% mutate(xWOBA = sprintf('%.3f', xWOBA)) %>% mutate_at(
        'xWOBA', as.numeric) %>% arrange(zone)
    
    
    data <- matrix(u$xWOBA, nrow=3, ncol=3, byrow=F)
    df <- expand.grid(x=1:3,y=1:3)
    df$val <- data[as.matrix(df[c('x','y')])]
    
    plots[[i]] <- ggplot(df, aes(x=x, y=y, label=val)) + 
      geom_tile(aes(fill = val), colour='white') + scale_fill_gradient2(low='#ED2939', mid='white', high='#1A5FAD', midpoint = .3, limits = c(0,1))  +
      geom_text(size = 6) + theme_bw() +
      theme(panel.border = element_blank(),
            axis.text  = element_blank(),
            panel.grid = element_blank(),
            axis.line  = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank()) + labs(title = paste(ab[1], ab[2], 'xWOBA by Zone'),
                                                 subtitle = paste('Pitch:', i))
  }
  return(list("overall" = final, "plots" = plots))
}

cole <- pitch('03/28/2023', '10/25/2023', 'Gerrit Cole')
cole$overall

cole$plots$Slider




#Maybe Add Something to filter handiness, show where he was successful, give advice on how he can approach hitters, etc
#Stand --> Side the hitter is on
#Zone --> Show the .xwoba per pitch

#Against Lefties/Rights, show the xwoba for each of the 14 for each pitch

#I figured out the x,z (Pf/x used z, not y; not sure if anything changed) 
#of each zone a while back by running SQL queries to return MIN and MAX values of each WHERE zone=1, etc. 
#Just noting incase you need them. I could dig them up, but it’s probably best to make sure you have fresh values.

#Statcast Zone Measurements
p <- statcast_search('2023-02-20', player_type='pitcher') %>% group_by(zone) %>% summarise(x1 = min(plate_x),
                                                                                           x2 = max(plate_x),
                                                                                           y1 = min(plate_z),
                                                                                           y2 = max(plate_z)
                                                                                   )
p <- p %>% filter(zone <= 10)
min(p$x1)
ggplot() + geom_rect(aes(xmin=-5.07, xmax = 4.11, ymin = -2.3, ymax=2.39))
#Strike Zone
ggplot(p, aes(xmin = -.83, xmax = .83, ymin = 1.17, ymax = 3.92) +
  geom_rect(aes(color = zone), alpha = 0.5)






