library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)

source('01_get_recent_tracks.R')
source('02_classify_sessions.R')
source('03_classify_tracks.R')

tracks <- get_recent_tracks()
sessions_classified <- classify_sessions(tracks)
tracks_classified <- classify_tracks(sessions_classified)

#get position for xaxis labels
xaxis <- tracks_classified %>% 
  ungroup() %>% 
  mutate(min = round_any(min(track_position), 10),
         max = round_any(max(track_position), 10)) %>% 
  select(min, max) %>% 
  unique.data.frame() %>% 
  mutate(label.min = '← повторення',
         label.max = 'відкриття →')

#get positions for yaxis labels
yaxis <- data.frame(
  
  y = round_any(max(tracks_classified$track_position), 10),
  x = max(tracks_classified$track_num),
  label = '← кількість композицій у сесії')

#create annotations
annotations <- data.frame(
  
  x = c(1, 1),
  y = c(round_any(min(tracks_classified$track_position), 10) + 1, 
        round_any(max(tracks_classified$track_position), 10) - 1),
  label = c('якщо користувач слухає композицію, котра вже є в його базі, лінія зміщується на одну позицію вліво', 'якщо користувач слухає композицію, котрої ще нема в його базі, лінія зміщується на одну позицію вправо'),
  hjust = c(0, 1)
  
)

png(filename = 'ihorko_sessions.png', width = 1000, height = 1000)

ggplot(tracks_classified)+
  geom_line(aes(x = track_num, track_position, group = session_num, color = session_status), 
            size = 0.25, alpha = 0.25, position = 'jitter')+
  geom_text(data = xaxis, aes(x = -2, y = min, label = label.min, family = 'Ubuntu Condensed'), 
            size = 6, hjust = 0, color = '#3A3F4A', fontface = 'bold')+
  geom_text(data = xaxis, aes(x = -2, y = max, label = label.max, family = 'Ubuntu Condensed'), 
            size = 6, hjust = 1, color = '#3A3F4A', fontface = 'bold')+
  geom_text(data = yaxis, aes(x = x, y = y, label = label, family = 'Ubuntu Condensed'), 
            size = 6, angle = 90, hjust = 0, vjust = 0, color = '#3A3F4A', fontface = 'bold')+
  geom_text(data = annotations, aes(x = x, y = y, label = str_wrap(label, 35), 
                                    family = 'Ubuntu Condensed', hjust = hjust), 
            size = 5, vjust = 1, color = '#3A3F4A')+
  scale_x_reverse(breaks = seq(0, round_any(max(tracks_classified$track_num), 10), 10),
                  labels = c('', seq(0, round_any(max(tracks_classified$track_num), 10), 10)[-1]),
                  expand = c(0.01, 0.01))+
  scale_y_continuous(breaks = 0, limits = c(xaxis$min, xaxis$max), 
                     expand = c(0, 0))+
  scale_color_manual(values = c('#1b7837', '#762a83'))+
  coord_flip()+
  labs(title = 'Статистика прослуховування музики',
       subtitle = str_wrap('Кожна лінія на графіку - одна сесія прослуховування музики. Довжина лінії позначає кількість композицій у сесії. Зелений колір лінії означає, що в сесії переважають нові композиції. Фіолетовий колір лінії означає, що в сесії переважають вже відомі користувачу композиції', 115),
       caption = 'Дані: Last.fm | Візуалізація: Textura.in.ua')+
  theme_minimal()+
  theme(text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
        legend.position = 'none',
        axis.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.text.x = element_blank(),
        panel.grid.major = element_line(size = 0.35, linetype = 'dotted', color = '#5D646F'),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(3, 'lines'),
        strip.text = element_text(size = 16, hjust = 0),
        plot.title = element_text(face = 'bold', size = 40, margin = margin(b = 10, t = 20)),
        plot.subtitle = element_text(size = 19, face = 'plain', margin = margin(b = 30)),
        plot.caption = element_text(size = 16, margin = margin(b = 10, t = 30), color = '#5D646F'),
        plot.background = element_rect(fill = '#EFF2F4'),
        plot.margin = unit(c(2, 2, 2, 2), 'cm'))

dev.off()