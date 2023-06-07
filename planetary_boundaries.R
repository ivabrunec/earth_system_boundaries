# 7 out of 8 planetary boundaries breached
# original paper: https://www.nature.com/articles/s41586-023-06083-8/tables/1
# write up: https://www.ft.com/content/b59f9fea-500c-43c0-9b70-f0f7e866fd0e

library(ggplot2)
library(dplyr)
library(showtext)
library(ggtext)

font_add_google(name = "Staatliches", family = "Staatliches")
font_add_google(name = "Chivo", family = "Chivo")
showtext_auto(enable = T)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv('safe_just_limits.csv')

data$x_index <- data$index * 4

# create slab 'towers' ####
# create coordinates for slab tops
# create x&y coordinate lists based on existing indices and heights
temp <- data[1,]

# loop over coordinates, otherwise the polygons look weird
# angle coordinate
angle_coord <- 30
# color of bars, automatically applied
select_color = 'grey40'

for (i in 1:nrow(data)){
  temp <- data[i,]
  # slab tops
  x_top <- c(temp$x_index, temp$x_index + 1,
             temp$x_index + 2, temp$x_index + 1)
  y_top <- c(temp$polygon_top, temp$polygon_top + angle_coord,
             temp$polygon_top + angle_coord, temp$polygon_top)
  
  top_df <- data.frame(x = x_top, y = y_top)
  
  # slab fronts
  x_front <- c(temp$x_index, temp$x_index,
               temp$x_index + 1, temp$x_index + 1)
  y_front <- c(0, temp$polygon_top,
               temp$polygon_top, 0)
  
  front_df <- data.frame(x = x_front, y = y_front)
  
  # slab sides
  x_side <- c(temp$x_index + 1, temp$x_index + 1,
              temp$x_index + 2, temp$x_index + 2)
  y_side <- c(0, temp$polygon_top,
              temp$polygon_top + angle_coord, 0 + angle_coord)
  
  side_df <- data.frame(x = x_side, y = y_side)
  
  if (i == 1){
  base_plot <- ggplot() +
    geom_polygon(data = top_df, aes(x = x, y= y),
                 color = NA, fill = 'grey40') +
    geom_polygon(data = front_df, aes(x = x, y= y),
                 color = NA, fill = 'grey60') +
    geom_polygon(data = side_df, aes(x = x, y= y),
                 color = NA, fill = 'grey30') 
  }
  else{
    
    base_plot <- base_plot +
      geom_polygon(data = top_df, aes(x = x, y= y),
                   color = NA, fill = 'grey40') +
      geom_polygon(data = front_df, aes(x = x, y= y),
                   color = NA, fill = 'grey60') +
      geom_polygon(data = side_df, aes(x = x, y= y),
                   color = NA, fill = 'grey30') 
    
    }
}

# plot 
base_plot

# add 100% horizontal plane ####
x <- c(2, 4, 36, 34)  
y <- c(90, 130, 130, 90)
data_horiz <- data.frame(x = x, y = y)

base_plot2 <- base_plot +
  geom_polygon(data = data_horiz, aes(x = x, y = y), 
               fill = "#F07b58", alpha = .8) 
base_plot2

# add sections above 100% horizontal plane ####
for (i in 2:nrow(data)){
  temp <- data[i,]
  # slab tops
  x_top <- c(temp$x_index, temp$x_index + 1,
             temp$x_index + 2, temp$x_index + 1)
  y_top <- c(temp$polygon_top, temp$polygon_top + angle_coord,
             temp$polygon_top + angle_coord, temp$polygon_top)
  
  top_df2 <- data.frame(x = x_top, y = y_top)
  
  # slab fronts
  x_front <- c(temp$x_index, temp$x_index,
               temp$x_index + 1, temp$x_index + 1)
  y_front <- c(100, temp$polygon_top,
               temp$polygon_top, 100)
  
  front_df2 <- data.frame(x = x_front, y = y_front)
  
  # slab sides
  x_side <- c(temp$x_index + 1, temp$x_index + 1,
              temp$x_index + 2, temp$x_index + 2)
  y_side <- c(100, temp$polygon_top,
              temp$polygon_top + angle_coord, 100 + angle_coord)
  
  side_df2 <- data.frame(x = x_side, y = y_side)
  
  if (i == 2){
  base_plot3 <- base_plot2 +
    geom_polygon(data = top_df2, aes(x = x, y= y),
                 color = NA, fill = 'grey20') +
    geom_polygon(data = front_df2, aes(x = x, y= y),
                 color = NA, fill = select_color) +
    geom_polygon(data = side_df2, aes(x = x, y= y),
                 color = NA, fill = 'grey10') 
  }
  else{
    base_plot3 <- base_plot3 +
      geom_polygon(data = top_df2, aes(x = x, y= y),
                   color = NA, fill = 'grey20') +
      geom_polygon(data = front_df2, aes(x = x, y= y),
                   color = NA, fill = select_color) +
      geom_polygon(data = side_df2, aes(x = x, y= y),
                   color = NA, fill = 'grey10') 
  }
}

# main plot ####
p_main <- base_plot3 +
  geom_text(data = data, aes(x = x_index+.5, y = -2, label=label), angle = 30,
            hjust = 1, family = 'Chivo', size = 16) +
  labs(title = 'SAFE AND JUST LIMIT',
       subtitle = "
       The red horizontal plane represents the 'safe and just limit' for 8 indicators of planetary health.\n
       The bars extending above this plane represent the percentage beyond the limit for each of these measures.") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = '#ABC7B2'),
        plot.background = element_rect(fill = '#ABC7B2'),
        panel.grid.minor = element_line(linewidth = .1, linetype = 'dashed',
                                        color = 'grey50'),
        panel.grid.major = element_line(linewidth = .2, linetype = 'dashed',
                                        color = 'grey50'),
        plot.title = element_text(family = 'Staatliches', size = 280,
                                  color = 'grey10', hjust=.1),
        plot.subtitle = element_text(family = 'Chivo', size = 50, 
                                    color = 'grey10', lineheight = .15)) +
  ylim(c(-80, 200)) 

p_main
ggsave('plot_main.png', height = 8, width = 12, dpi = 300)

## plot 1: aerosols ####

radius <- 5
num_points <- 100

theta <- seq(0, 2 * pi, length.out = num_points)
x <- radius * cos(theta)
y <- radius * sin(theta)

data_circle <- data.frame(x = x, y = y)

p1 <- ggplot(data_circle, aes(x, y)) +
  geom_ribbon(aes(ymin = -radius * 0.15, ymax = radius * 0.15), fill = "grey40", alpha=.4) +
  geom_ribbon(aes(ymin = -radius * 0.05, ymax = radius * 0.05), fill = "#Ff3800") +
  geom_path(color = 'grey10', linewidth = 2) +
  annotate("text", x=0, y=6, label= "N", family = 'Chivo', size = 20) +
  annotate("text", x=0, y=-6, label= "S", family = 'Chivo', size = 20) +
  annotate("text", x=6, y=0, label= "E", family = 'Chivo', size = 20) +
  annotate("text", x=-6, y=0, label= "W", family = 'Chivo', size = 20) +
  labs(title = 'Aerosol pollutants',
       subtitle = 'Interhemispheric difference in aerosol optical depth',
       caption = "<span style='color:white'>Safe and just boundary: <span style='color:grey60'>0.15 AOD.</span> <br> 
       Current state: <span style='color:#Ff3800'>0.05 AOD.</span></span>") +
  coord_equal() +
  theme_void() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = '#ABC7B2', color =NA),
        plot.background = element_rect(fill = '#ABC7B2', color=NA),
        panel.grid.minor = element_line(linewidth = .1, linetype = 'dashed',
                                        color = 'grey50'),
        panel.grid.major = element_line(linewidth = .2, linetype = 'dashed',
                                        color = 'grey50'),
        plot.title = element_text(family = 'Staatliches', size = 140,
                                  color = 'grey10', hjust = 0),
        plot.subtitle = element_text(family = 'Chivo', size = 46, 
                                     color = 'grey10', lineheight = .15),
        plot.caption = element_textbox_simple(family = 'Chivo', size = 50,
                                              lineheight = .2, hjust = 0.1,
                                              fill = "grey20", 
                                              padding = margin(5, 5, 5, 5)),
        plot.margin = margin(5,5,5,5)) 

p1 
ggsave('plot1.png', height = 6, width = 6, dpi = 300)

## plot 2: temperature ####
triangle <- data.frame(
  x = c(0, 1, 1),   
  y = c(0, 0, 1)    
)
scaled_triangle <- data.frame(
  x = triangle$x * 0.8,
  y = triangle$y * 0.8
)

p2 <- ggplot() +
  geom_polygon(data = triangle, aes(x = x, y = y),
               fill = '#Ff3800') +
  geom_polygon(data = scaled_triangle, aes(x =  x, y = y),
               fill = 'grey50') +
  labs(title = 'Rise in temperature',
       subtitle = 'Global temperature change since pre-industrial era',
       caption = "<span style='color:white'>Safe and just boundary: <span style='color:grey60'>1°C.</span> <br> 
       Current state: <span style='color:#Ff3800'>1.2°C.</span></span>") +
  coord_equal() +
  theme_void() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = '#ABC7B2', color =NA),
        plot.background = element_rect(fill = '#ABC7B2', color=NA),
        panel.grid.minor = element_line(linewidth = .1, linetype = 'dashed',
                                        color = 'grey50'),
        panel.grid.major = element_line(linewidth = .2, linetype = 'dashed',
                                        color = 'grey50'),
        plot.title = element_text(family = 'Staatliches', size = 125,
                                  color = 'grey10', hjust = 0),
        plot.subtitle = element_text(family = 'Chivo', size = 46, 
                                     color = 'grey10', lineheight = .15),
        plot.caption = element_textbox_simple(family = 'Chivo', size = 50,
                                        lineheight = .2, hjust = 0.1,
                                        fill = "grey20", 
                                        padding = margin(5, 5, 5, 5)),
        plot.margin = margin(5,5,5,5)) 

p2 
ggsave('plot2.png', height = 6, width = 6, dpi = 300)

## plot 3: percentage of intact ecosystems ####

square <- data.frame(
  x = c(0, 1, 1, 0),   
  y = c(0, 0, 1, 1)    
)

square2 <- data.frame(
  x = c(0, 1, 1, 0),
  y = c(0, 0, .6, .6)
)

square3 <- data.frame(
  x = c(0, 1, 1, 0),
  y = c(0, 0, .5, .5)
)

p3 <- ggplot() +
  geom_polygon(data=square, aes(x, y), fill=NA, color='grey10') +
  geom_polygon(data=square2, aes(x,y), fill = 'grey40') +
  geom_polygon(data=square3, aes(x,y), fill = '#Ff3800')+
  labs(title = 'Intact ecosystems',
       subtitle = '% of land area covered by relatively intact ecosystems',
       caption = "<span style='color:white'>Safe and just boundary: <span style='color:grey60'>50-60%.</span> <br> 
       Current state: <span style='color:#Ff3800'>45-50%.</span></span>") +
  coord_equal() +
  theme_void() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = '#ABC7B2', color =NA),
        plot.background = element_rect(fill = '#ABC7B2', color=NA),
        panel.grid.minor = element_line(linewidth = .1, linetype = 'dashed',
                                        color = 'grey50'),
        panel.grid.major = element_line(linewidth = .2, linetype = 'dashed',
                                        color = 'grey50'),
        plot.title = element_text(family = 'Staatliches', size = 125,
                                  color = 'grey10', hjust = 0),
        plot.subtitle = element_text(family = 'Chivo', size = 46, 
                                     color = 'grey10', lineheight = .15),
        plot.caption = element_textbox_simple(family = 'Chivo', size = 50,
                                              lineheight = .2, hjust = 0.1,
                                              fill = "grey20", 
                                              padding = margin(5, 5, 5, 5)),
        plot.margin = margin(5,5,5,5)) 

p3
ggsave('plot3.png', height = 6, width = 6, dpi = 300)


## plot 4: percentage of land area with natural habitats ####

square <- data.frame(
  x = c(0, 1, 1, 0),   
  y = c(0, 0, 1, 1)    
)

square2 <- data.frame(
  x = c(1.5, 2.5, 2.5, 1.5),
  y = c(0, 0, 1, 1)
)

square3 <- data.frame(
  x = c(1.5, 2.5, 2.5, 1.5),
  y = c(0, 0, .36, .36)
)

p4 <- ggplot() +
  geom_polygon(data=square, aes(x, y), fill='grey40', color='grey10') +
  geom_polygon(data=square3, aes(x,y), fill = '#Ff3800')+
  geom_polygon(data=square2, aes(x,y), fill = NA, color='grey10') +
  labs(title = expression(paste('Semi-natural habitats/',km^2)),
       subtitle = expression(paste("Land area where at least 20% of each  ", km^2, " is semi-natural habitat")),
       caption = "<span style='color:white'>Safe and just boundary: <span style='color:grey60'>100%.</span> <br> 
       Current state: <span style='color:#Ff3800'>36%.</span></span>") +
  coord_equal(1.3) +
  theme_void() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = '#ABC7B2', color =NA),
        plot.background = element_rect(fill = '#ABC7B2', color=NA),
        panel.grid.minor = element_line(linewidth = .1, linetype = 'dashed',
                                        color = 'grey50'),
        panel.grid.major = element_line(linewidth = .2, linetype = 'dashed',
                                        color = 'grey50'),
        plot.title = element_text(family = 'Staatliches', size = 125,
                                  color = 'grey10', hjust = 0),
        plot.subtitle = element_text(family = 'Chivo', size = 46, 
                                     color = 'grey10', lineheight = .15),
        plot.caption = element_textbox_simple(family = 'Chivo', size = 50,
                                              lineheight = .2, hjust = 0.1,
                                              fill = "grey20", 
                                              padding = margin(5, 5, 5, 5)),
        plot.margin = margin(10,10,10,10)) 
p4
ggsave('plot4.png', height = 6, width = 8, dpi = 300)


## plot 5: surplus of nitrogen  ####
library(ggwaffle)
nitro_data <- data.frame(
  tonnes = c(57,119-57),
  categ = c('1','2')
)
nitro_data <- uncount(nitro_data, tonnes)
nitro_data$count <- 1

nitro_waffle <- waffle_iron(nitro_data, aes_d(group = categ))

p5 <- ggplot(nitro_waffle, aes(x, y, fill = group)) + 
  geom_waffle(color = '#ABC7B2', size = .05) +
  scale_fill_manual(values = c("grey40", "#Ff3800")) +
  labs(title = 'Surplus of nitrogen',
       subtitle = 'Agricultural surplus of nitrogen per year',
       caption = "<span style='color:white'>Safe and just boundary: <span style='color:grey60'>57 million tonnes.</span> <br> 
       Current state: <span style='color:#Ff3800'>119 million tonnes.</span></span>") +
  coord_equal() +
  theme_void() +
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = '#ABC7B2', color =NA),
        plot.background = element_rect(fill = '#ABC7B2', color=NA),
        panel.grid.minor = element_line(linewidth = .1, linetype = 'dashed',
                                        color = 'grey50'),
        panel.grid.major = element_line(linewidth = .2, linetype = 'dashed',
                                        color = 'grey50'),
        plot.title = element_text(family = 'Staatliches', size = 125,
                                  color = 'grey10', hjust = 0),
        plot.subtitle = element_text(family = 'Chivo', size = 46, 
                                     color = 'grey10', lineheight = .15),
        plot.caption = element_textbox_simple(family = 'Chivo', size = 50,
                                              lineheight = .2, hjust = 0.1,
                                              fill = "grey20", 
                                              padding = margin(5, 5, 5, 5)),
        plot.margin = margin(10,10,10,10)) 

p5
ggsave('plot5.png', height = 6, width = 8, dpi = 300)

## plot 6: surplus of phosphorus ####
phospho_data <- data.frame(
  tonnes = c(9,1),
  categ = c('1','2')
)
phospho_data <- uncount(phospho_data, tonnes)
phospho_data$count <- 1

phospho_waffle <- waffle_iron(phospho_data, aes_d(group = categ))

p6 <- ggplot(phospho_waffle, aes(x, y, fill = group)) + 
  geom_waffle(color = '#ABC7B2', size = .05) +
  scale_fill_manual(values = c("grey40", "#Ff3800")) +
  labs(title = 'Surplus of phosphorus',
       subtitle = 'Agricultural surplus of phosphorus per year',
       caption = "<span style='color:white'>Safe and just boundary: <span style='color:grey60'>4.5-9 million tonnes.</span> <br> 
       Current state: <span style='color:#Ff3800'>10 million tonnes.</span></span>") +
  theme_void() +
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = '#ABC7B2', color =NA),
        plot.background = element_rect(fill = '#ABC7B2', color=NA),
        panel.grid.minor = element_line(linewidth = .1, linetype = 'dashed',
                                        color = 'grey50'),
        panel.grid.major = element_line(linewidth = .2, linetype = 'dashed',
                                        color = 'grey50'),
        plot.title = element_text(family = 'Staatliches', size = 125,
                                  color = 'grey10', hjust = 0),
        plot.subtitle = element_text(family = 'Chivo', size = 46, 
                                     color = 'grey10', lineheight = .15),
        plot.caption = element_textbox_simple(family = 'Chivo', size = 50,
                                              lineheight = .2, hjust = 0.1,
                                              fill = "grey20", 
                                              padding = margin(5, 5, 5, 5)),
        plot.margin = margin(10,10,10,10)) 

p6
ggsave('plot6.png', height = 6, width = 6, dpi = 300)

## plot 7: percentage of river flows that replenish ####
## REinterpreting: if altered allowed is 0%, then unaltered is 100-value
# this plot is showing percentage of altered river flows

# Set the radius and center coordinates of the circle
radius <- 1
center_x <- 0
center_y <- 0

# circle vertices
circle <- data.frame(
  x = center_x + radius * cos(seq(0, 2 * pi, length.out = 360)),
  y = center_y + radius * sin(seq(0, 2 * pi, length.out = 360))
)

# select n% of the vertices to change to zero
circle2 <- data.frame(
  x = 2 + radius * cos(seq(0, 2 * pi, length.out = 360)),
  y = 2 + radius * sin(seq(0, 2 * pi, length.out = 360))
)
num_vertices <- nrow(circle2)
num_vertices_to_change <- round(0.53 * num_vertices)
vertices_to_change <- sample(1:num_vertices, num_vertices_to_change, replace = FALSE)

# create separate df with 30% of vertices set to 0
circle3 <- circle2 %>%
  mutate(x = ifelse(row_number() %in% vertices_to_change, 2, x),
         y = ifelse(row_number() %in% vertices_to_change, 2, y))

# Plot the modified circle
p7 <- ggplot() +
  geom_polygon(data = circle, aes(x, y), fill = 'grey40') +
  geom_polygon(data = circle2, aes(x, y), fill='grey40') +
  geom_polygon(data = circle3, aes(x, y), fill='#Ff3800') +
  coord_equal() +
  theme_void() +
  labs(title = 'Over-taxed river basins',
       subtitle = 'Percentage of river basins fully replenished',
       caption = "<span style='color:white'>Safe and just boundary: <span style='color:grey60'>100%.</span> <br> 
       Current state: <span style='color:#Ff3800'>47%.</span></span>") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = '#ABC7B2', color =NA),
        plot.background = element_rect(fill = '#ABC7B2', color=NA),
        panel.grid.minor = element_line(linewidth = .1, linetype = 'dashed',
                                        color = 'grey50'),
        panel.grid.major = element_line(linewidth = .2, linetype = 'dashed',
                                        color = 'grey50'),
        plot.title = element_text(family = 'Staatliches', size = 125,
                                  color = 'grey10', hjust = 0),
        plot.subtitle = element_text(family = 'Chivo', size = 46, 
                                     color = 'grey10', lineheight = .15),
        plot.caption = element_textbox_simple(family = 'Chivo', size = 50,
                                              lineheight = .2, hjust = 0.1,
                                              fill = "grey20", 
                                              padding = margin(5, 5, 5, 5)),
        plot.margin = margin(5,5,5,5)) 

p7
ggsave('plot7.png', height = 6, width = 6, dpi = 300)


## plot 8: percentage of river flows altered ####

# Set the radius and center coordinates of the circle
radius <- 1
center_x <- 2
center_y <- 2

# circle vertices
circle <- data.frame(
  x = center_x + radius * cos(seq(0, 2 * pi, length.out = 360)),
  y = center_y + radius * sin(seq(0, 2 * pi, length.out = 360))
)

# select n% of the vertices to change to zero
circle2 <- data.frame(
  x = 0 + radius * cos(seq(0, 2 * pi, length.out = 360)),
  y = 0 + radius * sin(seq(0, 2 * pi, length.out = 360))
)
num_vertices <- nrow(circle2)
num_vertices_to_change <- round(0.64 * num_vertices)
vertices_to_change <- sample(1:num_vertices, num_vertices_to_change, replace = FALSE)

# create separate df with 30% of vertices set to 0
circle3 <- circle2 %>%
  mutate(x = ifelse(row_number() %in% vertices_to_change, 0, x),
         y = ifelse(row_number() %in% vertices_to_change, 0, y))

# Plot the modified circle
p8 <- ggplot() +
  geom_polygon(data = circle, aes(x, y), fill = 'grey40') +
  geom_polygon(data = circle2, aes(x, y), fill='grey40') +
  geom_polygon(data = circle3, aes(x, y), fill='#Ff3800') +
  coord_equal() +
  theme_void() +
  labs(title = 'Altered stream flows',
       subtitle = "Natural stream flows not altered more than 20%",
       caption = "<span style='color:white'>Safe and just boundary: <span style='color:grey60'>100%.</span> <br> 
       Current state: <span style='color:#Ff3800'>34%.</span></span>") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = '#ABC7B2', color =NA),
        plot.background = element_rect(fill = '#ABC7B2', color=NA),
        panel.grid.minor = element_line(linewidth = .1, linetype = 'dashed',
                                        color = 'grey50'),
        panel.grid.major = element_line(linewidth = .2, linetype = 'dashed',
                                        color = 'grey50'),
        plot.title = element_text(family = 'Staatliches', size = 125,
                                  color = 'grey10', hjust = 0),
        plot.subtitle = element_text(family = 'Chivo', size = 46, 
                                     color = 'grey10', lineheight = .15),
        plot.caption = element_textbox_simple(family = 'Chivo', size = 50,
                                              lineheight = .2, hjust = 0.1,
                                              fill = "grey20", 
                                              padding = margin(5, 5, 5, 5)),
        plot.margin = margin(5,5,5,5)) 

p8
ggsave('plot8.png', height = 6, width = 6, dpi = 300)

### put it all together ####
library(magick)

top_img <- image_read('plot_main.png')
plot1 <- image_scale(image_read("plot1.png"),'1200')
plot2 <- image_scale(image_read("plot2.png"),'1200')
plot3 <- image_scale(image_read("plot3.png"),'1200')
plot6 <- image_scale(image_read("plot6.png"),'1200')
plot7 <- image_scale(image_read("plot7.png"),'1200')
plot8 <- image_scale(image_read("plot8.png"),'1200')

plot4 <- image_scale(image_read("plot4.png"),'1800')
plot5 <- image_scale(image_read("plot5.png"),'1800')

row1 <- image_append(c(plot1,plot2,plot3))
row2 <- image_append(c(plot6,plot7,plot8))
row3 <- image_append(c(plot4,plot5))

full_stack <- image_append(c(top_img, row1,row2,row3),stack=T)

image_write(full_stack, "earth_health.png")
