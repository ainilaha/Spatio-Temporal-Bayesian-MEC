library(plotly)


p <- plot_ly(sum_time, x =~long, y = ~lat, z = ~sum_time, type = 'mesh3d')
p


p <- plot_ly(sum_time, x =~long,
             y = ~lat, z = ~sum_time,
             color = ~rnd_strat,
             colors = c('#CCFF00', '#0000CC'),type = "scatter3d")
p




Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiYWluaWxhaGEiLCJhIjoiY2p1enYzbm9hMWNzMTQzbXcyNWt0M3VwNyJ9._PwdGJ4wfOnzfZ9Ux9Oxvw')


p <- plot_mapbox(data, x = ~long, y = ~lat) %>%
  add_paths(size = I(2)) %>%
  add_segments(x = -100, xend = -50, y = 50, 75)

p


p <- data %>%
  plot_mapbox(lat = ~lat, lon = ~long,
              split = ~class, size=2,
              mode = 'scattermapbox', hoverinfo='user_id') %>%
  layout(title = 'Shanghai Network (by Laha Ale)',
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))
p


