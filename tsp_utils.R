# Note: start seems to not be the # appearing in df_xy (look at as.matrix etc)

get_tour <- function(df_xy,method="2-opt",start=1) { 
  coords_mtx <- df_xy %>% as.matrix
  dist_mtx <- coords_mtx %>% dist
  tsp_ins <- tspmeta::tsp_instance(coords_mtx, dist_mtx)
  tour <- tspmeta::run_solver(tsp_ins, method=method, start=start, verbose=F) 
  # tour <- TSP::solve_TSP(tsp_ins, method = method, control = list(start = start))
  tour
}

plot_tour <- function(df, tour, color, algm)
  df[tour %>% as.integer,] %>%
  ggplot(aes(x,y)) +
  geom_path(color=color) +
  geom_point(size=I(2),color="black") +
  #autoplot(tsp.ins, tour) +
  ggtitle(sprintf("algm: %s\npoints: %d, length: %.2f",
                  algm, tour %>% as.integer %>% length,
                  TSP::tour_length(tour))) +
  geom_text(aes(x,y,label=id_tsp),
            data=df[tour %>% as.integer,]%>%mutate(id_tsp=row_number())%>%slice(c(1,tour %>% as.integer %>% length)),
            color="black",vjust=-1) +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(colour = "black"))

process_tour <- function(tour,capitais) capitais %>%
  mutate(id=row_number()) %>%
  slice(tour%>%as.integer) %>%
  mutate(id_tsp=row_number())