df_links <- read_data(input_file_path = "myapp/www/trip_assignment_example.xlsx",
                      sheet_name = "links")
df_flows <- read_data(input_file_path = "myapp/www/trip_assignment_example.xlsx",
                      sheet_name = "flows")

road_network <- creating_graph(df_links, a = 1, b = 1)
road_network <- initialization(g = road_network,
                               df_flows = df_flows)



fw <- trip_assignment(g = road_network,
                      df_flows = df_flows,
                      iterations = 2,
                      bisection_iterations = 5,
                      gap = 0.01,
                      alg = "fw",
                      AoN_algorithm = AoN)



msa <- trip_assignment(g = road_network, df_flows = df_flows,
                       iterations = 6,
                       bisection_iterations = 0,
                       gap = 0.001,
                       alg = "msa",
                       AoN_algorithm = AoN)


as_data_frame(fw[[1]])
as_data_frame(msa[[1]])

data.frame(
  "num" = msa[[2]],
  "aec" = msa[[3]]
) %>% 
  ggplot(aes(num, aec))+
  geom_line()+
  geom_point()+
  theme_minimal()
