# Plotting Figure (HR, Distance, Season, Sex, Crossing, Dielphase)
plot_HrSexSeason # Home range Vs Sex Vs Season (From HR_akde)
plot_DistSexSeason # Daily travel distance Vs sex Vs Season (from plot Distance)
plot_hourlyDistance  # Diel phase plot (circular) - from Distance page
plot_Diel    # Distance Vs diel phase and Season - faceted (from distance page) 
# No need to plot this 

plot_crossing # Track crossing plot (from crossing page)

library(cowplot)

all_plots <- plot_grid(
  plot_grid(plot_DistSexSeason, plot_HrSexSeason, ncol = 2, rel_widths = c(1, 1), 
            labels = c('A.', 'B.'), label_size = 10, vjust = 2.75),  # First row with labels 'A.' and 'B.'
  plot_grid(plot_hourlyDistance, labels = 'C.', label_size = 10, hjust = -15, vjust = 2.75),  # Second row with label 'C.' centered
  ncol = 1,  # Stack rows
  rel_heights = c(1, 1.15))  # Adjust heights for the two rows



ggsave(filename = "./Output/All_plots.jpg", plot = all_plots, 
       height = 7.5, width = 7.5, dpi = 300)
