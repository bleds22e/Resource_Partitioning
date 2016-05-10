library(dplyr)

portal <- read.csv("Portal_rodent.csv")

march2016 <- select(portal, period, species) %>% 
             filter(period == 447, species == 'DM' | species == 'DO' | species == 'PP') %>% 
             group_by(species) %>% 
             summarise(count = n())
             
july_nov <- select(portal, yr, mo, species, plot) %>% 
            filter(yr >= 2010, mo >= 7 & mo < 12, 
                   species == 'DM' | species == 'DO' | species == 'PP') %>% 
            group_by(yr, mo, plot, species)

plot_type <- function(plot){
  # function to add plot type column
  if (plot == 1 | plot == 9 | plot == 10 | plot == 12 | plot == 16 | plot == 23){
    plot_type = 'rodent_exclosure'
  } else if (plot == 2 | plot == 3 | plot == 8 | plot == 15 | 
             plot == 19 | plot == 20 | plot == 21 | plot == 22) {
    plot_type = 'krat_exclosure'
  } else {
    plot_type = 'control'
  }
  return(plot_type)
}

july_nov_plot <- as.list(july_nov$plot)
july_nov$plot_type <- rapply(july_nov_plot, plot_type)

july_nov <- july_nov %>% select(yr, mo, plot_type, species) %>% 
            group_by(yr, mo, plot_type, species) %>% 
            summarise(count = n())
july_nov_avg <- select(july_nov, yr, mo, plot_type, species, count) %>% 
                group_by(mo, plot_type, species) %>% 
                summarise(avg = mean(count))
