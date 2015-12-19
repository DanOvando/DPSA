library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)


dat <- read.csv('Galapagos Red Spiny Lobster/2014-2015 full length data.csv', stringsAsFactors = F)

dat$weight_grams <- as.numeric(dat$weight_grams)

boot <- list()

ids <- unique(dat$id)


TripSummary <- subset(dat, species == 'R') %>%
  group_by(date,fishing_island) %>%
  summarize(weight_landed = sum(weight_grams, na.rm= T))

quartz()
(ggplot(TripSummary, aes(date,weight_landed, fill = fishing_island))
+ geom_bar(stat = 'identity') + 
  scale_fill_brewer(palette = 'Dark2')+
  theme(axis.text.x=element_text(angle=45,hjust=0.9,vjust=0.9)))

samps <- seq(25,500,by = 25)

cc <- 0

Iterations <- 100

dat$binned_length <- round(dat$total_length_cm,0)

ids <- unique(dat$id)

cc <- 0

Breaks <- seq(min(dat$binned_length, na.rm = T), max(dat$binned_length, na.rm = T), by = 1)

length_comp_ss <- function(samp_length,n,Breaks)
{
  
  OrigHist <- hist(samp_length, breaks = Breaks, plot = F)
  
  samp_size <- min(sum(is.na(samp_length)==F),n)
  
  NewHist <- hist(sample(samp_length,size = n,replace = T), breaks = Breaks, plot = F)
  
  SSD <- sum((OrigHist$density - NewHist$density)^2)
  return(SSD)
}

cc <- 0

for (s in 1:length(samps))
{
  
  for (i in 1:Iterations)
  {
    cc <- cc+1
    show(cc)
    SS <- dat %>%
      group_by(id) %>%
      summarise(ss = length_comp_ss(total_length_cm,samps[s], Breaks))
    
    temp <- data.frame(SS,samps[s],i)
    
    boot[[cc]] <- temp
  }
}


boot <- ldply(boot)

bootstrap_plot <- ggplot(boot, aes(factor(samps.s.),ss)) + xlab('Muestras') + ylab('Error')+geom_boxplot()

ggsave(file = 'bootstrapped sampline size.pdf', plot = bootstrap_plot)

