rm(list=ls())
graphics.off()
# # # Install relevant branch of YAPS from github - One time operation
# devtools::install_github("baktoft/yaps")

# Setup paths to your specific setup...
script_path <- 'YAPS_simulation_study'
data_path = "./data" # this directory must exist and contain dir sim_out with subdirs sim_part_0, sim_part_1, sim_part_2, sim_part_3, sim_part_4
setwd(script_path)

library(yaps)
library(dplyr)
library(plyr)
library(data.table)
library(ggplot2)
source('functions.R')
#set.seed(42)

#############################
### Simulate track specified in getSimSetup()
# # # sim_part 0 - test - short version of everything
# # # sim_part 1 - effect of pMP
# # # sim_part 2 - effect of shifts
# # # sim_part 3 - effect of track length
# # # sim_part 4 - effect of temporal resolution of hydros
# specify which part of the study to simulate for...
sim_part <- 0
# get the sim_setup....
sim_setup <- getSimSetup(sim_part=sim_part)

# get list of simulated tracks etc...
# this can take quite a long time, so save the results for future use
simul <- getSimul(sim_setup)
save(list=c('simul'), file=paste0(data_path, "/sim_part", sim_part,"_sim_dat.rObj"))

# When all tracks are simulated and saved, we can start positioning from here...
load(file=paste0(data_path, "/sim_part", sim_part,"_sim_dat.rObj"))
# Yaps sometimes fail, so we give it a chance to retry...
max_retries <- 100
gb <- doRunYapsParallel(simul=simul, max_retries=max_retries, sim_part, data_path)


###############################
### post-processing and plotting
# Takes a long time, so save when done for future use...
summ_part0 <- collate_output(sim_part=0, data_path)
summ_part1 <- collate_output(sim_part=1, data_path)
summ_part2 <- collate_output(sim_part=2, data_path)
summ_part3 <- collate_output(sim_part=3, data_path)
summ_part4 <- collate_output(sim_part=4, data_path)

save(summ_part0, file=paste0(data_path, "/summ_part0.rObj"))
save(summ_part1, file=paste0(data_path, "/summ_part1.rObj"))
save(summ_part2, file=paste0(data_path, "/summ_part2.rObj"))
save(summ_part3, file=paste0(data_path, "/summ_part3.rObj"))
save(summ_part4, file=paste0(data_path, "/summ_part4.rObj"))

load(file=paste0(data_path, "/summ_part1.rObj"))
load(file=paste0(data_path, "/summ_part2.rObj"))
load(file=paste0(data_path, "/summ_part3.rObj"))
load(file=paste0(data_path, "/summ_part4.rObj"))

## lay-out stuff for figures
vir_begin = 0
vir_end = 0.8
text_size = 16
# Change htr values to the ones used in paper
summ_part4$htr <- lapply(summ_part4$htr, function(x) replace(x,x==1000, 1000))
summ_part4$htr <- lapply(summ_part4$htr, function(x) replace(x,x==4800, 200))
summ_part4$htr <- lapply(summ_part4$htr, function(x) replace(x,x==19200, 52))
summ_part4$htr <- lapply(summ_part4$htr, function(x) replace(x,x==7692, 130))
summ_part4$htr <- unlist(summ_part4$htr)
# Fix right order
summ_part4$htr <- factor(summ_part4$htr, levels=c(1000, 200, 130, 52))

p1 <- ggplot(data=summ_part1, aes(x=jitter(mean_bi), y=median_dev, col=factor(ping_type), fill=factor(ping_type))) + geom_point(alpha=0.5) + geom_smooth(method='loess') +  facet_grid(.~pMP)
p2 <- ggplot(data=summ_part2, aes(x=jitter(mean_bi), y=median_dev, col=factor(ping_type), fill=factor(ping_type))) + geom_point(alpha=0.5) + geom_smooth(method='loess') +  facet_grid(.~shift)
p3 <- ggplot(data=summ_part3, aes(x=jitter(mean_bi), y=median_dev, col=factor(ping_type), fill=factor(ping_type))) + geom_point(alpha=0.5) + geom_smooth(method='loess') +  facet_grid(.~track_length)
p4 <- ggplot(data=summ_part4, aes(x=jitter(mean_bi), y=median_dev, col=factor(ping_type), fill=factor(ping_type))) + geom_point(alpha=0.5) + geom_smooth(method='loess') +  facet_grid(.~htr)

p1 <- p1 + theme_minimal() + theme(text = element_text(size=text_size), legend.position="none", plot.background=element_rect(fill="white"), axis.line=element_line(color="black")) + coord_cartesian(ylim=c(0,20)) + viridis::scale_color_viridis(discrete=TRUE, option="D", begin=vir_begin, end=vir_end) + viridis::scale_fill_viridis(discrete = TRUE, option="D", begin=vir_begin, end=vir_end) + labs(title="p(MP)") + xlab("mean burst interval (s)") + ylab("median error (m)")
p2 <- p2 + theme_minimal() + theme(text = element_text(size=text_size), legend.position="none", plot.background=element_rect(fill="white"), axis.line=element_line(color="black")) + coord_cartesian(ylim=c(0,20)) + viridis::scale_color_viridis(discrete=TRUE, option="D", begin=vir_begin, end=vir_end) + viridis::scale_fill_viridis(discrete = TRUE, option="D", begin=vir_begin, end=vir_end) + labs(title="Out-of-array shifts") + xlab("mean burst interval (s)") + ylab("median error (m)")
p3 <- p3 + theme_minimal() + theme(text = element_text(size=text_size), legend.position="none", plot.background=element_rect(fill="white"), axis.line=element_line(color="black")) + coord_cartesian(ylim=c(0,20)) + viridis::scale_color_viridis(discrete=TRUE, option="D", begin=vir_begin, end=vir_end) + viridis::scale_fill_viridis(discrete = TRUE, option="D", begin=vir_begin, end=vir_end) + labs(title="Track length") + xlab("mean burst interval (s)") + ylab("median error (m)")
p4 <- p4 + theme_minimal() + theme(text = element_text(size=text_size), legend.position="none", plot.background=element_rect(fill="white"), axis.line=element_line(color="black")) + coord_cartesian(ylim=c(0,20)) + viridis::scale_color_viridis(discrete=TRUE, option="D", begin=vir_begin, end=vir_end) + viridis::scale_fill_viridis(discrete = TRUE, option="D", begin=vir_begin, end=vir_end) + labs(title="Temporal hydrophone resolution (1E-6 s)") + xlab("mean burst interval (s)") + ylab("median error (m)")

show(p1)
gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol=2)

