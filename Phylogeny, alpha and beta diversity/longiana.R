#read the data from here
library(tidyverse)
library(ggplot2)
library(ggpubr)
longi <- read.table("longitudinal.tsv", header = TRUE, sep = "\t", comment.char = "#")
analysis <- longi[,-c(1:18, 21,22)]
analysis <- analysis[,-3]

m_weaning <- aggregate(analysis[,c("observed_features", "shannon_entropy","faith_pd")], by = list(analysis$age_months, analysis$diet_weaning), FUN = mean)

rlist <- list(m_delivery = data.frame(), s_delivery = data.frame(),
              m_zygosity = data.frame(), s_zygosity = data.frame(),
              m_milk = data.frame(), s_milk = data.frame(),
              m_weaning = data.frame(), s_weaning = data.frame())
  
for ( i in seq(1,7,2)){
  rlist[[i]] <- aggregate(analysis[,c("observed_features", "shannon_entropy","faith_pd")], by = list(analysis$age_months, analysis[,(i+1)/2]), FUN = mean, na.rm = T)
  rlist[[i+1]] <- aggregate(analysis[,c("observed_features", "shannon_entropy","faith_pd")], by = list(analysis$age_months, analysis[,(i+1)/2]), FUN = sd, na.rm = T)
}

#rename the variables
for (i in c(2,4,6,8)){
  names(rlist[[i]])[3:5] <- c("sd1","sd2","sd3")
}

alllist <- list(delivery = data.frame(),
                zygosity = data.frame(),
                milk = data.frame(),
                weaning = data.frame())

for ( i in seq(2,8,2)){
   alllist[[i/2]] <- full_join(rlist[[i-1]],rlist[[i]], by = c("Group.1","Group.2"), keep = F)
}
#trying examples
ggplot(data = alllist$delivery, aes(x = Group.1, y = observed_features, col = Group.2))+
  geom_line(linewidth = 1.5)+
  geom_point()+
  geom_errorbar(aes(ymin=observed_features-sd1, ymax=observed_features+sd1))+
  theme_classic()+
  theme(legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))+
  guides(color=guide_legend(title = "Delivery Mode"))+
  xlab("Month of Life")+
  ylab("Observed Featrues")

#store the plots
plotlist <- list(a = list(), b = list(), c= list(), d = list(),
                 e = list(), f = list(), g = list(), h = list(),
                 i = list(), j = list(), k = list(), l = list())
titletable <- c("Delivery Mode", "Zygosity Type", "Feeding Milk Type", "Weaning Status")
for (ii in 1:4){
  plotlist[[ii]] <- ggplot(data = alllist[[ii]], aes(x = Group.1, y = observed_features, col = Group.2))+
    geom_line(linewidth = 1.5)+
    geom_point()+
    geom_errorbar(aes(ymin=observed_features-sd1, ymax=observed_features+sd1))+
    theme_classic()+
    theme(legend.position = c(.95, .05),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6))+
    guides(color=guide_legend(title = titletable[ii]))+
    xlab("Month of Life")+
    ylab("Observed Featrues")
}
for ( ii in 5:8){
  plotlist[[ii]] <- ggplot(data = alllist[[ii-4]], aes(x = Group.1, y = shannon_entropy, col = Group.2))+
    geom_line(linewidth = 1.5)+
    geom_point()+
    geom_errorbar(aes(ymin=shannon_entropy-sd2, ymax=shannon_entropy+sd2))+
    theme_classic()+
    theme(legend.position = c(.95, .05),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6))+
    guides(color=guide_legend(title = titletable[ii-4]))+
    xlab("Month of Life")+
    ylab("Shannon Entropy")
}
for(ii in 9:12){
  plotlist[[ii]] <- ggplot(data = alllist[[ii-8]], aes(x = Group.1, y = faith_pd, col = Group.2))+
    geom_line(linewidth = 1.5)+
    geom_point()+
    geom_errorbar(aes(ymin=faith_pd-sd3, ymax=faith_pd+sd3))+
    theme_classic()+
    theme(legend.position = c(.95, .05),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6))+
    guides(color=guide_legend(title = titletable[ii-8]))+
    xlab("Month of Life")+
    ylab("Phylogenetic Diversity")
}

#arrange the plot

ggarrange(plotlist[[1]],plotlist[[5]], plotlist[[9]] ,plotlist[[2]], 
          plotlist[[6]] ,plotlist[[10]], plotlist[[3]], plotlist[[7]],
          plotlist[[11]],plotlist[[4]],plotlist[[8]],plotlist[[12]],
          labels = c("A", "B", "C", "D", "E", "F", "G","H","I","J","K","L"),
          ncol = 3, nrow = 4)
