rm(list=ls())

#load required packages
packages <- c('plyr',
              'ggplot2',
              'reshape',
              'dplyr',
              'cowplot',
              'RColorBrewer',
              'rprojroot',
              'tidyr',
              'viridis')
lapply(packages, require, character.only = TRUE)

#set path
path <- function(x) rprojroot::find_root_file(x, criterion = has_file("social-recommendation.Rproj"))



############################################################################################
############################################################################################
#PLOT 1: Best performing strategy for each individual at 3 different levels of experience###
############################################################################################
############################################################################################

load(path("/jesterDatasets/jesterFull.Rdata"))
load(path("/results/baseline.Rdata"))

a<-cor(t(bigLaugh))
means <- colMeans(a)
sds <- apply(a,2,sd)

#remove strategies not used in the analysis, namely: random other, contextual benachmark, similar options
baseline <- baseline[,,-c(4,7,8)]

cbPalette <-brewer.pal(5,"Set1")
cbPalette <- cbPalette[c(2,1,3,4,5)]

#select the three experience levels: 10, 25, 90%
mat <- baseline[,c(1,5,15),]

names<-             c("Doppelgänger",
                      "Whole crowd",
                      "Clique (k=10)",
                      "Similarity-weighted crowd",
                      "Similar crowd (t=0)")

#select best performing strategy for each individual and experience level
best1 <- sapply(1:nrow(mat), function(x) which.max(mat[x,1,]) )
best2 <- sapply(1:nrow(mat), function(x) which.max(mat[x,2,]) )
best3 <- sapply(1:nrow(mat), function(x) which.max(mat[x,3,]) )

mat2 <- cbind(means,
              sds,
              best1,
              best2,
              best3)

df <- melt(as.data.frame(mat2),id.vars=1:2)
df$value <- names[df$value]
levels(df$variable) <- c("10 options experienced",
                         "25 options experienced",
                         "75 options experienced")

df$value <- factor(df$value, levels = levels(factor(df$value))[c(2,1,3,4,5)])

plot <- ggplot(df,aes(x=means,y=sds,color=value,group=value))+
  geom_point(size=0.1)+facet_wrap(~variable)+
  geom_segment(data=df, aes(x=mean(df$means), xend=mean(df$means), y=min(df$sds), yend=max(df$sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(data=df, aes(x=min(df$means), xend=max(df$means), y=mean(df$sds), yend=mean(df$sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.direction="horizontal")+
  guides(fill=guide_legend(ncol=3),
         colour = guide_legend(override.aes = list(size=1)))+
  scale_color_manual(values=cbPalette)+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())



save_plot("bestStrategyCloud.png",plot,base_height=4,base_width=10)



############################################################################################
############################################################################################
#PLOT 2: Total error at 3 different levels of experience for 5 different strategies#########
############################################################################################
############################################################################################

load(path("/jesterDatasets/jesterFull.Rdata"))
load(path("/results/baseline.Rdata"))

a<-cor(t(bigLaugh))
means <- colMeans(a)
sds <- apply(a,2,sd)

baseline <- baseline[,,-c(7,8)]

cbPalette <-brewer.pal(5,"Set1")
cbPalette <- cbPalette[c(2,1,3,4,5)]
mat <- baseline[,c(1,5,15),]

names <-             c("Doppelgänger",
                      "Whole crowd",
                      "Clique (k=10)",
                      "Random other",
                      "Similarity-weighted crowd",
                      "Similar crowd (t=0)")

mat1 <- cbind(means,sds,mat[,,1])
df1 <- melt(as.data.frame(mat1),id.vars=1:2)
levels(df1$variable) <- c("10 options experienced",
                         "25 options experienced",
                         "75 options experienced")

mat2 <- cbind(means,sds,mat[,,2])
df2 <- melt(as.data.frame(mat2),id.vars=1:2)
levels(df2$variable) <- c("10 options experienced",
                          "25 options experienced",
                          "75 options experienced")
mat3 <- cbind(means,sds,mat[,,3])
df3 <- melt(as.data.frame(mat3),id.vars=1:2)
levels(df3$variable) <- c("10 options experienced",
                          "25 options experienced",
                          "75 options experienced")

mat4 <- cbind(means,sds,mat[,,4])
df4 <- melt(as.data.frame(mat4),id.vars=1:2)
levels(df4$variable) <- c("10 options experienced",
                          "25 options experienced",
                          "75 options experienced")

mat5 <- cbind(means,sds,mat[,,5])
df5 <- melt(as.data.frame(mat5),id.vars=1:2)
levels(df5$variable) <- c("10 options experienced",
                          "25 options experienced",
                          "75 options experienced")

mat6 <- cbind(means,sds,mat[,,6])
df6 <- melt(as.data.frame(mat6),id.vars=1:2)
levels(df6$variable) <- c("10 options experienced",
                          "25 options experienced",
                          "75 options experienced")

myPalette <- colorRampPalette(rev(brewer.pal(9,"YlOrRd")))

subPlot1 <- df1 %>% ggplot(aes(x=means,y=sds,color=value,group=value))+
  geom_point(size=0.1)+facet_wrap(~variable)+
  geom_segment(aes(x=mean(means), xend=mean(means), y=min(sds), yend=max(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(aes(x=min(means), xend=max(means), y=mean(sds), yend=mean(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="none",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_color_viridis(option="inferno", limits=c(0.3,0.8))

subPlot2 <- df2 %>% ggplot(aes(x=means,y=sds,color=value,group=value))+
  geom_point(size=0.1)+facet_wrap(~variable)+
  geom_segment(aes(x=mean(means), xend=mean(means), y=min(sds), yend=max(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(aes(x=min(means), xend=max(means), y=mean(sds), yend=mean(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="none",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_color_viridis(option="inferno", limits=c(0.3,0.8))

subPlot3 <- df3 %>% ggplot(aes(x=means,y=sds,color=value,group=value))+
  geom_point(size=0.1)+facet_wrap(~variable)+
  geom_segment(aes(x=mean(means), xend=mean(means), y=min(sds), yend=max(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(aes(x=min(means), xend=max(means), y=mean(sds), yend=mean(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="none",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_color_viridis(option="inferno", limits=c(0.3,0.8))

subPlot4 <- df4 %>% ggplot(aes(x=means,y=sds,color=value,group=value))+
  geom_point(size=0.1)+facet_wrap(~variable)+
  geom_segment(aes(x=mean(means), xend=mean(means), y=min(sds), yend=max(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(aes(x=min(means), xend=max(means), y=mean(sds), yend=mean(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="none",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_color_viridis(option="inferno", limits=c(0.3,0.8))

subPlot5 <- df5 %>% ggplot(aes(x=means,y=sds,color=value,group=value))+
  geom_point(size=0.1)+facet_wrap(~variable)+
  geom_segment(aes(x=mean(means), xend=mean(means), y=min(sds), yend=max(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(aes(x=min(means), xend=max(means), y=mean(sds), yend=mean(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="none",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_color_viridis(option="inferno", limits=c(0.3,0.8))

subPlot6 <- df6 %>% ggplot(aes(x=means,y=sds,color=value,group=value))+
  geom_point(size=0.1)+facet_wrap(~variable)+
  geom_segment(aes(x=mean(means), xend=mean(means), y=min(sds), yend=max(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(aes(x=min(means), xend=max(means), y=mean(sds), yend=mean(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="none",
        legend.title=element_blank(),
        legend.direction="horizontal")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_color_viridis(option="inferno", limits=c(0.3,0.8))


fullPlot <- plot_grid(subPlot1+ggtitle("Doppelgänger")+theme(plot.title = element_text()),
               subPlot3+ggtitle("Clique")+theme(plot.title = element_text()),
               subPlot5+ggtitle("Similarity-weighted crowd")+theme(plot.title = element_text()),
               subPlot6+ggtitle("Similar crowd")+theme(plot.title = element_text()),
               subPlot2+ggtitle("Whole crowd")+theme(plot.title = element_text()),
               nrow=5)
save_plot("totalError.png",fullPlot,base_height=16,base_width=10)



############################################################################################
############################################################################################
#PLOT 3: Total error for different strategies at 25% experience level#######################
############################################################################################
############################################################################################

load(path("/jesterDatasets/jesterFull.Rdata"))
load(path("/results/baseline.Rdata"))

a<-cor(t(bigLaugh))
means <- colMeans(a)
sds <- apply(a,2,sd)

baseline <- baseline[,,-c(7,8)]

cbPalette <-brewer.pal(5,"Set1")
cbPalette <- cbPalette[c(2,1,3,4,5)]
mat <- baseline[,c(5),]

names <-             c("Doppelgänger",
                       "Whole crowd",
                       "Clique (k=10)",
                       "Random",
                       "Similarity-weighted crowd",
                       "Similar crowd (t=0)")


mat1 <- cbind(means,sds,mat[,1])
df1 <- melt(as.data.frame(mat1),id.vars=1:2)

mat2 <- cbind(means,sds,mat[,2])
df2 <- melt(as.data.frame(mat2),id.vars=1:2)

mat3 <- cbind(means,sds,mat[,3])
df3 <- melt(as.data.frame(mat3),id.vars=1:2)

mat4 <- cbind(means,sds,mat[,4])
df4 <- melt(as.data.frame(mat4),id.vars=1:2)

mat5 <- cbind(means,sds,mat[,5])
df5 <- melt(as.data.frame(mat5),id.vars=1:2)

mat6 <- cbind(means,sds,mat[,6])
df6 <- melt(as.data.frame(mat6),id.vars=1:2)



subPlot1 <- df1 %>% ggplot(aes(x=means,y=sds,color=value,group=value))+
  geom_point(size=0.1)+
  geom_segment(aes(x=mean(means), xend=mean(means), y=min(sds), yend=max(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(aes(x=min(means), xend=max(means), y=mean(sds), yend=mean(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="right",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_color_viridis(option="inferno", limits=c(0.3,0.8))

subPlot2 <- df2 %>% ggplot(aes(x=means,y=sds,color=value,group=value))+
  geom_point(size=0.1)+
  geom_segment(aes(x=mean(means), xend=mean(means), y=min(sds), yend=max(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(aes(x=min(means), xend=max(means), y=mean(sds), yend=mean(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="right",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_color_viridis(option="inferno", limits=c(0.3,0.8))

subPlot3 <- df3 %>% ggplot(aes(x=means,y=sds,color=value,group=value))+
  geom_point(size=0.1)+
  geom_segment(aes(x=mean(means), xend=mean(means), y=min(sds), yend=max(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(aes(x=min(means), xend=max(means), y=mean(sds), yend=mean(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="right",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_color_viridis(option="inferno", limits=c(0.3,0.8))

subPlot4 <- df4 %>% ggplot(aes(x=means,y=sds,color=value,group=value))+
  geom_point(size=0.1)+
  geom_segment(aes(x=mean(means), xend=mean(means), y=min(sds), yend=max(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(aes(x=min(means), xend=max(means), y=mean(sds), yend=mean(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="right",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_color_viridis(option="inferno", limits=c(0.3,0.8))

subPlot5 <- df5 %>% ggplot(aes(x=means,y=sds,color=value,group=value))+
  geom_point(size=0.1)+
  geom_segment(aes(x=mean(means), xend=mean(means), y=min(sds), yend=max(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(aes(x=min(means), xend=max(means), y=mean(sds), yend=mean(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="right",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_color_viridis(option="inferno", limits=c(0.3,0.8))

subPlot6 <- df6 %>% ggplot(aes(x=means,y=sds,color=value,group=value))+
  geom_point(size=0.1)+
  geom_segment(aes(x=mean(means), xend=mean(means), y=min(sds), yend=max(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(aes(x=min(means), xend=max(means), y=mean(sds), yend=mean(sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="right",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_color_viridis(option="inferno", limits=c(0.3,0.8))



#leg <- get_legend(subPlot7)

fullPlot <- plot_grid(subPlot1+ggtitle("Doppelgänger")+theme(plot.title = element_text())+theme(legend.position="none"),
               subPlot3+ggtitle("Clique")+theme(plot.title = element_text())+theme(legend.position="none"),
               subPlot5+ggtitle("Similarity-weighted crowd")+theme(plot.title = element_text()),
               subPlot6+ggtitle("Similar crowd")+theme(plot.title = element_text())+theme(legend.position="none"),
               subPlot2+ggtitle("Whole crowd")+theme(plot.title = element_text())+theme(legend.position="none"),
               subPlot4+ggtitle("Random other")+theme(plot.title = element_text()),
               nrow=2,
               rel_widths=c(.8,.8,1,.8,.8,1))
save_plot("totalErrorExp25.png",fullPlot,base_height=7,base_width=13)

############################################################################################
############################################################################################
#PLOT 4: Strategy match-ups#################################################################
############################################################################################
############################################################################################

load(path("/jesterDatasets/jesterFull.Rdata"))
load("/results/baseline.Rdata")

a<-cor(t(bigLaugh))
means <- colMeans(a)
sds <- apply(a,2,sd)

baseline <- baseline[,,-c(4,7,8)] #remove benchmarks
mat <- baseline[,c(1,5,15),] #select experience levels

colors <- brewer.pal(5,"Set1")


#1: Doppelganger vs whole crowd
df1 <- cbind(means,
             sds,
             ifelse(mat[,,1]-mat[,,2]>0,-1,1))

#1: Similarity-weighted vs Similar
df2 <- cbind(means,
             sds,
             ifelse(mat[,,4]-mat[,,5]>0,-1,1))

#1: Doppelganger vs clique
df3 <- cbind(means,
             sds,
             ifelse(mat[,,1]-mat[,,3]>0,-1,1))

#1: Clique vs similar
df4 <- cbind(means,
             sds,
             ifelse(mat[,,3]-mat[,,5]>0,-1,1))

#1: Similarity-weighted vs whole crowd
df5 <- cbind(means,
             sds,
             ifelse(mat[,,4]-mat[,,2]>0,-1,1))



df <- cbind(means,
            sds,
            ifelse(mat[,,1]-mat[,,2]>0,1,-1),
            ifelse(mat[,,4]-mat[,,5]>0,1,-1),
            ifelse(mat[,,3]-mat[,,4]>0,1,-1),
            ifelse(mat[,,3]-mat[,,5]>0,1,-1),
            ifelse(mat[,,4]-mat[,,2]>0,1,-1))


colnames(df1)<- c("means","sds","1","2","3")
colnames(df2)<- c("means","sds","1","2","3")
colnames(df3)<- c("means","sds","1","2","3")
colnames(df4)<- c("means","sds","1","2","3")
colnames(df5)<- c("means","sds","1","2","3")

df1 <- melt(as.data.frame(df1),id.vars=1:2)
levels(df1$variable) <- c("10 options experienced",
                          "25 options experienced",
                          "75 options experienced")
df2 <- melt(as.data.frame(df2),id.vars=1:2)
levels(df2$variable) <- c("10 options experienced",
                          "25 options experienced",
                          "75 options experienced")
df3 <- melt(as.data.frame(df3),id.vars=1:2)
levels(df3$variable) <- c("10 options experienced",
                          "25 options experienced",
                          "75 options experienced")
df4 <- melt(as.data.frame(df4),id.vars=1:2)
levels(df4$variable) <- c("10 options experienced",
                          "25 options experienced",
                          "75 options experienced")
df5 <- melt(as.data.frame(df5),id.vars=1:2)
levels(df5$variable) <- c("10 options experienced",
                          "25 options experienced",
                          "75 options experienced")


subPlot1 <- ggplot(df1,aes(x=means,y=sds))+
  geom_point(size=0.6,aes(color=factor(value)),alpha=0.4)+
  facet_wrap(~variable)+
  geom_segment(data=df1, aes(x=mean(df1$means), xend=mean(df1$means), y=min(df1$sds), yend=max(df1$sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(data=df1, aes(x=min(df1$means), xend=max(df1$means), y=mean(df1$sds), yend=mean(df1$sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.title=element_blank(),
        legend.direction="horizontal",
        legend.position="top")+
  guides(fill=guide_legend(ncol=3),
         colour = guide_legend(override.aes = list(size=2, alpha=1)))+
  xlab("Mean taste similarity (correlation)")+
  ylab("Taste diversity (SD)")+
  theme(strip.background = element_blank(),
        strip.text=element_blank())+
  scale_color_manual(values=c(colors[1],colors[2]),
                     labels=c("Doppelgänger","Whole crowd"))+
  #scale_color_distiller(type="div",palette = "BrBG")+
  ggtitle("")+
  theme(axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        legend.margin=margin(0,0,0,0,unit="cm"),
        plot.margin = margin(0,0,0,0,unit="cm"))

subPlot2 <- ggplot(df2,aes(x=means,y=sds))+
  geom_point(size=0.6,aes(color=factor(value)),alpha=0.4)+
  facet_wrap(~variable)+
  geom_segment(data=df2, aes(x=mean(df2$means), xend=mean(df2$means), y=min(df2$sds), yend=max(df2$sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(data=df2, aes(x=min(df2$means), xend=max(df2$means), y=mean(df2$sds), yend=mean(df2$sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.title=element_blank(),
        legend.direction="horizontal",
        legend.position="top")+
  guides(fill=guide_legend(ncol=3),
         colour = guide_legend(override.aes = list(size=2, alpha=1)))+
  xlab("Mean taste similarity (correlation)")+
  ylab("Taste diversity (SD)")+
  theme(strip.background = element_blank(),
        strip.text=element_blank())+
  scale_color_manual(values=c(colors[4],colors[5]),
                     labels=c("Similarity-weighted crowd","Similar crowd"))+
  ggtitle("")+
  theme(axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        legend.margin=margin(0,0,0,0,unit="cm"),
        plot.margin = margin(0,0,0,0,unit="cm"))

subPlot3 <- ggplot(df3,aes(x=means,y=sds))+
  geom_point(size=0.6,aes(color=factor(value)),alpha=0.4)+
  facet_wrap(~variable)+
  geom_segment(data=df3, aes(x=mean(df3$means), xend=mean(df3$means), y=min(df3$sds), yend=max(df3$sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(data=df3, aes(x=min(df3$means), xend=max(df3$means), y=mean(df3$sds), yend=mean(df3$sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.title=element_blank(),
        legend.direction="horizontal",
        legend.position="top")+
  guides(fill=guide_legend(ncol=3),
         colour = guide_legend(override.aes = list(size=2, alpha=1)))+
  xlab("Mean taste similarity (correlation)")+
  ylab("Taste diversity")+
  theme(strip.background = element_blank(),
        strip.text=element_blank())+
  scale_color_manual(values=c(colors[1],colors[3]), labels=c("Doppelgänger","Clique") )+
  #scale_color_distiller(type="div",palette = "PRGn")+
  ggtitle("")+
  theme(axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "black"),
        legend.margin=margin(0,0,0,0,unit="cm"),
        plot.margin = margin(0,0,0,0,unit="cm"))

subPlot4 <- ggplot(df4,aes(x=means,y=sds))+
  geom_point(size=0.6,aes(color=factor(value)),alpha=0.4)+
  facet_wrap(~variable)+
  geom_segment(data=df4, aes(x=mean(df4$means), xend=mean(df4$means), y=min(df4$sds), yend=max(df4$sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(data=df4, aes(x=min(df4$means), xend=max(df4$means), y=mean(df4$sds), yend=mean(df4$sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.title=element_blank(),
        legend.direction="horizontal",
        legend.position="top")+
  guides(fill=guide_legend(ncol=3),
         colour = guide_legend(override.aes = list(size=2, alpha=1)))+
  xlab("Mean taste similarity (correlation)")+
  ylab("Taste diversity (SD)")+
  theme(strip.background = element_blank(),
        strip.text=element_blank())+
  scale_color_manual(values=c(colors[3],colors[5]), labels=c("Clique","Similar crowd") )+
  ggtitle("")+
  theme(axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        legend.margin=margin(0,0,0,0,unit="cm"),
        plot.margin = margin(0,0,0,0,unit="cm"))

subPlot5 <- ggplot(df5,aes(x=means,y=sds))+
  geom_point(size=0.6,aes(color=factor(value)),alpha=0.4)+
  facet_wrap(~variable)+
  geom_segment(data=df5, aes(x=mean(df5$means), xend=mean(df5$means), y=min(df5$sds), yend=max(df5$sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(data=df5, aes(x=min(df5$means), xend=max(df5$means), y=mean(df5$sds), yend=mean(df5$sds)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.title=element_blank(),
        legend.direction="horizontal",
        legend.position="top")+
  guides(fill=guide_legend(ncol=3),
         colour = guide_legend(override.aes = list(size=2, alpha=1)))+
  xlab("Mean taste similarity (correlation)")+
  ylab("Taste diversity")+
  ggtitle("")+
  theme(strip.background = element_blank(),
        strip.text=element_blank())+
  scale_color_manual(values=c(colors[4],colors[2]), labels=c("Similarity-weighted crowd","Whole crowd"))+
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "white"),
        legend.margin=margin(0,0,0,0,unit="cm"),
        plot.margin = margin(0,0,0,0,unit="cm"))


prow <- plot_grid(subPlot1,
                  subPlot2,
                  subPlot3,
                  subPlot4,
                  subPlot5,
                  nrow=5,
                  labels=c("A",
                           "B",
                           "C",
                           "D",
                           "E"))

save_plot("strategyMatchUps.png",prow,base_height=13,base_width=10)


############################################################################################
############################################################################################
#PLOT 5: Inidividual-level bias-variance analysis###########################################
############################################################################################
############################################################################################

load(path("results/biasVariance.Rdata"))


names <- c("Whole crowd",
           "Similar crowd ",
           "Joke length",
           "Similarity-weighted crowd",
           "Clique",
           "Similar options",
           "Random other",
           "Doppelgänger")
names <- rev(names)


mat1 <- cbind(means,sds,biasVariance[biasVariance$strategy==5,c(3,4)])
mat1 <- cbind(mat1,mat1[,3]+ mat1[,4])
colnames(mat1) <- c("means","sd","Variance","Bias","Total error")

mat2 <- cbind(means,sds,biasVariance[biasVariance$strategy==8,c(3,4)])
mat2 <- cbind(mat2,mat2[,3]+ mat2[,4])
colnames(mat2) <- c("means","sd","Variance","Bias","Total error")

mat3 <- cbind(means,sds,biasVariance[biasVariance$strategy==4,c(3,4)])
mat3 <- cbind(mat3,mat3[,3]+ mat3[,4])
colnames(mat3) <- c("means","sd","Variance","Bias","Total error")

mat4 <- cbind(means,sds,biasVariance[biasVariance$strategy==3,c(3,4)])
mat4 <- cbind(mat4,mat4[,3]+ mat4[,4])
colnames(mat4) <- c("means","sd","Variance","Bias","Total error")

mat5 <- cbind(means,sds,biasVariance[biasVariance$strategy==2,c(3,4)])
mat5 <- cbind(mat5,mat5[,3]+ mat5[,4])
colnames(mat5) <- c("means","sd","Variance","Bias","Total error")


df <- melt(as.data.frame(mat1),id.vars=1:2)
df$variable <- factor(df$variable,levels=c("Total error","Bias","Variance"))
myPalette <- colorRampPalette(brewer.pal(9,"YlOrRd"))
subPlot1 <- df  %>%
  ggplot(aes(x=means,y=sd,group=variable,color=value))+
  geom_point(size=0.1)+
  facet_wrap(~variable)+
  geom_segment(data=df, aes(x=mean(means), xend=mean(means), y=min(sd), yend=max(sd)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(data=df, aes(x=min(means), xend=max(means), y=mean(sd), yend=mean(sd)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="right",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_colour_gradientn(colours = myPalette(100),limits=c(0,0.65))

df2 <- melt(as.data.frame(mat2),id.vars=1:2)
df2$variable <- factor(df2$variable,levels=c("Total error","Bias","Variance"))
subPlot2 <- df2 %>%
  ggplot(aes(x=means,y=sd,group=variable,color=value))+
  geom_point(size=0.1)+
  facet_wrap(~variable)+
  geom_segment(data=df, aes(x=mean(means), xend=mean(means), y=min(sd), yend=max(sd)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(data=df, aes(x=min(means), xend=max(means), y=mean(sd), yend=mean(sd)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="right",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_colour_gradientn(colours = myPalette(100),limits=c(0,0.65))

df3 <- melt(as.data.frame(mat3),id.vars=1:2)
df3$variable <- factor(df3$variable,levels=c("Total error","Bias","Variance"))
subPlot3 <- df3 %>%
  ggplot(aes(x=means,y=sd,group=variable,color=value))+
  geom_point(size=0.1)+
  facet_wrap(~variable)+
  geom_segment(data=df, aes(x=mean(means), xend=mean(means), y=min(sd), yend=max(sd)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(data=df, aes(x=min(means), xend=max(means), y=mean(sd), yend=mean(sd)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="right",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_colour_gradientn(colours = myPalette(100),limits=c(0,0.65))

df4 <- melt(as.data.frame(mat4),id.vars=1:2)
df4$variable <- factor(df4$variable,levels=c("Total error","Bias","Variance"))
subPlot4 <- df4 %>%
  ggplot(aes(x=means,y=sd,group=variable,color=value))+
  geom_point(size=0.1)+
  facet_wrap(~variable)+
  geom_segment(data=df, aes(x=mean(means), xend=mean(means), y=min(sd), yend=max(sd)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(data=df, aes(x=min(means), xend=max(means), y=mean(sd), yend=mean(sd)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="right",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_colour_gradientn(colours = myPalette(100),limits=c(0,0.65))

df5 <- melt(as.data.frame(mat5),id.vars=1:2)
df5$variable <- factor(df5$variable,levels=c("Total error","Bias","Variance"))
subPlot5 <- df5 %>%
  ggplot(aes(x=means,y=sd,group=variable,color=value))+
  facet_wrap(~variable)+
  geom_point(size=0.1)+
  geom_segment(data=df, aes(x=mean(means), xend=mean(means), y=min(sd), yend=max(sd)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  geom_segment(data=df, aes(x=min(means), xend=max(means), y=mean(sd), yend=mean(sd)), alpha=0.5, lty="dotted",size=0.25,color="black")+
  theme(legend.position="right",
        legend.title=element_blank(),
        legend.direction="vertical")+
  xlab("Mean taste similarity (correlation)")+
  ylab("Dispersion in taste similarity")+
  theme(strip.background = element_blank())+
  scale_colour_gradientn(colours = myPalette(100),limits=c(0,0.65))

fullPlot <- plot_grid(subPlot1+ggtitle("Doppelgänger")+theme(plot.title = element_text()),
               subPlot3+ggtitle("Clique")+theme(plot.title = element_text()),
               subPlot5+ggtitle("Similarity-weighted crowd")+theme(plot.title = element_text()),
               subPlot4+ggtitle("Similar crowd")+theme(plot.title = element_text()),
               subPlot2+ggtitle("Whole crowd")+theme(plot.title = element_text()),
               nrow=5)
save_plot(fullPlot,file="biasVarianceIndividual.png",base_width=12,base_height=16)


############################################################################################
############################################################################################
#PLOT 5: Aggregate-level bias-variance analysis#############################################
############################################################################################
############################################################################################


load(path("results/biasVariance.Rdata"))

biasVariance <- biasVariance[,-c(3)]
colnames(biasVariance) <- c("participant","strategy","variance","biasIrError")

biasVariance2 <- melt(biasVariance, id=c("participant","strategy"))
biasVariance2$participant <- as.factor(biasVariance2$participant)
biasVariance2$strategy <- as.factor(biasVariance2$strategy)
biasVarianceGrouped <- group_by(biasVariance2,strategy,variable)
biasVariance4 <- dplyr::summarize(biasVarianceGrouped, mean = mean(value,na.rm=T))
biasVariance4$variable <- factor(biasVariance4$variable,levels=c("variance","biasIrError"))
coinflip <- biasVariance4[1:2,]
coinflip[,3] <- 0.25
coinflip[,1] <- "9"

biasVariance4 <- rbind(biasVariance4,coinflip)

biasVariance4$variable <- factor(biasVariance4$variable, levels = rev(levels(biasVariance4$variable)))

names <- c("Whole crowd",
           expression(paste("Similar crowd (", italic(t),"=0)")),
           "Joke length",
           "Similarity-weighted crowd",
           expression(paste("Clique (", italic(k),"=10)")),
           expression(paste("Similar options (", italic(k),"=5)")),
           "Random other",
           "Doppelgänger",
           "Coin flipping")


biasVariance4$strategy <- factor(biasVariance4$strategy,levels=rev(c(8,3,7,2,4,6,1,5,9)))

plot <- ggplot(data=biasVariance4,
               aes(x=strategy,
                   y=mean,
                   fill=variable)) +
  coord_flip()+
  geom_col(position = position_stack(reverse=TRUE),
           width=0.9)+
  scale_x_discrete("",
                   labels=rev(names)) +
  scale_y_continuous("Total Error",limits=c(0,0.5),expand = c(0.01,0)) +
  scale_fill_manual(labels=c( "Bias + Irreducible error","Variance"),
                    values= c("lightsalmon2","steelblue")) +
  theme(legend.position="bottom",
        legend.title=element_blank())


save_plot("biasVarianceAggregate.png",plot,base_width=12,base_height=6)



############################################################################################
############################################################################################
#PLOT 6: Aggregate-level performance of different strategies as a function of experience####
############################################################################################
############################################################################################


load(path("results/baseline.Rdata"))
dat_all <- baseline; rm(baseline)

# strategies
strategies <- vector(mode = "character", length = dim(dat_all)[3])
strategies[1] <- "Doppelgänger"
strategies[2] <- "Whole crowd"
strategies[3] <- "Clique (k=10)"
strategies[4] <- "Random other"
strategies[5] <- "Similarity-weighted crowd"
strategies[6] <- "Similar crowd (t=0)"
strategies[7] <- "Joke length"
strategies[8] <- "Similar options (k=5)"

# dimnames
dim_names <- list(id = as.character(1:dim(dat_all)[1]),
                  n = as.character(seq(
                    from = 5, to = 75, by = 5
                  )),
                  strategy = strategies)

# assign all attributes to array
mostattributes(dat_all) <- list(
  dim = dim(dat_all),
  names = names(dim_names),
  dimnames = dim_names
)

dat <- dat_all[,,(strategies != "")]
dat %>% str

dat_melted <- dat %>%
  melt(varnames = names(dimnames(dat))) %>%
  tbl_df %>%
  mutate(id = as.factor(id),
         strategy = factor(
           strategy,
           levels = c(
             "Clique (k=10)",
             "Similarity-weighted crowd",
             "Doppelgänger",
             "Similar options (k=5)",
             "Whole crowd",
             "Similar crowd (t=0)",
             "Random other",
             "Joke length")))



set.seed(12345)
best_strategy <- dat_melted %>%
  group_by(n, id) %>%
  mutate(rank = rank(value, ties.method = "random")) %>%
  arrange(n, id, desc(rank)) %>%
  slice(1) %>%  # only keep best strategy per id-X-n combination
  ungroup

best_strategy_props <- best_strategy %>%
  dplyr::group_by(n, strategy) %>%
        dplyr::summarize(counts = n()) %>%
        dplyr::group_by(n) %>%
        dplyr::mutate(prop = counts/sum(counts))

cbPalette <- c("red","#dd1c77","red3","red4","#3182BD","#9ecae1","black","#636363")

subPlot1 <- best_strategy_props %>%
  ggplot(aes(x = as.factor(n),
             y = prop,
             fill = strategy)) +
  geom_bar(stat = "identity") +
  scale_x_discrete("Experience (Number of previously evaluated options)") +
  scale_y_continuous("% individuals", labels = scales::percent) +
  scale_fill_manual(values=cbPalette,
                    labels= c(
                      expression(paste("Clique (", italic(k),"=10)")),
                      "Similarity-weighted crowd",
                      "Doppelgänger",
                      expression(paste("Similar options (", italic(k),"=5)")),
                      "Whole crowd",
                      expression(paste("Similar crowd (", italic(t),"=0)")),
                      "Random other",
                      "Joke length"))+
  theme(legend.position="bottom",
        legend.direction="horizontal" ,
        legend.title=element_blank(),
        legend.text.align="0")+
  guides(fill=guide_legend(ncol=2))




subPlot2 <- dat_melted %>%
  group_by(n, strategy) %>%
        dplyr::summarize(mean_performance = mean(value, na.rm = TRUE)) %>%  # NAs!

  ggplot(aes(x = n, y = mean_performance,
             color = strategy,
             shape = strategy)) +
  geom_line() +
  geom_point() +
  scale_x_continuous("Experience (Number of previously evaluated options)", breaks = seq(5, 75, by = 5)) +
  scale_y_continuous("Mean accuracy") +
  scale_color_manual(values=cbPalette,
                     labels= c(
                       expression(paste("Clique (", italic(k),"=10)")),
                       "Similarity-weighted crowd",
                       "Doppelgänger",
                       expression(paste("Similar options (", italic(k),"=5)")),
                       "Whole crowd",
                       expression(paste("Similar crowd (", italic(t),"=0)")),
                       "Random other",
                       "Joke length"))+
  theme(legend.position="bottom",
        legend.direction="horizontal",
        legend.title=element_blank(),
        legend.text.align="0")+
  guides(color=guide_legend(ncol=2,bycol=T,label=TRUE,label.position="right"))+
  scale_shape_manual(values=c(1,2,3,4,5,6,7,8),
                     labels= c(
                       expression(paste("Clique (", italic(k),"=10)")),
                       "Similarity-weighted crowd",
                       "Doppelgänger",
                       expression(paste("Similar options (", italic(k),"=5)")),
                       "Whole crowd",
                       expression(paste("Similar crowd (", italic(t),"=0)")),
                       "Random other",
                       "Joke length"))



plot1 <- plot_grid(subPlot2, subPlot1,
                   labels = c("A", "B"))

save_plot("aggregateResults.png",plot1,base_height=6,base_width=11)



############################################################################################
############################################################################################
#PLOT 7: Crossing point between different strategies########################################
############################################################################################
############################################################################################


load(path("results/crossingPoint.Rdata"))

crossingPoint <- crossingPoint[,c(1,2,3,6,8)]
colnames(crossingPoint) <- c("id",
                             "similarityWeighted_Better",
                             "clique_Better",
                             "similarityWeighted_Ncross",
                             "clique_Ncross")

cols <- c("orangered","dodgerblue4")
crossing <- crossingPoint %>%
  data.frame %>%
  tbl_df %>%
  gather(key = variable, value = value, -id) %>%
  tidyr::separate(col = variable,
           into = c("strategy","measure"),
           sep = "_") %>%
  spread(key = measure, value = value) %>%
  mutate(strategy = factor(
    strategy,
    levels = c("clique",
               "similarityWeighted"),
    labels = c("Clique (k=10)",
               "Similarity-weighted crowd")
    )
  )


n_prop <- crossing %>%
  group_by(Better) %>%
  summarize(n = n()) %>%
  ungroup %>%
  mutate(n_prop = n/sum(n))

n_cross_summary <- crossing %>%
  group_by(Better) %>%
  do(tidy(summary(.$Ncross, na.rm = TRUE)))

n_prop %>%
  left_join(n_cross_summary) %>%
  kable

lab1 <- expression(paste("Similar crowd outperformed at ", italic(n)," experiences"))
lab2 <- expression(paste("Whole crowd outperformed at ", italic(n)," experiences"))
lab3 <- c(expression(paste("Clique (", italic(k),"=10)")),"Similarity-weighted crowd")

crossing_density <- crossing %>%
  # only consider cases where whole crowd is overtaken
  filter(Better == 1, !is.na(Ncross)) %>%

  ggplot(aes(x = Ncross, fill = strategy)) +
  geom_density(alpha = .5, adjust = 2, col = NA) +
  ylab("Density") +
  scale_x_continuous(lab2,
                     breaks = seq(5, 75, 10)) +
  scale_fill_manual(
    values=c(cols),
    labels=c(
      expression(paste("Clique (", italic(k),"=10)")),
      "Similarity-weighted crowd"
    )) +
  theme(legend.position = "bottom")

crossing_ecdf <- crossing %>%
  # only consider cases where whole crowd is overtaken
  filter(Better == 1, !is.na(Ncross)) %>%

  ggplot(aes(x = Ncross, color = strategy)) +
  stat_ecdf(size = 1, alpha = .8) +
  ylab("Cumulative probability") +
  scale_x_continuous(lab2,
                     breaks = seq(5, 75, 10)) +
  scale_color_manual(
    values=c(cols),
    labels=c(
      expression(paste("Clique (", italic(k),"=10)")),
      "Similarity-weighted crowd"
    )) +
  theme(legend.position = "bottom")


load(path("Results/crossingPoint.Rdata"))

crossingPoint <- crossingPoint[,c(1,4,5,7,9)]
colnames(crossingPoint) <- c("id",
                             "similarityWeighted_Better",
                             "clique_Better",
                             "similarityWeighted_Ncross",
                             "clique_Ncross")

crossing <- crossingPoint %>%
  data.frame %>%
  tbl_df %>%
  gather(key = variable, value = value, -id) %>%
  separate(col = variable,
           into = c("strategy","measure"),
           sep = "_") %>%
  spread(key = measure, value = value) %>%
  mutate(strategy = factor(
    strategy,
    levels = c("clique",
               "similarityWeighted"),
    labels = c(expression(paste("Clique (", italic(k),"=10)")),
               "Similarity-weighted crowd")
  ))


n_prop <- crossing %>%
  group_by(better) %>%
  summarize(n = n()) %>%
  ungroup %>%
  mutate(n_prop = n/sum(n))

n_cross_summary <- crossing %>%
  group_by(better) %>%
  do(tidy(summary(.$ncross, na.rm = TRUE)))

n_prop %>%
  left_join(n_cross_summary) %>%
  kable


crossing_density2 <- crossing %>%
  # only consider cases where whole crowd is overtaken
  filter(Better == 1, !is.na(Ncross)) %>%

  ggplot(aes(x = Ncross, fill = strategy)) +
  geom_density(alpha = .5, adjust = 2, col = NA) +
  ylab("Density") +
  scale_x_continuous(lab1,
                     breaks = seq(5, 75, 10)) +
  scale_fill_manual(
    values=c(cols),
    labels=c(
      expression(paste("Clique (", italic(k),"=10)")),
      "Similarity-weighted crowd"
    )) +
  theme(legend.position = "bottom")



expression(paste("Clique (", italic(k),"=10)"))
crossing_ecdf2 <- crossing %>%
  # only consider cases where whole crowd is overtaken
  filter(Better == 1, !is.na(Ncross)) %>%

  ggplot(aes(x = Ncross, color = strategy)) +
  stat_ecdf(size = 1, alpha = .8) +
  ylab("Cumulative probability") +
  scale_x_continuous(lab1,
                     breaks = c(seq(5, 75, 10))) +
  scale_color_manual(
    values=c(cols),
    labels=c(
      expression(paste("Clique (", italic(k),"=10)")),
      "Similarity-weighted crowd"
    )) +
  theme(legend.position = "bottom")


plot_final <- plot_grid(crossing_density+theme(legend.title=element_blank(),legend.position="none"),
                        crossing_ecdf+theme(legend.title=element_blank(),legend.position="none"),
                        crossing_density2+theme(legend.title=element_blank(),legend.position="none"),
                        crossing_ecdf2+theme(legend.title=element_blank(),legend.position="none"),
                        labels = c("A", "B","C","D"),
                        scale = .8, vjust = 0)

legend1 <- get_legend(crossing_density2+theme(legend.title = element_blank(),legend.position="bottom"))
legend2 <- get_legend(crossing_ecdf2+theme(legend.title = element_blank(),legend.position="bottom"))

plot_final2 <- plot_grid(plot_final,legend1,nrow=2,ncol=1,rel_heights=c(2,.1))

save_plot("crossingPoint.png",plot_final2,base_width=10,base_height=10)




############################################################################################
############################################################################################
#PLOT 8: Heatmap showing performance of Clique for different levels of 'k' and #############
#histogram of joke ratings##################################################################
############################################################################################
############################################################################################

load(path("/jesterDatasets/jesterFull.Rdata"))
load(path("/results/baseline.Rdata"))

myPalette <- brewer.pal(9,"PuBu")
a <- cor(t(bigLaugh[sample(1:14000,1000),]))
b <- melt(a[upper.tri(a)])
myPalette <- brewer.pal(9,"PuBu")

subPlot1 <- ggplot(b, aes(value)) +
  geom_histogram(aes(y=..density..),bins=500,color="grey")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  xlab("Correlation")+
  ylab("Density")+
  stat_function(fun=dnorm, args=list(mean=mean(b$value), sd=sd(b$value)),color=myPalette[6])+
  scale_y_continuous(breaks=c(0,0.5,1,1.5,2,2.5),labels=c(0,0.5,1.0,1.5,2.0,2.5))+
  scale_x_continuous(breaks=c(-0.5,-0.04,0.11,0.26,0.5),
                     labels=c(-0.5,-0.04,0.11,0.26,0.5),
                     limits=c(-0.65,0.75))+
  geom_segment(data=b, aes(x=-0.04, xend=-0.04, y=0, yend=1.52), alpha=0.25, lty="dotted",size=0.25,color=myPalette[6])+
  geom_segment(data=b, aes(x=-0.04, xend=0.26, y=1.52, yend=1.52), alpha=0.25, lty="dotted",size=0.25,color=myPalette[6])+
  geom_segment(data=b, aes(x=0.26, xend=0.26, y=0, yend=1.52), alpha=0.25, lty="dotted",size=0.25,color=myPalette[6])+
  geom_point(data=b, aes(y=1.52, x=0.11),color=myPalette[6])


load(path("results/varyK.Rdata"))

nRow <- 15
nCol <- 21

myData<-sapply(1:dim(memoryStrategies)[3], function(x) colMeans(memoryStrategies[,,x]))
myData<-as.matrix(myData)


rownames(myData) <-  c("5","10","15","20","25","30","35","40","45","50","55","60","65","70","75","80","85","90")
colnames(myData) <- c("D","5","C","15","20","25","30","35","40","45","50","60","70","80","90","100","130","160","190","220","WC")

myData<-t(myData)
longData <- melt(myData)
colnames(longData)<-c("X1","X2","Performance")

longData$X1<-factor(longData$X1,levels=c("D","5","C","15","20","25","30","35","40","45","50","60","70","80","90","100","130","160","190","220","WC"))

myPalette <- brewer.pal(9,"PuBu")
myPalette[9] <- c("#000033")

lab <- expression(paste("Number of neighbors (",italic(n),")"))
subPlot2<- longData %>% filter(X2 <= 75)  %>%
        ggplot(aes(x = factor(X2), y = factor(X1), fill = Performance))+
  geom_tile()+
  scale_fill_gradientn(colours = myPalette)+ scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))+ #coord_equal()+
  theme_bw()+
  labs(x="\nExperience (# of previously experienced options)",
       y=lab,title="")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        text = element_text(size=11),
        axis.line = element_line(size=0.25),
        axis.ticks=element_line(size=0.25),panel.grid.major = element_blank(),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))


fullPlot <- plot_grid(subPlot1,subPlot2,rel_widths = c(1,1.1),labels=c("A","B"))
save_plot("heatmapJokeHist.png",fullPlot,base_width=10,base_heigh=5)


############################################################################################
############################################################################################
#PLOT 9: Histogram of joke ratings separately for each joke#################################
############################################################################################
############################################################################################

library(MASS)

load(path("jesterDatasets/jesterFull.Rdata"))
jokes <- bigLaugh

#order jokes by mean rating
means <- colMeans(jokes)
order_means <- order(means,decreasing=T)

# #order jokes by sum of alpha and beta parametbuers
# sum <- vector()
# for(i in 1:100){
# a <- (jokes[,i]-min(jokes[,i])) / (max(jokes[,i])-min(jokes[,i]))
# a[a==1] <- 0.9999
# a[a==0] <- 0.0001
# fit.beta <- suppressWarnings(fitdistr(a, "beta", start = list( shape1=0.1, shape2=0.1 ) ))
#
# #get alpha and beta parameters
# alpha <- fit.beta$estimate[1]
# beta <- fit.beta$estimate[2]
# sum[i] <- alpha+beta
# }
# order_params <- order(sum,decreasing=T)


#normalize the ratings from 0 to 1
h <- list()
for(i in 1:100) h[[i]] <- hist((jokes[,i]-min(jokes[,i])) / (max(jokes[,i])-min(jokes[,i])))


pdf("jokeHistogram.pdf",height=10,width=10)
par(mfrow=c(10,10),mar=c(2,2,1.5,0.8))

#for(i in 1:100){ #use this if you want the plots to be ordered by joke number
for(i in order_means) { #use this if you want the plots to be ordered by mean joke rating
#for(i in order_params){ #use this if you want the plots to be ordered by the sum of alpha and beta


  plot(h[[i]],freq=FALSE,cex.main=1,cex.axis=1,main=title,col='dimgrey',lty='blank',xaxt='n',yaxt='n',cex.main=0.9,font.main = 1)
  axis(2,at=c(0,max(h[[i]]$density)/2,max(h[[i]]$density)),labels=c(0,0.5,1))
  axis(1,at=seq(0,1,0.5),labels=c(-10,0,10))

  #fit beta distribution
  a <- (jokes[,i]-min(jokes[,i])) / (max(jokes[,i])-min(jokes[,i]))
  a[a==1] <- 0.9999
  a[a==0] <- 0.0001
  fit.beta <- suppressWarnings(fitdistr(a, "beta", start = list( shape1=0.1, shape2=0.1 ) ))

  #get alpha and beta parameters
  alpha <- fit.beta$estimate[1]
  beta <- fit.beta$estimate[2]

  #overlay beta distribution
  plot(function(x) dbeta(x,alpha,beta), add=TRUE, col=1, lwd=1)

}
dev.off()




#code for simulating two representative individuals
load(path("/jesterDatasets/jesterFull.Rdata"))
dataset <- bigLaugh

averagePerson <- colMeans(dataset)

zeros <- c(rep(0,99),1)
tens <- c(rep(10,99),9)
loveOrHate <- c(rep(10,99),9)

sequence <- c(seq(0.1,10,0.1))
random <- runif(100,min = -10, max =10)
normal <- rnorm(100,mean = 0, sd = 1)
oppositePerson <- - colmeans(dataset)




#Coin-tossing example with learning based on experience, deriving predictions using maximum likelihood estimation.

observations <- 3 #number of observations seen before making a decision.
coinTossMatrix <- matrix(rep(0,observations*10000),nrow = 10000) # a matrix with all the simulated draws.
riggedParameter <- 0.65 #the probability that a coin will come out heads.
for (k in 1:10000){coinTossMatrix[k,] <- rbinom(observations,1,riggedParameter)}

likelihood <- rowMeans(coinTossMatrix) # the probability indicated by the maximum likelihood principle
onTheFence <- which(likelihood == 0.5) # find the cases that cannot be decided for MLE
likelihood[onTheFence] <- sample(0:1, length(onTheFence), replace=T) # randomize for the above cases
predictions <- ifelse(likelihood< 0.5,0,1) #calling all the remaining cases on the basis of a 0.5 threshold.
hypothesisProbability <- mean(predictions) #the probability that this approach yields heads. #calculating the total error.
bitterTruth <- rbinom(10000,1,riggedParameter) #results generated from the actual distribution
totalLoss <- sum(abs(bitterTruth - predictions))/10000 #calculating variance and sigma^2
variance <- 1/2 * (1 - (hypothesisProbability^2 + (1 - hypothesisProbability)^2))
sigma2 <- 1/2 * (1 - (riggedParameter^2 + (1 - riggedParameter)^2))
bias <- totalLoss - variance - sigma2
