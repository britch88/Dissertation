#title: "2c_trajectory_analysis_clean_results"
#author: "Brit Henderson"
#date: "4/3/2022"
#output: html_document

# directories ----
readdatdir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"
rdadir <- "/data/share/xproject/Training/Practice/henderson/Dissertation/rda"


# packages ----
# Loads crimCV into the interpreter
library(crimCV)
library(tidyverse)
library(Hmisc)
library(GGally)
library(cowplot)
library(reshape2)


# Source Functions ----
knitr::purl("rmd/0a_functions.Rmd")
source("rmd/0a_functions.R")


# Read in data ----
trajdat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/gbtm4g2p.rds")
trajdat2 <- trajdat %>% 
  mutate(maxprob = pmax(probG1,probG2,probG3,probG4))


# transform data ----

### long data with
long_dat <- trajdat2 %>% 
  pivot_longer(contains("year"), 
               names_to = "Year",
               names_prefix = "year",
               values_to = "Arrest rate") 


sumtraj <- long_dat %>% 
  group_by(cluster,Year) %>% 
  summarise(cluster.rate = mean(`Arrest rate`),
            n.counties = n(),
            avg.pp = mean(maxprob))


# summary table ----

traj.table <- sumtraj %>% 
  select(cluster, n.counties,avg.pp) %>% 
  distinct() %>% 
  mutate(group.prop = n.counties/2435 )


# plot all trajectory groups  ----
y.axis.label = "Young Adult Arrests"
x.axis.label = "Year"
plot.title = "Yearly Average Young Adult Arrest Rate by Trajectory Group"


group.colors <- c("1" = "#CC6677", "2" = "#6699CC", "3" ="#44aa99", "4" = "#882255")

ggplot(sumtraj) +
  #geom_line(aes(x=as.numeric(Year),y=cluster.rate,col=as.factor(cluster), group=as.factor(cluster))) +
  geom_smooth(method =loess, se = FALSE, aes(x=as.numeric(Year),y=cluster.rate, col=as.factor(cluster), group=as.factor(cluster))) +
  scale_y_continuous(name=y.axis.label) +
  scale_x_continuous(name=x.axis.label) +
  ggtitle(plot.title) +
  scale_color_manual(values =group.colors,
                     name = "Trajectory Group",
                     breaks = c("1","2","3","4"),
                     labels = c("Group 1","Group 2","Group 3","Group 4")) +
  theme_minimal() +
    theme(legend.title = element_text())


## Plot  trajectories for each county, facetted by trajectory group ----
y.axis.label = "Young Adult Arrests"
x.axis.label = "Year"
plot.title = "County Young Adult Arrest Rate by Trajectory Group"


ggplot(long_dat) +
  geom_line(data=long_dat, 
            aes(x=as.numeric(Year),
                y=`Arrest rate`, 
                group=as.factor(fips_clean)),
            alpha=.08) +
  geom_smooth(data=sumtraj,
              method =loess, 
              se = FALSE, 
              aes(x=as.numeric(Year),
                  y=cluster.rate, 
                  group=as.factor(cluster)), 
              col = "red") +
  facet_wrap(cluster~.) +
  scale_y_continuous(name=y.axis.label) +
  scale_x_continuous(name=x.axis.label) +
  ggtitle(plot.title) +
  theme_minimal() +
  theme(legend.title = element_text())




#plot of maximum posterior probabilities ----

#Two types of absolute fit measures I’ve seen advocated in the past are the 
#average maximum posterior probability per group and the odds of correct classification. 
#The occ function calculates these numbers given two vectors (one of the max 
#probabilities and the other of the group classifications). We can get this info 
#from our long data by just selecting a subset from one time period. Here the 
#output at the console shows that we have quite large average posterior probabilities 
#as well as high odds of correct classification. (Also updated to included the 
#observed classified proportions and the predicted proportions based on the 
#posterior probabilities. Again, these all show very good model fit.) Update: Jeff 
#Ward sent me a note saying I should be using the predicted proportion in each group 
#for the occ calculation, not the assigned proportion based on the max. post. prob. So I 
#have updated to include the occ_pp column for this, but left the old occ column in as a 
#paper trail of my mistake.
#occ(longD)
#A plot to accompany this though is a jittered dot plot showing the maximum posterior probability per group. You can here that groups 3 and 4 are more fuzzy, whereas 1 and 2 mostly have very high probabilities of group assignment.


ggplot(data=trajdat2, aes(y=maxprob, x=cluster)) +
  geom_jitter(alpha=.3) +
  ylim(0,1) +
  xlab("Assigned Group") +
  ylab("Posterior Probability") +
  theme(text =element_text(size=14))
  

#scatterplot matrix ----
#Remember that these latent class models are fuzzy classifiers. That is each point has a 
#probability of belonging to each group. A scatterplot matrix of the individual probabilities 
#will show how well the groups are separated. Perfect separation into groups will result in points 
#hugging along the border of the graph, and points in the middle suggest ambiguity in the class 
#assignment. You can see here that each group closer in number has more probability swapping between them.


my_cols <- c("#00AFBB", "#E7B800", "#FC4E07","#882255")
library(GGally)
options(scipen = 100)
ggpairs(data=trajdat2, columns=48:51)


library(psych)
pairs.panels(trajdat2[,48:51], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07","#882255")


panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[trajdat2$cluster])
}

# Create the plots
pairs(trajdat2[,48:51], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)

