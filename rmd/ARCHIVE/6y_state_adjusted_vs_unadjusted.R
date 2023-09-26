

# Make state adjusted vs unadjusted figure
library(readxl)
state.dat  <- read_excel("rda/state_adjusted_vs_unadjusted_230101.xlsx")

state.dat2 <- state.dat %>% 
  mutate(significant = ifelse((Low < 0 & High < 0) | (Low > 0 & High >0),1,0))


ggplot(state.dat2,  aes(x = reorder(State,Coef), y = Coef, color = Model)) +
 # facet_grid(.~outcome, scales = "free_y") +
  geom_hline(yintercept = 0, lwd=1.5) +
  geom_segment(aes(xend = State, yend = High), size = 1) +
  geom_segment(aes(xend = State, yend = Low), size = 1) +
  geom_point(size = 3) +
 # geom_text(aes(label = paste0(round(estimate_percent), "%"), vjust = -.7), size = 4) +
  coord_flip() +
  theme_bw() +
#  theme(legend.title = element_blank(), legend.position = "none", text = element_text(size = 12)) +
  scale_color_manual(values = c("Adjusted" = "dodgerblue4", "Unadjusted" = "firebrick1")) +
#  scale_y_discrete(labels = predictorLabelsdemog) +
  xlab("") +
  ylab("Estimate") +
  ggtitle(paste("State by Year Interaction Coefficients, Adjusted vs Unadjusted")) +
  theme (plot.title = element_text(size = 12, face = "bold",vjust = 1, hjust = 0.5))
