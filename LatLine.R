library(tidyverse)
library(cowplot)
library(patchwork)
setwd("C:/Users/Melanie/Downloads")
morph <- read_csv("LL.Morph.csv")
activity <- read_csv("LL.Activity.csv")
combined <- inner_join(morph, activity)
figA <- ggplot(data = combined) + aes(x = Activity, y = Norm.C.Width) + geom_boxplot() +  geom_jitter() + labs(y= "Canal Width (mm/TL)") +  theme_cowplot()
figB <- ggplot(data = combined) + aes(x = Family, y = Norm.C.Width, fill = Activity) +  geom_boxplot() +  geom_jitter() + labs(y= "Canal Width (mm/TL)") + theme_cowplot()
fig1 <-figA + figB + plot_annotation(tag_levels = 'A')
ggsave("LL_fig1.jpg", fig1, width = 12, height = 8)
fig1
figB

figc <- ggplot(data = combined) + aes(x = Activity, y = Norm.C.Width, color = Family) +  geom_point() + labs(y= "Canal Width (mm/TL)") +  theme_cowplot()
figc
figd <- ggplot(data = combined) + aes(x = Family, y = Norm.C.Width, color = Activity) + geom_point() + labs(y= "Canal Width (mm/TL)") + theme_cowplot()
figd
fig2 <-figc + figd + plot_annotation(tag_levels = 'A')
fig2
ggsave("LL_fig2.jpg", fig2, width = 12, height = 8)
