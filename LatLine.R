library(tidyverse)
library(cowplot)
library(patchwork)
setwd("C:/Users/Melanie/Downloads")
morph <- read_csv("LL.Morph.csv")
activity <- read_csv("LL.Activity.csv")
combined <- inner_join(morph, activity)
diurnal <- filter(combined, Activity == "D") #66 species, 19 families
nocturnal <- filter(combined, Activity == "N") #38 species, 15 families

#violin plots of three parameters: figure 1
figA <- ggplot(data = combined) + aes(x = Activity, y = Norm.PA) +  geom_violin() + geom_point() + labs(y= "Pore Area (mm2/TL)") +  theme_cowplot()
#violin plot of canal neuromasts
figB <- ggplot(data = combined) + aes(x = Activity, y = Norm.CN) +  geom_violin() + geom_point() + labs(y= "Canal Neuromasts (#/TL)") +  theme_cowplot()
#violin plot of visible scale area
figC <- ggplot(data = combined) + aes(x = Activity, y = Norm.C.Width) +  geom_violin() + geom_point() + labs(y= "Canal Width (mm/TL)") +  theme_cowplot()
fig1 <-figA + figB + figC + plot_annotation(tag_levels = 'A')
fig1
ggsave("LL_fig1.jpg", fig1, width = 12, height = 8)

#aim 2: figures 2-4
fig2 <- ggplot(data = nocturnal) + aes(x = Family, y = Norm.C.Width) + geom_point(size = 2) + labs (y = "Canal Width (mm/TL)") + theme_cowplot()
fig2
ggsave("LL_fig2.jpg", fig2, width = 12, height = 8)
#apogonidae and myctophidae seem to be widening canals (see literature for which species have been noted to do this), also look and see which species have been noted to use replacement neuromasts- should be ones with higher CN values- wait that should be superficial ones not canal neuromasts
fig3 <- ggplot(data = nocturnal) + aes(x = Family, y = Norm.CN) + geom_point(size = 2) + labs (y = "Canal Neuromasts (#/TL)") + theme_cowplot()
fig3
ggsave("LL_fig3.jpg", fig3, width = 12, height = 8)
#a species of serranid and sciaenid (see which ones specifically: equetus punctatus and rypictus saponaceus)
fig4 <- ggplot(data = nocturnal) + aes(x = Family, y = Norm.PA) + geom_point(size = 2) + labs (y = "Pore Area (mm2/TL)") + theme_cowplot()
fig4
ggsave("LL_fig4.jpg", fig4, width = 12, height = 8)
#same two families with higher values/wider pores

#pca- figure 5
library(ggbiplot)
noct.pca <- prcomp (~ Norm.CN + Norm.C.Width + Norm.PA, data = nocturnal,
                    scale = T,
                    center = T)
noct_plot <- ggbiplot(noct.pca, obs.scale = 1, var.scale = 1, varname.size = 6, varname.adjust = 1.6) + geom_point(aes(color = nocturnal$Family), size = 4) + theme_cowplot() + labs(color = "Family")
print(noct_plot)
ggsave("LL_fig5.jpg", noct_plot, width = 12, height = 8)

#correlation- figure 6
cor.test(combined$Norm.VSA, combined$Norm.CN)
fig6 <- ggplot(data = combined) + aes(x = Norm.VSA, y = Norm.CN) +geom_point(size = 2) +geom_smooth(se = F, method = "lm", color = "red") + stat_cor(method = "pearson", label.x = 0.15, label.y = 2, size = 6)+ labs(x = "Visible Scale Area (mm2/TL)", y = "Canal Neuromasts (#/TL)") + theme_cowplot()
fig6
ggsave("LL_fig6.jpg", fig6, width = 12, height = 8)

#t-tests comparing two groups
library(lsr)
t.test(diurnal$Norm.PA, nocturnal$Norm.PA) #p-value = 0.320
cohensD(diurnal$Norm.PA, nocturnal$Norm.PA) #cohen's d = 0.251

t.test(diurnal$Norm.CN, nocturnal$Norm.CN) #p-value = 0.034
cohensD(diurnal$Norm.CN, nocturnal$Norm.CN) #cohen's d = 0.367

t.test(diurnal$Norm.C.Width, nocturnal$Norm.C.Width) #p-value = 0.074
cohensD(diurnal$Norm.C.Width, nocturnal$Norm.C.Width) #cohen's d = 0.441

#other variables measured but not using
t.test(diurnal$Norm.Scale.N, nocturnal$Norm.Scale.N) #p-value = 0.036
t.test(diurnal$TL, nocturnal$TL) #p-value = 0.579

#anovas for comparing families and post-hoc tests
pa_aov <- aov(nocturnal$Norm.PA~factor(nocturnal$Family)) 
summary(pa_aov) 
pa_results <- TukeyHSD(pa_aov, conf.level = 0.95)
pa_results
cn_aov <- aov(nocturnal$Norm.CN~factor(nocturnal$Family)) 
summary(cn_aov)
cn_results <- TukeyHSD(cn_aov, conf.level = 0.95)
cn_results
cw_aov <- aov(nocturnal$Norm.C.Width~factor(nocturnal$Family)) 
summary(cw_aov)
