library(dplyr)
library(tidyverse)
library(plotrix)
library(gridExtra)
#### Chla by site and month

#making a tibble called summary with means and standard error for all samples (across the 3 filters)
summary <- chla_2022 %>% group_by(site,month) %>% 
  summarise(across(.cols=chla_ug_L_fin, list(mean=mean, se=std.error), na.rm=TRUE))
summary <- filter(summary, site != "na")

#Chlorophyll A by site with a line for each month
chla_2022_plot <- ggplot(data=subset(summary, site != "na"), aes(x= site, y=chla_ug_L_fin_mean, color=factor(month), group=month)) +
  geom_point(size=1)+
  geom_line(size=0.3)+
  geom_errorbar(aes(ymin=chla_ug_L_fin_mean-2*chla_ug_L_fin_se, ymax=chla_ug_L_fin_mean+2*chla_ug_L_fin_se), width=.2,  alpha=0.5)+
  ggtitle("2022 Chlorophyll A") +
  theme(panel.background= element_rect(color="black"),
        panel.grid = element_line(color="gray"),
        plot.title = element_text(hjust=0.5),
        legend.position = "bottom", 
        axis.title.y= element_text(size=14),
        axis.text.x= element_text(size=12, angle=45, hjust=1)) +
  labs(x=" ", y="ChlA (ug/L)", color="Month") +
  scale_color_brewer(palette= "Spectral")
chla_2022_plot

#### chla by month with Larval densities
summary <- chla_2022 %>% group_by(month) %>% 
  summarise(across(.cols=chla_ug_L_fin, list(mean=mean, se=std.error), na.rm=TRUE))
summary <- summary %>% mutate(month=recode(month, "may"="5", "jun"="6", "jul"="7", "aug"="8"))

sum_mon_lt <- ltdensities_2022 %>% group_by(Date) %>% summarise(across(.cols=Tui_m3, list(mean=mean, se=std.error)))
names(sum_mon_lt)[1]<-"month"

summary <- merge(summary, sum_mon_lt, by="month")                                                                   

#Chlorophyll A vs LT density by month
chla_lt_22_plot <- ggplot(data=summary, aes(x= month, y=chla_ug_L_fin_mean, group=1)) +
  geom_point(size=1)+
  geom_line(size=0.3)+
  geom_errorbar(aes(ymin=chla_ug_L_fin_mean-2*chla_ug_L_fin_se, ymax=chla_ug_L_fin_mean+2*chla_ug_L_fin_se), width=.2,  alpha=0.5)+
  ggtitle("2022 Chlorophyll A by Month") +
  theme(panel.background= element_rect(color="black"),
        panel.grid = element_line(color="gray"),
        plot.title = element_text(hjust=0.5),
        legend.position = "bottom", 
        axis.title.y= element_text(size=14),
        axis.text.x= element_text(size=12, angle=45, hjust=1)) +
  labs(x=" ", y="ChlA (ug/L)") 
chla_lt_22_plot

#lt by month
lt_22_plot <- ggplot(data=summary, aes(x= month, y=Tui_m3_mean, group=1)) +
  geom_point(size=1)+
  geom_line(size=.3)+
  geom_errorbar(aes(ymin=Tui_m3_mean-2*Tui_m3_se, ymax=Tui_m3_mean+2*Tui_m3_se), width=.2,  alpha=0.5)+
  ggtitle("Larval Density by Month") +
  theme(panel.background= element_rect(color="black"),
        panel.grid = element_line(color="gray"),
        plot.title = element_text(hjust=0.5),
        legend.position = "bottom", 
        axis.title.y= element_text(size=14),
        axis.text.x= element_text(size=12, angle=45, hjust=1)) +
  labs(x=" ", y="Larval Density (Tui m3)") 
lt_22_plot

grid.arrange(lt_22_plot, chla_lt_22_plot, ncol=1)

