library(tidyverse)
library(sf)
library(camcorder)
library(MetBrewer)
library(giscoR)
library(ggpattern)
library(readxl)

#-----------------Figure1a-----------------
life_df <- read_csv('/Users/zhongcideng/Desktop/MND.csv')

le_change <- reshape2::melt(life_df, id.vars=c("ISO3","Region Code"),variable.name="age_groups",value.name = "intake") %>%
  filter(age_groups == 'MND')

world <- giscoR::gisco_get_countries() %>%
  janitor::clean_names() %>%
  rename(ISO3 = iso3_code) %>%
  filter(ISO3 != "ATA") %>%
  #st_transform(crs = "+proj=moll")
  st_transform(graticules, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


world_le_MND <- world %>% left_join(le_change) %>%
  filter(!is.na(intake))  %>%
  mutate(intake = ifelse(intake > 0.25, 0.25, intake))


Figure1a <- ggplot(world_le_MND) +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.1) +
  geom_sf(aes(fill = intake), linewidth = 0, color = "black") +

  #scale_fill_gradient2(name = "MND", limits = c(0, 0.2), low = "steelblue2", high = "tomato", midpoint = 0.1,na.value = "gray") +
  scale_fill_gradient2(name = "Mean nutrient deficiency", limits = c(0, 0.25), breaks = c(0,0.25),low = "white", mid = '#474747',high = "black", midpoint = 0.125) +

  #facet_wrap(vars(age_groups), ncol = 2) +
  guides(fill = guide_colorbar(title.position = "top"))+
  theme_minimal(base_size = 12,base_family = 'Arial') +
  theme(
    legend.position = "bottom",
    legend.position.inside = c(0, 0),
    legend.title = element_text(hjust = 0.5),
    legend.key.width = unit(3, "lines"),
    legend.key.height = unit(1, "lines"),
    plot.background = element_rect(fill = "white", color = NA),
    #panel.background = element_rect(fill = NA),
    strip.text = element_text(size = 12),
    axis.text = element_blank(),
    panel.grid = element_line(linewidth = 0.1, color = "black")
  )

ggsave(Figure1a,file="/Users/zhongcideng/Desktop/optimize/GENuS age data/Figure/Figure1/Figure1a.png",dpi = 1200,units = "mm")


#-----------------Figure1b-----------------
MND <- read_excel('/Users/zhongcideng/Desktop/MND.xlsx',range = 'A1:J299') %>%
  select(-c(`Region Code`)) %>%
  reshape2::melt(id.vars=c("ISO3","Region"),variable.name="age_groups",value.name = "intake") %>%
  filter(age_groups != 'MND')

MND$Region <- MND$Region %>%
  fct_relevel('World', 'EURU', 'NAMO', 'IND-AS', 'SSA','NAWCA','SSEA','LATAM', after = 0)


Figure1b <- ggplot(MND,aes(age_groups,intake,fill=age_groups))+
  stat_boxplot(geom="errorbar",width=0.5)+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(alpha = 0.2, width = 0.2, size = 1)+
  stat_summary(
    geom = "point",  # 以点的形式显示平均值
    fun = mean,
    position = position_dodge(width = 0.75),
    color = 'red'
  ) +

  scale_y_continuous(expand = expansion(0),limits = c(-0.05,0.3),
                     breaks = c(0.0,0.1,0.2,0.3))+

  facet_wrap(~Region, nrow = 2)+
  theme_bw(base_size = 9,base_family = "Arial") +
  theme(

    panel.background = element_rect(fill = NULL,color = "black",linewidth = 0.5),
    panel.grid  = element_blank(),
    axis.title = element_text(size=9),

    legend.position = "bottom",
    legend.position.inside = c(0, 0),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_blank(),
    axis.title.x =  element_blank(),
    axis.text.y = element_text(size = 12,family = "Arial"),

    strip.text=element_text(size=9),
    panel.border = element_rect(size = 0.5, color = "black"),
    axis.ticks.length = unit(-0.1, "cm"),
    axis.ticks.length.x = unit(0, "cm"),
    strip.text.y = element_text(size = 9),
    strip.background = element_rect(fill = "lightblue", color = c("black", NA, "black", "black"),size=0.5))+
    guides(fill = guide_legend(nrow = 1))+  # 图例显示在一行
  scale_fill_manual(values=c("#95D0E9","#1DA3D3","#E3A1BD","#CF4956","#B0CE9A","#3CA955")) +
  scale_color_manual(values=c("#95D0E9","#1DA3D3","#E3A1BD","#CF4956","#B0CE9A","#3CA955")) +
  labs(y = "Mean nutrient deficiency")

ggsave(Figure1b,file="/Users/zhongcideng/Desktop/optimize/GENuS age data/Figure/Figure1/Figure1b.png",dpi = 1200,units = "mm")


#-----------------Figure1c-----------------
MER <- read.csv("/Users/zhongcideng/Desktop/MER.csv")

world <- giscoR::gisco_get_countries() %>%
  janitor::clean_names() %>%
  rename(ISO3 = iso3_code) %>%
  filter(ISO3 != "ATA") %>%
  st_transform(graticules, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

world_data <- world %>% left_join(MER) %>%
  filter(!is.na(MER)) %>%
  mutate(MER = ifelse(MER > 1.5, 1.5, MER)) %>%
  mutate(MER = ifelse(MER < 0.3, 0.3, MER))

Figure1c <- ggplot(world_data) +
  geom_sf(data = world, fill = NA, color = "black", linewidth = 0.1) +
  geom_sf(aes(fill = MER), linewidth = 0, color = "black") +

  scale_fill_gradient2(name = "Mean environmental ratio",
                       breaks = c(0.3,0.9,1.5),
                       limits = c(0.3, 1.5),
                       low = "#436FAA", mid = '#FBF7BC',high = "#CB3027",
                       midpoint = 0.9) +

  guides(fill = guide_colorbar(title.position = "top"))+
  theme_minimal(base_size = 12,base_family = 'Arial') +
  theme(
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    legend.key.width = unit(3, "lines"),
    legend.key.height = unit(1, "lines"),
    plot.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 12),
    axis.text = element_blank(),
    panel.grid = element_line(linewidth = 0.1, color = "black")
  )

ggsave(Figure1c,file="/Users/zhongcideng/Desktop/optimize/GENuS age data/Figure/Figure1/Figure1c.png",dpi = 1200,units = "mm")

#-----------------Figure1d-----------------
EF_age <- read_excel('/Users/zhongcideng/Desktop/EF_new_region.xlsx',range = 'B1:H43') %>%
  mutate_at(vars("Land use", "Nitrogen", "Phosphorus"), ~ . / 1000) %>%
  reshape2::melt(id.vars=c("Region","Age-groups"),variable.name="items",value.name = "value")

EF_age$Region <- EF_age$Region %>%
  fct_relevel('EURU', 'NAMO', 'IND-AS', 'SSA','NAWCA','SSEA','LATAM', after = 0)

EF_age$`Age-groups` <- EF_age$`Age-groups` %>%
  fct_relevel('Age0-5', 'Age6-10', 'Age11-19', 'Age20-44','Age45-59','Age60+', after = 0)

Figure1d <- ggplot(EF_age, aes(x = Region, y = value, fill = `Age-groups`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~items, nrow = 2, scales = "free_y")+
  theme_bw(base_size = 10,base_family = "Arial") +
  #labs(x = "Age Groups", y = "Value", fill = "Items") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = NULL,color = "black",linewidth = 0.5),
    panel.grid  = element_blank(),
    axis.title = element_text(size=12),

    legend.position = "right",
    axis.title.y = element_blank(),
    #axis.text.x = element_text(size = 10,family = "Arial",vjust = 1),
    #axis.title.x =  element_blank(),
    axis.text.y = element_text(size = 10,family = "Arial"),

    strip.text=element_text(size=10),
    panel.border = element_rect(size = 0.5, color = "black"),
    axis.ticks.length = unit(-0.1, "cm"),
    axis.ticks.length.x = unit(0, "cm"),
    strip.text.y = element_text(size = 10),
    strip.background = element_rect(fill = "lightblue", color = c("black", NA, "black", "black"),size=0.5))+
  scale_fill_manual(values=c("#95D0E9","#1DA3D3","#E3A1BD","#CF4956","#B0CE9A","#3CA955")) +
  scale_color_manual(values=c("#95D0E9","#1DA3D3","#E3A1BD","#CF4956","#B0CE9A","#3CA955"))
Figure1d

ggsave(Figure1d,file="/Users/zhongcideng/Desktop/optimize/GENuS age data/Figure/Figure1/Figure1d.png",dpi = 1200,units = "mm")

