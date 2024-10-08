library(gtsummary)

# Fonctions
source(here("functions.R"))

# Translate TEND in BAU
scenario_name <- c(S1="S1", S2="S2", S3="S3", S4="S4",BAU = "BAU")

## Evolution of transport distances per scenario

km_1 = ademe_all_type_S1 %>%
  group_by(year, type) %>%
  summarise(mean_km = mean(km_pp_w)) %>%
  ggplot(aes(x=year, y = mean_km, color = type, linetype = type)) +
  geom_line(size = 1) + 
  theme_pubr() +
  xlab("") +
  ylab("km/pp/week")+
  ggtitle("S1")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
  scale_linetype_manual(values=c("solid", "longdash", "longdash","solid"))+
  scale_color_manual(values=c("#0072B2","#E69F00", "#F0E442", "firebrick")) +
  xlim (2021,2050)+
  ylim (c(0,25))

km_2 = ademe_all_type_S2 %>%
  group_by(year, type) %>%
  summarise(mean_km = mean(km_pp_w)) %>%
  ggplot(aes(x=year, y = mean_km, color = type, linetype = type)) +
  geom_line(size = 1) + 
  theme_pubr() +
  xlab("") +
  ylab("km/pp/week")+
  ggtitle("S2")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
  scale_linetype_manual(values=c("solid", "longdash", "longdash","solid"))+
  scale_color_manual(values=c("#0072B2","#E69F00", "#F0E442","firebrick")) +
  xlim (2021,2050)+
  ylim (c(0,25))

km_3 = ademe_all_type_S3 %>%
  group_by(year, type) %>%
  summarise(mean_km = mean(km_pp_w)) %>%
  ggplot(aes(x=year, y = mean_km, color = type, linetype = type)) +
  geom_line(size = 1) + 
  theme_pubr() +
  xlab("") +
  ylab("km/pp/week")+
  ggtitle("S3")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
  scale_linetype_manual(values=c("solid", "longdash", "longdash","solid"))+
  scale_color_manual(values=c("#0072B2","#E69F00", "#F0E442","firebrick")) +
  xlim (2021,2050)+
  ylim (c(0,25))

km_4 = ademe_all_type_S4 %>%
  group_by(year, type) %>%
  summarise(mean_km = mean(km_pp_w)) %>%
  ggplot(aes(x=year, y = mean_km, color = type, linetype = type)) +
  geom_line(size = 1) + 
  theme_pubr() +
  xlab("") +
  ylab("km/pp/week")+
  ggtitle("S4")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
  scale_linetype_manual(values=c("solid", "longdash", "longdash","solid"))+
  scale_color_manual(values=c("#0072B2","#E69F00", "#F0E442","firebrick")) +
  xlim (2021,2050)+
  ylim (c(0,25))


min_1 = ademe_all_type_S1 %>%
  group_by(year, type) %>%
  summarise(mean_min = mean(min_pp_w)) %>%
  ggplot(aes(x=year, y = mean_min, color = type, linetype = type)) +
  geom_line(size = 1) + 
  theme_pubr() +
  xlab("") +
  ylab("min/pp/week")+
  ggtitle("S1")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
  scale_linetype_manual(values=c("solid", "longdash", "longdash","solid"))+
  scale_color_manual(values=c("#0072B2","#E69F00", "#F0E442", "firebrick")) +
  xlim (2021,2050)+
  ylim (c(0,100))

min_2 = ademe_all_type_S2 %>%
  group_by(year, type) %>%
  summarise(mean_min = mean(min_pp_w)) %>%
  ggplot(aes(x=year, y = mean_min, color = type, linetype = type)) +
  geom_line(size = 1) + 
  theme_pubr() +
  xlab("") +
  ylab("min/pp/week")+
  ggtitle("S2")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
  scale_linetype_manual(values=c("solid", "longdash", "longdash","solid"))+
  scale_color_manual(values=c("#0072B2","#E69F00", "#F0E442","firebrick")) +
  xlim (2021,2050)+
  ylim (c(0,100))

min_3 = ademe_all_type_S3 %>%
  group_by(year, type) %>%
  summarise(mean_min = mean(min_pp_w)) %>%
  ggplot(aes(x=year, y = mean_min, color = type, linetype = type)) +
  geom_line(size = 1) + 
  theme_pubr() +
  xlab("") +
  ylab("min/pp/week")+
  ggtitle("S3")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
  scale_linetype_manual(values=c("solid", "longdash", "longdash","solid"))+
  scale_color_manual(values=c("#0072B2","#E69F00", "#F0E442","firebrick")) +
  xlim (2021,2050)+
  ylim (c(0,100))

min_4 = ademe_all_type_S4 %>%
  group_by(year, type) %>%
  summarise(mean_min = mean(min_pp_w)) %>%
  ggplot(aes(x=year, y = mean_min, color = type, linetype = type)) +
  geom_line(size = 1) + 
  theme_pubr() +
  xlab("") +
  ylab("min/pp/week")+
  ggtitle("S4")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12))+
  scale_linetype_manual(values=c("solid", "longdash", "longdash","solid"))+
  scale_color_manual(values=c("#0072B2","#E69F00", "#F0E442","firebrick")) +
  xlim (2021,2050)+
  ylim (c(0,100))

Figure_S2 = ggarrange(km_1,km_2, km_3, km_4,min_1,min_2, min_3, min_4, ncol = 4 , nrow = 2, common.legend = TRUE,
                            legend = "bottom", align = "v")
Figure_S2
ggsave(here("figures","Figure_S2.png"), plot = Figure_S2 , width = 10, height = 7)


################################################## Duration of active travel

Figure_1 = ademe_velo_marche_3550_no_0 %>%
  group_by(year, scenario) %>%
  summarise(mean_min = mean(min_pp_w)) %>%
  ungroup %>%
  mutate(scenario = factor(scenario, levels = rev(unique(scenario)))) %>%
  ggplot() +
  geom_segment( aes(x=scenario, xend=scenario, y=0, yend=mean_min)) +
  geom_point( aes(x=scenario, y=mean_min, color=scenario), size=3 ) +
  coord_flip()+
  xlab("") +
  ylab("Durée d'activité physique (min/pp/sem)") +
  facet_wrap(~year, ncol=1) +
  theme_pubr() +
  geom_hline(aes(yintercept = mean_min_2015, linetype ="France 2015"), color= "black", linewidth = 1)+
  geom_hline(aes(yintercept = 150, linetype ="Recommandations minimals (OMS)"), color= "black", linewidth = 1)+
  scale_linetype_manual(name = "", values = c(3,2))+
  labs(color = "" )+
  scale_color_manual(breaks = c("BAU", "S1", "S2", "S3", "S4"),
                     values=c("#999999", "#56B4E9", "#CC79A7", "#009E73", "#D55E00"))

Figure_1
ggsave(here("figures_fr","Figure_1.png"), plot = Figure_1, width = 10, height = 7 )


Figure_S3 =  ademe_velo_marche_3550_no_0 %>%
  mutate(age_grp_cont = as.numeric(gsub("-.*", "", age_grp))) %>%
  select(-age) %>% 
  distinct() %>%
  filter(age_grp_cont >= 20) %>%
  ggplot(.) +
  geom_point( aes(x=scenario, y=min_pp_w, color=age_grp_cont), size=2 )+
  coord_flip()+
  xlab("") +
  ylab("Duration of active travel (min/pp/w)") +
  facet_wrap(~year, ncol=1) +
  theme_pubr() +
  geom_hline(aes(yintercept = mean_min_2015, linetype ="France 2015"), color= "black", linewidth = 1)+
  geom_hline(aes(yintercept = 150, linetype ="Minimal recommandations (WHO)"), color= "black", linewidth = 1)+
  scale_linetype_manual(name = "", values = c(3,2))+
  scale_colour_viridis_c()+
  labs(color = "Age group")

Figure_S3
ggsave(here("figures","Figure_S3.png"), plot = Figure_S3, width = 10, height = 7 )


###########################################################################
#   death prevented 

p1= ademe_velo_marche_no_BAU_age %>%
  group_by(year, scenario) %>%
  summarise(sum_diff_death = sum(diff_death),
            sum_diff_death_low = sum(diff_death_low),
            sum_diff_death_high = sum(diff_death_high)) %>%
  ggplot(aes(x=year, y = sum_diff_death, fill = scenario, color = scenario)) +
  geom_line( size = 1.2) + 
  geom_ribbon(aes(ymin = sum_diff_death_low, ymax = sum_diff_death_high), alpha = 0.1 , linetype ="dotted" )+
  scale_y_continuous(name = "Décès évités",labels = scales::comma)  +
  theme_pubr() +
  xlab("Year") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#56B4E9", "#CC79A7", "#009E73", "#D55E00", "#999999")) +
  scale_color_manual(values=c("#56B4E9", "#CC79A7", "#009E73", "#D55E00", "#999999")) +
  geom_hline(aes(yintercept = 0), color= "black", linetype = "solid")+
  xlim (2021,2050)

#   death prevented in thousand
ademe_velo_marche_no_BAU_age$diff_death_mil <- ademe_velo_marche_no_BAU_age$diff_death /1000
ademe_velo_marche_no_BAU_age$diff_death_low_mil <- ademe_velo_marche_no_BAU_age$diff_death_low /1000
ademe_velo_marche_no_BAU_age$diff_death_high_mil <- ademe_velo_marche_no_BAU_age$diff_death_high /1000

p2= ademe_velo_marche_no_BAU_age %>%
  group_by(age_grp_10, scenario) %>%
  summarise(sum_diff_death = sum(diff_death_mil),
            sum_diff_death_low = sum(diff_death_low_mil),
            sum_diff_death_high = sum(diff_death_high_mil)) %>%
  ungroup %>%
  mutate(scenario = factor(scenario, levels = rev(unique(scenario)))) %>%
  ggplot(aes(x=age_grp_10, y = sum_diff_death, fill = scenario)) +
  geom_bar(position = position_dodge(), stat = "identity", width=0.8) + 
  geom_errorbar(aes(ymin = sum_diff_death_low, ymax = sum_diff_death_high ), width = 0.5, position = position_dodge(0.8))+
  theme_pubr() +
  xlab("Groupes d'âge") +
  ylab("Décès évités (milliers)")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#56B4E9", "#CC79A7", "#009E73", "#D55E00"), breaks = c("S1","S2","S3","S4")) +
  geom_hline(aes(yintercept = 0), color= "black")+
  xlim ("20-29","30-39","40-49","50-59","60-69","70-79", "80-89")+
  coord_flip()


# YLL

y1 = ademe_velo_marche_no_BAU_age %>%
  group_by(year, scenario) %>%
  summarise(sum_yll = sum(yll_corr),
            sum_yll_low = sum(yll_low_corr),
            sum_yll_high = sum(yll_high_corr)) %>%
  ggplot(aes(x=year, y = sum_yll, fill = scenario, color = scenario)) +
  geom_line( size = 1.2) + 
  geom_ribbon(aes(ymin = sum_yll_low, ymax = sum_yll_high), alpha = 0.1 , linetype ="dotted")+
  scale_y_continuous(name = "YLL prévenues" ,labels = scales::comma)  +
  theme_pubr() +
  xlab("Années") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(hjust=0.5))+
  scale_fill_manual(values=c("#56B4E9", "#CC79A7", "#009E73", "#D55E00", "#999999")) +
  scale_color_manual(values=c("#56B4E9", "#CC79A7", "#009E73", "#D55E00", "#999999")) +
  geom_hline(aes(yintercept = 0), color= "black", linetype = "solid")+
  xlim (2021,2050)


y2 = ademe_velo_marche_no_BAU_age %>%
  group_by(age_grp_10, scenario) %>%
  summarise(sum_yll = sum(yll_mil),
            sum_yll_low = sum(yll_low_mil),
            sum_yll_high = sum(yll_high_mil)) %>%
  ungroup %>%
  mutate(scenario = factor(scenario, levels = rev(unique(scenario)))) %>%
  ggplot(aes(x=age_grp_10, y = sum_yll, fill = scenario)) +
  geom_bar(position = position_dodge(), stat = "identity", width=0.8) + 
  geom_errorbar(aes(ymin = sum_yll_low, ymax = sum_yll_high ), width = 0.5, position = position_dodge(0.8))+
  theme_pubr() +
  xlab("Groupes d'âge") +
  scale_y_continuous(name = "YLL prévenues (milliers)" ,labels = scales::comma)  +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(hjust=0.8))+
  scale_fill_manual(values=c("#56B4E9", "#CC79A7", "#009E73", "#D55E00"), breaks = c("S1","S2","S3","S4")) +
  geom_hline(aes(yintercept = 0), color= "black")+
  xlim ("20-29","30-39","40-49","50-59","60-69","70-79", "80-89")+
  coord_flip()

Figure_2 = ggarrange(p1,y1,p2,y2, ncol = 2 , nrow = 2, common.legend = TRUE, 
                     labels = c("A", "B", "C", "D"), hjust = -9,
                     legend = "bottom", align = "v", heights = c(1,1.2))
Figure_2

ggsave(here("figures_fr","Figure_2.png"), plot = Figure_2, width=12, height=8)

####################################################################
# Type of physical activity

# Stacked area
# YLL prevented
t5 = ademe_all_type_S1 %>%
  filter(type != "Total cycle")%>%
  group_by(year, type) %>%
  summarise(sum_yll_corr = sum(yll_corr)) %>%
  ggplot(aes(x=year, y = sum_yll_corr, fill = type)) +
  geom_area() + 
  scale_y_continuous(labels = scales::comma, name = "", limits =  c(-22000,350000))  +
  theme_pubr() +
  xlab("") +
  scale_fill_manual(values=c( "#0072B2","#E69F00", "#F0E442"), name = "") +
  geom_hline(aes(yintercept = 0), color= "black", linetype = "solid")+
  xlim (2021,2050)

t6 = ademe_all_type_S2 %>%
  filter(type != "Total cycle")%>%
  group_by(year, type) %>%
  summarise(sum_yll_corr = sum(yll_corr)) %>%
  ggplot(aes(x=year, y = sum_yll_corr, fill = type)) +
  geom_area() + 
  scale_y_continuous(labels = scales::comma, name = "", limits =  c(-22000,350000))  +
  theme_pubr() +
  xlab("") +
  ylab("") +
  scale_fill_manual(values=c( "#0072B2","#E69F00", "#F0E442"), name = "") +
  geom_hline(aes(yintercept = 0), color= "black", linetype = "solid")+
  xlim (2021,2050)

t7 = ademe_all_type_S3 %>%
  group_by(year, type) %>%
  filter(type != "Total cycle")%>%
  summarise(sum_yll_corr = sum(yll_corr)) %>%
  ggplot(aes(x=year, y = sum_yll_corr, fill = type)) +
  geom_area() + 
  scale_y_continuous(labels = scales::comma, name = "", limits =  c(-22000,350000))  +
  theme_pubr() +
  xlab("") +
  ylab("") +
  scale_fill_manual(values=c( "#0072B2","#E69F00", "#F0E442"), name = "") +
  geom_hline(aes(yintercept = 0), color= "black", linetype = "solid")+
  xlim (2021,2050)

t8 = ademe_all_type_S4 %>%
  group_by(year, type) %>%
  filter(type != "Total cycle")%>%
  summarise(sum_yll_corr = sum(yll_corr)) %>%
  ggplot(aes(x=year, y = sum_yll_corr, fill = type)) +
  geom_area() + 
  scale_y_continuous(labels = scales::comma, name = "", limits = c(-22000,350000))  +
  theme_pubr() +
  xlab("") +
  ylab("") +
  scale_fill_manual(values=c( "#0072B2","#E69F00", "#F0E442"), name = "") +
  geom_hline(aes(yintercept = 0), color= "black", linetype = "solid")+
  xlim (2021,2050)


plot_type2 = ggarrange(t5,t6,t7,t8 , ncol = 2 , nrow = 2, common.legend = TRUE, legend = "bottom", labels = c("A: S1", "B: S2", "C: S3", "D: S4"), hjust = -2.2, vjust = 1.5)
Figure_3 = annotate_figure(plot_type2, left = text_grob("YLL prevented", size = 20, rot = 90))
Figure_3

ggsave(here("figures_fr","Figure_3.png"), plot = Figure_3, width=10, height=7)

###############################################################################
#   deaths prevented 2035 / 2050
d1.4 = ademe_velo_marche_no_BAU_age_2035 %>%
  group_by(scenario) %>%
  summarise(sum_diff_death = sum(diff_death),
            sum_diff_death_low = sum(diff_death_low),
            sum_diff_death_high = sum(diff_death_high)) %>%
  ggplot(aes(fill = scenario , y=sum_diff_death, x=scenario)) + 
  geom_bar(position="stack", stat="identity") + 
  geom_errorbar(aes(ymin = sum_diff_death_low, ymax = sum_diff_death_high ), width = 0.1, position = position_dodge(0.8))+
  scale_y_continuous(labels = scales::comma, name = "Décès prévenues", limits =  c(-4500,38000))  +
  xlab("")+
  theme(axis.text = element_text(size = 15))+
  scale_fill_manual(values=c("#56B4E9", "#CC79A7", "#009E73", "#D55E00"), name = "") +
  geom_hline(aes(yintercept = 0), color= "black", linetype = "solid")+
  theme_pubr()

d1.5 = ademe_velo_marche_no_BAU_age_2050 %>%
  group_by(scenario) %>%
  summarise(sum_diff_death = sum(diff_death),
            sum_diff_death_low = sum(diff_death_low),
            sum_diff_death_high = sum(diff_death_high)) %>%
  ggplot(aes(fill = scenario , y=sum_diff_death, x=scenario)) + 
  geom_bar(position="stack", stat="identity") + 
  geom_errorbar(aes(ymin = sum_diff_death_low, ymax = sum_diff_death_high ), width = 0.1, position = position_dodge(0.8))+
  scale_y_continuous(labels = scales::comma, name = "Décès prévenues", limits =  c(-4500,38000))  +
  xlab("")+
  theme(axis.text = element_text(size = 15))+
  scale_fill_manual(values=c("#56B4E9", "#CC79A7", "#009E73", "#D55E00"), name = "") +
  geom_hline(aes(yintercept = 0), color= "black", linetype = "solid")+
  theme_pubr()

# Cost

d1.2 = ademe_velo_marche_no_BAU_age_2035 %>%
  group_by(scenario) %>%
  summarise(sum_cout = sum(cout_md),
            sum_cout_low = sum(cout_low_md),
            sum_cout_high = sum(cout_high_md)) %>%
  ggplot(aes(fill = scenario , y=sum_cout, x=scenario)) + 
  geom_bar(position="stack", stat="identity") + 
  geom_errorbar(aes(ymin = sum_cout_low, ymax = sum_cout_high ), width = 0.1, position = position_dodge(0.8))+
  ylab("Coût évité (Milliards)") + 
  xlab("")+
  theme(axis.text = element_text(size = 15))+
  scale_fill_manual(values=c("#56B4E9", "#CC79A7", "#009E73", "#D55E00"), name = "") +
  geom_hline(aes(yintercept = 0), color= "black", linetype = "solid")+
  theme_pubr()+
  ylim(c(-7,87))

d1.3 = ademe_velo_marche_no_BAU_age_2050 %>%
  group_by(scenario) %>%
  summarise(sum_cout = sum(cout_md),
            sum_cout_low = sum(cout_low_md),
            sum_cout_high = sum(cout_high_md)) %>%
  ggplot(aes(fill = scenario , y=sum_cout, x=scenario)) + 
  geom_bar(position="stack", stat="identity") + 
  geom_errorbar(aes(ymin = sum_cout_low, ymax = sum_cout_high ), width = 0.1, position = position_dodge(0.8))+
  ylab("Coût évité (Milliards)") + 
  xlab("")+
  theme(axis.text = element_text(size = 15))+
  scale_fill_manual(values=c("#56B4E9", "#CC79A7", "#009E73", "#D55E00"), name = "") +
  geom_hline(aes(yintercept = 0), color= "black", linetype = "solid")+
  theme_pubr()+
  ylim(c(-7,87))

l1 = ademe_lifexp_no_BAU_age_2035 %>%
  group_by(scenario) %>%
  summarise(moy_lifexp = mean(diff_lifexp_mois),
            moy_lifexp_low = mean(diff_lifexp_mois_low),
            moy_lifexp_high = mean(diff_lifexp_mois_high)) %>%
  ggplot(aes(fill = scenario , y=moy_lifexp, x=scenario)) + 
  geom_bar(position="stack", stat="identity") + 
  geom_errorbar(aes(ymin = moy_lifexp_low, ymax = moy_lifexp_high ), width = 0.1, position = position_dodge(0.8))+
  ylab("Gain d'espérence de vie (mois)") + 
  xlab("")+
  theme(axis.text = element_text(size = 15))+
  scale_fill_manual(values=c("#56B4E9", "#CC79A7", "#009E73", "#D55E00"), name = "") +
  geom_hline(aes(yintercept = 0), color= "black", linetype = "solid")+
  theme_pubr()+
  ylim(c(-0.35,4.6))

l2 = ademe_lifexp_no_BAU_age_2050 %>%
  group_by(scenario) %>%
  summarise(moy_lifexp = mean(diff_lifexp_mois),
            moy_lifexp_low = mean(diff_lifexp_mois_low),
            moy_lifexp_high = mean(diff_lifexp_mois_high)) %>%
  ggplot(aes(fill = scenario , y=moy_lifexp, x=scenario)) + 
  geom_bar(position="stack", stat="identity") + 
  geom_errorbar(aes(ymin = moy_lifexp_low, ymax = moy_lifexp_high ), width = 0.1, position = position_dodge(0.8))+
  ylab("Gain d'espérence de vie (mois)") + 
  xlab("")+
  theme(axis.text = element_text(size = 15))+
  scale_fill_manual(values=c("#56B4E9", "#CC79A7", "#009E73", "#D55E00"), name = "") +
  geom_hline(aes(yintercept = 0), color= "black", linetype = "solid")+
  theme_pubr()+
  ylim(c(-0.35,4.6))

Figure_4 = ggarrange(d1.4,d1.5,l1, l2,d1.2,d1.3,  ncol = 2 , nrow = 3, common.legend = TRUE, legend = "bottom",
                      labels = c("A: 2035","B: 2050"," C:2035","D: 2050","E: 2035","F: 2050"), hjust = -1.4, align = "v")

Figure_4

ggsave(here("figures_fr","Figure_4.png"), plot = Figure_4, width=10, height=9)

####################################################################

# Remove
rm(p1,p2,y1,y2)

rm(d1.2,d1.3,d1.4,d1.5,l1, l2)

rm(t5,t6,t7,t8)

############################################################################# graph distrib
pyr1 = distrib_marche_fr %>%
  ggplot(aes(x=age_grp, y=rho))+
  geom_bar(position="stack", stat="identity", fill="#0072B2", alpha = 0.7)+
  theme_pubr()+
  theme(plot.title = element_text(size=15))+
  ylab("Rho")+
  xlab("Age group")+
  xlim ("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89")+
  scale_y_continuous(limits=c(0,1.5))


pyr3 = distrib_velo_fr %>%
  ggplot(aes(x=age_grp, y=rho))+
  geom_bar(position="stack", stat="identity", fill="#0072B2", alpha = 0.7)+
  theme_pubr()+
  theme(plot.title = element_text(size=15))+
  ylab("Rho")+
  xlab("Age group")+
  xlim ("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89")+
  scale_y_continuous(limits=c(0,1.5))

pyr4 = den_velo %>%
  group_by(age_grp)%>%
  summarise(mean_rho = mean(rho))%>%
  ungroup %>%
  ggplot(aes(x=age_grp, y=mean_rho))+
  geom_bar(position="stack", stat="identity", fill="#D55E00", alpha = 0.7)+
  theme_pubr()+
  theme(plot.title = element_text(size=15))+
  ylab("Rho")+
  xlab("Age group")+
  xlim ("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89")+
  scale_y_continuous(limits=c(0,1.5))



Figure_S4 = ggarrange(pyr3, pyr4,  ncol = 1 , nrow = 2, common.legend = TRUE, legend = "bottom",
                          labels = c("FR bike","DEN bike"), hjust = -1)

Figure_S5 = ggarrange(pyr1,  ncol = 1 , nrow = 1, common.legend = TRUE, legend = "bottom",
                          labels = c("FR walk"), hjust = -1)

Figure_S4
Figure_S5
ggsave(here("figures","Figure_S4.png"), plot = Figure_S4, width=10, height=7)
ggsave(here("figures","Figure_S5.png"), plot = Figure_S5, width=10, height=9)

############################################################################# Table


# Main text
# YLL
# 2021-2050
aggregate(list("YLL" = ademe_velo_marche$yll_corr), list("Scenario" = ademe_velo_marche$scenario), sum)
aggregate(list("YLL" = ademe_velo_marche$yll_low_corr), list("Scenario" = ademe_velo_marche$scenario), sum)
aggregate(list("YLL" = ademe_velo_marche$yll_high_corr), list("Scenario" = ademe_velo_marche$scenario), sum)


# Supplementary materials

# Deaths prevented
# 2021-2050
aggregate(list("Nb of  Deaths" = ademe_velo_marche$diff_death), list("Scenario" = ademe_velo_marche$scenario), sum)
aggregate(list("Nb of  Deaths" = ademe_velo_marche$diff_death_low), list("Scenario" = ademe_velo_marche$scenario), sum)
aggregate(list("Nb of  Deaths" = ademe_velo_marche$diff_death_high), list("Scenario" = ademe_velo_marche$scenario), sum)

# 2035
aggregate(list("Nb of  Deaths" = ademe_velo_marche_no_BAU_age_2035$diff_death), list("Scenario" = ademe_velo_marche_no_BAU_age_2035$scenario), sum)
aggregate(list("Nb of  Deaths" = ademe_velo_marche_no_BAU_age_2035$diff_death_low), list("Scenario" = ademe_velo_marche_no_BAU_age_2035$scenario), sum)
aggregate(list("Nb of  Deaths" = ademe_velo_marche_no_BAU_age_2035$diff_death_high), list("Scenario" = ademe_velo_marche_no_BAU_age_2035$scenario), sum)

# 2050
aggregate(list("Nb of  Deaths" = ademe_velo_marche_no_BAU_age_2050$diff_death), list("Scenario" = ademe_velo_marche_no_BAU_age_2050$scenario), sum)
aggregate(list("Nb of  Deaths" = ademe_velo_marche_no_BAU_age_2050$diff_death_low), list("Scenario" = ademe_velo_marche_no_BAU_age_2050$scenario), sum)
aggregate(list("Nb of  Deaths" = ademe_velo_marche_no_BAU_age_2050$diff_death_high), list("Scenario" = ademe_velo_marche_no_BAU_age_2050$scenario), sum)

# YLL
# 2021-2050
aggregate(list("YLL" = ademe_velo_marche$yll_corr), list("Scenario" = ademe_velo_marche$scenario), sum)
aggregate(list("YLL" = ademe_velo_marche$yll_low_corr), list("Scenario" = ademe_velo_marche$scenario), sum)
aggregate(list("YLL" = ademe_velo_marche$yll_high_corr), list("Scenario" = ademe_velo_marche$scenario), sum)

# 2035
aggregate(list("YLL" = ademe_velo_marche_no_BAU_age_2035$yll_corr), list("Scenario" = ademe_velo_marche_no_BAU_age_2035$scenario), sum)
aggregate(list("YLL" = ademe_velo_marche_no_BAU_age_2035$yll_low_corr), list("Scenario" = ademe_velo_marche_no_BAU_age_2035$scenario), sum)
aggregate(list("YLL" = ademe_velo_marche_no_BAU_age_2035$yll_high_corr), list("Scenario" = ademe_velo_marche_no_BAU_age_2035$scenario), sum)

# 2050
aggregate(list("YLL" = ademe_velo_marche_no_BAU_age_2050$yll_corr), list("Scenario" = ademe_velo_marche_no_BAU_age_2050$scenario), sum)
aggregate(list("YLL" = ademe_velo_marche_no_BAU_age_2050$yll_low_corr), list("Scenario" = ademe_velo_marche_no_BAU_age_2050$scenario), sum)
aggregate(list("YLL" = ademe_velo_marche_no_BAU_age_2050$yll_high_corr), list("Scenario" = ademe_velo_marche_no_BAU_age_2050$scenario), sum)



# Life expectancy
# 2035
aggregate(list("lifexp" = ademe_lifexp_no_BAU_age_2035$diff_lifexp_mois), list("Scenario" = ademe_lifexp_no_BAU_age_2035$scenario), mean)
aggregate(list("lifexp" = ademe_lifexp_no_BAU_age_2035$diff_lifexp_mois_low), list("Scenario" = ademe_lifexp_no_BAU_age_2035$scenario), mean)
aggregate(list("lifexp" = ademe_lifexp_no_BAU_age_2035$diff_lifexp_mois_high), list("Scenario" = ademe_lifexp_no_BAU_age_2035$scenario), mean)

# 2050
aggregate(list("lifexp" = ademe_lifexp_no_BAU_age_2050$diff_lifexp_mois), list("Scenario" = ademe_lifexp_no_BAU_age_2050$scenario), mean)
aggregate(list("lifexp" = ademe_lifexp_no_BAU_age_2050$diff_lifexp_mois_low), list("Scenario" = ademe_lifexp_no_BAU_age_2050$scenario), mean)
aggregate(list("lifexp" = ademe_lifexp_no_BAU_age_2050$diff_lifexp_mois_high), list("Scenario" = ademe_lifexp_no_BAU_age_2050$scenario), mean)

# Cost
# 2021-2050
aggregate(list("costs avoided" = ademe_velo_marche$cout), list("Scenario" = ademe_velo_marche$scenario), sum)
aggregate(list("costs avoided" = ademe_velo_marche$cout_low), list("Scenario" = ademe_velo_marche$scenario), sum)
aggregate(list("costs avoided" = ademe_velo_marche$cout_high), list("Scenario" = ademe_velo_marche$scenario), sum)

# 2035
aggregate(list("costs avoided" = ademe_velo_marche_no_BAU_age_2035$cout), list("Scenario" = ademe_velo_marche_no_BAU_age_2035$scenario), sum)
aggregate(list("costs avoided" = ademe_velo_marche_no_BAU_age_2035$cout_low), list("Scenario" = ademe_velo_marche_no_BAU_age_2035$scenario), sum)
aggregate(list("costs avoided" = ademe_velo_marche_no_BAU_age_2035$cout_high), list("Scenario" = ademe_velo_marche_no_BAU_age_2035$scenario), sum)

# 2050
aggregate(list("costs avoided" = ademe_velo_marche_no_BAU_age_2050$cout), list("Scenario" = ademe_velo_marche_no_BAU_age_2050$scenario), sum)
aggregate(list("costs avoided" = ademe_velo_marche_no_BAU_age_2050$cout_low), list("Scenario" = ademe_velo_marche_no_BAU_age_2050$scenario), sum)
aggregate(list("costs avoided" = ademe_velo_marche_no_BAU_age_2050$cout_high), list("Scenario" = ademe_velo_marche_no_BAU_age_2050$scenario), sum)
