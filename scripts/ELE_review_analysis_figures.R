### Data analysis and figures for ELE review (2023)

library(readxl)
library(tidyverse)

# Download data from "ELA Review paper / Data Collection Google Drive 
ela <- as.data.frame(read_xlsx("~/Downloads/Data extraction_2021Dec.xlsx",skip = 1))[-1,]

#
### Make figure theme ----------
ela_theme <- theme_classic() + 
  theme(legend.position = "right",
        axis.title.x = element_text(color = "black", size = 16, face = "bold"), 
        axis.text.x = element_text(color = "black", size = 12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(color = "black", size = 16, face = "bold", angle = 90, vjust = 0.5),
        axis.text.y = element_text(color = "black", size = 12), 
        legend.title = element_text(color = "black", size = 18, face = "bold"), 
        legend.text = element_text(color = "black", size = 16, face = "bold"))

# Set taxa colors 
birds = c(RColorBrewer::brewer.pal(8,"Set1")[c(2)])
mammals = c(RColorBrewer::brewer.pal(8,"Set1")[c(3)])
fish = c(RColorBrewer::brewer.pal(8,"Set1")[c(1)])
reps="goldenrod"
#reps = c(RColorBrewer::brewer.pal(8,"Set1")[c(5)])

sig = c(RColorBrewer::brewer.pal(8,"Dark2")[c(1)])
not_sig = c(RColorBrewer::brewer.pal(8,"Dark2")[c(3)])
#


### Clean data -----------
# Clean sex
table(ela$sex)
ela[ela$sex == "F",]$sex <- "females"
ela[ela$sex == "female",]$sex <- "females"
ela[ela$sex == "M",]$sex <- "males"
ela[ela$sex == "male",]$sex <- "males"
table(ela$sex)

# Remove Insects and cumulative variable 
ela <- ela %>% 
  filter(!order=="Coleoptera") %>% 
  filter(!adversity_category1 == "cumulative variable")

# Add taxa column 
unique(ela$order)
ela$taxa <- NA
ela[ela$order %in% c("Passeriformes", "Pelecaniformes", "Strigiformes", "Falconiformes", 
                     "Charadriiformes", "Sphenisciformes", "Procellariiformes", 
                     "Apodiformes", "Accipitriformes"),]$taxa <- c("Birds")
ela[ela$order %in% c("Primates", "Artiodactyla", "Carnivora", "Rodentia", 
                     "Lagomorpha", "Diprotodontia"),]$taxa <- c("Mammals")
ela[ela$order %in% c("Perciformes", "Salmoniformes", "Labriformes", "Pleuronectiformes", 
                     "Gasterosteiformes", "Esociformes", "Acipenseriformes"),]$taxa <- c("Fishes")
ela[ela$order %in% c("Testudines", "Anura", "Urodela"),]$taxa <- c("Reptiles and Amphibians")

# Order taxa levels 
ela <- ela %>% 
  mutate(taxa=as.factor(taxa)) %>%
  mutate(taxa=fct_relevel(taxa,c("Birds","Mammals","Fishes","Reptiles and Amphibians")))

# Order order column by taxa 
length(unique(ela$order))
ela$order <- as.factor(ela$order)
ela$order <- fct_relevel(ela$order, rev(c(c("Passeriformes", "Pelecaniformes", "Strigiformes", "Falconiformes",
                                            "Charadriiformes", "Sphenisciformes", "Procellariiformes",
                                            "Apodiformes", "Accipitriformes"), 
                                          c("Primates", "Artiodactyla", "Carnivora", "Rodentia", 
                                            "Lagomorpha", "Diprotodontia"),
                                          c("Perciformes", "Salmoniformes", "Labriformes", 
                                            "Pleuronectiformes","Gasterosteiformes", 
                                            "Esociformes", "Acipenseriformes"), 
                                          c("Testudines", "Anura", "Urodela")))) 

# Change effect significance to just be sig. or not sig. 
ela$effect_sig <- NA
ela[ela$effect_significance_tidy %in% c("not significant", "Bayesian no effect"),]$effect_sig <- c("Not significant")
ela[ela$effect_significance_tidy %in% c("significant", "Bayesian effect"),]$effect_sig <- c("Significant")
ela[ela$effect_significance_tidy %in% c("trending", "Bayesian trending"),]$effect_sig <- c("Not significant")
ela$effect_sig <- fct_relevel(ela$effect_sig, c("Not significant","Significant")) 
table(ela$effect_sig, ela$effect_significance_tidy)
dim(ela[is.na(ela$effect_sig),])[1]

# Remove rows where we could not determine effect significance 
ela <- ela %>% 
  filter(!is.na(effect_sig))

# Remove individuals where the adversity was measured only in adulthood and not at any other time 
dim(ela[ela$adversity_life_stage_category1 == "Adult",])[1]
ela <- ela[!ela$adversity_life_stage_category1 == "Adult",]

# Revise and relevel adversity categories 
unique(ela$adversity_category1) # Change to: social environment, parental effects, ecological conditions, body condition 
# Change these labels to be more intuitive 
ela[ela$adversity_category1 == "ecological condition",]$adversity_category1 <- c("ecological conditions")
ela[ela$adversity_category1 == "parental effect",]$adversity_category1 <- c("parental effects")
ela[ela$adversity_category1 == "offspring condition",]$adversity_category1 <- c("body condition")
ela <- ela %>% 
  mutate(adversity_category1=as.factor(adversity_category1)) %>%
  mutate(adversity_category1=fct_relevel(adversity_category1,(c("social environment","body condition",
                                                                "parental effects","ecological conditions"))))
table(ela$adversity_category1)
levels(ela$adversity_category1)

# Revise and relevel outcome categories 
unique(ela$outcome_category1) 
ela[ela$outcome_category1 == "growth morphology",]$outcome_category1 <- c("growth/morphology")
ela[ela$outcome_category1 == "fitness reproduction",]$outcome_category1 <- c("fitness/reproduction")
ela[ela$outcome_category1 == "life history",]$outcome_category1 <- c("life history traits")
ela[ela$outcome_category1 == "behavior social",]$outcome_category1 <- c("social behaviors")
ela <- ela %>% 
  mutate(outcome_category1=as.factor(outcome_category1)) %>%
  mutate(outcome_category1=fct_relevel(outcome_category1,(c("fitness/reproduction","social behaviors","physiology","growth/morphology","life history traits"))))


# Clean lifestage data. Remove if adversity life stage was intergenerational or not specified 
table(ela$adversity_life_stage_category1)
ela <- ela[!ela$adversity_life_stage_category1 %in% c("Not specified", "Intergenerational - Preconception"),]

# Make earliest adversity life stage column 
ela <- ela %>% 
  mutate(earliest_adversity = ifelse(str_detect(adversity_life_stage_category1, pattern = "Prenatal/Prehatch"), 
                                     "Prenatal/Prehatch", 
                                     ifelse(str_detect(adversity_life_stage_category1, pattern = "Dependent juvenile"), 
                                            "Juvenile", 
                                            ifelse(str_detect(adversity_life_stage_category1, pattern = "Independent juvenile"), 
                                                   "Juvenile", 
                                                   NA))))
# Check that this got the earliest life stage 
table(ela$earliest_adversity, ela$adversity_life_stage_category1)
ela$earliest_adversity <- factor(ela$earliest_adversity, levels = c("Prenatal/Prehatch","Juvenile"))

# Make earliest outcome life stage column, filter out intergenerational effects
ela <- ela %>% 
  filter(!outcome_life_stage_category1 %in% 
           c("Intergenerational- Adult", "Intergenerational- Independent juvenile", 
             "Interdependent juvenile", "Intergenerational- Dependent juvenile", 
             "Intergenerational- Dependent juvenile; Intergenerational- Independent juvenile")) %>% 
  mutate(earliest_outcome = ifelse(str_detect(outcome_life_stage_category1, pattern = "Dependent juvenile"), "Juvenile", 
                                   ifelse(str_detect(outcome_life_stage_category1, pattern = "Independent juvenile"), "Juvenile", 
                                          ifelse(str_detect(outcome_life_stage_category1, pattern = "Adult"), "Adult", NA)))) 
# check that this got the earliest life stage 
table(ela$earliest_outcome, ela$outcome_life_stage_category1)
ela$earliest_outcome <- factor(ela$earliest_outcome, levels = c("Juvenile","Adult"))
# 


### Basic info about the data --------
# Number of comparisons
dim(ela)

# How many unique papers are there? 
length(unique(ela$title))

# How many unique species? 
length(unique(ela$species_latin_name))

# How many studies studied each taxa? 
length(unique(ela[ela$taxa=="Birds",]$title))
length(unique(ela[ela$taxa=="Mammals",]$title))
#


####### FINAL FIGURES #########
### Fig 1B, 1C) Number of adversity and outcome type ---------------
# Number of species 
length(unique(ela[ela$taxa=="Birds",]$species_latin_name))
length(unique(ela[ela$taxa=="Mammals",]$species_latin_name)) # Papio cynocephalus is in twice - so only count once, 
length(unique(ela[ela$taxa=="Fishes",]$species_latin_name))
length(unique(ela[ela$taxa=="Reptiles and Amphibians",]$species_latin_name))
# definitely some typos in the Latin names - check them 
View(data.frame(unique(ela[ela$taxa=="Birds",]$species_latin_name)))
View(data.frame(unique(ela[ela$taxa=="Mammals",]$species_latin_name)))

# Number of papers
length(unique(ela[ela$taxa=="Birds",]$title))
length(unique(ela[ela$taxa=="Mammals",]$title))
length(unique(ela[ela$taxa=="Fishes",]$title))
length(unique(ela[ela$taxa=="Reptiles and Amphibians",]$title))

# % of papers of the total 
length(unique(ela[ela$taxa=="Birds",]$title))/length(unique(ela$title))
length(unique(ela[ela$taxa=="Mammals",]$title))/length(unique(ela$title))
length(unique(ela[ela$taxa=="Fishes",]$title))/length(unique(ela$title))
length(unique(ela[ela$taxa=="Reptiles and Amphibians",]$title))/length(unique(ela$title))

# Number of comparisons 
dim(ela[ela$taxa=="Birds",])[1]#/dim(ela)[1]
dim(ela[ela$taxa=="Mammals",])[1]#/dim(ela)[1]
dim(ela[ela$taxa=="Fishes",])[1]#/dim(ela)[1]
dim(ela[ela$taxa=="Reptiles and Amphibians",])[1]#/dim(ela)[1]

# Number of studies on Passeriformes among Birds 
length(unique(ela[ela$order=="Passeriformes",]$title))/length(unique(ela[ela$taxa=="Birds",]$title))

# Plot number of comparisons per adversity per taxa 
fig_1b <- ela %>% 
  group_by(adversity_category1,taxa) %>%
  summarise(n=n()) %>% 
  ggplot(aes(x=adversity_category1, y=n, label = n, fill = taxa)) + #y=adversity_category1, x=n, label = n
  geom_col(color="black", show.legend = F, alpha=0.8) + #, fill = "grey70"
  labs(x="Adversity type\n" , y="") + 
  facet_wrap(.~taxa, ncol=1) + #nrow=1 
  ela_theme + 
  scale_fill_manual(values = c(birds,mammals,fish,reps)) +
  #  scale_x_discrete(limits=rev) + 
  scale_y_continuous(limits = c(0,570), breaks = c(0,250,500)) + 
  ggrepel::geom_text_repel(nudge_y = 5, aes(size=10), show.legend = F) + #nudge_x = 5, nudge_y = 0
  theme(strip.background = element_rect(color = "white"),
        strip.placement = "inside", 
        strip.text = element_blank(), 
        axis.text.x = element_text(angle=45, vjust = 0.8, hjust = 0.8, size = 16))
ggsave(fig_1b, filename = "~/Desktop/fig_1b.pdf", device = "pdf", width = 4, height = 12) #width = 14, height = 2

fig_1c <- ela %>% 
  group_by(outcome_category1,taxa) %>%
  summarise(n=n()) %>% 
  ggplot(aes(x=outcome_category1, y=n, label = n, fill = taxa)) + 
  geom_col(color="black", show.legend = F, alpha=0.8) + #, fill = "grey70"
  labs(x="Outcome type\n", y="") + 
  facet_wrap(.~taxa, ncol=1) + 
  ela_theme + 
  scale_fill_manual(values = c(birds,mammals,fish,reps)) +
  scale_y_continuous(limits = c(0,570), breaks = c(0,250,500)) + 
  ggrepel::geom_text_repel(nudge_y=5, aes(size=10), show.legend = F) + #nudge_x = 5, nudge_y = 0
  theme(strip.background = element_rect(color = "white"),
        strip.placement = "inside", 
        strip.text = element_blank(), 
        axis.text.x = element_text(angle=45, vjust = 0.8, hjust = 0.8, size = 16))
ggsave(fig_1c, filename = "~/Desktop/fig_1c.pdf", device = "pdf", width = 4.3, height = 12) #width = 14, height = 2
#


### Fig 1D) Counts for adversity-outcome type links (Eli Strauss' code to be added) ---------
### Fig 1E) Counts for adversity-outcome life stage links (From Liz Lange) -----------
adversity.outcome.segments <- ela %>% 
  group_by(taxa, earliest_adversity, earliest_outcome) %>% 
  summarise(count = length(earliest_adversity)) %>% 
  ungroup() %>% right_join(tidyr::expand(ela, taxa, earliest_adversity, earliest_outcome)) %>% 
  arrange(taxa, earliest_adversity, earliest_outcome)
adversity.outcome.segments <- adversity.outcome.segments %>%
  group_by(taxa) %>% 
  summarise(total = sum(count, na.rm = TRUE)) %>% 
  ungroup() %>% right_join(adversity.outcome.segments, by="taxa")
colnames(adversity.outcome.segments) <- c('taxa', 'total', 'adversity', 'outcome', 'count')

### Make empty plotting data frames for points and segments:
adversity.outcome.points <- rbind(data.frame(label = 'adversity', category = levels(ela$earliest_adversity),
                                             x = 1, 
                                             y = seq(from = 0, to = 0.5,
                                                     length.out = length(levels(ela$earliest_adversity))),
                                             image = paste0("~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/", 
                                                            gsub(' ', '_', levels(ela$earliest_adversity)), '.png')),
                                  data.frame(label = 'outcome', category = levels(ela$earliest_outcome), x = 2, 
                                             y = seq(from = 0.5, to = 1,
                                                     length.out = length(levels(ela$earliest_outcome))),
                                             image = paste0("~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/", 
                                                            gsub(' ', '_', levels(ela$earliest_outcome), '.png'))))

# Make data frame for segments
segments.one.group <- cbind(expand.grid(adversity.outcome.points$category[1:length(levels(ela$earliest_adversity))], 
                                        adversity.outcome.points$category[(length(levels(ela$earliest_adversity))+1):
                                                                            length(c(levels(ela$earliest_outcome), 
                                                                                     levels(ela$earliest_adversity)))],
                                        stringsAsFactors = F),
                            expand.grid(adversity.outcome.points$x[1:length(levels(ela$earliest_adversity))], 
                                        adversity.outcome.points$x[(length(levels(ela$earliest_adversity))+1):
                                                                     length(c(levels(ela$earliest_outcome), 
                                                                              levels(ela$earliest_adversity)))],
                                        stringsAsFactors = F),
                            expand.grid(adversity.outcome.points$y[1:length(levels(ela$earliest_adversity))], 
                                        adversity.outcome.points$y[(length(levels(ela$earliest_adversity))+1):
                                                                     length(c(levels(ela$earliest_outcome), 
                                                                              levels(ela$earliest_adversity)))],
                                        stringsAsFactors = F))

names(segments.one.group) <- c('adversity', 'outcome', 'x', 'xend', 'y', 'yend')
adversity.outcome.segments <- left_join(adversity.outcome.segments, segments.one.group, by = c('adversity', 'outcome'))
head(adversity.outcome.segments)

# Birds
fig1E_birds <- ggplot(data = adversity.outcome.points, aes(x = x, y = y)) +
  geom_segment(data = filter(adversity.outcome.segments, taxa == "Birds"), 
               aes(xend=xend, yend = yend, size = 10*count/total), 
               color = alpha(birds, 0.5))+
  geom_image(aes(image = c("~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Prenatal_Birds.png",       
                           "~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Juvenile_Birds.png",       
                           "~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Juvenile_Birds.png",       
                           "~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Adult_Birds.png")), size = 0.17) +
  ylim(-0.1, 1.1)+
  scale_size_identity()+
  scale_x_continuous(label = c('Adversity', 'Outcome'), limits = c(0.8, 2.2), breaks = c(1,2), position = 'top')+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
                     axis.title = element_blank(), axis.ticks = element_blank(), 
                     axis.text.x.top = element_text(), axis.text.x.bottom = element_blank(),
                     axis.text.x = element_text(size=20, face="bold"), axis.text.y = element_blank())
ggsave(fig1E_birds, filename = "~/Desktop/fig1E_birds.pdf", device = "pdf", width = 3.2, height = 4)

# Mammals
fig1E_mammals <- ggplot(data = adversity.outcome.points, aes(x = x, y = y))+
  geom_segment(data = filter(adversity.outcome.segments, taxa == "Mammals"), 
               aes(xend=xend, yend = yend, size = 10*count/total), 
               color = alpha(mammals, 0.5))+
  geom_image(aes(image = c("~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Prenatal_Mammals.png",       
                           "~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Juvenile_Mammals.png",       
                           "~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Juvenile_Mammals.png",       
                           "~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Adult_Mammals.png")), size = 0.17)+
  ylim(-0.1, 1.1)+
  scale_size_identity()+
  scale_x_continuous(label = c('Adversity', 'Outcome'), limits = c(0.8, 2.2), breaks = c(1,2), position = 'top')+
  #theme_void()+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
                     axis.title = element_blank(), axis.ticks = element_blank(), 
                     axis.text.x.top = element_text(), axis.text.x.bottom = element_blank(),
                     axis.text.x = element_text(size=20, face="bold"), axis.text.y = element_blank())
ggsave(fig1E_mammals, filename = "~/Desktop/fig1E_mammals.pdf", device = "pdf", width = 3.2, height = 4)

#Amphibians & Reptiles
fig1E_reps <- ggplot(data = adversity.outcome.points, aes(x = x, y = y)) +
  geom_segment(data = filter(adversity.outcome.segments, taxa == "Reptiles and Amphibians"), 
               aes(xend=xend, yend = yend, size = 10*count/total), 
               color = alpha(reps, 0.5))+
  geom_image(aes(image = c("~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Prenatal_RepAmp.png",
                           "~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Juvenile_RepAmp.png",
                           "~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Juvenile_RepAmp.png",
                           "~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Adult_RepAmp.png")), size = 0.17) +
  ylim(-0.1, 1.1)+
  scale_size_identity()+
  scale_x_continuous(label = c('Adversity', 'Outcome'), limits = c(0.8, 2.2), breaks = c(1,2), position = 'top')+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
                     axis.title = element_blank(), axis.ticks = element_blank(), 
                     axis.text.x.top = element_text(), axis.text.x.bottom = element_blank(),
                     axis.text.x = element_text(size=20, face="bold"), axis.text.y = element_blank())
ggsave(fig1E_reps, filename = "~/Desktop/fig1E_reps.pdf", device = "pdf", width = 3.2, height = 4)

#Fish
fig1E_fish <- ggplot(data = adversity.outcome.points, aes(x = x, y = y))+
  geom_segment(data = filter(adversity.outcome.segments, taxa == "Fishes"), 
               aes(xend=xend, yend = yend, size = 10*count/total), 
               color = alpha(fish, 0.5))+
  geom_image(aes(image = c("~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Prenatal_Fish.png",
                           "~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Juvenile_Fish.png",
                           "~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Juvenile_Fish.png",
                           "~/Documents/_UW/_SMackLab/ELA_review/ELA_code/code_from_Liz/Adult_Fish.png")), size = 0.17)+
  ylim(-0.1, 1.1)+
  scale_size_identity()+
  scale_x_continuous(label = c('Adversity', 'Outcome'), limits = c(0.8, 2.2), breaks = c(1,2), position = 'top')+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
                     axis.title = element_blank(), axis.ticks = element_blank(), 
                     axis.text.x.top = element_text(), axis.text.x.bottom = element_blank(),
                     axis.text.x = element_text(size=20, face="bold"), axis.text.y = element_blank())
ggsave(fig1E_fish, filename = "~/Desktop/fig1E_fish.pdf", device = "pdf", width = 3.2, height = 4)
#


### Fig 2) Percent significant: adversity x outcome --------------
adv_out <- ela %>% 
  group_by(outcome_category1, effect_sig, adversity_category1) %>% 
  summarise(n=n()) %>% #n_studies=length(unique(title))
  group_by(outcome_category1, adversity_category1) %>% 
  mutate(prop=n/sum(n), n_total=sum(n)) #, n_studies_total=sum(n_studies)

# Binomial test of adversity-outcome pair significance enrichment 
binom_adv_out <- do.call(rbind, lapply(1:nrow(adv_out[adv_out$effect_sig=="Significant",]), function(x){ 
  new_df <- adv_out[adv_out$effect_sig=="Significant",]
  binom_out<-binom.test(x = new_df[x,]$n, 
                        n = new_df[x,]$n_total, 
                        p = dim(ela[ela$effect_sig == "Significant",])[1]/dim(ela)[1])
  data.frame(prop=binom_out$estimate, 
             pval=binom_out$p.value,
             n=new_df[x,]$n,
             n_total=new_df[x,]$n_total,
             adversity=new_df[x,]$adversity_category1,
             outcome=new_df[x,]$outcome_category1, 
             bkgd_prop=dim(ela[ela$effect_sig == "Significant",])[1]/dim(ela)[1])
}))
binom_adv_out$p_adj <- p.adjust(binom_adv_out$pval,method = "BH")
#write.table(binom_adv_out,file = "~/Desktop/binom_adv_out.csv",sep = ",",col.names = T,row.names = F)

# Plot 
fig_2<-adv_out %>% 
  left_join(binom_adv_out[,c("p_adj","pval","outcome","adversity", "bkgd_prop")], 
            by = c("outcome_category1"="outcome","adversity_category1"="adversity")) %>% 
  ggplot(aes(y=outcome_category1, x=prop*100, fill = effect_sig, label = n_total)) + 
  geom_col(show.legend = F) + 
  scale_x_continuous(limits = c(0,115), breaks = c(0,25,50,75,100)) + 
  scale_y_discrete(position = "right", limits=rev) + 
  labs(x="", y="") + 
  facet_grid(~ adversity_category1, scales = "free", switch = "y") +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(8,"Dark2")[c(3,1)])) +
  ela_theme + 
  theme(axis.text.y = element_text(face="bold"),
        legend.title = element_blank(),
        strip.background = element_rect(color = "white"),
        strip.placement = "inside", 
        strip.text = element_text(face="bold", size = 12)) + 
  geom_vline(aes(xintercept = 100*(bkgd_prop)), 
             color = "grey90", lty = 3, size = 1) + 
  annotate("segment",x = 1,xend = 112, y=5.6, yend=5.6, size = 1.5) + 
  ggrepel::geom_text_repel(aes(vjust=0.3),nudge_x=115, segment.color=NA,
                           data = adv_out[adv_out$effect_sig=="Significant",])
ggsave(fig_2, filename = "~/Desktop/fig_2.pdf", device = "pdf", width = 12, height = 3)
#


### Fig 3a) Adversity x adversity life stage -------------
table(ela$earliest_adversity, ela$adversity_life_stage_category1)
adv_ls_new <- ela[,c("title","adversity_category1","effect_sig","adversity_life_stage_category1","earliest_adversity")] %>%
  group_by(earliest_adversity, effect_sig, adversity_category1) %>% 
  summarise(n=n()) %>% 
  group_by(earliest_adversity, adversity_category1) %>% 
  mutate(prop=n/sum(n), n_total=sum(n)) 

# Stats - enrichment test for sig. 
binom_adv_ls_out_new <- do.call(rbind, lapply(1:nrow(adv_ls_new[adv_ls_new$effect_sig=="Significant",]), function(x){ 
  new_df <- adv_ls_new[adv_ls_new$effect_sig=="Significant",]
  binom_out<-binom.test(x = new_df[x,]$n, 
                        n = new_df[x,]$n_total, 
                        p = dim(ela[ela$effect_sig == "Significant",])[1]/dim(ela)[1])
  data.frame(prop=binom_out$estimate, 
             pval=binom_out$p.value,
             n=new_df[x,]$n,
             n_total=new_df[x,]$n_total,
             lifestage=new_df[x,]$earliest_adversity, 
             adversity=new_df[x,]$adversity_category1,
             bkgd_prop=dim(ela[ela$effect_sig == "Significant",])[1]/dim(ela)[1])
}))
binom_adv_ls_out_new$p_adj <- p.adjust(binom_adv_ls_out_new$pval,method = "BH")
#write.table(binom_adv_ls_out_new,file = "~/Desktop/binom_adv_ls_out_new.csv",sep = ",",col.names = T,row.names = F)

# Plots 
fig_3a <- adv_ls_new %>% 
  left_join(binom_adv_ls_out_new[,c("p_adj","pval","bkgd_prop","lifestage","adversity")], 
            by = c("earliest_adversity"="lifestage",
                   "adversity_category1"="adversity")) %>%
  ggplot(aes(y=adversity_category1, x=prop*100, fill = effect_sig, label = n_total)) + 
  geom_col(show.legend = F) + 
  scale_y_discrete(position = "right") + 
  scale_x_continuous(limits = c(0,112), breaks = c(0,25,50,75,100)) + 
  labs(x="Percent comparisons significant", y="Adversity type\n") + 
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(8,"Dark2")[c(3,1)])) +
  facet_grid(~earliest_adversity, switch="y") +
  ela_theme + 
  theme(axis.text.y = element_text(size=10, angle = 0, face="bold"),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(color = "black", size = 12, face = "bold"), 
        legend.title = element_blank(),
        strip.background = element_rect(color = "white"),
        strip.placement = "inside", 
        strip.text = element_text(face="bold", size = 12)) + 
  geom_vline(aes(xintercept = 100*(bkgd_prop)), color = "grey90", lty = 3, size = 1) +
  annotate("segment",x = 1,xend = 99, y=4.7, yend=4.7, size = 1.2) + 
  ggrepel::geom_text_repel(aes(vjust=0.3),nudge_x=105, segment.color=NA,
                           data = adv_ls_new[!duplicated(adv_ls_new[,c("earliest_adversity","adversity_category1")]),])
ggsave(fig_3a, filename = "~/Desktop/fig_3a.pdf", device = "pdf", width = 8, height = 2)
# 


### Fig 3b) Outcome x outcome life stage -------------
table(ela$earliest_outcome, ela$outcome_life_stage_category1)
out_ls_new <- ela[,c("title","outcome_category1","effect_sig","outcome_life_stage_category1","earliest_outcome")] %>%
  group_by(earliest_outcome, effect_sig, outcome_category1) %>% 
  summarise(n=n()) %>% 
  group_by(earliest_outcome, outcome_category1) %>% 
  mutate(prop=n/sum(n), n_total=sum(n)) 

# Stats - enrichment test for sig. 
binom_out_ls_out_new <- do.call(rbind, lapply(1:nrow(out_ls_new[out_ls_new$effect_sig=="Significant",]), function(x){ 
  new_df <- out_ls_new[out_ls_new$effect_sig=="Significant",]
  binom_out<-binom.test(x = new_df[x,]$n, 
                        n = new_df[x,]$n_total, 
                        p = dim(ela[ela$effect_sig == "Significant",])[1]/dim(ela)[1])
  data.frame(prop=binom_out$estimate, 
             pval=binom_out$p.value,
             n=new_df[x,]$n,
             n_total=new_df[x,]$n_total,
             lifestage=new_df[x,]$earliest_outcome, 
             outcome=new_df[x,]$outcome_category1,
             bkgd_prop=dim(ela[ela$effect_sig == "Significant",])[1]/dim(ela)[1])
}))
binom_out_ls_out_new$p_adj <- p.adjust(binom_out_ls_out_new$pval,method = "BH")
#write.table(binom_out_ls_out_new,file = "~/Desktop/binom_out_ls_out_new.csv",sep = ",",col.names = T,row.names = F)

# Plots 
fig_3b <- out_ls_new %>% 
  left_join(binom_out_ls_out_new[,c("p_adj","pval","bkgd_prop","lifestage","outcome")], 
            by = c("earliest_outcome"="lifestage",
                   "outcome_category1"="outcome")) %>%
  ggplot(aes(y=outcome_category1, x=prop*100, fill = effect_sig, label = n_total)) + 
  geom_col(show.legend = F) + 
  scale_y_discrete(position = "right") + 
  scale_x_continuous(limits = c(0,112), breaks = c(0,25,50,75,100)) + 
  labs(x="Percent comparisons significant", y="Outcome type\n") + 
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(8,"Dark2")[c(3,1)])) +
  facet_grid(~earliest_outcome, switch="y") +
  ela_theme + 
  theme(axis.text.y = element_text(size=10, angle = 0, face="bold"),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(color = "black", size = 12, face = "bold"),
        legend.title = element_blank(),
        strip.background = element_rect(color = "white"),
        strip.placement = "inside", 
        strip.text = element_text(face="bold", size = 12)) + 
  geom_vline(aes(xintercept = 100*(bkgd_prop)), color = "grey90", lty = 3, size = 1) +
  annotate("segment",x = 1,xend = 99, y=5.7, yend=5.7, size = 1.2) + 
  ggrepel::geom_text_repel(aes(vjust=0.3),nudge_x=105, segment.color=NA,
                           data = out_ls_new[!duplicated(out_ls_new[,c("earliest_outcome","outcome_category1")]),])
ggsave(fig_3b, filename = "~/Desktop/fig_3b.pdf", device = "pdf", width = 8, height = 2.5)
# 


### Supp Fig 1) Number papers per order ----------------
# n papers per order bar plot
fig_order_supp <- ela %>% 
  filter(!duplicated(title, order, taxa)) %>% 
  group_by(taxa, order) %>%
  summarise(n=n()) %>% 
  arrange(taxa, desc(n)) %>% 
  mutate(order=factor(order, unique(as.character(order)))) %>% 
  ggplot(aes(y=order, x=n, label = n, fill = taxa)) + #reorder(order, desc(order))
  geom_col(color="black", alpha=0.8, show.legend = F) + 
  labs(y="Order\n", fill = "Taxa", x = "Number of papers") + 
  scale_fill_manual(values = c(birds,mammals,fish,reps)) +
  ela_theme + 
  scale_y_discrete(limits=rev) + 
  scale_x_continuous(limits = c(0,80), breaks = c(0,25,50,75)) + 
  ggrepel::geom_text_repel(nudge_x = 2, nudge_y = 0) + 
  theme(strip.background = element_rect(color = "white"),
        strip.placement = "inside", 
        strip.text = element_blank(), 
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))
ggsave(fig_order_supp, filename = "~/Desktop/SI_fig1.pdf", device = "pdf", width = 8, height = 9)
rm(fig_order_supp)
#


### Supp Fig 2) Percent significant: adversity x outcome x taxa ----------------
adv_out_taxa <- ela %>% 
  group_by(outcome_category1, effect_sig, adversity_category1, taxa) %>% 
  summarise(n=n()) %>% #n_studies=length(unique(title))
  group_by(outcome_category1, adversity_category1, taxa) %>% 
  mutate(prop=n/sum(n), n_total=sum(n)) %>% #, n_studies_total=sum(n_studies)
  filter(n_total>5) 

# Stats - enrichment test for sig. 
binom_adv_out_taxa <- do.call(rbind, lapply(unique(adv_out_taxa$taxa), function(y){
  new_df <- adv_out_taxa[adv_out_taxa$taxa==y & adv_out_taxa$effect_sig=="Significant",]
  do.call(rbind, lapply(1:nrow(new_df), function(x){ 
    prop = dim(ela[ela$taxa==y & ela$effect_sig == "Significant",])[1]/dim(ela[ela$taxa==y,])[1]
    binom_out<-binom.test(x = new_df[x,]$n, 
                          n = new_df[x,]$n_total, 
                          p = prop)
    data.frame(prop=binom_out$estimate, 
               pval=binom_out$p.value,
               n=new_df[x,]$n,
               n_total=new_df[x,]$n_total,
               outcome=new_df[x,]$outcome_category1, 
               adversity=new_df[x,]$adversity_category1,
               taxa=new_df[x,]$taxa,
               prop_per_taxa=prop)
  }))
}))
binom_adv_out_taxa$p_adj <- p.adjust(binom_adv_out_taxa$pval,method = "BH")
#write.table(binom_adv_out_taxa,file = "~/Desktop/binom_adv_out_taxa.csv",sep = ",",col.names = T,row.names = F)

# Add number of studies to the df 
#adv_out_taxa <- left_join(adv_out_taxa, ela %>% 
#  group_by(outcome_category1, adversity_category1, taxa) %>% 
#  summarise(n_studies=length(unique(title))), 
#  by = c("outcome_category1", "adversity_category1", "taxa")) %>% 
#  mutate(Ntotal_Nstudies = paste0(n_total, ", ", n_studies))

# Plots 
supp_fig_2i <- adv_out_taxa %>% 
  filter(taxa %in% c("Birds","Mammals")) %>% 
  left_join(binom_adv_out_taxa[,c("pval","prop_per_taxa","outcome","adversity","taxa")], 
            by = c("outcome_category1"="outcome",
                   "adversity_category1"="adversity",
                   "taxa"="taxa")) %>% 
  ggplot(aes(y=outcome_category1, x=prop*100, fill = effect_sig, label = n_total)) + #Ntotal_Nstudies
  geom_col(show.legend = F) + 
  scale_x_continuous(limits = c(0,115), breaks = c(0,25,50,75,100)) + 
  scale_y_discrete(position = "right", limits=rev) + 
  labs(x="", y="") + 
  facet_grid(taxa ~ adversity_category1, scales = "free", switch = "y") +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(8,"Dark2")[c(3,1)])) +
  ela_theme + 
  theme(axis.text.y = element_text(face="bold"),
        legend.title = element_blank(),
        strip.background = element_rect(color = "white"),
        strip.placement = "inside", 
        strip.text = element_text(face="bold", size = 12)) + 
  geom_vline(aes(xintercept = 100*(prop_per_taxa)), 
             color = "grey90", lty = 3, size = 1) + 
  annotate("segment",x = 1,xend = 112, y=5.7, yend=5.7, size = 1.5) + 
  ggrepel::geom_text_repel(aes(vjust=0.3),nudge_x=115, segment.color=NA,
                           data = adv_out_taxa[!duplicated(adv_out_taxa[,c("outcome_category1","adversity_category1", "taxa")]),] %>% 
                             filter(taxa %in% c("Birds","Mammals")) %>% 
                             filter(n_total>5))
ggsave(supp_fig_2i, filename = "~/Desktop/supp_fig_2i.pdf", device = "pdf", width = 14, height = 4) # height = 8
#
supp_fig_2ii <- adv_out_taxa %>% 
  filter(taxa %in% c("Reptiles and Amphibians")) %>% 
  filter(n_total>5) %>% 
  left_join(binom_adv_out_taxa[,c("pval","prop_per_taxa","outcome","adversity","taxa")], 
            by = c("outcome_category1"="outcome",
                   "adversity_category1"="adversity",
                   "taxa"="taxa")) %>% 
  ggplot(aes(y=outcome_category1, x=prop*100, fill = effect_sig, label = n_total)) + 
  geom_col(show.legend = F) + 
  scale_y_discrete(position = "right", limits=rev) + 
  scale_x_continuous(limits = c(0,115), breaks = c(0,25,50,75,100)) + 
  labs(x="", y="") + 
  facet_grid(taxa ~ adversity_category1, scales = "free", switch = "y") +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(8,"Dark2")[c(3,1)])) +
  ela_theme + 
  theme(axis.text.y = element_text(face="bold"),
        legend.title = element_blank(),
        strip.background = element_rect(color = "white"),
        strip.placement = "inside", 
        strip.text = element_text(face="bold", size = 12)) + 
  geom_vline(aes(xintercept = 100*(prop_per_taxa)), 
             color = "grey90", lty = 3, size = 1) +
  annotate("segment",x = 1,xend = 112, y=2.6, yend=2.6, size = 1.5) + 
  ggrepel::geom_text_repel(aes(vjust=0.3),nudge_x=115, segment.color=NA,
                           data = adv_out_taxa[!duplicated(adv_out_taxa[,c("outcome_category1","adversity_category1", "taxa")]),] %>% 
                             filter(taxa %in% c("Reptiles and Amphibians")) %>% 
                             filter(n_total>5))
ggsave(supp_fig_2ii, filename = "~/Desktop/supp_fig_2ii.pdf", device = "pdf", width = 8, height = 1.5) #height = 2.4
#
supp_fig_2iii <- adv_out_taxa %>% 
  filter(taxa %in% c("Fishes")) %>% 
  filter(n_total>5) %>% 
  left_join(binom_adv_out_taxa[,c("pval","prop_per_taxa","outcome","adversity","taxa")], 
            by = c("outcome_category1"="outcome",
                   "adversity_category1"="adversity",
                   "taxa"="taxa")) %>% 
  ggplot(aes(y=outcome_category1, x=prop*100, fill = effect_sig, label = n_total)) + 
  geom_col(show.legend = F) + 
  scale_y_discrete(position = "right", limits=rev) + 
  scale_x_continuous(limits = c(0,115), breaks = c(0,25,50,75,100)) + 
  labs(x="", y="") + 
  facet_grid(taxa ~ adversity_category1, scales = "free", switch = "y") +
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(8,"Dark2")[c(3,1)])) +
  ela_theme + 
  theme(axis.text.y = element_text(face="bold"),
        legend.title = element_blank(),
        strip.background = element_rect(color = "white"),
        strip.placement = "inside", 
        strip.text = element_text(face="bold", size = 12)) + 
  geom_vline(aes(xintercept = 100*(prop_per_taxa)), 
             color = "grey90", lty = 3, size = 1) +
  annotate("segment",x = 1,xend = 112, y=4.7, yend=4.7, size = 1.5) + 
  ggrepel::geom_text_repel(aes(vjust=0.3),nudge_x=115, segment.color=NA,
                           data = adv_out_taxa[!duplicated(adv_out_taxa[,c("outcome_category1","adversity_category1", "taxa")]),] %>% 
                             filter(taxa %in% c("Fishes")) %>% 
                             filter(n_total>5))
ggsave(supp_fig_2iii, filename = "~/Desktop/supp_fig_2iii.pdf", device = "pdf", width = 8, height = 2)
# 


### Supp Fig 3a) Adversity x adversity life stage x taxa ------------
table(ela$earliest_adversity,ela$adversity_life_stage_category1)
adv_ls_taxa_new <- ela[,c("title","adversity_category1","effect_sig","adversity_life_stage_category1","taxa","earliest_adversity")] %>% 
  group_by(earliest_adversity, effect_sig, adversity_category1, taxa) %>% 
  summarise(n=n()) %>% 
  group_by(earliest_adversity, adversity_category1, taxa) %>% 
  mutate(prop=n/sum(n), n_total=sum(n)) %>% 
  filter(n_total>5)

# Stats - enrichment test for sig. 
binom_adv_ls_taxa_out_new <- do.call(rbind, lapply(unique(adv_ls_taxa_new$taxa), function(y){
  new_df <- adv_ls_taxa_new[adv_ls_taxa_new$taxa==y & adv_ls_taxa_new$effect_sig=="Significant",]
  do.call(rbind, lapply(1:nrow(new_df), function(x){ 
    prop = dim(ela[ela$taxa==y & ela$effect_sig == "Significant",])[1]/dim(ela[ela$taxa==y,])[1]
    binom_out<-binom.test(x = new_df[x,]$n, 
                          n = new_df[x,]$n_total, 
                          p = prop)
    data.frame(prop=binom_out$estimate, 
               pval=binom_out$p.value,
               n=new_df[x,]$n,
               n_total=new_df[x,]$n_total,
               lifestage=new_df[x,]$earliest_adversity, 
               adversity=new_df[x,]$adversity_category1,
               taxa=new_df[x,]$taxa,
               prop_per_taxa=prop)
  }))
}))
binom_adv_ls_taxa_out_new$p_adj <- p.adjust(binom_adv_ls_taxa_out_new$pval,method = "BH")
#write.table(binom_adv_ls_taxa_out_new,file = "~/Desktop/binom_adv_ls_taxa_out_new.csv",sep = ",",col.names = T,row.names = F)

# Plots 
tmp <- adv_ls_taxa_new %>% 
  left_join(binom_adv_ls_taxa_out_new[,c("pval","prop_per_taxa","lifestage","adversity","taxa")], 
            by = c("earliest_adversity"="lifestage",
                   "adversity_category1"="adversity",
                   "taxa"="taxa"))
tmp[tmp$prop==1 & tmp$n_total>5,]$prop_per_taxa <- unique(tmp[tmp$taxa=="Reptiles and Amphibians" & 
                                                                !is.na(tmp$prop_per_taxa),]$prop_per_taxa)
supp_fig_3a_new <- tmp %>%
  ggplot(aes(y=adversity_category1, x=prop*100, fill = effect_sig, label = n_total)) + 
  geom_col(show.legend = F) + 
  scale_y_discrete(position = "right") + 
  scale_x_continuous(limits = c(0,115), breaks = c(0,25,50,75,100)) + 
  labs(x="Percent comparisons significant", y="Adversity type\n") + 
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(8,"Dark2")[c(3,1)])) +
  facet_grid(taxa ~ earliest_adversity, switch="y") +
  ela_theme + 
  theme(axis.text.y = element_text(size=10, angle = 0, face="bold"),
        axis.title.x = element_text(size=12, angle = 0, face="bold"),
        axis.title.y = element_text(size=12, angle = 0, face="bold"), 
        legend.title = element_blank(),
        strip.background = element_rect(color = "white"),
        strip.placement = "inside", 
        strip.text = element_text(face="bold", size = 12)) + 
  geom_vline(aes(xintercept = 100*(prop_per_taxa)), color = "grey90", lty = 3, size = 1) +
  annotate("segment",x = 1,xend = 99, y=4.75, yend=4.75, size = 1.5) + 
  ggrepel::geom_text_repel(aes(vjust=0.3),nudge_x=105, segment.color=NA,
                           data = adv_ls_taxa_new[!duplicated(adv_ls_taxa_new[,c("earliest_adversity",
                                                                                 "adversity_category1", "taxa")]),] %>% 
                             filter(n_total>5))
ggsave(supp_fig_3a_new, filename = "~/Desktop/supp_fig_3a_new.pdf", device = "pdf", width = 7, height = 5.5) # width = 9, height = 10
# 


### Supp Fig 3b) Outcome x outcome life stage x taxa ------------
table(ela$earliest_outcome,ela$outcome_life_stage_category1)
out_ls_taxa_new <- ela[,c("title","outcome_category1","effect_sig","outcome_life_stage_category1","taxa","earliest_outcome")] %>% 
  group_by(earliest_outcome, effect_sig, outcome_category1, taxa) %>% 
  summarise(n=n()) %>% 
  group_by(earliest_outcome, outcome_category1, taxa) %>% 
  mutate(prop=n/sum(n), n_total=sum(n)) %>% 
  filter(n_total>5)

# Stats - enrichment test for sig. 
binom_out_ls_taxa_out_new <- do.call(rbind, lapply(unique(out_ls_taxa_new$taxa), function(y){
  new_df <- out_ls_taxa_new[out_ls_taxa_new$taxa==y & out_ls_taxa_new$effect_sig=="Significant",]
  do.call(rbind, lapply(1:nrow(new_df), function(x){ 
    prop = dim(ela[ela$taxa==y & ela$effect_sig == "Significant",])[1]/dim(ela[ela$taxa==y,])[1]
    binom_out<-binom.test(x = new_df[x,]$n, 
                          n = new_df[x,]$n_total, 
                          p = prop)
    data.frame(prop=binom_out$estimate, 
               pval=binom_out$p.value,
               n=new_df[x,]$n,
               n_total=new_df[x,]$n_total,
               lifestage=new_df[x,]$earliest_outcome, 
               outcome=new_df[x,]$outcome_category1,
               taxa=new_df[x,]$taxa,
               prop_per_taxa=prop)
  }))
}))
binom_out_ls_taxa_out_new$p_adj <- p.adjust(binom_out_ls_taxa_out_new$pval,method = "BH")
#write.table(binom_out_ls_taxa_out_new,file = "~/Desktop/binom_out_ls_taxa_out_new.csv",sep = ",",col.names = T,row.names = F)

# Plots 
tmp1 <- out_ls_taxa_new %>% 
  left_join(binom_out_ls_taxa_out_new[,c("p_adj","pval","prop_per_taxa","lifestage","outcome","taxa")], 
            by = c("earliest_outcome"="lifestage",
                   "outcome_category1"="outcome",
                   "taxa"="taxa"))
tmp1[tmp1$prop==1 & tmp1$n_total>5,]$prop_per_taxa <- unique(tmp1[tmp1$taxa=="Mammals" & 
                                                                    !is.na(tmp1$prop_per_taxa),]$prop_per_taxa)

supp_fig_3b_new <- tmp1 %>%
  ggplot(aes(y=outcome_category1, x=prop*100, fill = effect_sig, label = n_total)) + 
  geom_col(show.legend = F) + 
  scale_y_discrete(position = "right") + 
  scale_x_continuous(limits = c(0,115), breaks = c(0,25,50,75,100)) + 
  labs(x="Percent comparisons significant", y="Outcome type\n") + 
  scale_fill_manual(values = c(RColorBrewer::brewer.pal(8,"Dark2")[c(3,1)])) +
  facet_grid(taxa ~ earliest_outcome, switch="y") +
  ela_theme + 
  theme(axis.text.y = element_text(size=10, angle = 0, face="bold"),
        axis.title.y = element_text(size=12, angle = 0, face="bold"),
        axis.title.x = element_text(size=12, angle = 0, face="bold"),
        legend.title = element_blank(),
        strip.background = element_rect(color = "white"),
        strip.placement = "inside", 
        strip.text = element_text(face="bold", size = 12)) + 
  geom_vline(aes(xintercept = 100*(prop_per_taxa)), color = "grey90", lty = 3, size = 1) +
  annotate("segment",x = 1,xend = 99, y=5.75, yend=5.75, size = 1.5) + 
  ggrepel::geom_text_repel(aes(vjust=0.3),nudge_x=105, segment.color=NA,
                           data = out_ls_taxa_new[!duplicated(out_ls_taxa_new[,c("earliest_outcome", "outcome_category1", "taxa")]),] %>% 
                             filter(n_total>5))
ggsave(supp_fig_3b_new, filename = "~/Desktop/supp_fig_3b_new.pdf", device = "pdf", width = 7, height = 5.5) #width = 9, height = 10
#


### Supp Fig XX: Publication journals and paper abstract descriptive statistics -------------
early_life <- read.csv("~/Downloads/early_life.csv", sep = ",", header=T)
early_life <- early_life[,c("Publication.Type", "Article.Title", "Source.Title", "Publication.Year", "Abstract")]

development <- read.csv("~/Downloads/development.csv", sep = ",", header=T)
development <- development[,c("Publication.Type", "Article.Title", "Source.Title", "Publication.Year", "Abstract")]

cohort <- read.csv("~/Downloads/cohort.csv", sep = ",", header=T)
cohort <- cohort[,c("Publication.Type", "Article.Title", "Source.Title", "Publication.Year", "Abstract")]

parental <- read.csv("~/Downloads/parental.csv", sep = ",", header=T)
parental <- parental[,c("Publication.Type", "Article.Title", "Source.Title", "Publication.Year", "Abstract")]

pubs_info <- rbind(early_life, development, cohort, parental)
pubs_info <- pubs_info[!duplicated(pubs_info$Article.Title, pubs_info$Source.Title, pubs_info$Publication.Year),] # rm duplicates
rm(early_life, development, cohort, parental)
dim(pubs_info)

## Keep only the papers in the final dataset - uses fuzzy matching to match papers b/w ela and pubs dfs 
library(fuzzyjoin)
# set title columns to be the same format 
pubs_info$lower_title <- tolower(pubs_info$Article.Title)
ela$lower_title <- tolower(ela$title)

# how many unique papers are in the final dataset? 
dim(ela[!duplicated(ela$lower_title),c("lower_title","name")])

# Fuzzy match by title 
pubs_info <- stringdist_join(ela[!duplicated(ela$lower_title),c("lower_title","name")], pubs_info, 
                             by = "lower_title",
                             mode = "left",
                             ignore_case = FALSE, 
                             method = "jw", 
                             max_dist = 1,
                             distance_col = "dist") %>%
  group_by(lower_title.x) %>%
  slice_min(order_by = dist, n = 1)
dim(pubs_info)

# Number of publications by year 
pubs_info$lower_title.x <- NULL
pubs_info$dist <- NULL
pubs_info$lower_title.y <- NULL
head(pubs_info,2)
pubs_info %>% 
  filter(!is.na(Publication.Year)) %>% 
  ggplot(aes(x = Publication.Year)) + 
  geom_histogram(color = "black", fill = "grey70") + 
  ela_theme + 
  labs(x = "Publication Year", y = "Count")

# Number of papers from each journal 
pubs_info[pubs_info$Source.Title == "PROCEEDINGS OF THE NATIONAL ACADEMY OF SCIENCES OF THE UNITED STATES OF AMERICA",]$Source.Title <- "PROC. OF THE NATIONAL ACADEMY OF SCIENCES OF THE USA" # Make PNAS title shorter 
pubs_info[pubs_info$Source.Title == "PHILOSOPHICAL TRANSACTIONS OF THE ROYAL SOCIETY B-BIOLOGICAL SCIENCES",]$Source.Title <- "PHIL. TRANS. OF THE ROYAL SOCIETY B-BIOLOGICAL SCIENCES" # Make Phil Trans title shorter 
n_papers_per_journal <- pubs_info %>% 
  group_by(Source.Title) %>% 
  summarise(n = n()) %>% 
  filter(!n == 1) %>% 
  ggplot(aes(x = n, y = Source.Title)) + 
  scale_y_discrete(limits = rev) + 
  geom_col(color = "black", fill = "grey70") + 
  ela_theme + 
  geom_text(aes(label = n), hjust = -1) + 
  labs(x = "Count", y = "Journal Title")
ggsave(n_papers_per_journal, filename = "~/Desktop/n_papers_per_journal.pdf", device = "pdf", width = 14, height = 16)

# Top words in journal titles - that relay info about the field of biology
top_words_titles <- do.call(rbind, lapply(1:nrow(pubs_info), function(x) { 
  df <- as.data.frame(str_split(string = pubs_info$Source.Title[x], pattern = " "))
  colnames(df) <- "word"
  return(df)})) %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% c("OF", "THE", "AND", "ONE", "&", "IN", "JOURNAL", 
                      "UNITED", "STATES", "ROYAL", "PROC.", "AMERICA", "ACADEMY", #PROCEEDINGS
                      "LETTERS", "ELIFE", "PLOS", "AMERICAN", "NATIONAL", "GENERAL", 
                      "COMMUNICATIONS", "B-BIOLOGICAL", "SOCIETY", "FRONTIERS", 
                      "TRANSACTIONS", "TRANS.", "PHILOSPHICAL", "PHIL.", "USA")) %>% 
  filter(n > 1) %>% 
  mutate(word = tolower(word)) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col(color = "black", fill = "grey70") +
  coord_flip() +
  labs(x = "Top words in journal titles \n", y = "\n Count ") +
  geom_text(aes(label = n), hjust = -1) + 
  ela_theme
ggsave(top_words_titles, filename = "~/Desktop/top_words_ELAtitles.pdf", device = "pdf", width = 12, height = 20)

# Top words in abstracts 
library(stopwords)
fig_tmp <- do.call(rbind, lapply(1:nrow(pubs_info), function(x) { 
  df <- as.data.frame(str_split(string = gsub("\\:","", 
                                              gsub("\\/","",
                                                   gsub("\\)","",
                                                        gsub("\\(","",
                                                             gsub("(C)", "", 
                                                                  gsub("-", "", 
                                                                       gsub("\\;", "", 
                                                                            gsub(",", "", 
                                                                                 gsub("\\.", "", 
                                                                                      pubs_info$Abstract[x]))))))))), 
                                pattern = " "))
  colnames(df) <- "word"
  df$word <- tolower(df$word)
  return(df)
})) %>% 
  count(word, sort = TRUE) %>% 
  mutate(word == tolower(word)) %>% 
  filter(!word %in% stopwords("en")) %>% 
  filter(!word %in% c("can", "may", "however", "found","also","whether","suggest","show")) %>% 
  filter(n > 50) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col(color = "black", fill = "grey70") +
  coord_flip() +
  labs(x = "Top words in abstracts \n", y = "\n Count ") +
  geom_text(aes(label = n), hjust = -1) + 
  ela_theme
ggsave(fig_tmp, filename = "~/Desktop/top_words_ELAabstracts.pdf", device = "pdf", width = 14, height = 20)
#


### Observational vs. experimental significance -----------
ex_df <- data.frame(observation = c(nrow(subset(ela, subset = study_type == "observation" & effect_sig == "Significant")), 
                                    nrow(subset(ela, subset = study_type == "observation" & effect_sig == "Not significant"))), 
                    experiment = c(nrow(subset(ela, subset = study_type == "experiment" & effect_sig == "Significant")), 
                                   nrow(subset(ela, subset = study_type == "experiment" & effect_sig == "Not significant"))), 
                    row.names = c("Significant", "Not Significant"))
fisher.test(ex_df)
ex_df
chisq.test(ex_df)$expected
mosaicplot(ex_df, color = TRUE)
log2(1.224)
rm(ex_df)
#
