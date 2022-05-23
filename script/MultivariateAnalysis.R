pacman::p_load(tidyverse, vegan, RVAideMemoire)
source("https://raw.githubusercontent.com/devanmcg/IntroRangeR/master/11_IntroMultivariate/ordinationsGGplot.R")

# Load and arrange data 
veg_all <-
  read_csv('./data/CGRECVegCompCameron.csv') %>% 
    mutate(block = substr(transect, 1,1), 
           pasture = substr(transect, 2,3), 
           patch = substr(transect, 4,5), 
           subpatch = substr(transect, 6,6), 
           transect = paste(block, pasture, patch, subpatch, sep = "."), 
           pasture = paste(block, pasture, sep = "_")) %>%
    select(block, pasture, patch, subpatch, transect, 
           TRT, year, TSF, BurnSeason, 
           Bare:LitterDepth, 
           POPR:IntWoody) %>%
    mutate( TRT = factor(TRT, levels = c("PBG20", "PBG40", "SLG", "WAG")), 
            TRT = recode(TRT, 
                        "PBG20" = "2 burns/yr", 
                        "PBG40" = '1 burn/yr',
                        "SLG" = "Continuous", 
                        "WAG" = "Rotational") )
# Alternatively, with relative dominance
veg_ra <- 
  veg_all %>%
  rowwise() %>%
  mutate(TotalCover = sum(c_across(POPR:IntWoody)), 
         across(POPR:IntWoody, ~(./TotalCover))) %>%
  filter(TotalCover != 0.0) %>%
  ungroup() %>%
  select(-TotalCover) 

# Each variable by treatment over study timeframe 
veg_univ <-
  veg_ra %>%
    select(block, pasture, patch, subpatch, transect, 
           TRT, year, Bare:IntWoody) %>%
    pivot_longer(cols = c(Bare:IntWoody), 
                 names_to = "spp", 
                 values_to = "cover") %>%
    group_by(TRT, pasture, year, spp) %>%
      summarize(Mean = mean(cover), 
                .groups = 'drop') %>%
    group_by(TRT, year, spp) %>%
    summarize(CoverMean = mean(Mean), 
              CoverSE = sd(Mean)/sqrt(n()), 
              .groups = 'drop') %>%
    rename("Management" = "TRT") %>%
  # filter(! spp %in% c('IntC4', 'IntWoody')) %>%
    mutate(group = case_when(
      spp %in% c('Bare', 'Dead', 'Robel', 'LitterDepth') ~ "Structural", 
      spp %in% c('POPR', 'BRIN') ~ "Focal", 
      TRUE ~ "Functional" )) %>%
    mutate(status = case_when(
      substr(spp, 1,3) == "Nat" ~ "Native",
      substr(spp, 1,3) == "Int" ~ "Exotic",
      TRUE ~ 'NA' )) %>%
  mutate(spp = recode(spp,
    "Bare" = "Bare ground", 
    'BRIN' = "Bromus inermis", 
    'Dead' = "Litter cover", 
    'IntC3' = 'Cool-season\ngrasses', 
    'IntForb' = 'Forbs', 
    'Intleg' ="Legumes", 
    'LitterDepth' = 'Litter depth', 
    'NatC3' = 'Cool-season\ngrasses', 
    'IntC4' = 'Warm-season\ngrasses', 
    'NatC4' = 'Warm-season\ngrasses', 
    'NatForb' = 'Forbs', 
    'Natleg' = 'Legumes', 
    'NatWoody' = "Woody\nplants", 
    'IntWoody' = "Woody\nplants", 
    'POPR' = "Poa pratensis", 
    'Robel' = 'Robel height'  ))

# POPR & BRIN  
  veg_univ %>%
    filter(group == 'Focal') %>%
    ggplot(aes(x = year)) + theme_bw(16) +
      geom_line(aes(y = CoverMean, 
                    color = Management), 
                position = position_dodge(width = 0.3)) +
      geom_errorbar(aes(ymin = CoverMean - CoverSE, 
                        ymax = CoverMean + CoverSE, 
                        color = Management), 
                    width = 0.25,
                    position = position_dodge(width = 0.3)) +
      geom_point(aes(y = CoverMean, 
                     fill = Management, 
                     shape = Management),
                 position = position_dodge(width = 0.3)) +
    scale_shape_manual(values = c(21:24)) +
      facet_wrap(~spp, scales = "free_y") + 
    theme(strip.text = element_text(face = "italic"))
# FUnctional groups
  veg_univ %>%
    filter(group == 'Functional') %>%
    mutate(spp = factor(spp, levels = c("Forbs", "Legumes", 
                                        "Cool-season\ngrasses", 
                                        "Warm-season\ngrasses", 
                                        "Woody\nplants"))) %>% 
    ggplot(aes(x = year)) + theme_bw(16) +
    geom_line(aes(y = CoverMean, 
                  color = Management), 
              position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = CoverMean - CoverSE, 
                      ymax = CoverMean + CoverSE, 
                      color = Management), 
                  width = 0.25,
                  position = position_dodge(width = 0.3)) +
    geom_point(aes(y = CoverMean, 
                   fill = Management, 
                   shape = Management),
               position = position_dodge(width = 0.3)) +
    scale_shape_manual(values = c(21:24)) +
    facet_grid(status~spp, scales = "free_y") 
# Vegetation structure
  veg_univ %>%
    filter(group == 'Structural') %>%
    ggplot(aes(x = year)) + theme_bw(16) +
    geom_line(aes(y = CoverMean, 
                  color = Management), 
              position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = CoverMean - CoverSE, 
                      ymax = CoverMean + CoverSE, 
                      color = Management), 
                  width = 0.25,
                  position = position_dodge(width = 0.3)) +
    geom_point(aes(y = CoverMean, 
                   fill = Management, 
                   shape = Management),
               position = position_dodge(width = 0.3)) +
    scale_shape_manual(values = c(21:24)) +
    facet_wrap(~spp, scales = "free_y") 
  
# 
# Changes 
# 
  
veg_ch <-
  veg_ra  %>%   
    mutate(bookend = case_when(
      TRT == "Rotational" & year == '2018' ~ "Initial", 
      TRT != "Rotational" & year == '2017' ~ "Initial", 
      year == '2020' ~ "Final", 
      TRUE ~ 'NA'      )) %>% 
    filter(bookend %in% c("Initial", "Final")) %>%
  select(block, pasture, transect, 
         TRT, bookend, Bare:IntWoody) %>%
  pivot_longer(cols = c(Bare:IntWoody), 
               names_to = "var", 
               values_to = "value") %>%
    pivot_wider(names_from = bookend, 
                values_from = value) %>%
    mutate(diff = Final - Initial) %>%
  group_by(TRT, pasture, var) %>%
  summarize(change = mean(diff), 
            .groups = 'drop') %>%
  group_by(TRT, var) %>%
  summarize(ChangeMean = mean(change, na.rm = T), 
            ChangeSE = sd(change, na.rm = T)/sqrt(n()), 
            .groups = 'drop') %>%
  rename("Management" = "TRT") %>% 
    mutate(group = case_when(
      var %in% c('Bare', 'Dead', 'Robel', 'LitterDepth') ~ "Structural", 
      var %in% c('POPR', 'BRIN') ~ "Focal", 
      TRUE ~ "Functional" )) %>%
    mutate(status = case_when(
      substr(var, 1,3) == "Nat" ~ "Native",
      substr(var, 1,3) == "Int" ~ "Exotic",
      TRUE ~ 'NA' )) %>%
    mutate(var = recode(var,
                        "Bare" = "Bare ground", 
                        'BRIN' = "Bromus inermis", 
                        'Dead' = "Litter cover", 
                        'IntC3' = 'Cool-season\ngrasses', 
                        'IntForb' = 'Forbs', 
                        'Intleg' ="Legumes", 
                        'LitterDepth' = 'Litter depth', 
                        'NatC3' = 'Cool-season\ngrasses', 
                        'IntC4' = 'Warm-season\ngrasses', 
                        'NatC4' = 'Warm-season\ngrasses', 
                        'NatForb' = 'Forbs', 
                        'Natleg' = 'Legumes', 
                        'NatWoody' = "Woody\nplants", 
                        'IntWoody' = "Woody\nplants", 
                        'POPR' = "Poa pratensis", 
                        'Robel' = 'Robel height'  ))
# POPR & BRIN  
veg_ch %>%
  filter(group == 'Focal') %>%
  ggplot(aes(x = var)) + theme_bw(16) +
  geom_hline(yintercept = 0, lty = 3, col = "black") + 
  geom_errorbar(aes(ymin = ChangeMean - ChangeSE, 
                    ymax = ChangeMean + ChangeSE, 
                    color = Management), 
                width = 0.25,
                position = position_dodge(width = 0.3)) +
  geom_point(aes(y = ChangeMean, 
                 fill = Management, 
                 shape = Management),
             position = position_dodge(width = 0.3)) +
  scale_shape_manual(values = c(21:24)) +
  labs(x = "Species", 
       y = "Change over study\n(mean % ± s.e.)")
  theme(axis.text.x = element_text(face = "italic"))
# FUnctional groups
veg_ch %>%
  filter(group == 'Functional') %>%
  mutate(spp = factor(var, levels = c("Forbs", "Legumes", 
                                      "Cool-season\ngrasses", 
                                      "Warm-season\ngrasses", 
                                      "Woody\nplants"))) %>% 
  ggplot(aes(x = var)) + theme_bw(16) +
  geom_hline(yintercept = 0, lty = 3, col = "black") + 
  geom_errorbar(aes(ymin = ChangeMean - ChangeSE, 
                    ymax = ChangeMean + ChangeSE, 
                    color = Management), 
                width = 0.25,
                position = position_dodge(width = 0.3)) +
  geom_point(aes(y = ChangeMean, 
                 fill = Management, 
                 shape = Management),
             position = position_dodge(width = 0.3)) +
  scale_shape_manual(values = c(21:24)) +
  labs(x = "Plant functional group", 
       y = "Change over study\n(mean % ± s.e.)") +
  facet_wrap(~status, scales = "free_y") + 
theme(axis.text.x = element_text(face = "italic"))
# Vegetation structure
veg_ch %>%
  filter(group == 'Structural') %>%
  ggplot(aes(x = var)) + theme_bw(16) +
  geom_hline(yintercept = 0, lty = 3, col = "black") + 
  geom_errorbar(aes(ymin = ChangeMean - ChangeSE, 
                    ymax = ChangeMean + ChangeSE, 
                    color = Management), 
                width = 0.25,
                position = position_dodge(width = 0.3)) +
  geom_point(aes(y = ChangeMean, 
                 fill = Management, 
                 shape = Management),
             position = position_dodge(width = 0.3)) +
  scale_shape_manual(values = c(21:24)) +
  labs(x = "Structural component", 
       y = "Change over study\n(mean ± s.e.)")
theme(axis.text.x = element_text(face = "italic"))
  
# Multivariate analysis
  # Focus on first and final years (2018 & 2020)
    veg_d <- 
      veg_all %>%
      mutate(bookend = case_when(
              TRT == "Rotational" & year == '2018' ~ "Initial", 
              TRT != "Rotational" & year == '2017' ~ "Initial", 
              year == '2020' ~ "Final", 
              TRUE ~ 'NA'      )) %>% 
        filter(bookend %in% c("Initial", "Final")) %>%
      mutate(YrTrt = paste0(bookend,"_", TRT))

  # spp data only
    comm_d <- 
      veg_d %>%
      select(NatC3:IntWoody)  %>%
      select(-IntWoody, -IntC4) %>%
      replace(is.na(.), 0) 
    
    veg_m <- vegdist(comm_d, "bray")
    
 # ordination
    # Fit 
   veg_ord <- capscale(comm_d ~ 1, distance = 'bray', metaMDSdist = TRUE)
   # Test
     veg_fit <- envfit(veg_ord ~ YrTrt + Bare + Dead + Robel + LitterDepth + POPR + BRIN, 
                       veg_d, strata = veg_d$block, choices = c(1:3), 199)
     pw <- pairwise.factorfit(veg_ord, veg_d$YrTrt, 
                              perm = 999, p.method = "holm" )$p.value %>%
            round(., 2)
     bd <- betadisper(veg_m, veg_d$YrTrt, bias.adjust=TRUE  )
    TukeyHSD(bd)
    

  # Getting scores 
  veg_gg <- gg_ordiplot(veg_ord, groups = veg_d$YrTrt, 
                         plot=FALSE)
  
  veg_scores <- lst(species=scores(veg_ord, display = "species", choices = c(1:2)) %>%
                            as.data.frame %>%
                            as_tibble(rownames="group"), 
                    sites=scores(veg_ord, display = "sites", choices = c(1:2)) %>%
                      as_tibble() %>%
                      bind_cols(YrTrt = veg_d$YrTrt) %>%
                      separate(YrTrt, into=c('bookend', 'Trt'), sep = "_", remove = F) %>%
                      mutate(bookend = factor(bookend, levels = c("Initial", "Final"))), 
                    spiders = veg_gg$df_spiders %>% 
                           rename(MDS1 = x, MDS2 = y) %>%
                           as_tibble %>%
                           separate(Group, into=c('bookend', 'Trt'), sep = "_", remove = F) %>%
                      mutate(bookend = factor(bookend, levels = c("Initial", "Final"))), 
                    vectors = scores(veg_fit,  "vectors") %>%
                              as.data.frame %>% 
                              round(3) %>%
                              as_tibble(rownames="gradient"), 
                    groups = pw, 
                    beta = bd$distances)
# Plot ordination 
  
  ord_gg <- ggplot() + theme_bw(16) + 
    labs(x="MDS Axis 1", 
         y="MDS Axis 2") + 
    geom_vline(xintercept = 0, lty=3, color="darkgrey") +
    geom_hline(yintercept = 0, lty=3, color="darkgrey") +
    theme(panel.grid=element_blank())
  
  # overall
  ord_gg + geom_point(data=veg_scores$sites, 
                      aes(x=MDS1, y=MDS2,
                          fill = bookend, 
                          shape = Trt), 
                      size=2) +
    scale_shape_manual(values = c(21:24)) + 
    scale_fill_manual(values = c("lightblue", "blue"))
  
  # initial
  ord_gg + 
    geom_segment(data=veg_scores$spiders %>%
                   filter(bookend == "Initial"), 
                 aes(x=cntr.x, y=cntr.y,
                     xend=MDS1, yend=MDS2, 
                     color=Trt), 
                 alpha = 0.5,
                 size=1.2) + 
    geom_point(data=veg_scores$spiders %>%
                            filter(bookend == "Initial"), 
                      aes(x=MDS1, y=MDS2,
                          fill = Trt, 
                          shape = Trt), 
                      size=2) +
    geom_label(data=veg_scores$spiders %>%
                 filter(bookend == "Initial"),
               aes(x=cntr.x, y=cntr.y, 
                   label=Trt, 
                   color=Trt), 
               fontface="bold", size=4,
               label.size = 0, 
               label.r = unit(0.5, "lines")) +
    geom_label(data=veg_scores$species %>%
                    filter(! group %in% c("IntWoody", "IntC4")), 
               aes(x=MDS1*0.25, 
                   y=MDS2*0.5, 
                   label=group), 
               label.padding=unit(0.1,"lines"),
               label.size = 0, 
               fontface="bold", color="darkred") +
    scale_shape_manual(values = c(21:24)) +
    labs(title = "Initial composition") +
    theme(legend.position = 'none')
  # Final
  ord_gg + 
    geom_segment(data=veg_scores$spiders %>%
                   filter(bookend == "Final"), 
                 aes(x=cntr.x, y=cntr.y,
                     xend=MDS1, yend=MDS2, 
                     color=Trt), 
                 alpha = 0.5,
                 size=1.2) + 
    geom_point(data=veg_scores$spiders %>%
                 filter(bookend == "Final"), 
               aes(x=MDS1, y=MDS2,
                   fill = Trt, 
                   shape = Trt), 
               size=2) +
    geom_label(data=veg_scores$spiders %>%
                 filter(bookend == "Final"),
               aes(x=cntr.x, y=cntr.y, 
                   label=Trt, 
                   color=Trt), 
               fontface="bold", size=4,
               label.size = 0, 
               label.r = unit(0.5, "lines")) +
    geom_label(data=veg_scores$species %>%
                 filter(! group %in% c("IntWoody", "IntC4")), 
               aes(x=MDS1*0.25, 
                   y=MDS2*0.5, 
                   label=group), 
               label.padding=unit(0.1,"lines"),
               label.size = 0, 
               fontface="bold", color="darkred") +
    scale_shape_manual(values = c(21:24)) +
    labs(title = "Final composition") +
    theme(legend.position = 'none')

# Together, faceted 
  ord_gg + 
    geom_segment(data=veg_scores$spiders, 
                 aes(x=cntr.x, y=cntr.y,
                     xend=MDS1, yend=MDS2, 
                     color=Trt), 
                 alpha = 0.5,
                 size=1.2) + 
    geom_point(data=veg_scores$spiders, 
               aes(x=MDS1, y=MDS2,
                   fill = Trt, 
                   shape = Trt), 
               size=2) +
    geom_label(data=veg_scores$spiders ,
               aes(x=cntr.x, y=cntr.y, 
                   label=Trt, 
                   color=Trt), 
               fontface="bold", size=4,
               label.size = 0, 
               label.r = unit(0.5, "lines")) +
    geom_label(data=veg_scores$species %>%
                 filter(! group %in% c("IntWoody", "IntC4")), 
               aes(x=MDS1*0.225, 
                   y=MDS2*0.225, 
                   label=group), 
               label.padding=unit(0.1,"lines"),
               label.size = 0, 
               fontface="bold", color="darkred") +
    scale_shape_manual(values = c(21:24)) +
facet_wrap(~bookend) + 
    theme(legend.position = 'none')
  
# Trajectory analysis  
veg_scores$sites %>%
  bind_cols(pasture = veg_d$pasture, .) %>%
  pivot_longer(cols = c(MDS1, MDS2), 
               names_to = 'axis', 
               values_to = 'score') %>%
  group_by(Trt, pasture, bookend, axis) %>%
    summarize(centroid = mean(score), 
              error = sd(score)/sqrt(n()),
              .groups = 'drop') %>%
  select(-error) %>%
  pivot_wider(names_from = c(bookend, axis), 
              values_from = centroid) %>%
  ggplot() + theme_bw(16) + 
  geom_segment(aes(x=Initial_MDS1, y=Initial_MDS2,
                   xend=Final_MDS1, yend=Final_MDS2, 
                   color=Trt), 
               arrow = arrow(length = unit(0.03, "npc")),
                 size=1.2)  +
  geom_label(data=veg_scores$species %>%
               filter(! group %in% c("IntWoody", "IntC4")), 
             aes(x=MDS1*0.5, 
                 y=MDS2*0.5, 
                 label=group), 
             label.padding=unit(0.1,"lines"),
             label.size = 0, 
             fontface="bold", color="darkred") +
  coord_cartesian(xlim = c(-0.75, 2))  +   
    labs(x="MDS Axis 1", 
         y="MDS Axis 2") + 
    geom_vline(xintercept = 0, lty=3, color="darkgrey") +
    geom_hline(yintercept = 0, lty=3, color="darkgrey") +
    theme(panel.grid=element_blank())
  
# Plotting changes in beta dispersion
  bind_cols(pasture = veg_d$pasture,
            YrTrt = veg_d$YrTrt, 
            disp = veg_scores$beta) %>% 
    separate(YrTrt, into = c("bookend", "management"), sep = '_') %>%
    group_by(management, bookend) %>%
    summarize(DispMean = mean(disp), 
              DispSE = sd(disp)/sqrt(n()), 
              .groups = 'drop') %>%
    mutate(bookend = factor(bookend, levels = c("Initial", "Final"))) %>%
    ggplot(aes(x = bookend)) + theme_bw(16) + 
    geom_line(aes(y = DispMean, 
                  color = management, 
                  group = management), 
              position = position_dodge(width = 0.1)) +
    geom_errorbar(aes(ymin = DispMean - DispSE, 
                      ymax = DispMean + DispSE, 
                      color = management), 
                  width = 0.25, 
                  size = 1.1,
                  position = position_dodge(width = 0.1)) +
    geom_point(aes(y = DispMean, 
                   shape = management, 
                   fill = management), 
               size = 3, stroke = 1.25,
               position = position_dodge(width = 0.1)) + 
    coord_cartesian(xlim = c(1.4,1.6)) + 
    labs(x = "Data period", 
         y = "Within-group diversity") +
    scale_shape_manual(values = c(21:24)) 
  