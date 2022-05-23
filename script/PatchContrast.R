pacman::p_load(tidyverse, foreach)

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
  mutate( TRT = factor(TRT, levels = c("PBG40", "PBG20", "SLG", "WAG")), 
          TRT = recode(TRT, 
                       "PBG20" = "2 burns/yr", 
                       "PBG40" = '1 burn/yr',
                       "SLG" = "Continuous", 
                       "WAG" = "Rotational") )

# Check distributions
veg_long <- 
  veg_all %>%
    pivot_longer(c(Bare:LitterDepth), 
                 names_to = 'response', 
                 values_to = 'values'
                 ) 

veg_long %>% 
ggplot(aes(x=log(values+1))) + theme_bw(16) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=0.1,
                 colour="black",
                 fill="lightgreen") + 
  facet_wrap(~response, scales = 'free')

# Patch contrast

  #over time
contrastVOR <- tibble() 

foreach(r=1:length(unique(veg_long$response))) %:%
foreach(y=1:length(unique(veg_long$year))) %:%
foreach(p=1:length(unique(veg_long$pasture))) %do% {
  d = filter(veg_long, 
             response == unique(veg_long$response)[r],
             year == unique(veg_long$year)[y], 
             pasture == unique(veg_long$pasture)[p])
  lme4::lmer(log(values + 1) ~ 
               (1|patch), 
           data= d,
           REML = FALSE) %>% 
    lme4::VarCorr(. ) %>%
    as_tibble() %>%
    mutate(response = unique(d$response),
           treatment = unique(d$TRT), 
           pasture = unique(d$pasture), 
           year = unique(d$year)) %>%
    rename(term = grp, 
           variance = sdcor) %>%
    select(response, treatment, pasture, year, term, variance) %>%
    bind_rows(contrastVOR) -> contrastVOR
}

# save(contrastVOR, file = './data/contrastVOR.Rdata')

contrastVOR %>%
  pivot_wider(names_from = term, 
              values_from = variance) %>%
  # mutate(sum = patch + Residual, 
  #        patch = patch / sum) %>%
  group_by(response, treatment, year) %>%
  summarize(PatchMean = mean(patch), 
            PatchSE = sd(patch)/sqrt(n()), 
            .groups = 'drop') %>%
  mutate(response = recode(response, 
                           Bare = 'Bare ground cover', 
                           Dead = 'Litter cover', 
                           LitterDepth = 'Litter depth', 
                           Robel = 'Visual Obstruction Reading')) %>% 
  rename(Management = treatment) %>%
  ggplot(aes(x = year, 
             color = Management)) + theme_bw(14) + 
    geom_path(aes(y = PatchMean), 
              position = position_dodge(width = 0.1) ) + 
    geom_errorbar(aes(ymin = PatchMean - PatchSE, 
                      ymax = PatchMean + PatchSE), 
                  width = 0.2, 
                  position = position_dodge(width = 0.1)) + 
    geom_point(aes(y = PatchMean, 
                   fill = Management,
                   shape = Management), 
               size = 3, 
               color = "black", 
              position = position_dodge(width = 0.1) ) +
  labs(x = 'Year', 
       y = "Spatial heterogeneity") + 
  scale_shape_manual(values = c(21:24)) + 
  scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1:3,5)]) + 
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1:3,5)]) + 
    facet_wrap(~response, scales = "free_y") +
  theme(panel.grid.minor.x = element_blank())


# spatial vs. time 
contrastVOR2 <- tibble() 

  foreach(r=1:length(unique(veg_long$response))) %:%
   foreach(p=1:length(unique(veg_long$pasture))) %do% {
    d = filter(veg_long, 
               response == unique(veg_long$response)[r],
               pasture == unique(veg_long$pasture)[p])
    lme4::lmer(log(values + 1) ~ 
                 (1|year) + 
                 (1|patch), 
               data= d,
               REML = FALSE) %>% 
      lme4::VarCorr(. ) %>%
      as_tibble() %>%
      mutate(response = unique(d$response),
             treatment = unique(d$TRT), 
             pasture = unique(d$pasture) ) %>%
      rename(term = grp, 
             variance = sdcor) %>%
      select(response, treatment, pasture, term, variance) %>%
      bind_rows(contrastVOR2) -> contrastVOR2
  }

contrastVOR2 %>%
  filter(term != 'Residual') %>%
  pivot_wider(names_from = term, 
              values_from = variance) %>%
  mutate(response = recode(response, 
                           Bare = 'Bare ground cover', 
                           Dead = 'Litter cover', 
                           LitterDepth = 'Litter depth', 
                           Robel = 'Visual Obstruction Reading')) %>% 
  rename(Management = treatment) %>%
  ggplot(aes(x = patch,
             y = year, 
             fill = Management, 
             color = Management, 
             shape = Management)) + theme_bw(14) + 
  geom_smooth(method = 'lm') + 
  geom_point(size = 3, 
             color = "black" ) +
  labs(y = 'Temporal variability', 
       x = "Spatial heterogeneity") +
  scale_shape_manual(values = c(21:24)) + 
  scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1:3,5)]) + 
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1:3,5)]) + 
  facet_wrap(~response, scales = "free") 
