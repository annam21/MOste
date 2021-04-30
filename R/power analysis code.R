# Power Analysis from Anna's Package
rslt <- ste_pwr_sim(
  N = 140,
  study_area = 96161402,
  ncam = 36,
  nocc = c(2000, 50000, 130000),
  cam_area = 73,
  niter = 20
)
rslt

# Visualize
plot_pwr_sim(rslt)

# Visualize CVs
plot_pwr_cv(rslt)


#####
# Checking CV when you only have 1 possible number of cameras
cv <- rslt %>% 
  group_by(TrueN,
           NCam, 
           NOcc, 
           StudyArea, 
           CamArea) %>% 
  summarize(mean.estN = mean(EstN, na.rm = T), 
            sd.estN = sd(EstN, na.rm = T)) %>% 
  mutate(cv = sd.estN/mean.estN * 100,
         NOcc = as.factor(NOcc))
                                                                                                   100, NOcc = as.factor(NOcc))