library(ggplot2)

setwd("C:/Users/elewis6.stu/OneDrive - UBC/Desktop/DS_Files/Results_Final")
merged <- read.csv("merged.csv")



ggplot(data = merged) +
  geom_point(aes(x = Mass, y = kde.ml,)) +
  geom_smooth(aes(y = kde.ml, x = Mass), se = F, method = "lm") +
  geom_point(aes(x = Mass, y = akde.ml,  )) +
  geom_smooth(aes(y = akde.ml, x = Mass), se = F, method = "lm",col = "red" ) +
  scale_y_log10() +
  scale_x_log10()

ggplot(data = merged) +
  geom_point(aes(x = akde.ml, y = tau_p,)) +
  geom_smooth(aes(x = akde.ml, y = tau_p), se = F, method = "lm") +
  scale_y_log10() +
  scale_x_log10()

ggplot(data = merged) +
  geom_point(aes(x = tau_p, y = N_area,)) +
  geom_smooth(aes(x = tau_p, y = N_area,), se = F, method = "lm") +
  scale_y_log10() +
  scale_x_log10()

ggplot(data = merged) +
  geom_point(aes(x = Mass, y = N_area,)) +
  geom_smooth(aes(x = Mass, y = N_area,), se = F, method = "lm") +
  scale_y_log10() +
  scale_x_log10()

ggplot(data = merged) +
  geom_point(aes(y = (kde.ml)/(akde.ml), x = Mass, )) +
  geom_smooth(aes(y = (kde.ml)/(akde.ml), x = Mass, ), se = F, method = "lm") + 
  scale_x_log10() +
  scale_y_log10()

ggplot(data = merged) +
  geom_point(aes(x = Mass, y = Duration,)) +
  geom_smooth(aes(x = Mass, y = Duration,), se = F, method = "lm") +
  scale_y_log10() +
  scale_x_log10()

ggplot(data = merged) +
  geom_point(aes(x = Year, y = Mass)) +
  geom_smooth(aes(x = Year, y = Mass), se = F, method = "lm") +
  scale_y_log10()

ggplot(data = merged) +
  geom_point(aes(x = Year, y = Duration)) +
  geom_smooth(aes(x = Year, y = Duration), se = F, method = "lm") +
  scale_y_log10()

ggplot(data = merged) +
  geom_point(aes(x = Year, y = Frequency)) +
  geom_smooth(aes(x = Year, y = Frequency), se = F, method = "lm") +
  scale_y_log10()

ggplot(data = merged) +
  geom_point(aes(y = (kde.ml)/(akde.ml), x = Year, )) +
  geom_smooth(aes(y = (kde.ml)/(akde.ml), x = Year, ), se = F, method = "lm") + 
  scale_y_log10() 
  
ggplot(data = merged) +
  geom_point(aes(y = (tau_p)/(Frequency), x = Mass/1000)) +
  geom_smooth(aes(y = (tau_p)/(Frequency), x = Mass/1000), se = F, method = "lm") + 
  scale_y_log10() +
  scale_x_log10()

ggplot(data = merged) +
  geom_point(aes(x = (tau_p)/(Frequency), y = (kde.ml)/(akde.ml))) +
  geom_smooth(aes( x = (tau_p)/(Frequency), y = (kde.ml)/(akde.ml)), se = F, method = "lm") + 
  scale_y_log10() +
  scale_x_log10()

ggplot(data = merged) +
  geom_point(aes(y = tau_p, x = Year)) +
  geom_smooth(aes(y = (tau_p), x = Year), se = F, method = "lm") + 
  scale_y_log10()

ggplot(data = merged) +
  geom_point(aes(y = (Frequency)/(tau_p), x = Mass)) +
  geom_smooth(aes(y = (Frequency)/(tau_p), x = Mass), se = F, method = "lm") + 
  scale_y_log10() + 
  scale_x_log10()

ggplot(data = merged) +
  geom_point(aes(y = Frequency, x = Mass)) +
  geom_smooth(aes(y = Frequency, x = Mass), se = F, method = "lm") +
  scale_y_log10()+ 
  scale_x_log10()

ggplot(data = merged) +
  geom_point(aes(x = Mass, y = tau_p,)) +
  geom_smooth(aes(x = Mass, y = tau_p,), se = F, method = "lm") +
  scale_y_log10() +
  scale_x_log10()


ggplot(data = merged) +
  geom_point(aes(x = Mass, y = tau_v,)) +
  geom_smooth(aes(x = Mass, y = tau_v,), se = F, method = "lm") +
  scale_y_log10() +
  scale_x_log10()

ggplot(data = merged) +
  geom_point(aes(x = Mass, y = akde.ml,col = Trophic.Level)) +
  geom_smooth(aes(x = Mass, y = akde.ml, col = Trophic.Level,), se = F, method = "lm") +
  scale_y_log10() +
  scale_x_log10()

  


   
  
             