library(gamlss)
library(ggplot2)
library(gamlss.ggplots)
library(dplyr)

ggplot(data = abdom) +
  geom_point(aes(x = x, y = y)) +
  stat_smooth(aes(x = x, y = y), method = "lm", color = "#7EBE91") +
  labs(title = "Abdominal Circumfrence in Fetus Ultrasound Scans",
       x = "Gestational age",
       y = "Abdominal circumfrence")

fpmodel <- gamlss(y~bfp(x,c(1,0.5)), data = abdom)

df <- abdom %>% 
  mutate(fp = fitted(fp_model))

ggplot(data = df) +
  geom_point(aes(x = x, y = y)) +
  stat_smooth(aes(x = x, y = y), method = "lm", color = "#7EBE91", linewidth = 1.5) +
  geom_line(aes(x = x, y = fp), color = "#b17ebe", linewidth = 1.5) +
  labs(title = "Abdominal Circumfrence in Fetus Ultrasound Scans",
       x = "Gestational age",
       y = "Abdominal circumfrence")
