library(pacman)
p_load(
  tidyverse,
  readxl
)

# Inladen EIB tabel A.1
df <- read_excel("afstand_depositie/stikstofdepositie_eib.xlsx", skip = 3)
df <- df[1:10, 1:11]

# Depositie per woning berekenen
df$molwoning <- df$`800` / 800
sprintf("Berekende depositie voor %f m per woning op basis van 800 woningen: %f mol/woning", round(df$afstand[1]), df$molwoning[1])
sprintf("Controle met 400 woningen. Rapport %f mol/woning; Berekend %f mol/woning", df$`400`[1], df$molwoning[1] * 400)
sprintf("Controle met 10 woningen. Rapport %f mol/woning; Berekend %f mol/woning", df$`10`[1], df$molwoning[1] * 10)



# Schatten afstands-depositie relatie

df_fit <- filter(df, molwoning > 0.0000125) # platte observaties weglaten

# Vermoedelijke functie: log(y) = a + b * log(x)
model <- lm(log(molwoning) ~ log(afstand), data = df_fit)
summary(model)

# Visuele controle
df_fit %>%
  mutate(pred = exp(predict(model))) %>%
  ggplot(aes(x = afstand, y = molwoning)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red") +
  scale_x_log10() +
  scale_y_log10()

# Idem maar nu ook met alle observaties
df %>%
  mutate(pred = exp(predict(model, newdata = df))) %>%
  ggplot(aes(x = afstand, y = molwoning)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red") +
  scale_x_log10() +
  scale_y_log10()

# Estimates in functie:
sprintf(
  "y = exp(%f + %f * log(x))",
  coef(model)[1], coef(model)[2]
)

# Controle door terug te rekenen vanaf afstand en woningen
df_ctrl <- data.frame(afstand = df$afstand)
df_ctrl$molwoning <- exp(predict(model, newdata = df_ctrl))