summary(liga)
library(tidyverse)
library(naniar)   # valores ausentes
library(janitor)  # duplicados
library(gt)       # tablas

liga <- read_csv("season-2526.csv")
#FIGURA1
colSums(is.na(liga))
vis_miss(liga) + labs(title = NULL)

# 2.2 Incompletas y estructura
sum(complete.cases(liga))
sum(complete.cases(select(liga, -Referee)))
table(liga$HomeTeam); table(liga$AwayTeam)       # 19 y 19 cada uno
liga |> count(HomeTeam, AwayTeam) |> filter(n > 1)

# 2.3 Inconsistencias
liga |> mutate(calc = case_when(FTHG > FTAG ~ "H", FTHG < FTAG ~ "A", TRUE ~ "D")) |>
  filter(calc != FTR)
liga |> filter(HTHG > FTHG | HTAG > FTAG)
liga |> filter(HST > HS | AST > AS)
liga |> filter(FTHG > HST | FTAG > AST) |>
  select(Date, HomeTeam, AwayTeam, FTHG, FTAG, HST, AST)

# 2.4 Duplicados
get_dupes(liga)
get_dupes(liga, HomeTeam, AwayTeam)

# 2.5 Atípicos (IQR) + Figura 2
num <- liga |> select(FTHG:AR, -FTR, -HTR, -Referee)
num |> pivot_longer(everything()) |>
  group_by(name) |>
  summarise(Q1 = quantile(value, .25), Q3 = quantile(value, .75),
            lim_sup = Q3 + 1.5 * (Q3 - Q1),
            n_atipicos = sum(value > lim_sup | value < Q1 - 1.5 * (Q3 - Q1)))

num |> pivot_longer(everything()) |>
  ggplot(aes(x = "", y = value)) + geom_boxplot(fill = "#9ECAE1") +
  facet_wrap(~name, scales = "free_y", ncol = 6) +
  labs(x = NULL, y = NULL) + theme_minimal()

#FIGURA 2 Nombres legibles para cada variable (en el orden en que se mostrarán)
etiquetas <- c(
  FTHG = "Goles local (final)",          FTAG = "Goles visitante (final)",
  HTHG = "Goles local (descanso)",       HTAG = "Goles visitante (descanso)",
  HS   = "Tiros local",                  AS   = "Tiros visitante",
  HST  = "Tiros a puerta local",         AST  = "Tiros a puerta visitante",
  HC   = "Córners local",                AC   = "Córners visitante",
  HF   = "Faltas local",                 AF   = "Faltas visitante",
  HY   = "Amarillas local",              AY   = "Amarillas visitante",
  HR   = "Rojas local",                  AR   = "Rojas visitante"
)

liga |>
  select(all_of(names(etiquetas))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  mutate(variable = factor(variable, levels = names(etiquetas))) |>
  ggplot(aes(x = "", y = valor)) +
  geom_boxplot(fill = "#9ECAE1", outlier.colour = "#D7301F") +
  facet_wrap(~ variable, scales = "free_y", ncol = 4,
             labeller = as_labeller(etiquetas)) +
  labs(x = NULL, y = "Valor") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 9))

ggsave("figura2_boxplots.png", width = 10, height = 9, dpi = 300)

# 2.6 Limpieza
liga_limpia <- liga |>
  select(-Referee) |>
  mutate(Date = as.Date(Date),
         across(c(HomeTeam, AwayTeam), as.factor),
         across(c(FTR, HTR), ~ factor(.x, levels = c("H", "D", "A"),
                                      labels = c("Local", "Empate", "Visitante"))))
dim(liga_limpia)   # 380 21



install.packages("psych")
summary(liga)
psych::describe(liga)

install.packages("modeest")
library(modeest)

# Calcular la moda para datos discretos
mlv(liga$FTAG, method = "mfv")

cv(liga)

library(tidyverse)
library(moments)    # asimetría
library(gt)
library(patchwork)  # combinar gráficos

liga_limpia <- liga |>
  select(-Referee) |>
  mutate(across(c(FTR, HTR), ~ factor(.x, levels = c("H","D","A"),
                                      labels = c("Victoria local","Empate","Victoria visitante"))))

etiquetas <- c(
  FTHG = "Goles local (final)",          FTAG = "Goles visitante (final)",
  HTHG = "Goles local (descanso)",       HTAG = "Goles visitante (descanso)",
  HS   = "Tiros local",                  AS   = "Tiros visitante",
  HST  = "Tiros a puerta local",         AST  = "Tiros a puerta visitante",
  HC   = "Córners local",                AC   = "Córners visitante",
  HF   = "Faltas local",                 AF   = "Faltas visitante",
  HY   = "Amarillas local",              AY   = "Amarillas visitante",
  HR   = "Rojas local",                  AR   = "Rojas visitante"
)

#Figura3

liga_limpia |>
  select(all_of(names(etiquetas))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  mutate(variable = factor(variable, levels = names(etiquetas))) |>
  ggplot(aes(valor)) +
  geom_bar(fill = "#3182BD") +
  facet_wrap(~ variable, scales = "free", ncol = 4,
             labeller = as_labeller(etiquetas)) +
  labs(x = "Valor", y = "Nº de partidos") +
  theme_minimal() + theme(strip.text = element_text(face = "bold", size = 9))

# Figura 4
liga_limpia |>
  mutate(total = FTHG + FTAG) |>
  ggplot(aes(total)) +
  geom_bar(fill = "#3182BD") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4) +
  scale_x_continuous(breaks = 0:8) +
  labs(x = "Goles totales en el partido", y = "Nº de partidos") +
  theme_minimal()

# Figura 5
g_desc <- ggplot(liga_limpia, aes(HTR)) +
  geom_bar(fill = "#9ECAE1") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4) +
  labs(title = "Al descanso", x = NULL, y = "Nº de partidos") +
  ylim(0, 200) + theme_minimal()
g_fin <- ggplot(liga_limpia, aes(FTR)) +
  geom_bar(fill = "#3182BD") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4) +
  labs(title = " Resultado final", x = NULL, y = NULL) +
  ylim(0, 200) + theme_minimal()
g_desc + g_fin

# Figura 6
Sys.setlocale("LC_TIME", "es_ES.UTF-8")   # meses y días en castellano
g_mes <- liga_limpia |>
  mutate(mes = floor_date(Date, "month")) |>
  ggplot(aes(mes)) +
  geom_bar(fill = "#3182BD") +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
  labs(title = "Por mes", x = NULL, y = "Nº de partidos") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g_dia <- liga_limpia |>
  mutate(dia = wday(Date, label = TRUE, abbr = FALSE, week_start = 1)) |>
  ggplot(aes(dia)) +
  geom_bar(fill = "#3182BD") +
  labs(title = "Por día de la semana", x = NULL, y = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g_mes + g_dia
liga_limpia <- liga_limpia |> mutate(Date = as.Date(Date))
class(liga_limpia$Date)   # tiene que salir "Date"


# ---------- 4.1 ----------
library(tidyverse)
library(corrplot)

liga_limpia <- liga |>
  select(-Referee) |>
  mutate(Date = as.Date(Date))


pares <- tribble(
  ~Variable,          ~loc,   ~vis,
  "Goles (final)",    "FTHG", "FTAG",
  "Goles (descanso)", "HTHG", "HTAG",
  "Tiros",            "HS",   "AS",
  "Tiros a puerta",   "HST",  "AST",
  "Córners",          "HC",   "AC",
  "Faltas",           "HF",   "AF",
  "Amarillas",        "HY",   "AY",
  "Rojas",            "HR",   "AR"
)

# Datos en formato largo: una fila por partido, variable y condición
largo <- pares |>
  pmap_dfr(function(Variable, loc, vis) {
    tibble(Variable  = Variable,
           Local     = liga_limpia[[loc]],
           Visitante = liga_limpia[[vis]])
  }) |>
  mutate(Variable = factor(Variable, levels = pares$Variable))


# Figura 7: múltiples diagramas de caja
largo |>
  pivot_longer(c(Local, Visitante), names_to = "Condición", values_to = "Valor") |>
  ggplot(aes(Condición, Valor, fill = Condición)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c(Local = "#3182BD", Visitante = "#FD8D3C")) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))
ggsave("figura7_boxplots_local_visitante.png", width = 10, height = 6, dpi = 300)

# Puntos por equipo en casa y fuera
puntos <- full_join(
  liga_limpia |> group_by(Equipo = HomeTeam) |>
    summarise(Casa = sum(case_when(FTR == "H" ~ 3, FTR == "D" ~ 1, TRUE ~ 0))),
  liga_limpia |> group_by(Equipo = AwayTeam) |>
    summarise(Fuera = sum(case_when(FTR == "A" ~ 3, FTR == "D" ~ 1, TRUE ~ 0))),
  by = "Equipo")
puntos
cor(puntos$Casa, puntos$Fuera)   # r = 0,60

# Figura 8: diagrama de dispersión
ggplot(puntos, aes(Casa, Fuera)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 3, colour = "#3182BD") +
  geom_text(aes(label = Equipo), vjust = -0.8, size = 3) +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60)) +
  labs(x = "Puntos como local", y = "Puntos como visitante") +
  theme_minimal()
ggsave("figura8_puntos_casa_fuera.png", width = 7, height = 7, dpi = 300)

# ---------- 4.2 ----------
equipo <- bind_rows(
  liga_limpia |> transmute(Tiros = HS, TirosPuerta = HST, Corners = HC, Goles = FTHG),
  liga_limpia |> transmute(Tiros = AS, TirosPuerta = AST, Corners = AC, Goles = FTAG)
)

# Tasas de conversión
sum(equipo$Goles) / sum(equipo$Tiros) * 100        # 10,8 %
sum(equipo$Goles) / sum(equipo$TirosPuerta) * 100  # 31,1 %

# Figura 9: matriz de correlaciones
etiquetas <- c(
  FTHG = "Goles local",       FTAG = "Goles visit.",
  HTHG = "Goles desc. local", HTAG = "Goles desc. visit.",
  HS   = "Tiros local",       AS   = "Tiros visit.",
  HST  = "T. puerta local",   AST  = "T. puerta visit.",
  HC   = "Córners local",     AC   = "Córners visit.",
  HF   = "Faltas local",      AF   = "Faltas visit.",
  HY   = "Amarillas local",   AY   = "Amarillas visit.",
  HR   = "Rojas local",       AR   = "Rojas visit."
)
m <- cor(liga_limpia[names(etiquetas)])
dimnames(m) <- list(etiquetas, etiquetas)
png("figura9_correlaciones.png", width = 9, height = 9, units = "in", res = 300)
corrplot(m, method = "color", type = "upper", addCoef.col = "black",
         number.cex = 0.6, tl.col = "black", tl.cex = 0.8, diag = FALSE)
dev.off()

# Recta de regresión
modelo <- lm(Goles ~ TirosPuerta, data = equipo)
coef(modelo)                  # a = 0,32 ; b = 0,24
summary(modelo)$r.squared     # R² = 0,26
predict(modelo, data.frame(TirosPuerta = c(5, 10)))

# Figura 10: dispersión + recta
ggplot(equipo, aes(TirosPuerta, Goles)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.4, colour = "#3182BD") +
  geom_smooth(method = "lm", se = FALSE, colour = "#D7301F", linewidth = 1) +
  scale_x_continuous(breaks = 0:12) +
  labs(x = "Tiros a puerta", y = "Goles marcados") +
  theme_minimal()


# Figura 11: residuos
equipo |>
  mutate(Residuo = residuals(modelo)) |>
  ggplot(aes(TirosPuerta, Residuo)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.4, colour = "#3182BD") +
  geom_hline(yintercept = 0, colour = "#D7301F") +
  scale_x_continuous(breaks = 0:12) +
  labs(x = "Tiros a puerta", y = "Residuo (goles reales − predichos)") +
  theme_minimal()

# ---------- 4.3 ----------
liga_res <- liga_limpia |>
  mutate(HTR = factor(HTR, levels = c("H", "D", "A"),
                      labels = c("Gana el local", "Empate", "Gana el visitante")),
         FTR = factor(FTR, levels = c("H", "D", "A"),
                      labels = c("Victoria local", "Empate", "Victoria visitante")))
# Figura 12
as.data.frame(prop.table(tabla, margin = 1) * 100) |>
  ggplot(aes(Descanso, Freq, fill = Final)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Freq, 1), "%")),
            position = position_stack(vjust = 0.5), colour = "white", size = 3.5) +
  scale_fill_manual(values = c("#3182BD", "#969696", "#FD8D3C")) +
  labs(x = "Resultado al descanso", y = "% de partidos", fill = "Resultado final") +
  theme_minimal()