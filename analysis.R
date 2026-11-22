# ==============================================================================
# Determinants of Firm Sales in Kuwait
# OLS regression, variable selection and path analysis (WBES Kuwait 2025)
#
# Input : data/Project_Data.csv  (World Bank Enterprise Survey, Kuwait 2025;
#         not included in this repo - see README for download instructions)
# Output: console results + outputs/residual_plots.png
# ==============================================================================

library(dplyr); library(tidyr)
library(lmtest); library(sandwich); library(car); library(lavaan)

set.seed(123)
dir.create("outputs", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. DATA CLEANING
# ------------------------------------------------------------------------------
raw <- read.csv("data/Project_Data.csv", stringsAsFactors = FALSE)
dim(raw)  # expected: 150 x 337

d <- raw %>% transmute(
  sales    = d2,
  emp      = l1,
  exper    = suppressWarnings(as.numeric(b7)),       # "One year or less" -> NA
  cert     = as.integer(b8 == "Yes"),                # 1 = certified
  fin      = case_when(k30 == "No obstacle" ~ 0, k30 == "Minor obstacle" ~ 1,
                       k30 == "Moderate obstacle" ~ 2, TRUE ~ NA_real_),
  sole     = as.integer(b1 == "Sole proprietorship"),
  manuf    = as.integer(a4a == "Manufacturing"),
  age      = 2025 - b5,
  web      = as.integer(c22b == "Yes"),
  audited  = as.integer(k21 == "Yes"),
  rd       = as.integer(h8 == "Yes"),
  training = as.integer(l10 == "Yes"),
  govt     = as.integer(j42 == "Yes"),
  epay     = suppressWarnings(as.numeric(k33))
) %>% mutate(ln_sales = log(sales), ln_emp = log(emp), ln_age = log(age))

table(d$cert)
table(d$fin, useNA = "ifany")
table(d$sole)
table(d$manuf)
colSums(is.na(d[, c("sales", "emp", "exper", "cert", "fin", "sole", "age")]))

# ------------------------------------------------------------------------------
# 2. DESCRIPTIVE STATISTICS (complete cases, N = 146)
# ------------------------------------------------------------------------------
Sraw <- d %>%
  select(sales, emp, age, ln_sales, ln_emp, ln_age, exper,
         cert, fin, sole, manuf, web, audited, rd, training, govt, epay) %>%
  drop_na()
nrow(Sraw)
S <- Sraw

cand <- c("ln_emp", "exper", "cert", "fin", "sole", "manuf", "ln_age",
          "web", "audited", "rd", "training", "govt", "epay")

# Continuous variables
cont <- c("sales", "ln_sales", "emp", "ln_emp", "age", "ln_age", "exper", "epay")
desc_cont <- data.frame(
  N      = sapply(Sraw[cont], function(x) sum(!is.na(x))),
  Mean   = sapply(Sraw[cont], mean),
  SD     = sapply(Sraw[cont], sd),
  Min    = sapply(Sraw[cont], min),
  Median = sapply(Sraw[cont], median),
  Max    = sapply(Sraw[cont], max)
)
round(desc_cont, 2)

# Binary variables
bin <- c("cert", "audited", "rd", "manuf", "sole", "web", "training", "govt")
desc_bin <- data.frame(
  Yes_n   = sapply(Sraw[bin], sum),
  Yes_pct = round(100 * sapply(Sraw[bin], mean), 1)
)
desc_bin

# Financing obstacle (0 = none, 1 = minor, 2 = moderate)
table(Sraw$fin)
round(100 * prop.table(table(Sraw$fin)), 1)

# ------------------------------------------------------------------------------
# 3. CORRELATIONS (all 13 candidates)
# ------------------------------------------------------------------------------
vars13 <- c("ln_sales", cand)
round(sort(cor(S[, cand], S$ln_sales)[, 1], decreasing = TRUE), 2)
round(cor(S[, vars13]), 2)

# ------------------------------------------------------------------------------
# 4. MODEL 1: FULL OLS (13 predictors, HC3 robust SEs)
# ------------------------------------------------------------------------------
full <- lm(reformulate(cand, "ln_sales"), data = S)
coeftest(full, vcov. = vcovHC(full, type = "HC3"))
cat("Full model adj R2:", summary(full)$adj.r.squared, "\n")
coefci(full, vcov. = vcovHC(full, type = "HC3"))

# ------------------------------------------------------------------------------
# 5. BACKWARD ELIMINATION (AIC and BIC)
# ------------------------------------------------------------------------------
b_aic <- step(full, direction = "backward", k = 2,           trace = 0)  # AIC
b_bic <- step(full, direction = "backward", k = log(nrow(S)), trace = 0) # BIC (stricter)
names(coef(b_aic))
names(coef(b_bic))
sapply(list(full = full, backward_AIC = b_aic, backward_BIC = b_bic),
       function(m) summary(m)$adj.r.squared)

# We adopt the AIC model (7 predictors) and go forward with it.

# ------------------------------------------------------------------------------
# 6. FINAL MODEL (7 predictors, HC3 robust SEs)
# ------------------------------------------------------------------------------
final <- lm(ln_sales ~ ln_emp + exper + cert + manuf + audited + ln_age + rd, data = S)
V <- vcovHC(final, type = "HC3")
coeftest(final, vcov. = V)
coefci(final, vcov. = V)
cat("R2:", summary(final)$r.squared, " adj R2:", summary(final)$adj.r.squared, "\n")

# Dummy coefficients as % difference in sales: 100 * (exp(b) - 1)
b <- coef(final)
round(100 * (exp(b[c("cert", "manuf", "audited")]) - 1), 1)

# ------------------------------------------------------------------------------
# 7. DIAGNOSTICS
# ------------------------------------------------------------------------------
vif(final)                          # multicollinearity (want < 5)
bptest(final, studentize = FALSE)   # constant variance (want p > 0.05)
shapiro.test(resid(final))          # normal residuals (want p > 0.05)
max(cooks.distance(final))          # influential points (want < 0.5)
sum(abs(rstandard(final)) > 3)      # outliers
which(abs(rstandard(final)) > 3)
max(hatvalues(final))
which.max(hatvalues(final))
2 * length(coef(final)) / nobs(final)  # leverage threshold 2k/n

residualPlots(final)                # curvature tests incl. Tukey

png("outputs/residual_plots.png", width = 1800, height = 1200, res = 200)
par(mfrow = c(2, 3))
plot(final, which = 1)  # residuals vs fitted
plot(final, which = 2)  # normal Q-Q
plot(final, which = 3)  # scale-location
plot(final, which = 5)  # leverage
plot(S$ln_emp, resid(final), xlab = "ln(employees)", ylab = "Residual")
abline(h = 0); lines(lowess(S$ln_emp, resid(final)), col = "red")
plot(S$exper, resid(final), xlab = "Manager experience (years)", ylab = "Residual")
abline(h = 0); lines(lowess(S$exper, resid(final)), col = "red")
dev.off()
par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# 8. ROBUSTNESS: sector from realised ISIC code instead of sampling frame
# ------------------------------------------------------------------------------
d$manuf_r <- as.integer(floor(as.numeric(raw$d1a2_v4) / 100) %in% 10:33)
S2 <- d[complete.cases(d[, c("exper", "fin")]), ]
nrow(S2)
table(frame = S2$manuf, realised = S2$manuf_r)
rob <- lm(ln_sales ~ ln_emp + exper + cert + manuf_r + ln_age + audited + rd, data = S2)
coeftest(rob, vcov. = vcovHC(rob, type = "HC3"))

# ------------------------------------------------------------------------------
# 9. PATH ANALYSIS (firm size as mediator; 2,000 bootstrap resamples)
# ------------------------------------------------------------------------------
pm <- '
  ln_emp   ~ a1*exper + a2*cert + a3*audited + a4*rd + a5*manuf + a6*ln_age
  ln_sales ~ b*ln_emp + c1*exper + c2*cert + c3*audited + c4*rd + c5*manuf + c6*ln_age

  ind_exper   := a1*b
  ind_cert    := a2*b
  ind_audited := a3*b
  ind_rd      := a4*b

  tot_exper   := c1 + a1*b
  tot_cert    := c2 + a2*b
  tot_audited := c3 + a3*b
  tot_rd      := c4 + a4*b
'
set.seed(123)
fit <- sem(pm, data = S, se = "bootstrap", bootstrap = 2000)
parameterEstimates(fit, boot.ci.type = "perc", level = 0.95) %>%
  filter(op %in% c("~", ":=")) %>%
  select(lhs, op, rhs, label, est, se, pvalue, ci.lower, ci.upper)

lavInspect(fit, "rsquare")

# ------------------------------------------------------------------------------
# 10. CORRELATIONS FOR THE RETAINED 7 VARIABLES (with p-values)
# ------------------------------------------------------------------------------
vars <- c("ln_sales", "ln_emp", "exper", "ln_age", "cert", "audited", "rd", "manuf")
cc7 <- S[, vars]
round(cor(cc7), 2)

cor_p <- sapply(vars, function(a) sapply(vars, function(b)
  if (a == b) NA else cor.test(cc7[[a]], cc7[[b]])$p.value))
ifelse(is.na(cor_p), "", ifelse(cor_p < 0.001, "<0.001", round(cor_p, 3)))
