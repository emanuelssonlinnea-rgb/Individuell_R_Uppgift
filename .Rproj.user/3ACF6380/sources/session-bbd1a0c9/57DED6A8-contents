# 1) Dataförståelse

library(tidyverse)
library(glmnet)
library(Metrics)
library(here)


raw_data <- read_csv(here("data", "insurance_costs.csv"))

glimpse(raw_data)
summary(raw_data)

colSums(is.na(raw_data))

raw_data %>% 
  count(sex, sort = TRUE)

raw_data %>% 
  count(region, sort = TRUE)

raw_data %>% 
  count(smoker, sort = TRUE)

raw_data %>% 
  count(chronic_condition, sort = TRUE)

raw_data %>% 
  count(exercise_level, sort = TRUE)

raw_data %>% 
  count(plan_type, sort = TRUE)

# Kommentar:
# Datasetet består av 1100 observationer och 14 variabler. 
# Av dessa 14 variabler är hälften numeriska och den andra hälften kategoriska.
# Datasetet har 3 kolumner med saknade värden: exercise_level (22), annual_checkups(20)
# och bmi(28)
# Inom 3 kategoriska variabler (smoker, region, plan_type) är värdena otydliga så kommer att rättas i nästa sektion.

#--------------------------------------------------------------------------
# 2) Datastädning och förberedelse

clean_data <- raw_data %>% 
  mutate(
    sex = as.factor(sex),
    region = str_trim(region),
    region = str_to_title(region),
    region = as.factor(region),
    smoker = str_trim(smoker),
    smoker = str_to_title(smoker),
    smoker = if_else(smoker == "Yes", TRUE, FALSE, missing = NA),
    smoker = as.factor(smoker),
    chronic_condition = if_else(chronic_condition == "yes", TRUE, FALSE, missing = NA),
    chronic_condition = as.factor(chronic_condition),
    exercise_level= factor(exercise_level),
    plan_type = str_trim(plan_type),
    plan_type = str_to_title(plan_type),
    plan_type = as.factor(plan_type)
    )

glimpse(clean_data)
summary(clean_data)

clean_data %>% 
  count(region, sort = TRUE)

clean_data %>% 
  count(smoker, sort = TRUE)

clean_data %>% 
  count(plan_type, sort = TRUE)

clean_data %>% 
  filter(is.na(bmi) | is.na(exercise_level) | is.na(annual_checkups))

ready_data <- clean_data %>% 
  mutate(
    bmi = if_else(
      is.na(bmi),
      median(bmi, na.rm = TRUE),
      bmi),
    annual_checkups = if_else(
      is.na(annual_checkups),
      median(annual_checkups, na.rm = TRUE),
      annual_checkups),
    exercise_level = fct_na_value_to_level(exercise_level, "missing"),
    bmi_group = case_when(
      is.na(bmi)      ~ "NA",
      bmi <= 26       ~ "lower",
      bmi > 26 & bmi <= 35 ~ "average",
      bmi > 35        ~ "higher",
      ),
    age_group = case_when(
      is.na(age)      ~ "NA",
      age <= 24       ~ "youth",
      age > 24 & age < 60   ~ "adult",
      age >= 60       ~ "senior",
    ),
  )

glimpse(ready_data)

ready_data %>% 
  filter(is.na(bmi) | is.na(exercise_level) | is.na(annual_checkups))

# Kommentar:
# Numeriska saknade värden (bmi & annual_checkups) hanterades genom att inputera medianen av värdena,
# dem kategoriska saknade värdena (exercise_level) hanterades genom att skapa en egen kategori för saknade värden = missing.
# 2 nya variabler skapades - bmi_group och age_group - det tillåter en lättare jämförelse mellan grupper och prissättningen,
# det ger en tydligare beskrivning när man jämför relationer, jobbar med regressionsmodeller samt för att se skillnaden mellan 
# dem olika grupperna. 

#------------------------------------------------------------------------------------------
# 3) Beskrivande analys
# Med tanke på att vi har flera variabler att jämföra så har jag delat upp analysen i numeriska och kategoriska variabler:

# Numeriska variabler ----------------------------------------------------------

df_num <- ready_data

num_vars <- c("age","bmi","children","prior_accidents","prior_claims","annual_checkups")

corr_matrix <- df_num %>% 
  select(all_of(num_vars), charges) %>% 
  cor(use = "complete.obs") %>% 
  round(2)

corr_matrix

corr_long <- corr_matrix %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation")

save_corr_heatmap <- function(corr_long, filename = "correlation_heatmap.png") {
  
  if (!dir.exists("output")) {
    dir.create("output")
  }
  
  p <- ggplot(corr_long, aes(x = var1, y = var2, fill = correlation)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "blue", high = "red", mid = "white",
      midpoint = 0, limit = c(-1, 1), space = "Lab"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Correlation Heatmap",
      x = "",
      y = "",
      fill = "Correlation"
    )
  
  save_path <- file.path("output", "correlation_heatmap.png")
  ggsave(save_path, p, width = 8, height = 6, dpi = 300)
  
  p
}

save_corr_heatmap(corr_long, "my_heatmap.png")

# Kommentar: 
# Heatmappen visar att dem numeriska variablerna med högst korrelation till charges är age( 0.26), prior_accidents (0.24) och prior_claims (0.18).
# Det betyder att dessa 3 variabler borde inkluderas i en prediktiv model även om korrelationerna inte är väldigt höga (ligger under 0.5), 
# vilket kan innebära att korrelationen blir starkare ihop med visa kategoriska variabler. 
# Dem lägsta numeriska korrelationerna med charge är children (0.03), annual_checkups(0.06). 
# Det som inte visas är icke-linjära relationer, interaktioner mellan andra variabler samt kategoriska variabler.
# Vi kommer att titta på kategoriska variabler härnäst och regressionsmodeller därefter. 


# Kategoriska variabler --------------------------------------------------------

analyze_costs <- function(data, group_var, filename = "boxplot.png") {
  
  group_var <- rlang::ensym(group_var)   
  
  if (!dir.exists("output")) {
    dir.create("output")
  }

  summary_tbl <- data %>%
    dplyr::group_by(!!group_var) %>%
    dplyr::summarize(
      avg_cost = mean(charges, na.rm = TRUE),
      median_cost = median(charges, na.rm = TRUE),
      sd_cost = sd(charges, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  box <- ggplot2::ggplot(data, ggplot2::aes(x = !!group_var, y = charges, fill = !!group_var)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(
      title = paste("Charges by", rlang::as_string(group_var)),
      x = rlang::as_string(group_var),
      y = "Charges"
    ) +
    ggplot2::theme_minimal()
  
  save_path <- file.path("output", filename)
  
  ggplot2::ggsave(
    filename = save_path,
    plot = box,
    width = 8,
    height = 5,
    dpi = 300
  )
  # Return both results
  list(
    summary = summary_tbl,
    plot = box,
    saved_to = save_path
  )
}

# Sex
result <- analyze_costs(ready_data, sex, "sex_boxplot.png")
result$summary
result$plot

# Kommentar: 
# Kön verkar inte vara en stor prediktor för försäkringskostnad (charges) 
# - det är väldigt liten skillnad mellan könen när man tittar på genomsnitt, median och standard deviation, även om antalet observationer mellan könen är nästan samma.


# Region
result <- analyze_costs(ready_data, region, "region_boxplot.png")
result$summary
result$plot

# Kommentar: 
# Region verkar också vara en ganska låg prediktor för charges - North och South har dem högsta genomsnittsliga försäkringskostnaderna 
# men skillnaderna är inte stora när man jämför mellan dem 4 regionerna, vilket också syns i boxploten där boxarna överlappar varandra. 
# Medianen och sd är också väldig nära varann så detta är en låg prediktor för försäkringskostnader.

#Smoker
result <- analyze_costs(ready_data, smoker, "smoker_boxplot.png")
result$summary
result$plot

# Kommentar: 
# Smoker är den starkaste prediktorn vi sett hittills - boxarna i boxplotten överlappar inte alls 
# och när man tittar på genomsnittet har rökare nästan dubbelt så hög försäkringskostnad som icke-rökare, samma när man tittar på medianen.
# Intressant nog är standard deviation närmare varandra vilket med tanke på antal observationer per grupp visar tydligt 
# att skillnaderna mellan rökare och icke-rökare inte beror på vissa avvikande siffror men att skillnaderna mellan grupperna gäller generellt mellan dem flesta observationerna.

#Chronic condition
result <- analyze_costs(ready_data, chronic_condition, "chronic_condition_boxplot.png")
result$summary
result$plot

# Kommentar: 
# Det är en signifikant skillnad mellan grupperna i chronic_condition -> i boxplotten överlappar dem nästan inget
# genomsnittet och medianen visar samma skillnader - dem med kroniska sjukdomar betalar i genomsnitt 150% av det icke-kroniska betalar.
# Standard deviation är liknande i båda grupperna vilket visar på att skillnaderna mellan grupperna inte skapades baserat mestadels på avvikare även om det tydligen finns flera i boxploten.

# Exercise level
result <- analyze_costs(ready_data, exercise_level, "exercise_level_boxplot.png")
result$summary
result$plot

# Kommentar: 
# Exercise_level visar en tylidg trend - ju högre exercise_level desto lägre charge. 
# Med tanke på att "missing" kategorin har så lågt antal (22) så analyseras inte denna närmare.
# Skillnaderna mellan genomsnitt, median och sd är signifikanta mellan dem olika kategorierna. 
# SKillnaderna mellan low (11576), medium(9753)och high (8639) kan verka mindre tills man jämför 
# skillnaden mellan high och low - det är en tydlig trend och detta är en signifikant prediktor för charges.

# Plan type
result <- analyze_costs(ready_data, plan_type, "plan_type.png")
result$summary
result$plot

# Kommentar: 
# Ett naturligt antagande är att plan_type är en signifikant prediktor av försäkringskostnader (charges),
# detta syns i boxplotten och i genomsnitten där basic har lägst och premium ligger på högst - det är inte ett random nummer utan ett strukturerat mönster.
# Det visas ännu tydligare i medianen där skilladerna mellan plan_typerna sysn tydligt. SD - siffrorna är nära varandra vilket ihop med medianerna visar 
# att skillnaderna mellan basic, standard o premium inte är på grund av extremvärden. Med tanke på att hur olika antalen observationer är mellan 
# basic (339), standard (553) och premium (208) - så är hela premium-fördelningen förskjuten, samma som i exercise_level.

# BMI_group
result <- analyze_costs(ready_data, bmi_group, "bmi_group_boxplot.png")
result$summary
result$plot

# Kommentar: 
# bmi_group skapades genom att se på bmi min (17) och max (44) och dela upp bmi-räckvidden (17-44) i tre grupper: lower, average och higher för att försöka få en så bra fördelning av siffrorna som möjligt. 
# Genomsnittet och medianen visar en tydlig trend - ju lägre bmi desto lägre charges. Standard deviation visar att spridningen av dem olika grupperna inte kommer från avvikare utan är en generell skillnad.
# Även med den fördelningen som gjordes av grupperna är det fortfarande inte jämna antal observationer mellan lower, average och higher där higher har ett väldigt litet antal observationer (54) vilket innebär att vi inte kan vara helt säkra på resultaten här även om mönstret är tydligt. 
# Med allt detta i åtanke är det här en bra prediktor för charges. Inte lika stark som smoker men inte lika låg som sex. 

# Age_group
result <- analyze_costs(ready_data, age_group, "age_group_boxplot.png")
result$summary
result$plot

# Kommentar:
# age_group skapades genom att dela upp age i enlighet med klassiska åldersgrupp för att lättare identifiera grupper med liknande livsstil och kroppsålder. 
# Den här variabeln är en bra prediktor för försäkringskostnad då man kan se en väldigt tydlig trend där ju äldre gruppen - desto dyrare är försäkringen. 
# Genomsnittet visar en stor skillnad mellan youth (7999) och senior(12381). Detta bekräftas även av medianen som ligger kring liknande siffror och påvisar att skillnaderna inte handlar om avvikare. 
# Standardavvikelsen ökar med ålder vilket förväntas då hälsotillstånd ökar med ålder men skillnaderna mellan grupperna är inte så stora vilket matchar trenden vi sett innan.
# Sample storlek är ganska liten för youth och senior men trenden är ändå stark igenom alla siffror så som sagt - age_group är en stark prediktor av charges. 



# SAMMANFATTNING ----------------------------------------------------------------------------------
# Baserat på kalkyleringarna så är följande variabler dem bästa prediktorerna för försäkringskostnader:
# Numeriska: age, prior_accidents och prior_claims - dessa är måttliga prediktorer men inte lika starka som dem vi sett bland dem kategoriska variablerna.
# Kategoriska: smoker, chronic_condition, exercise_level, age_group och plan_type är bland dem starkaste prediktorerna.
# Slutlig rankning på dem starkaste prediktorerna är smoker på först plats med tanke på dem stora skillnaderna, följt av chronic_condition. 
# Sedan har vi andra ganska starka prediktorer som exercise_level, age_group och plan_type. 
# Dem svagaste prediktorerna är sex, region, children, annual_checkups och bmi. 
# Med tanke på detta vill jag gå vidare med fokus på följande variabler: 
# smoker, chronic_condition, exercise_level, age_group och plan_type.

# ------------------------------------------------------------------------------------------

# 4) Regressionsanalys

# Fokus på denna sektionen är att bygga minst en regressionsmodell där charges används som målvariabel.
# Jag har valt följande variabler som fokus: smoker, chronic_condition, exercise_level, age_group och plan_type.


# Linjär regressionsmodel ---------------------------------------------------------------

model_linear_focus <- lm(
  charges ~ smoker + chronic_condition + exercise_level + age_group + plan_type,
  data = ready_data
)

summary(model_linear_focus)
coef(model_linear_focus)

# Kommentar:
# Multiple R-squared ligger på 0.68 vilket skulle kunna förbättras, så med tanke på det testar vi att köra linjär regression med alla variabler.
# Smoker - TRUE och chronic_condition- TRUE har dem högsta koefficienterna som vi såg innan.
# Det är tydligt att försäkringskostnaden går upp markant om man är rökare och/ har en kronisk sjukdom.
# Sedan visar koefficienterna att ju lägre exercise_level personen har - desto dyrare blir det, liknande med åldergrupp - ju äldre desto dyrare.


model_linear_all <- lm(
  charges ~ age + sex + region + bmi + children + smoker + chronic_condition +
  exercise_level + plan_type + prior_accidents + prior_claims + annual_checkups + age_group + bmi_group,
  data = ready_data
)

summary(model_linear_all)
coef(model_linear_all)


# Kommentar:
# Residual standard error har nu gått ner från 2594 till 2313 vilket visar på att tillagda variabler ger lite mer precision i beräkningarna inte bara noise.
# Multiple R-squared har nu gått upp från 0.68 till 0.75 vilket innebär att modellen är något starkare än i första versionen.
# Koefficienterna visar liknande trender som tidigare - rökare med kronisk sjukdom, som inte tränar mycket och har premium paket betalar högst. 
# En variabel som inte visade sig ha lika stor betydelse som trott är  age_group. 
# Istället visade sig 2 andra prediktorer ha högre påverkan på charges: prior_accidents och prior_claims. 

# Lasso regressionsmodell -----------------------------------------------------
# Den första modellen - linjär regression är enkel och lätt att tolka som modell men
# den har hög risk för overfitting samt att den inte hanterar multikollinearitet väl. 
# Med det i åtanke valde jag att testa en modell till - lasso regression, som valdes för att:
  # Lasso krymper orelevanta koefficienter till noll
  # Bra på att identifiera starkaste prediktorer
  # Bättre på att identifiera när variabler är korrelerade än linjär regression.



vars <- c("age","sex","region","bmi","children","smoker","chronic_condition",
          "exercise_level","plan_type","prior_accidents","prior_claims",
          "annual_checkups", "age_group", "bmi_group")

df <- ready_data[, vars]


x <- model.matrix(~ ., data = df)[, -1]
y <- ready_data$charges
model_lasso <- glmnet(x, y, alpha = 1)

summary(model_lasso)

cv_lasso <- cv.glmnet(x, y, alpha = 1) 
plot(cv_lasso)

best_lambda <- cv_lasso$lambda.min
best_lambda

coef(cv_lasso, s = "lambda.min")

# Kommentar:
# Koefficienterna visae samma trend som vi såg i linjär regression där smoker- TRUE, chronic_condition-TRUE,
# plan_type-Premium, exercise_level-low och prior_accidents ligger högst upp och har därmed starkast påverkan på försäkringskostnad.

# Model jämförelse ------------------------------------------------------
# Målet är att välja en regressionsmodell som kan användas som stöd vid framtida prissättning med liknande kunder.
# Med det i åtanke ville jag nu testa dem fyra modellernas prediktionsförmåga genom att dela upp datan i test och train set 
# och sedan jämföra RMSE, MAE & R2. 

set.seed(123)

n <- nrow(df)
train_index <- sample(seq_len(n), size = 0.8 * n)


x_train <- x[train_index, ]
x_test  <- x[-train_index, ]

y_train <- y[train_index]
y_test  <- y[-train_index]

model_linear_focus <- lm(
  charges ~ smoker + chronic_condition + exercise_level + prior_accidents + prior_claims + plan_type,
  data = ready_data[train_index, ]
)

model_linear_full <- lm(
  charges ~ .,
  data = ready_data[train_index, c("charges", vars)]
)

x_train_focus <- model.matrix(
  charges ~ smoker + chronic_condition + exercise_level + prior_accidents + prior_claims + plan_type,
  data = ready_data[train_index, ]
)[, -1]

x_test_focus <- model.matrix(
  charges ~ smoker + chronic_condition + exercise_level + prior_accidents + prior_claims + plan_type,
  data = ready_data[-train_index, ]
)[, -1]

cv_lasso_focus <- cv.glmnet(x_train_focus, y_train, alpha = 1)
lasso_focus <- glmnet(x_train_focus, y_train, alpha = 1,
                      lambda = cv_lasso_focus$lambda.min)


cv_lasso_full <- cv.glmnet(x_train, y_train, alpha = 1)

pred_linear_focus <- predict(model_linear_focus, 
                      newdata = ready_data[-train_index, ])
pred_linear_full <- predict(model_linear_full, 
                     newdata = ready_data[-train_index, vars])
pred_lasso_focus <- predict(lasso_focus, s = cv_lasso_focus$lambda.min,
                            newx = x_test_focus)
pred_lasso_full <- predict(cv_lasso_full, s = "lambda.min", newx = x_test)


# RMSE
rmse_linear_focus <- rmse(y_test, pred_linear_focus)
rmse_linear_full  <- rmse(y_test, pred_linear_full)
rmse_lasso_focus <- rmse(y_test, pred_lasso_focus)
rmse_lasso_full <- rmse(y_test, pred_lasso_full)

# MAE
mae_linear_focus <- mae(y_test, pred_linear_focus)
mae_linear_full  <- mae(y_test, pred_linear_full)
mae_lasso_focus  <- mae(y_test, pred_lasso_focus)
mae_lasso_full <- mae(y_test, pred_lasso_full)

# R2
r2_linear_focus <- 1 - sum((y_test - pred_linear_focus)^2) / sum((y_test - mean(y_test))^2)
r2_linear_full  <- 1 - sum((y_test - pred_linear_full)^2) / sum((y_test - mean(y_test))^2)
r2_lasso_focus   <- 1 - sum((y_test - pred_lasso_focus)^2) / sum((y_test - mean(y_test))^2)
r2_lasso_full <- 1 - sum((y_test - pred_lasso_full)^2) / sum((y_test - mean(y_test))^2)

model_comparison <- tibble(
  model = c(
    "Linjär (top prediktorer)",
    "Linjär (alla variabler)",
    "Lasso (top prediktorer)",
    "Lasso (alla variabler)"
  ),
  RMSE = c(rmse_linear_focus, rmse_linear_full, rmse_lasso_focus, rmse_lasso_full),
  MAE = c(mae_linear_focus, mae_linear_full, mae_lasso_focus, mae_lasso_full),
  R_squared = c(r2_linear_focus, r2_linear_full, r2_lasso_focus, r2_lasso_full)
)

model_comparison

# Kommentar:
# Den modell som förklarar variationen mellan charges bäst är lasso regressionsmodellen med alla variabler inkluderade.
# När man jämför den linjära och lasso modellerna är skillnaderna försumbara. Detta betyder att datasetet är stabilt och att korrelationerna mellan variablerna inte var så komplexa att dem inte kunde fångas up av en linjär regressionsmodell.
# Anledningen kan vara att vi inte har så många variabler och att korrelationerna mellan dem inte är så komplexa.
# Resultaten visar att dem fulla modellerna med alla prediktorer presterar bättre än fokusmodellerna vilket är fullt rimligt. 
# Fokusvariablerna som valdes gav omkring 71.5% av variationen i charges vilket är ganska bra, när man tänker på att i modellerna med fulla variabler så var resultatet 75%.
# Det visar att fokus prediktorerna som valdes var dem som hade högst påverkan på försäkringskostnaden.
  

# Modellutvärdering ----------------------------------------------------------

fitted_lasso <- predict(cv_lasso_full, s = "lambda.min", newx = x)

residuals_lasso <- y - fitted_lasso


model_lasso_diagnostics <- ready_data %>%
  mutate(
    fitted_value = as.numeric(fitted_lasso),
    residual = as.numeric(residuals_lasso)
  )

model_lasso_diagnostics %>%
  select(charges, fitted_value, residual) %>%
  slice_head(n = 10)



ggplot(model_lasso_diagnostics, aes(x = fitted_value, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residualer mot predikterade värden",
    x = "Predikterat pris",
    y = "Residual"
  )



ggplot(model_lasso_diagnostics, aes(x = fitted_value, y = charges)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Faktiskt pris mot predikterat pris",
    x = "Predikterat pris",
    y = "Faktiskt pris"
  )


# Kommentar:
# Residualer mot predikterade värden - grafen visar en rimlig spridning, där kunderna är ganska centrerade omkring det predikterade värdet utan tydliga mönster. 
# Spridningen ökar lite ju högre det predikterade priset blir, vilket är rimligt eftersom högre priser kan innebära mer varierade kundtyper, situationer, etc.
# Det är inte några stora avvikelser i siffrorna vilket pekar på en stabil modell som skulle fungera som beslutsstöd för framtida kunder.
# Faktiskt pris mot predikterat pris - punkterna är centrerade omkring den röda linjen vilket visar på att modellen fungerar då den predikterar väldigt nära den faktiska kostnaden.
# Spridningen är ganska jämn och ökar i storlek ju högre priset blir vilket matchar förväntningarna för dyrare kunder där variationen ökar.
# Vi ser inga tydliga mönster förutom att punkterna följer det fakstiska priset så modellen har fångat upp det linjära sambandet. 
