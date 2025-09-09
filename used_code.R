library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fastDummies)
library(corrplot)
library(caret)
library(MASS)
library(performance)
library(pscl)
library(glmnet)
library(zoo)
library(rpart)
library(rpart.plot)
library(knitr)

options(scipen=999)
data <- readxl::read_excel("Premier-League_Data.xlsx")


data <- data %>% separate(Result, c("Home team goals", "Away team goals"), sep = ' - ')
data <- data %>% mutate(`Home team at home` = 'Yes') %>% mutate(`Away team at home` = "No")


data_away <- data %>% dplyr::select(`Day`,`Round Number`, Location, `Away Team`, `Home Team`, `Away team at home`, Referee, `Away team goals`, `Away team manager`, `Home team manager`,
                                    `Away team elo`, `Home team elo`, `Away team formation`, `Home team formation`, `Days from last game away`, `Days from last game home team`,
                                    `Newly promoted away team`,`Newly promoted home team`,`Top six club away team`, `Top six club home team`, `Home team goals`
)
data_home <- data %>% dplyr::select(`Day`,`Round Number`,  Location, `Home Team`, `Away Team`, `Home team at home`, Referee, `Home team goals`, `Home team manager`, `Away team manager`,
                                    `Home team elo`, `Away team elo`, `Home team formation`, `Away team formation`, `Days from last game home team`, `Days from last game away` ,
                                    `Newly promoted home team`,`Newly promoted away team`,`Top six club home team`,`Top six club away team`, `Away team goals`
)

colnames(data_away) <- c("Day","Round Number", "Location", "Team", "Rival", "At home", "Referee", "Goals scored", "Team manager", "Rival's manager",
                         "Elo points", "Rival's elo points", "Formation", "Rival's formation", "Days from last game", "Days from rival's last game",
                         "Newly promoted", "Newly promoted rival", "Top six club", "Top six team rival", "Goals conceded")

colnames(data_home) <- colnames(data_away)
data_clubs <- rbind(data_home, data_away)







data_clubs <- data_clubs %>%
  mutate(`Goals scored` = as.numeric(`Goals scored`),
         `Goals conceded` = as.numeric(`Goals conceded`))

data_clubs <- data_clubs %>% 
  mutate_if(is.character, as.factor)

data_clubs <- data_clubs %>%
  mutate(`Round Number` = as.integer(as.character(`Round Number`)))

data_clubs <- data_clubs %>%
  mutate(
    Best_three = if_else(Team %in% c('Liverpool', 'Arsenal', 'Man City'), 'Yes', 'No')
  )

data_clubs <- data_clubs %>%
  mutate(
    Best_three_rival = if_else(Rival %in% c('Liverpool', 'Arsenal', 'Man City'), 'Yes', 'No')
  )

data_clubs <- data_clubs %>%
  arrange(Team, `Round Number`) %>%         
  group_by(Team) %>%              
  mutate(
    Goals_lag_conceded = lag(`Goals conceded`),        
    Goals_last5_conceded = rollapplyr(
      Goals_lag_conceded,              
      width = 3,
      FUN = sum,
      partial = TRUE        
    )
  ) %>%
  ungroup()

lost_goals <- c(1,1,1,
                9,9,8,
                7,5,4,
                5,6,7,
                5,7,6,
                4,4,5,
                1,3,4,
                3,5,9,
                6,7,4,
                4,3,6,
                2,3,5,
                5,3,0,
                1,1,2,
                3,2,2,
                5,4,3,
                5,5,2,
                7,3,3,
                3,3,1,
                9,6,5,
                10,7,10)
data_clubs <- data_clubs %>%
  mutate(
    Goals_last5_conceded = {
      na_idx <- which(is.na(Goals_last5_conceded))
      Goals_last5_conceded[na_idx] <- lost_goals[seq_along(na_idx)]
      Goals_last5_conceded
    }
  )


data_laczenie <- data_clubs %>% 
  dplyr::select(`Round Number`, Team, Rival, Goals_last5_conceded)


data_clubs <- data_clubs %>%
  left_join(data_laczenie, by=c("Round Number" = "Round Number", "Rival" = "Team", "Team" = "Rival"))

data_clubs <- data_clubs %>%
  dplyr::select(-Goals_last5_conceded.x, -Goals_lag_conceded) %>%
  mutate(Goals_last5_conceded_rival = Goals_last5_conceded.y) %>%
  dplyr::select(-Goals_last5_conceded.y)


data_cleaned <- data_clubs %>% dplyr::select(-`Team manager`,-Day,-Location, -`Rival's manager`, -Referee)


#####################

ggplot(data_cleaned, aes(x = `Goals scored`, y = `Days from last game`))+
  geom_jitter(width =0.1) +
  geom_violin()

ggplot(data_cleaned, aes(x = `Goals scored`, y = `Days from rival's last game`))+
  geom_jitter(width =0.1) +
  geom_violin()
###########################

#group
data_analysis <- data_cleaned %>%
  mutate(`Days from last game` = case_when(
    as.character(`Days from last game`) %in% c('3','4') ~ "Short break",
    as.character(`Days from last game`) %in% c('5','6','7','8','13','14','15') ~ "Normal break",
    `Days from last game` == 'start of the season' ~ "Normal break",
    as.character(`Days from last game`) %in% c('9','10','11','12','16','17','18') ~ "Normal break",
    TRUE ~ "Short break"  # reszta bez zmian, ale jako tekst
  ))


data_analysis <- data_analysis %>%
  mutate(`Days from rival's last game` = case_when(
    as.character(`Days from rival's last game`) %in% c('3','4')~ "Short break",
    as.character(`Days from rival's last game`) %in% c('5','6','7','8','13','14','15') ~ "Normal break",
    `Days from rival's last game` == 'start of the season' ~ "Normal break",
    as.character(`Days from rival's last game`) %in% c('9','10','11','12','16','17','18') ~ "Normal break",
    TRUE ~ as.character(`Days from last game`)
  ))


ggplot(data_analysis, aes(x = `Goals scored`, y = `Days from last game`))+
  geom_jitter(width =0.1) +
  geom_violin()

ggplot(data_analysis, aes(x = `Goals scored`, y = `Days from rival's last game`))+
  geom_jitter(width =0.1) +
  geom_violin()


tree <- rpart(`Goals scored` ~ `Formation`, control = rpart.control(    # minimalna liczba obserwacji w węźle do podziału
  minbucket = 20    # minimalna liczba obserwacji w liściu      # maksymalna głębokość drzewa
),data = data_analysis)
rpart.plot(tree, 
           type = 3,              # styl drzewa (prostokąty)
           extra = 1,             # dodatkowe info w węzłach
           fallen.leaves = TRUE,  # ładnie układa liście
           cex = 0.5,             # zmniejsza czcionkę
           faclen = 0,            # pełne nazwy kategorii (0 = bez skracania)
           under = TRUE,          # etykiety pod węzłami zamiast w środku
           tweak = 1)     




data_analysis <- data_analysis %>%
  mutate(Formation = case_when(
    Formation %in% c('4-3-3', '4-2-3-1','4-2-2-2',  '4-4-2', '4-1-4-1',  '4-5-1', '3-2-4-1', '3-4-1-2') ~ "more_popular",
    Formation %in% c('5-4-1', '5-3-2','4-4-1-1','4-3-1-2','4-1-2-1-2','3-5-2', '3-4-2-1') ~ "less_popular"
  ))




tree <- rpart(`Goals scored` ~ `Rival's formation`, control = rpart.control(    
  minbucket = 20    
),data = data_analysis)
rpart.plot(tree, 
           type = 3,             
           extra = 1,             
           fallen.leaves = TRUE,  
           cex = 0.5,            
           faclen = 0,            
           under = TRUE,          
           tweak = 1)

data_analysis <- data_analysis %>%
  mutate(`Rival's formation` = case_when(
    `Rival's formation` %in% c('3-5-2', '3-4-2-1', '3-4-1-2', '3-2-4-1','4-3-3', '4-2-3-1','4-2-2-2',  '4-1-2-1-2','4-4-2', '4-1-4-1') ~ "more_popular",
    `Rival's formation` %in% c('5-4-1', '5-3-2', '4-3-1-2', '4-4-1-1', '4-5-1') ~ "less_popular"
  ))




data_analysis <- data_analysis %>% 
  mutate(`Round Number` =  as.numeric(`Round Number`))

data_analysis <- data_analysis %>% 
  mutate_if(is.character, as.factor)

data_analysis <- data_analysis %>%
  mutate(`Elo difference`= `Elo points`-`Rival's elo points`)



data_analysis <- data_analysis %>%
  arrange(Team, `Round Number`) %>%        
  group_by(Team) %>%               
  mutate(
    Goals_lag = lag(`Goals scored`),         
    Goals_last5 = rollapplyr(
      Goals_lag,                
      width = 3,
      FUN = sum,
      partial = TRUE           
    )
  ) %>%
  ungroup()


replacement_values <- c(6,5,6,
                        3,5,2,
                        2,3,3,
                        3,5,4,
                        2,4,5,
                        7,4,8,
                        12,9,6,
                        6,3,1,
                        4,4,6,
                        7,4,3,
                        8,4,2,
                        9,7,6,
                        9,7,9,
                        5,6,4,
                        7,7,6,
                        7,5,4,
                        2,2,2,
                        5,4,8,
                        2,3,4,
                        2,1,2
                        
)

data_analysis <- data_analysis %>%
  mutate(
    Goals_last5 = {
     
      na_idx <- which(is.na(Goals_last5))
      
      Goals_last5[na_idx] <- replacement_values[seq_along(na_idx)]
      Goals_last5
    }
  )





tree <- rpart(`Goals scored` ~ Goals_last5_conceded_rival, data = data_analysis)
rpart.plot(tree, 
           type = 3,              
           extra = 1,             
           fallen.leaves = TRUE,  
           cex = 0.5,             
           faclen = 0,            
           under = TRUE,          
           tweak = 1)


tree <- rpart(`Goals scored` ~ Goals_last5, data = data_analysis)
rpart.plot(tree, 
           type = 3,             
           extra = 1,             
           fallen.leaves = TRUE,  
           cex = 0.5,            
           faclen = 0,           
           under = TRUE,         
           tweak = 1)

tree <- rpart(`Goals scored` ~ Team, data = data_analysis)
rpart.plot(tree, 
           type = 3,              
           extra = 1,           
           fallen.leaves = TRUE,  
           cex = 0.5,             
           faclen = 0,            
           under = TRUE,         
           tweak = 1)


tree <- rpart(`Goals scored` ~ Rival, data = data_analysis)
rpart.plot(tree, 
           type = 3,             
           extra = 1,             
           fallen.leaves = TRUE,  
           cex = 0.5,           
           faclen = 0,          
           under = TRUE,         
           tweak = 1)

data_analysis <- data_analysis %>%
  mutate(`Team` = case_when(
    `Team` %in% c('Crystal Palace','Southampton', 'Leicester','Everton', 'Ipswich',  'Man Utd',  'West Ham') ~ "Weak_off",
    `Team` %in% c('Aston Villa', 'Bournemouth', 'Fulham', "Nott'm Forest", 'Wolves') ~ "Av_off",
    `Team` %in% c('Arsenal', 'Brentford', 'Brighton', 'Chelsea', 'Liverpool', 'Man City', 'Newcastle', 'Spurs') ~ "Best_off"
  ))

data_analysis <- data_analysis %>%
  mutate(`Rival` = case_when(
    `Rival` %in% c('Ipswich', 'Leicester', 'Southampton') ~ "Rival_def_weak",
    `Rival` %in% c('Spurs', 'West Ham', 'Wolves') ~ "Rival_def_avg",
    `Rival` %in% c('Arsenal', 'Aston Villa','Brentford', 'Brighton', 'Chelsea', 'Liverpool', 'Man City', 'Newcastle', 'Bournemouth','Crystal Palace', 'Everton', 'Fulham', 'Man Utd', "Nott'm Forest") ~ "Rival_def_strong"
  ))


data_analysis <- data_analysis %>%
  mutate(Goals_last5_conceded_rival = cut(
    Goals_last5,
    breaks = c(-Inf, 6,  Inf),
    labels = c("less_equal_6", "more_than_6")
  ))

data_analysis <- data_analysis %>%
  mutate(Goals_last5 = cut(
    Goals_last5,
    breaks = c(-Inf, 3,  Inf),
    labels = c("less_equal_3","more_than_3")
  ))

ggplot(data_analysis, aes(x = Team, y = `Goals scored`)) +
  geom_jitter(height = 0.1, width = 0.1) +
  geom_violin() +
  scale_x_discrete(labels = c(
    "Av_off" = "average offense",
    "Best_off" = "great offense",
    "Weak_off" = "weak offense"
  )) 

ggplot(data_analysis, aes(x = Rival, y = `Goals scored`)) +
  geom_jitter(height = 0.1, width = 0.1) +
  geom_violin() +
  scale_x_discrete(labels = c(
    "Rival_def_avg" = "rival average defence",
    "Rival_def_strong" = "rival strong defence",
    "Rival_def_weak" = "rival weak defence"
  ))

g <- data_analysis$`Goals scored`


hist(g,
     breaks = seq(-0.5, max(g) + 0.5, 1), 
     freq = FALSE,                        
     ylim = c(0, 0.8),
     main = "Histogram with Poisson fit",
     xlab = "Goals scored",
     col = 'beige')


lambda_hat <- mean(g)
x_vals <- 0:max(g)


points(x_vals, dpois(x_vals, lambda = lambda_hat), col = "red", pch = 16)
lines(x_vals, dpois(x_vals, lambda = lambda_hat), col = "red")



data_dummy <- dummy_cols(
  data_analysis,
  remove_first_dummy = FALSE,
  remove_selected_columns = TRUE    
)

data_dummy <- data_dummy %>%
  dplyr::select(
    -`Round Number`, 
    -`At home_No`, 
    -`Days from last game_Normal break`, 
    -`Days from rival's last game_Normal break`, 
-`Newly promoted_No`,
-`Newly promoted_Yes`,
-`Top six club_No`, 
-`Newly promoted rival_No`,
-`Newly promoted rival_Yes`,
-`Top six team rival_No`,
-`Rival's elo points`,
-`Elo points`,
-`Top six club_Yes`,
-`Top six team rival_Yes`,
-`Formation_less_popular`,
-`Rival's formation_less_popular`,
-`Newly promoted_Yes`,
-Goals_lag,
-`Goals_last5_less_equal_3`,
-`Goals_last5_conceded_rival_less_equal_6`,
-`Rival_Rival_def_avg`,
-`Team_Av_off`,
-`Best_three_No`,
-`Best_three_rival_No`,
-`Goals conceded`
)

colnames(data_dummy) <- gsub(" ", "_", colnames(data_dummy))
colnames(data_dummy) <- gsub("'", "", colnames(data_dummy))




model_int_data <- as.data.frame(model.matrix(`Goals_scored` ~ .^2, data=data_dummy)[, -1])
model_int_data <- model_int_data[,-grep("^Elo_difference:", colnames(model_int_data))]
zero_var_cols <- colnames(model_int_data)[apply(model_int_data, 2, function(x) var(x) == 0)]
model_int_data <- model_int_data %>% dplyr::select(-all_of(zero_var_cols))




model_poisson0 <- glm(Goals_scored ~ ., family=poisson(), data=data_dummy)



performance::check_model(model_poisson0)
#performance::check_overdispersion(model_poisson0)
#performance::check_zeroinflation(model_poisson0)
#performance::check_collinearity(model_poisson0)


set.seed(10)
lasso_model <- glmnet(model_int_data, data_dummy$Goals_scored, alpha = 1, family = "poisson")
cv_lasso <- cv.glmnet(as.matrix(model_int_data), data_dummy$Goals_scored, alpha = 1, family = "poisson", nfolds= 10)  # lub "gaussian"
best_lambda <- cv_lasso$lambda.min
coef_all <- coef(cv_lasso, s = "lambda.min")
nonzero_coef <- coef_all[coef_all[,1] != 0, , drop = FALSE]
model_poisson2 <- glm(as.formula(paste("Goals_scored~", paste(names(nonzero_coef[,1])[-1], collapse='+'))), data=cbind("Goals_scored"=data_dummy$Goals_scored,model_int_data), family = poisson())
performance::check_model(model_poisson2)

model_poisson2 <- update(model_poisson2, 
                           . ~ . -Rival_Rival_def_strong:Rivals_formation_more_popular)
performance::check_model(model_poisson2)
#performance::check_overdispersion(model_poisson2)
#performance::check_zeroinflation(model_poisson2)
#performance::check_collinearity(model_poisson2)

model_nb <- glm.nb(Goals_scored ~., data=data_dummy)
performance::check_model(model_nb)

models_list <- list(model_poisson0,model_nb,   model_poisson2)
models_names <- c('Default Poisson model', "Negative binomial model",  "LASSO Poisson model with interaction")
AIC <- round(sapply(models_list, function(x) AIC(x)), digits=2)
num_coef <- sapply(models_list, function(x) length(coef(x)))
BIC <- round(sapply(models_list, function(x) BIC(x)), digits=2)
expected_zeros <- round(sapply(models_list, function(x) sum(exp(-predict(x, type='response')))), digits=2)
real_zeros <- rep(length(which(data_dummy$Goals_scored==0)), times=3)


expected_one <- round(sapply(models_list, function(x) sum(predict(x, type='response')*exp(-predict(x, type='response')))), digits=2)
real_ones <- rep(length(which(data_dummy$Goals_scored==1)), times=3)
expected_two <- round(sapply(models_list, function(x) sum(predict(x, type='response')^2 /2*exp(-predict(x, type='response')))),digits=2)
real_two <- rep(length(which(data_dummy$Goals_scored==2)), times=3)
expected_threes <- round(sapply(models_list, function(x) sum(predict(x, type='response')^3 /6*exp(-predict(x, type='response')))), digits=2)
real_three <- rep(length(which(data_dummy$Goals_scored==3)), times=3)



data_frame_results <- data.frame('Model' = models_names, 'AIC' = AIC, 'BIC'=BIC, `Number of coefficients` = num_coef)

data_frame_expected = data.frame(`Expected zero goals` = expected_zeros, 
                                 `Zero goals` = real_zeros,
                                 `Expected one goal` = expected_one, 
                                 `One goal` = real_ones,
                                 `Expected two goals` = expected_two,
                                 `Two goals` = real_two, 
                                 `Expected_threes` = expected_threes,
                                 `Three goals` = real_three)
kable(data_frame_results)
kable(data_frame_expected) 

data_test <- readxl::read_excel("premieleague_newseason.xlsx")

data_test <- data_test %>% separate(Result, c("Home team goals", "Away team goals"), sep = '-')
data_test <- data_test %>% mutate(`Home team at home` = 'Yes') %>% mutate(`Away team at home` = "No")

data_away2 <- data_test %>% dplyr::select(`Day`,`Round Number`, Location, `Away Team`, `Home Team`, `Away team at home`, Referee, `Away team goals`, `Away team manager`, `Home team manager`,
                                    `Away team elo`, `Home team elo`, `Away team formation`, `Home team formation`, `Days from last game away`, `Days from last game home team`,
                                    `Newly promoted away team`,`Newly promoted home team`,`Top six club away team`, `Top six club home team`, `Home team goals`
)
data_home2 <- data_test %>% dplyr::select(`Day`,`Round Number`,  Location, `Home Team`, `Away Team`, `Home team at home`, Referee, `Home team goals`, `Home team manager`, `Away team manager`,
                                    `Home team elo`, `Away team elo`, `Home team formation`, `Away team formation`, `Days from last game home team`, `Days from last game away` ,
                                    `Newly promoted home team`,`Newly promoted away team`,`Top six club home team`,`Top six club away team`, `Away team goals`
)


colnames(data_away2) <- c("Day","Round Number", "Location", "Team", "Rival", "At home", "Referee", "Goals scored", "Team manager", "Rival's manager",
                         "Elo points", "Rival's elo points", "Formation", "Rival's formation", "Days from last game", "Days from rival's last game",
                         "Newly promoted", "Newly promoted rival", "Top six club", "Top six team rival", "Goals conceded")

colnames(data_home2) <- colnames(data_away2)



data_test <- rbind(data_home2, data_away2)

data_test <- data_test %>%
  mutate(
    Best_three = if_else(Team %in% c('Liverpool', 'Arsenal', 'Man City'), 'Yes', 'No')
  )

data_test <- data_test %>%
  mutate(
    Best_three_rival = if_else(Rival %in% c('Liverpool', 'Arsenal', 'Man City'), 'Yes', 'No')
  )

data_test <- data_test %>%
  mutate(`Goals scored` = as.numeric(`Goals scored`),
         `Goals conceded` = as.numeric(`Goals conceded`))



data_test <- data_test %>% 
  mutate_if(is.character, as.factor)


data_test <- data_test %>%
  mutate(`Round Number` = as.integer(as.character(`Round Number`)))






data_test <- data_test %>%
  arrange(Team, `Round Number`) %>%        
  group_by(Team) %>%               
  mutate(
    Goals_lag_conceded = lag(`Goals conceded`),         
    Goals_last5_conceded = rollapplyr(
      Goals_lag_conceded,                
      FUN = sum,
      partial = TRUE           
    )
  ) %>%
  ungroup()





lost_goals <- c(3,1,
                2,2,
                4,7,
                4,7,
                3,4,
                2,4,
                2,0,
                3,3,
                1,1,
                7,5,
                1,0,
                6,6,
                1,1,
                3,2,
                2,2,
                4,3,
                8,6,
                3,2,
                3,6,
                7,9)
data_test <- data_test %>%
  mutate(
    Goals_last5_conceded = {
      na_idx <- which(is.na(Goals_last5_conceded))
      Goals_last5_conceded[na_idx] <- lost_goals[seq_along(na_idx)]
      Goals_last5_conceded
    }
  )


data_laczenie2 <- data_test %>% 
  dplyr::select(`Round Number`, Team, Rival, Goals_last5_conceded)


data_test <- data_test %>%
  left_join(data_laczenie2, by=c("Round Number" = "Round Number", "Rival" = "Team", "Team" = "Rival"))


data_test <- data_test %>%
  dplyr::select(-Goals_last5_conceded.x, -Goals_lag_conceded) %>%
  mutate(Goals_last5_conceded_rival = Goals_last5_conceded.y) %>%
  dplyr::select(-Goals_last5_conceded.y)




data_test <- data_test %>% dplyr::select(-`Team manager`,-Day,-Location, -`Rival's manager`, -Referee)






data_test <- data_test %>%
  mutate(`Days from last game` = case_when(
    as.character(`Days from last game`) %in% c('3','4') ~ "Short break",
    as.character(`Days from last game`) %in% c('5','6','7','8','13','14','15') ~ "Normal break",
    `Days from last game` == 'start of the season' ~ "Normal break",
    as.character(`Days from last game`) %in% c('9','10','11','12','16','17','18') ~ "Normal break",
    TRUE ~ "Short break"  
  ))


data_test <- data_test %>%
  mutate(`Days from rival's last game` = case_when(
    as.character(`Days from rival's last game`) %in% c('3','4')~ "Short break",
    as.character(`Days from rival's last game`) %in% c('5','6','7','8','13','14','15') ~ "Normal break",
    `Days from rival's last game` == 'start of the season' ~ "Normal break",
    as.character(`Days from rival's last game`) %in% c('9','10','11','12','16','17','18') ~ "Normal break",
    TRUE ~ as.character(`Days from last game`)
  ))





data_test <- data_test %>%
  mutate(Formation = case_when(
    Formation %in% c('4-3-3', '4-2-3-1','4-2-2-2',  '4-4-2', '4-1-4-1',  '4-5-1', '3-2-4-1', '3-4-1-2') ~ "more_popular",
    Formation %in% c('5-4-1', '5-3-2','4-4-1-1','4-3-1-2','4-1-2-1-2','3-5-2', '3-4-2-1', '3-4-3') ~ "less_popular"
  ))


data_test <- data_test %>%
  mutate(`Rival's formation` = case_when(
    `Rival's formation` %in% c('3-5-2', '3-4-2-1', '3-4-1-2', '3-2-4-1','4-3-3', '4-2-3-1','4-2-2-2',  '4-1-2-1-2','4-4-2', '4-1-4-1', '3-4-3') ~ "more_popular",
    `Rival's formation` %in% c('5-4-1', '5-3-2', '4-3-1-2', '4-4-1-1', '4-5-1') ~ "less_popular"
  ))



data_test <- data_test %>% 
  mutate(`Round Number` =  as.numeric(`Round Number`))

data_test <- data_test %>% 
  mutate_if(is.character, as.factor)

data_test <- data_test %>%
  mutate(`Elo difference`= `Elo points`-`Rival's elo points`)



data_test <- data_test %>%
  arrange(Team, `Round Number`) %>%         
  group_by(Team) %>%             
  mutate(
    Goals_lag = lag(`Goals scored`),         
    Goals_last5 = rollapplyr(
      Goals_lag,               
      width = 3,
      FUN = sum,
      partial = TRUE        
    )
  ) %>%
  ungroup()


replacement_values <- c(5,4,
                        3,2,
                        3,5,
                        4,4,
                        9,8,
                        10,7,
                        2,2,
                        7,5,
                        6,3,
                        4,4,
                        12,7,
                        5,7,
                        5,9,
                        2,2,
                        2,0,
                        4,5,
                        1,4,
                        5,6,
                        6,3,
                        3,0)

data_test <- data_test %>%
  mutate(
    Goals_last5 = {
      na_idx <- which(is.na(Goals_last5))
      
      Goals_last5[na_idx] <- replacement_values[seq_along(na_idx)]
      Goals_last5
    }
  )




data_test$Team
data_end <- data_test

data_test <- data_test %>%
  mutate(`Team` = case_when(
    `Team` %in% c('Crystal Palace','Leeds', 'Burnley','Everton', 'Sunderland',  'Man Utd',  'West Ham') ~ "Weak_off",
    `Team` %in% c('Aston Villa', 'Bournemouth', 'Fulham', "Nott'm Forest", 'Wolves') ~ "Av_off",
    `Team` %in% c('Arsenal', 'Brentford', 'Brighton', 'Chelsea', 'Liverpool', 'Man City', 'Newcastle', 'Spurs') ~ "Best_off"
  ))

data_test <- data_test %>%
  mutate(`Rival` = case_when(
    `Rival` %in% c('Leeds', 'Burnley', 'Sunderland') ~ "Rival_def_weak",
    `Rival` %in% c('Spurs', 'West Ham', 'Wolves') ~ "Rival_def_avg",
    `Rival` %in% c('Arsenal', 'Aston Villa','Brentford', 'Brighton', 'Chelsea', 'Liverpool', 'Man City', 'Newcastle', 'Bournemouth','Crystal Palace', 'Everton', 'Fulham', 'Man Utd', "Nott'm Forest") ~ "Rival_def_strong"
  ))
data_test$Team

data_test <- data_test %>%
  mutate(Goals_last5_conceded_rival = cut(
    Goals_last5,
    breaks = c(-Inf, 6,  Inf),
    labels = c("less_equal_6", "more_than_6")
  ))

data_test <- data_test %>%
  mutate(Goals_last5 = cut(
    Goals_last5,
    breaks = c(-Inf, 3,  Inf),
    labels = c("less_equal_3","more_than_3")
  ))










data_dummy2 <- dummy_cols(
  data_test,
  remove_first_dummy = FALSE,
  remove_selected_columns = TRUE   
)
colnames(data_dummy2)
data_dummy_test <- data_dummy2 %>%
  dplyr::select(
    -`Round Number`, 
    -`At home_No`, 
    -`Days from last game_Normal break`, 
    -`Days from rival's last game_Normal break`, 
    -`Newly promoted`,
    -`Newly promoted rival`,
    -`Rival's elo points`,
    -`Elo points`,
    -`Top six club`,
    -`Top six team rival`,
    -`Formation_less_popular`,
    -`Rival's formation_less_popular`,
    -Goals_lag,
    -`Goals_last5_less_equal_3`,
    -`Goals_last5_conceded_rival_less_equal_6`,
    -`Rival_Rival_def_avg`,
    -`Team_Av_off`,
    -`Best_three_No`,
    -`Best_three_rival_No`
  )
colnames(data_dummy)
colnames(data_dummy_test)

data_dummy_test <- data_dummy_test %>% mutate(
  Days_from_last_game_Short_break = 0,
  Days_from_rivals_last_game_Short_break = 0
)


colnames(data_dummy_test) <- gsub(" ", "_", colnames(data_dummy_test))
colnames(data_dummy_test) <- gsub("'", "", colnames(data_dummy_test))
model_int_data_test <- as.data.frame(model.matrix(`Goals_scored` ~ .^2, data=data_dummy_test)[, -1])
model_int_data_test <- model_int_data_test[,-grep("^Elo_difference:", colnames(model_int_data_test))]
zero_var_cols_test <- colnames(model_int_data)[apply(model_int_data, 2, function(x) var(x) == 0)]




plot(data_dummy_test$Goals_scored, predict(model_poisson2 ,newdata = model_int_data_test, type = 'response'), ylim = c(0,5), ylab = 'Predicted number of goals scored', xlab = 'Goals scored', pch=16)
abline(a=0, b=1, col = 'red', lwd = 2)

data_plot <- data.frame(
  obs = data_dummy_test$Goals_scored,
  pred = predict(model_poisson2, newdata = model_int_data_test, type='response')
)

data_summary <- data_plot %>%
  group_by(obs) %>%
  summarise(pred_mean = mean(pred), n = n())

plot(data_summary$obs, data_summary$pred_mean,
     ylim = c(0,5),
     xlab = 'Observed goals',
     ylab = 'Mean predicted goals',pch=16)
abline(a=0, b=1, col='red', lwd = 2)

