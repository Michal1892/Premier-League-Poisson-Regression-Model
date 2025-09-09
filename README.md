# ⚽ Premier League Goal Prediction – Poisson Regression Model  

This repository contains a project focused on **predicting the number of goals in Premier League matches** using statistical modeling.  

## 📊 Methodology  

The core idea is to apply **Poisson regression** to estimate the expected number of goals scored by each team in a match. The model is trained on features that capture both team strength and match context:  

- **Elo rating** of each team  
- **Home advantage** (whether the team is playing at home)  
- **Formation used** in the match  
- **Rest days** (number of days since the previous match)  
- **Goals scored and conceded in the last 3 matches** (form indicator)  

Several models were proposed and compared during experimentation. After evaluation, the **Poisson regression model** was chosen as the best-performing approach in this context.  

## 🧪 Evaluation  

The final model was tested on **Premier League 2025/26 data**, checking its performance on match results **up to and including Matchweek 2**.  
