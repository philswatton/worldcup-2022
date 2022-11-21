# GV2022 World Cup Predicitons

I entered a contest to try and predict the 2022 Fifa World Cup with very little prior knoweldge. You can read a blog post about my attempt here: <https://philswatton.github.io/r/2022/11/16/2022-world-cup-prediction.html>


## Files

- **01_elo.R**: Processing of data, computation of elo ratings for the teams (see note below on data)
- **02_monte-carlo.R**: Monte Carlo simulation of the tournament
- **03_results.R**: Producing outputs from the simulation into nicer formats


## Data

- **Past match results data**: https://www.kaggle.com/datasets/martj42/international-football-results-from-1872-to-2017?resource=download
- **2022 fixtures data**: https://fixturedownload.com/
- **team_ratings.RData** elo ratings data produced in script 01
- **fixtures.RData**: slightly tidied fixture data for group stages produced in script 01

As in the code, I stored the files from these in a 'data' folder. If using this repository, you will have to download the data and create this yourself. (Sorry!)

I have however included data I generated myself. So you'll need to reproduce the data folder for script 01, but


## Results

- **results.RData**: Various R objects encoding simulation results used in script 03
- **results/results_01_group_fixtures.csv**: Group stages game fixture probabilities
- **results/results_02_expected_group_order.csv**: Group stages order predictions and probabilities (for most likely order)
- **results/results_03_expected_team_performances.csv**: Expected team performances given in probabilities for reaching the last 16, reaching the quarter finals, reaching the semi finals, reaching the final, and winning overall
- HTML versions of results 02 and 03 are also available!


