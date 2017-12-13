
  #load data
  buli <- read.csv("C:\\Users\\nikla\\Documents\\buli.csv", header=TRUE)
  
  #coming games in a matrix
  teams <- c("SC Freiburg", "Bayern München", "TSG 1899 Hoffenheim", "Werder Bremen", "Bayer 04 Leverkusen",
            "1. FSV Mainz 05", "FC Schalke 04", "Hertha BSC", "VfL Wolfsburg","Hamburger SV", "Hannover 96",
            "RB Leipzig", "VfB Stuttgart", "Borussia Dortmund", "FC Augsburg", "1. FC Köln", "Eintracht Frankfurt",
            "Borussia Mönchengladbach")
  teams <- matrix(teams,nrow = 9,ncol = 2)
  print(teams)
  
  #fit poisson model and get a summary
  model <- glm(GoalsHome ~ GoalsAway + Home + Away, family=poisson(link=log), data=buli)
  print(summary(model))
  
  #probabilites of average goals scored
  for(i in teams){
    pH <- predict(model, data.frame(GoalsAway=1, Home=i[,1], Away=i[,2]), type="response")
    pA <- predict(model, data.frame(GoalsAway=0, Home=i[,2], Away=i[,1]), type="response")
  }
  print(pH)
  print(pA)
  
  
  #simulation with 10.000 possible results based on the probabilities
  set.seed(915706074)
  nsim <- 10000
  homeGoalsSim <- rpois(nsim, predictHome) 
  awayGoalsSim <- rpois(nsim, predictAway)
  goalDiffSim <- homeGoalsSim - awayGoalsSim
  #Home
  homeprob <- sum(goalDiffSim > 0) / nsim
  print(homeprob)
  #Draw
  drawprob <- sum(goalDiffSim == 0) / nsim
  print(drawprob)
  #Away
  awayprob <- sum(goalDiffSim < 0) / nsim
  print(drawprob)
