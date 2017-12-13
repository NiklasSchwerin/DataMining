{
  #load data
  buli <- read.csv("C:\\Users\\nikla\\Documents\\DAMI Project\\RapidMiner Processes\\Comparison ML and Statistical\\buli.csv", header=TRUE)
  
  #fit poisson model and get a summary
  model <- glm(GoalsHome ~ GoalsAway + Home + Away, family=poisson(link=log), data=buli)
  print(summary(model))
  
  #probabilites of average goals scored
  #Leverkusen
  predictHome <- predict(model, data.frame(GoalsAway=1, Home="Borussia Mönchengladbach", Away="Hamburger SV"), type="response")
  print(predictHome)
  #Ingolstadt
  predictAway <- predict(model, data.frame(GoalsAway=0, Home="Hamburger SV", Away="Borussia Mönchengladbach"), type="response")
  print(predictAway)
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
  print(awayprob)
}
