# Omitted Variables Bias illustration
gp_no_afg <- gp_subset[gp_subset$country!="Afghanistan",]

Nsim <- 9999
coefs <- rep(0,Nsim+1)
Nr <- nrow(gp_no_afg)

res <- lm(lifeExp ~ gdpPercap, data = gp_no_afg)
coefs[1] = res[[1]][2] 


# sample version
temp_idx <- sample(nrow(gp_no_afg),1)

temp_df <- gp_no_afg
temp_df[temp_idx,"lifeExp"] <- gp_no_afg[temp_idx,"lifeExp"]-20

ggplot(temp_df, aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  geom_smooth(method='lm', se=F, colour = "#f68f46") + 
  geom_smooth(data=gp_no_afg, aes(x=gdpPercap, y=lifeExp), colour = "blue", 
              method='lm', se = F)

# full version
war_reg <- function(x, input_df){ # x is unused, kept as argument to make sapply work
  war_country <- sample(nrow(input_df),1)
  
  gp_war <- input_df
  gp_war[war_country,"lifeExp"] <- input_df[war_country,"lifeExp"]-20
  
  res <- lm(lifeExp ~ gdpPercap, data = gp_war)
  coef <- res[[1]][2]
  return(coef)
}

coefs[2:(Nsim+1)] <- sapply(1:Nsim, war_reg, input_df = gp_no_afg)

coefs[1]
mean(coefs[2:Nsim+1])
