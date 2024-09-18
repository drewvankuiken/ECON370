### set up
# necessary packages
# install.packges(c("gapminder","L1pack","tidyverse"))
library(gapminder)
library(L1pack)
library(tidyverse)

### spring graphs - create geom_spring elements for graphs
create_spring <- function(x, 
                          y, 
                          xend, 
                          yend, 
                          diameter = 1, 
                          tension = 0.75, 
                          n = 50) {
  
  # Validate the input arguments
  if (tension <= 0) {
    rlang::abort("`tension` must be larger than zero.")
  }
  if (diameter == 0) {
    rlang::abort("`diameter` can not be zero.")
  }
  if (n == 0) {
    rlang::abort("`n` must be greater than zero.")
  }
  
  # Calculate the direct length of the spring path
  length <- sqrt((x - xend)^2 + (y - yend)^2)
  
  # Calculate the number of revolutions and points we need
  n_revolutions <- length / (diameter * tension)
  n_points <- n * n_revolutions
  
  # Calculate the sequence of radians and the x and y offset values
  radians <- seq(0, n_revolutions * 2 * pi, length.out = n_points)
  x <- seq(x, xend, length.out = n_points)
  y <- seq(y, yend, length.out = n_points)
  
  # Create and return the transformed data frame
  data.frame(
    x = cos(radians) * diameter/2 + x,
    y = sin(radians) * diameter/2 + y
  )
}

StatSpring <- ggproto("StatSpring", Stat,
                      
                      setup_data = function(data, params) {
                        if (anyDuplicated(data$group)) {
                          data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
                        }
                        data
                      },
                      
                      compute_panel = function(data, scales, n = 50) {
                        cols_to_keep <- setdiff(names(data), c("x", "y", "xend", "yend"))
                        springs <- lapply(seq_len(nrow(data)), function(i) {
                          spring_path <- create_spring(
                            data$x[i], 
                            data$y[i], 
                            data$xend[i], 
                            data$yend[i], 
                            data$diameter[i],
                            data$tension[i], 
                            n
                          )
                          cbind(spring_path, unclass(data[i, cols_to_keep]))
                        })
                        do.call(rbind, springs)
                      },
                      
                      required_aes = c("x", "y", "xend", "yend"),
                      optional_aes = c("diameter", "tension")
)

geom_spring <- function(mapping = NULL, 
                        data = NULL, 
                        stat = "spring", 
                        position = "identity", 
                        ..., 
                        n = 50, 
                        arrow = NULL, 
                        lineend = "butt", 
                        linejoin = "round", 
                        na.rm = FALSE,
                        show.legend = NA, 
                        inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = GeomPath, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(
      n = n, 
      arrow = arrow, 
      lineend = lineend, 
      linejoin = linejoin, 
      na.rm = na.rm, 
      ...
    )
  )
}

### spring graphs: graph the actual data
gp_subset <- gapminder[gapminder$continent=="Asia"&gapminder$year==2007,]
g <- ggplot(gp_subset,aes(x=gdpPercap, y=lifeExp)) + 
  geom_point()

#
res <- lm(lifeExp~gdpPercap, data=gp_subset)
gp_subset$newline = predict(res, gp_subset[,"gdpPercap"]) + 10

ggplot(gp_subset,aes(x=gdpPercap, y=lifeExp)) + 
  geom_point() + 
  geom_line(aes(x=gdpPercap,y=newline), colour="blue", linewidth = 1.5)
#

#
res <- lm(lifeExp~gdpPercap, data=gp_subset)
res$coefficients[2] = res$coefficients[2]/2
gp_subset$newline2 = predict(res, gp_subset[,"gdpPercap"])

ggplot(gp_subset,aes(x=gdpPercap, y=lifeExp)) + 
  geom_point() + 
  geom_line(aes(x=gdpPercap,y=newline2), colour="blue", linewidth = 1.5)
#

#
lad_res <- lad(lifeExp~gdpPercap, data=gp_subset)
gp_subset$newline3 = predict(lad_res, gp_subset[,"gdpPercap"])

ggplot(gp_subset,aes(x=gdpPercap, y=lifeExp)) + 
  geom_point() + 
  geom_line(aes(x=gdpPercap,y=newline3), colour="blue", linewidth = 1.5)
#

#
res <- lm(lifeExp~gdpPercap, data=gp_subset)
gp_subset$newline = predict(res, gp_subset[,"gdpPercap"]) + 10

ggplot(gp_subset |> 
         mutate(gdpPercap=gdpPercap/1000),
       aes(x=gdpPercap, y=lifeExp)) + 
  geom_point() + 
  geom_line(aes(x=gdpPercap,y=newline), colour="blue", linewidth = 1.5) +
  geom_spring(aes(x=gdpPercap,y=lifeExp,xend=gdpPercap, yend=newline, 
                  tension=abs(newline-lifeExp)*0.2,diameter=0.5), 
              colour="grey") +
  annotate("point",x=mean(gp_subset$gdpPercap)/1000,y=mean(gp_subset$lifeExp), 
           colour="blue",size=5)
#

#
res <- lm(lifeExp~gdpPercap, data=gp_subset)
gp_subset$newline = predict(res, gp_subset[,"gdpPercap"]) + 10
ggplot(gp_subset |> 
         mutate(gdpPercap=gdpPercap/1000),
       aes(x=gdpPercap, y=lifeExp)) + 
  geom_point() + 
  geom_line(aes(x=gdpPercap,y=newline), colour="blue", linewidth = 1.5) +
  geom_spring(aes(x=gdpPercap,y=lifeExp,xend=gdpPercap, yend=newline, 
                  tension=abs(newline-lifeExp)*0.2,diameter=0.5),
              colour = "grey") +
  annotate("point",x=mean(gp_subset$gdpPercap)/1000,y=mean(gp_subset$lifeExp), 
           colour="blue",size=5)
#

#
gp_subset$mline = ((gp_subset$gdpPercap)/1000*(-.744)+80)
ggplot(gp_subset |> 
         mutate(gdpPercap=gdpPercap/1000),
       aes(x=gdpPercap, y=lifeExp)) + 
  geom_point() + 
  geom_abline(intercept=80,slope=-.744, colour="blue", linewidth = 1.5) +
  geom_spring(aes(x=gdpPercap,y=lifeExp,xend=gdpPercap, yend=mline, 
                  tension=abs(mline-lifeExp)*0.2,diameter=0.5), 
              colour="grey") +
  annotate("point",x=mean(gp_subset$gdpPercap)/1000,y=mean(gp_subset$lifeExp), 
           colour="blue",size=5)
#

#
res <- lm(lifeExp~gdpPercap, data=gp_subset)
gp_subset$newline = predict(res, gp_subset[,"gdpPercap"])

ggplot(gp_subset |> 
         mutate(gdpPercap=gdpPercap/1000) |> 
         filter(lifeExp>50),
       aes(x=gdpPercap, y=lifeExp_alt)) + 
  geom_point() + 
  geom_smooth(method='lm',se=F, colour = "#f68f46") +
  geom_spring(aes(x=gdpPercap,y=lifeExp,xend=gdpPercap, yend=newline, 
                  tension=abs(newline-lifeExp)*0.2,diameter=0.5), 
              colour="grey") +
  geom_smooth(data=gp_subset, aes(x=(gdpPercap)/1000,y=lifeExp),method='lm', 
              colour = "blue", se = F)
#

#
head(gp_subset[gp_subset$lifeExp<=50,1:6])
#


# Omitted Variables Bias illustration
# slope of relationship without Afghanistan
gp_no_afg <- gp_subset[gp_subset$country!="Afghanistan",]

Nsim <- 9999
coefs <- rep(0,Nsim+1)
Nr <- nrow(gp_no_afg)

# regression for base data
res <- lm(lifeExp ~ gdpPercap, data = gp_no_afg)
coefs[1] = res[[1]][2] # retrieve coefficient for base regression, no countries at war
# note that res[[1]] returns a vector of coefficients. we can ignore intercept

# regressions for simulated war data. basic idea: 
# send country to war: lower life expectancy of randomly selected country by 20
# run regression for dataset with country at war, record slope
# resample Nsim times
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

# Calculating OVB:
res_with_ovb <- lm(lifeExp ~ gdpPercap, data = gp_subset)
ovb_coef = res_with_ovb[[1]][2] # retrieve coefficient for base regression, no countries at war

gp_subset$long_and_brutal_war = gp_subset[,"country"] == "Afghanistan"
res_without_ovb <- lm(lifeExp ~ gdpPercap + long_and_brutal_war, data = gp_subset)
no_ovb_coef = res_without_ovb[[1]][2]

print(cbind(ovb_coef,no_ovb_coef, ovb_coef-no_ovb_coef))











