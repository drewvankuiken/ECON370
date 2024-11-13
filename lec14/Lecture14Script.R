##-----

U_CD = function(xs,alphas){
  prod(xs^alphas)
}

x1Solve = function(x2,ps,w){(w-x2*ps[2])/ps[1]}

f_opt = function(x2,ps,w,U){
  x1 = x1Solve(x2,ps,w)   # use BC to solve for x1 as a function of x2, ps, w
  -U(c(x1,x2))            # get negative U(x1,x2) to solve problem via minimize
}

## ---- Set parameters and solve UMP
income   = 100           # set income
prices   = c(2,5)        # set prices
alphs    = c(1/3,2/3)    # set alphas
x2_guess = (0+income/prices[2])/2 # set first guess of x2 to be midpoint of [0,w/p2]
Uopt = function(x){U_CD(x,alphs)}

optoutCD = optim(x2_guess,fn=f_opt,
                 ps=prices,w=income,U=Uopt,
                 method="Brent",lower=0,upper=income/prices[2])

x2star = optoutCD$par # store optimal value of x2
xOptCD = c("x1_star"= x1Solve(x2star,prices,income), # calc optimal x1 using BC
           "x2_star"= x2star) 

alphs*(income/prices) 
xOptCD


x1Solve = function(x2,ps,w){(w-x2*ps[2])/ps[1]}
x1Solve = function(xsm1,ps,w){(w-sum(xsm1*ps[-1]))/ps[1]}


##----

U_QL = function(x,qlid=1,v=function(x){sqrt(prod(x))}) {  # Quasi-linear
  sum(x[qlid])+v(x[-qlid])} 

## ---- Set parameters and solve UMP
w        = 100           # set income
ps       = c(5,2)        # set prices
x2_guess = (0+w/ps[2])/2 # set first guess of x2 to be midpoint of [0,w/p2]

optoutQL = optim(x2_guess,fn=f_opt,
                 ps=ps,w=w,U=function(x){U_QL(x)},
                 method="Brent",lower=0,upper=w/ps[2])

x2star = optoutQL$par # store optimal value of x2
xOptQL = c("x1_star"= x1Solve(x2star,ps,w), # calc optimal x1 using BC
           "x2_star"= x2star)               # add optimal x2

xOptQL

x2_410 = (ps[1]/(ps[2]*2))^2    # solve x2 using 410 formula
c(x1Solve(x2_410,ps,w), x2_410) # optimal bundle with 410 formulas

##-----

## ---- Function to solve for x1
x1Solve = function(xsm1,ps,w){(w-sum(xsm1*ps[-1]))/ps[1]}

## ---- Objective function to minimize
f_opt = function(xsm1,ps,w,U){
  x1 = x1Solve(xsm1,ps,w) 
  -U(c(x1,xsm1))          
}

## ---- Set parameters and solve UMP for Cobb Douglas
w      = 100            
ps     = c(5,2,3)       
alphas = c(1/7,2/7,4/7) 
x_guess = rep(1,2)      

optoutCD3 = optim(x_guess,fn=f_opt,
                  ps=ps,w=w,U=function(x){U_CD(x,alphas)},
                  method="L-BFGS-B",lower=rep(0,2),upper=w/ps[-1])

xstar = optoutCD3$par 
xOptCD3 = c(x1Solve(xstar,ps,w), 
            xstar)               
names(xOptCD3) = paste0("x",1:length(ps),"_star")
xOptCD3

##-------

SolveDemand = function(ps,w,U){
  # ps are prices, w is income, and U is the utility function
  ## ---- Set parameters for optimization problem
  Np           = length(ps) 
  methd        = "L-BFGS-B" 
  methd[Np==2] = "Brent"    
  ubs = w/ps[-1]            
  lbs = rep(0,Np-1)         
  
  ## ---- Solve optimization problem
  optout = optim(rep(1,Np-1),fn=f_opt, ps=ps,w=w,U=U,
                 method=methd,lower=lbs,upper=ubs)
  
  ## ---- Format output 
  xOpt   = c(x1Solve(optout$par,ps,w),optout$par) # create optimal x bundle       
  names(xOpt) = paste0("x",1:Np)          # set names of xOpt
  xOpt
}

SolveDemand(c(5,2,3),100,U=function(x){U_CD(x,c(1/7,2/7,4/7))})
SolveDemand(c(5,2,3),200,U=function(x){U_CD(x,c(1/7,2/7,4/7))})
SolveDemand(c(2,5,4),100,U=function(x){U_CD(x,c(1/7,2/7,4/7))})
SolveDemand(c(5,2,3,4),100,U=function(x){U_CD(x,c(1/9,2/9,4/9,2/9))})

SolveDemand(c(5,2),100,U=function(x){U_QL(x)})
SolveDemand(c((5/2)*75,75),100,U=function(x){U_QL(x)})
SolveDemand(c(5,2,3),100,U=function(x){U_QL(x)})
SolveDemand(c(5,2,3,5),100,U=function(x){U_QL(x,qlid=c(1,4))})

##--------

segdata = data.table(x=c(0,0),y=pointdata$y,
                     xend=c(5,5),yend=c(pointdata$y),
                     funct=factor(flabs,levels=flabs))
MaxMinPlot + geom_segment(data=segdata,aes(x=x,y=y,xend=xend,yend=yend,
                                           color=funct),linetype="dashed")

f  = function(x){5-x^2+5*x} # define f
xs = seq(-2,6.5,0.1)        # form x's
ys = f(xs)                  # form f(x)'s

## ---- Find xstar and f(xstar)
ub   = 1000
xopt = optim(0,fn=function(x){-f(x)},
             method="Brent",lower=-ub,upper=ub)$par
fxopt = f(xopt)

h = function(x){(x-xopt)^3}
hxs = h(xs) 
hxopt = h(xopt)

## ---- Create data for plot
Nx    = length(xs)
flabs = c("f(x)","-f(x)","h(x)")

# make function data
functdata = data.table(x=c(xs,xs,xs),y=c(ys,-ys,hxs),
                       funct=rep(flabs,each=Nx))
# make point data
pointdata = data.table(x=rep(xopt,3),
                       y=c(fxopt,-fxopt,hxopt),
                       funct=flabs)
# make sure funct levels are in right order
functdata[,funct:=factor(funct,levels=flabs)]
pointdata[,funct:=factor(funct,levels=flabs)]

segdata = data.table(x=c(0,0,0),y=pointdata$y,
                     xend=c(5,5,5),yend=c(pointdata$y),
                     funct=factor(flabs,levels=flabs))

MaxMinPlot = ggplot(functdata,aes(x=x,y=y,color=funct)) + 
  geom_line() + geom_point(data=pointdata) +
  geom_segment(data=segdata,aes(x=x,y=y,xend=xend,yend=yend,
                                color=funct),linetype="dashed") +
  geom_text(inherit.aes = F,x = xopt, y = 12.5, 
            label = TeX(r"($f(x)=5+5x-x^2$)", output = "character"),
            parse = TRUE)+ 
  geom_text(inherit.aes = F,x = xopt, y = 1.5, 
            label = TeX(r"($h(x)=(x-x^*)^3$)", output = "character"),
            parse = TRUE)+ 
  scale_color_discrete(name="") + 
  coord_cartesian(ylim=c(-14,14))+
  theme_minimal()+theme(legend.position = "right")
MaxMinPlot

##------

f=function(x){x*sin(x)}
xs=seq(-10,10,0.1)
ys = f(xs)
cps = c(0,7.97866571241324,4.91318043943488,2.02875783811043)
cps = sort(unique(c(cps,-cps)))

linedata=data.table(x=xs,y=ys)
pointdata=data.table(x=cps,y=f(cps))

ggplot(linedata,aes(x=x,y=y))+
  geom_line()+geom_point(data=pointdata)+
  geom_text(inherit.aes = F,x = 0, y = -1, 
            label = TeX(r"($f(x)=x\sin(x)$)", output = "character"),
            parse = TRUE)+
  coord_cartesian(ylim=c(min(ys),max(ys)),xlim=c(min(xs),max(xs)))+
  theme_minimal()


##-----

library(wesanderson)
f = function(x){5+5*x-x^2}
fp = function(x){5-2*x}
g = function(x,h){(f(x+h)-f(x))/h}
y_pntslp = function(x,x1,y1,m){
  y1+m*(x-x1)
}
xs = seq(-5,5,0.05)
fxs = f(xs)
x0 = -2.5
fpx0 = fp(x0)
hs   = c(0.5,2.5,5)
gxhs = g(x0,hs)
y0 = y_pntslp(0,x0,f(x0),c(fpx0,gxhs))

fdat = data.table(x=xs,y=fxs,h="f(x)")
segdata = data.table(inter=y0,slopes=c(fpx0,gxhs),
                     "h"=c("f'(x)",paste0("h=",hs)))

cols=wes_palette("Darjeeling1",length(hs)+1)
cols=alpha(c("black",cols),1)
names(cols) = c("f(x)","f'(x)",paste0("h=",hs))

fdat[,h:=factor(h,c("f(x)","f'(x)",paste0("h=",hs)))]
segdata[,h:=factor(h,c("f(x)","f'(x)",paste0("h=",hs)))]

plotdata=rbind(fdat,segdata,fill=T)
plotdata[,h:=factor(h,c("f(x)","f'(x)",paste0("h=",hs)))]

ggplot(plotdata) + 
  geom_line(aes(x=x,y=y,color=h)) + 
  geom_abline(aes(intercept=inter,slope=slopes,color=h)) + 
  geom_text(inherit.aes = F,x = -1.25, y = -15, 
            label = TeX(r"($f(x)=5+5x-x^2$)", output = "character"),
            parse = TRUE)+
  
  scale_color_manual(name="",values = cols)+
  theme_minimal()

##------

f            = function(x){x^2}        #make function f(x)
fp           = function(x){2*x}        #make f'(x)
xs           = seq(0,2,0.5)            #store x's
h            = 0.01                    #store step size
der          = fp(xs)                  #calc actual derivatives
center_der   = (f(xs+h)-f(xs-h))/(2*h) #calc center differenced
forward_der  = (f(xs+h)-f(xs))/(h)     #calc forward differenced
backward_der = (f(xs)-f(xs-h))/(h)     #calc backwards differenced
deriv_data   = data.table("f'(x)"=der,"Center"=center_der,
                          "Foward"=forward_der,"Backward"=backward_der)
deriv_data

##------

f            = function(x){x[1]^2+sin(x[2])}        #make function f(x)
fp           = function(x){c(2*x[1],cos(x[2]))}     #make gradient of f(x)
h            = 0.01                                 #store step size
fp(c(1,0.5))
c((f(c(1+h,0.5))-f(c(1-h,0.5)))/(2*h),(f(c(1,0.5+h))-f(c(1,0.5-h)))/(2*h))
c((f(c(1+h,0.5))-f(c(1,0.5)))/h,(f(c(1,0.5+h))-f(c(1,0.5)))/h)
c((f(c(1,0.5))-f(c(1-h,0.5)))/h,(f(c(1,0.5))-f(c(1,0.5-h)))/h)

##-------

fa = function(x,a) x^2-a
a  = 2
xn = a
eps = 1e-15

xnp1 = 0.5*(xn+a/xn)
while(abs(fa(xnp1,a))>eps){
  xn   = xnp1
  xnp1 = 0.5*(xn+a/xn)
}
c("Newtons"=xnp1,"sqrt_fun"=sqrt(a))

##-----

FindRootsNewton = function(x0,f,...,eps=1e-15,MaxIt = 10000,h=0.01){
  
  f2 = function(x){f(x,...)}; n = 1; xn = x0
  gr2 = function(x) (f2(x+h)-f2(x-h))/(2*h) 
  if(abs(f2(x0))<eps){
    x0
  }else{
    xnp1 = xn - f2(xn)/gr2(xn)
    while(abs(f2(xnp1)) >= eps & n <= MaxIt){
      n    = n+1; xn = xnp1      # advance counter and update xn with xnp1
      xnp1 = xn - f2(xn)/gr2(xn) # update xnp1
    }
    warntxt = "Max iterations reached."
    warntxt = paste(warntxt,"|f(xnp1)|=",abs(f2(xnp1)))
    if(abs(f2(xnp1)) >= eps) warning(warntxt)
    xnp1 # return xnp1
  }
}

## --- create grid of as and a function fapp to pass to sapply
as = seq(0,10,0.5)
fsqrta = function(xa) FindRootsNewton(x0=xa,f=function(x,a){x^2-a},a=xa)
## --- calc sqrt(as) using Newton's Method and built-in sqrt function
sqrtnewt = sapply(as,fsqrta); sqrtfun  = sqrt(as) 
## --- give names to elements of sqrtnewt/sqrtfun & return summary stats
names(sqrtnewt) = paste0("a=",as); names(sqrtfun)=names(sqrtnewt)
summary(sqrtnewt-sqrtfun)

##------

plotdata = data.table("sqrt_fun"=sqrtfun,"sqrt_newt"=sqrtnewt)
ggplot(plotdata,aes(x=sqrt_fun,y=sqrt_newt)) + 
  geom_point(aes(color="Points"))+
  geom_abline(aes(color="45 Degree Line",slope=1,intercept=0))+
  scale_color_manual(name="",values=c("Points"="dodgerblue","45 Degree Line"="black"))+
  xlab("sqrt function")+ylab("sqrt Newton") + ggtitle("Comparing Newton's Sqrt Method with R Sqrt Function")+
  theme_minimal()+theme(plot.title = element_text(hjust=0.5))

##------

x1 = seq(0,5,0.05)
x2 = seq(0,5,0.05)
x3 = seq(0,5,0.05)
nrow(expand.grid(x1,x2))
nrow(expand.grid(x1,x2,x3))

NewtonOpt = function(x0,f,...,h=0.01,eps=1e-15,MaxIt=10000){
  ## --- Set initial x, counter, g(x)=f'(x), and g'(x)=f''(x)
  xn = x0; n = 1 
  g  = function(x){(f(x+h,...)-f(x-h,...))/(2*h)}
  gp = function(x){(g(x+h)-g(x-h))/(2*h)}
  
  ## ---- Code for Newton's Method
  xnp1 = xn - g(xn)/gp(xn) # update x_n to x_{n+1}
  while(abs(f(xnp1)) >= eps & n <= MaxIt){
    xn = xnp1; n = n + 1 # update initial guess and counter
    xnp1 = xn - g(xn)/gp(xn) # update x_n to x_{n+1}
  }
  warntxt = "Max iterations reached."
  warntxt = paste(warntxt,"|f'(xnp1)|=",abs(g(xnp1)))
  
  ## ---- Test for Convergence
  if(abs(f(xnp1))>=eps){
    warning(warntxt)
  }
  xnp1 # return xn1
}

## --- Example Using Different x0 values and f(x)=5+5x-x^2
x0s = seq(-5,3,0.5)                            # grid of x0 vals
sapply(x0s,NewtonOpt,f=function(x){5+5*x-x^2}) # min f(x) with each x0

f = function(x){x*sin(x)}
xs = seq(-10,10,0.1)
ys = f(xs)
cps = c(0,7.97866571241324,4.91318043943488,2.02875783811043)
cps = sort(unique(c(cps,-cps)))

linedata=data.table(x=xs,y=ys)
pointdata=data.table(x=cps,y=f(cps))

ggplot(linedata,aes(x=x,y=y))+
  geom_line()+geom_point(data=pointdata)+
  geom_text(inherit.aes = F,x = 0, y = -1, 
            label = TeX(r"($f(x)=x\sin(x)$)", output = "character"),
            parse = TRUE)+
  coord_cartesian(ylim=c(min(ys),max(ys)),xlim=c(min(xs),max(xs)))+
  theme_minimal()



##-----

fxsin          = function(x){x*sin(x)}         # create f(x)=x*sin(x)
x0s            = seq(-9,9,0.75)                # create grid of x0's
crtpnts        = sapply(x0s,NewtonOpt,f=fxsin) # min f(x) using each x0
names(crtpnts) = paste0("x0=",x0s)             # give names to crtpnts as x0s
round(crtpnts, digits = 2)                     # round crtpnts for display

f       = function(x) x*sin(x)
crtpnts = round(crtpnts,digits=10)
crtpnts = sort(unique(crtpnts))
round(crtpnts,digits=3)

fctpt = f(crtpnts)
vals  = crtpnts[c(which.min(fctpt),which.max(fctpt))]
names(vals) = c("x_star_min","x_star_max")
round(fctpt,digits=2)
vals

##--------

x = advert_data$TV
y = advert_data$Sales

f = function(theta) sum( (y - theta[1]-theta[2]*x)^2)

theta0 = c(mean(y),0)
optim_out = optim(par=theta0,fn = f,method="BFGS")

optim_out
coef(lm(Sales~TV,data=advert_data))

theta0    = c(mean(y),0)
optim_out = optim(par=theta0,fn = f,method="Nelder-Mead")
optim_out

# requires the numDeriv package
f      = function(theta) sum((y-theta[1]-theta[2]*x)^2) # define f
fgr    = function(theta) numDeriv::grad(f,theta)        # define grad of f
theta0 = c(mean(y),0)                                   # set theta0
optim(par=theta0,fn = f,gr=fgr,method="BFGS")           # solve opt problem


x1 = advert_data$TV        # store TV var in data as x1
x2 = advert_data$Radio     # store Radio var in data as x2
x3 = advert_data$Newspaper # store Newspaper var in data as x3

f = function(theta){ # define objective function for multivariate regression
  sum( (y - theta[1] - theta[2]*x1 - theta[3]*x2 - theta[4]*x3)^2  )}

fgrad     = function(theta) numDeriv::grad(f,theta)         # define grad of f
theta0    = c(mean(y),rep(0,3))                             # set theta0  
optimBFGS = optim(par=theta0,fn = f,gr=fgrad,method="BFGS") # solve using BFGS
optimNM   = optim(par=theta0,fn = f,gr=fgrad)               # solve with Nelder-Mead

coef(lm(Sales~TV+Radio+Newspaper,data=advert_data)) # solution from OLS formula
optimBFGS$par # solution using optim with BFGS
optimNM$par # solution using optim with Nelder-Mead










## -----
f = function(x){5+5*x-x^2}
xx = -2.5
h = 0.1
(f(xx + h)-f(xx-h))/(2*h)

