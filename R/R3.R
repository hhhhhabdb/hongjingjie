model <- function(t, y, param) {
  S <- y[1]
  E <- y[2]
  I1 <- y[3]
  I2 <- y[4]
  R <- y[5]
  
  N <- param["N"]
  beta <- param["beta"]
  mu <- param["mu"]
  gamma <- param["gamma"]
  lamda <- param["lamda"]
  delta <- param["delta"]
  
  dSt <- mu * (N - S) - beta * S * I1/N
  dEt <- beta * S * I1/N - mu * E-lamda*E
  dI1t <- - (mu + gamma) * I1+lamda*E
  dI2t <- -(mu + gamma) * I2 + delta * R
  dRt <- gamma * I1 - mu * R
  
  outcome <- c(dSt, dEt,dI1t,dI2t, dRt)
  
  list(outcome)
}


times <- seq(0, 156, by = 1/7)
param <- c(mu = 0.000, lamda = 0.03, beta = 4, gamma = 0.1,delta=0.02,N = 1)
init <- c(S = 0.9999, E = 0.00008,I1 = 0.0002,I2=0.00008, R = 0)


result <-  deSolve::ode(y=init, times=times, func=model, parms = param)
result <- as.data.frame(result)

tail(round(result, 3),10)

#结果画图
#' @export
seirplot <- ggplot2::ggplot(data=result)+
  ggplot2::geom_line(ggplot2::aes(x=time, y=S,col="S"), lwd=2) +
  ggplot2::geom_line(ggplot2::aes(x=time, y=I1,col="I1"),lwd=2) +
  ggplot2::geom_line(ggplot2::aes(x=time,y=I2,col="I2"),lwd=2) +
  ggplot2::geom_line(ggplot2::aes(x=time, y=R,col="R"), lwd=2) +
  ggplot2::geom_line(ggplot2::aes(x=time, y=E,col="E"), lwd=2) +
  ggplot2::labs(x = "Time",y = "Ratio")+
  ggplot2::scale_color_manual(name = "SEI1I2R", values = c("S" = "orange", "E" = "purple",
                                                        "I1" = "red","I2" = "pink", "R" = "green"))

seirplot


