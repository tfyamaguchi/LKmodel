#
# Longini IM, Koopman JS (1982) Household and community transmission
#parameters from final distributions of infections in households.
#
#
estmjk <- function(B,Q){
  mjk <- matrix(NA,6,6)
  #browser()
  for (j in 0:5){
    for (k in 0:5){
      if (j<k){
        mjk[j+1,k+1] <- choose(k,j)*mjk[j+1,j+1]*B^(k-j)*Q^(j*(k-j))
      }
      if (j==k){
        if (j==0 & k==0) mjk[k+1,k+1] <- 1
        else mjk[j+1,k+1] = 1- sum(mjk[(0:(j-1))+1,k+1])
      }
    }
  }
  return(mjk)
}
#
#
############## estimation ######################################
#
adjtrunc <- function(x){
  len <- length(x[!is.na(x)])
  x[1:len] <- x[1:len]/(1-x[1])
  x[1] <- NA
  x[len] <- 1-sum(x[1:(len-1)],na.rm=TRUE)
  return(x)
}
#
adjtrunc2 <- function(x){
  len <- length(x[!is.na(x)])
  x[1:len] <- x[1:len]/(1-x[1])
  x[1] <- NA
  #x[len] <- 1-sum(x[1:(len-1)],na.rm=TRUE)
  return(x)
}
#
adjajk <- function(ajk,B){
  #browser()
  a0 <- NULL
  for (k in 1:ncol(ajk)) {
    temp <- sum(ajk[2:nrow(ajk),k],na.rm=TRUE)*(B^k)/(1-B^k)
    a0 <- c(a0,ifelse(is.na(temp),NA,temp))
  }
  return(a0)
}
#
#
NB <- function(B,Q,ajk){
  #browser()
  mjk <- estmjk(B,Q)
  if (all(is.na(ajk[1,]))) {
    #ajk[1,] <- adjajk(ajk=ajk,B=B)
  }
  if (all(is.na(ajk[1,]))) {
    mjk <- apply(mjk,2,adjtrunc)
  }
  #browser()
  LL <- 0
  for (j in 0:(nrow(ajk)-1)){
    for (k in 1:ncol(ajk)){
      LLtemp <- ajk[j+1,k]*(log(mjk[j+1,j+1])+(k-j)*log(B)+j*(k-j)*log(Q))
      if (!is.na(LLtemp)) LL <- LL + LLtemp
    }
  }
  return(LL)
}
#
dm00 <- function(B,Q) {
  dB <- 0
  dQ <- 0
  dBB <- 0
  dQQ <- 0
  dBQ <- 0
  return(list(dB=dB,dQ=dQ,dBB=dBB,dQQ=dQQ,dBQ=dBQ))
}
dm11 <- function(B,Q) {
  dB <- -1
  dQ <- 0
  dBB <- 0
  dQQ <- 0
  dBQ <- 0
  return(list(dB=dB,dQ=dQ,dBB=dBB,dQQ=dQQ,dBQ=dBQ))
}
dm22 <- function(B,Q) {
  dB <- 2*(2*B*Q-B-Q)
  dQ <- 2*B*(B-1)
  dBB <- -2+4*Q
  dQQ <- 0
  dBQ <- -2+4*B
  return(list(dB=dB,dQ=dQ,dBB=dBB,dQQ=dQQ,dBQ=dBQ))
}
dm33 <- function(B,Q) {
  dB <- -3*B^2-6*B*Q^2+18*B^2*Q^2+12*B*Q^3-18*B^2*Q^3-3*Q^2
  dQ <- -6*B^2*Q+12*B^3*Q+18*B^2*Q^2-18*B^3*Q^2-6*B*Q
  dBB <- -6*B-6*Q^2+36*B*Q^2+12*Q^3-36*B*Q^3
  dQQ <- -6*B^2+12*B^3+36*B^2*Q-36*B^3*Q-6*B
  dBQ <- -12*B*Q+36*B^2*Q+36*B*Q^2-54*B^2*Q^2-6*Q
  return(list(dB=dB,dQ=dQ,dBB=dBB,dQQ=dQQ,dBQ=dBQ))
}
dm44 <- function(B,Q) {
  dB <- -4*B^3-12*B^2*Q^3+32*B^3*Q^3+72*B^2*Q^5+24*B^3*Q^4-144*B^3*Q^5-12*B*Q^4-4*Q^3-72*B^2*Q^6+96*B^3*Q^6+24*B*Q^5
  dQ <- -12*B^3*Q^2+24*B^4*Q^2+120*B^3*Q^4+24*B^4*Q^3-180*B^4*Q^4-24*B^2*Q^3-12*B*Q^2-144*B^3*Q^5+144*B^4*Q^5+60*B^2*Q^4
  dBB <- -12*B^2-24*B*Q^3+96*B^2*Q^3+144*B*Q^5+72*B^2*Q^4-432*B^2*Q^5-12*Q^4-144*B*Q^6+288*B^2*Q^6+24*Q^5
  dQQ <- -24*B^3*Q+48*B^4*Q+480*B^3*Q^3+72*B^4*Q^2-720*B^4*Q^3-72*B^2*Q^2-24*B*Q-720*B^3*Q^4+720*B^4*Q^4+240*B^2*Q^3
  dBQ <- -36*B^2*Q^2+96*B^3*Q^2+360*B^2*Q^4+96*B^3*Q^3-720*B^3*Q^4-48*B*Q^3-12*Q^2-432*B^2*Q^5+576*B^3*Q^5+120*B*Q^4
  return(list(dB=dB,dQ=dQ,dBB=dBB,dQQ=dQQ,dBQ=dBQ))
}
#
dm55 <- function(B,Q) {
  dB <- -5*B^4 - 20*B^3*Q^4 + 50*B^4*Q^4 - 30*B^2*Q^6 + 100*B^4*Q^6 + 160*B^3*Q^7 - 300*B^4*Q^7 - 20*B*Q^6 + 120*B^3*Q^8 - 300*B^4*Q^8 - 720*B^3*Q^9 + 150*B^4*Q^8 + 180*B^2*Q^8 - 5*Q^4 + 900*B^4*Q^9 + 40*B*Q^7 + 480*B^3*Q^10 - 600*B^4*Q^10 - 180*B^2*Q^9
  dQ <- -20*B^4*Q^3 + 40*B^5*Q^3 - 60*B^3*Q^5 + 120*B^5*Q^5 + 280*B^4*Q^6 - 420*B^5*Q^6 - 60*B^2*Q-5 + 240*B^4*Q^7 - 480*B^5*Q^7 - 1620*B^4*Q^8 + 240*B^5*Q^7 + 480*B^3*Q^7 - 20*B*Q^3 + 1620*B^5*Q^8 + 140*B^2*Q^6 + 1200*B^4*Q^9 - 1200*B^5*Q^9 - 540*B^3*Q^8
  dBB <- -20*B^3 - 60*B^2*Q^4 + 200*B^3*Q^4 - 60*B*Q^6 + 400*B^3*Q^6 + 480*B^2*Q^7 - 1200*B^2*Q^7 - 20*Q^6 + 360*B^2*Q^8 - 1200*B^3*Q^8 - 2160*B^2*Q^9 + 600*B^3*Q^8 + 360*B*Q^8 + 3600*B^3*Q^9 + 40*Q^7 + 1440*B^2*Q^10 - 2400*B^3*Q^10 - 360*B*Q^9
  dQQ <- -60*B^4*Q^2 + 120*B^5*Q^2 - 300*B^3*Q^4 + 600*B^5*Q^4 + 1680*B^4*Q^5 - 2520* B^5*Q^5 - 300*B^2*Q^4 + 1680*B^4*Q^6 - 3350*B^5*Q^6 - 12960*B^4*Q^7 + 1680*B^5*Q^6 + 3360*B^3*Q^6 - 60*B*Q^2 + 12960*B^5*Q^7 + 840*B^2*Q^5 + 10800*B^4*Q^8 - 10800*B^5*Q^8 - 540*B^3*Q^7
  dBQ <- -80*B^3*Q^3 + 200*B^4*Q^3 - 180*B^2*Q^5 + 600*B^4*Q^5 + 1120*B^3*Q^6 - 2100*B^4*Q^6 - 120*B*Q^5 + 960* B^3*Q^7 -2400*B^4*Q^7 - 6480*B^3*Q^8 + 1200*B^4*Q^7 + 1440*B^2*Q^7 - 20*Q^3 + 8100*B^4*Q^8 + 280*B*Q^6 + 4800*B^3*Q^9 - 6000*B^4*Q^9 - 1620*B^2*Q^8
  return(list(dB=dB,dQ=dQ,dBB=dBB,dQQ=dQQ,dBQ=dBQ))
}
#
dm00z <- function(B,Q) {
  dB <- 0
  dQ <- 0
  dBB <- 0
  dQQ <- 0
  dBQ <- 0
  return(list(dB=dB,dQ=dQ,dBB=dBB,dQQ=dQQ,dBQ=dBQ))
}
dm11z <- function(B,Q) {
  dB <- 0
  dQ <- 0
  dBB <- 0
  dQQ <- 0
  dBQ <- 0
  return(list(dB=dB,dQ=dQ))
}
dm22z <- function(B,Q) {
  dB <- (-2*Q+4*B*Q)/(1-B^2) + 2*B*(-2*B*Q+2*B^2*Q)/(1-B^2)^2
  dQ <- (-2*B+2*B^2)/(1-B^2)
  dBB <- 4*Q/(1-B^2) + 4*B*Q*(-3+5*B)/(1-B^2)^2 + 16*B^3*Q*(-1+B)/(1-B^2)^3
  dQQ <- 0
  dBQ <- (-2+4*B)/(1-B^2) + (-4*B^2+4*B^3)/(1-B^2)^2
  return(list(dB=dB,dQ=dQ,dBB=dBB,dQQ=dQQ,dBQ=dBQ))
}
dm33z <- function(B,Q) {
  dB <- (-6*B*Q^2 + 18*B^2*Q^2 + 12*B*Q^3 - 18*B^2*Q^3 - 3*Q^2)/(1-B^3) + 3*B^2*(-3*B^2*Q^2 + 6*B^3*Q^2 + 6*B^2*Q^3 - 6*B^3*Q^3 - 3*B*Q^2)/(1-B^3)^2
  dQ <- (-6*B^2*Q + 12*B^3*Q + 18*B^2*Q^2 - 18*B^3*Q^2 - 6*B*Q)/(1-B^3)
  dBB <- (-6*Q^2+36*B*Q^2+12*Q^3-36*B*Q^3)/(1-B^3) + (-54*B^3*Q^2 + 144*B^4*Q^2 + 108*B^3*Q^3 - 144*B^4*Q^3 - 27*B^2*Q^2)/(1-B^3)^2 + 6*B^2*(-9*B^4*Q^2 + 18*B^5*Q^2 + 18*B^4*Q^3 - 18*B^5*Q^3 - 9*B^3*Q^2)/(1-B^3)^3
  dQQ <- (-6*B^2 + 12*B^3 + 36*B^2*Q - 36*B^2*Q - 6*B)/(1-B^3) + 3*B^2*(-6*B^2*Q + 12*B^3*Q + 18*B^2*Q^2 - 18*B^3*Q^2 - 6*B*Q)/(1-B^3)^2
  dBQ <- (-12*B*Q + 36*B^2*Q + 36*B*Q^2 - 54*B^2*Q^2 - 6*Q)/(1-B^3) + 3*B^2*(-B^2*Q + 12*B^3*Q + 18*B^2*Q^2 - 18*B^3*Q^2 - 6*B*Q)/(1-B^3)^2
  return(list(dB=dB,dQ=dQ,dBB=dBB,dQQ=dQQ,dBQ=dBQ))
}
#
dm44z <- function(B,Q) {
  dB <- (-12*B^2*Q^3 + 32*B^3*Q^3 + 72*B^2*Q^5 + 24*B^3*Q^4 - 144*B^3*Q^5 - 12*B*Q^4 - 4*Q^3 - 72*B^2*Q^6 + 96*B^3*Q^6 + 24*B*Q^5)/(1-B^4) + 4*B^3*(- 4*B^3*Q^3 + 8*B^4*Q^3 + 24*B^3*Q^5 + 6*B^4*Q^4 - 36*B^4*Q^5 - 6*B^2*Q^4 - 4*B*Q^3 - 24*B^3*Q^6 + 24*B^4*Q^6 + 12*B^2*Q^5)/(1-B^4)^2
  dQ <- (-12*B^3*Q^2 + 24*B^4*Q^2 + 120*B^3*Q^4 + 24*B^4*Q^3 - 180*B^4*Q^4 - 24*B^2*Q^3 - 12*B*Q^2 - 144*B^3*Q^5 + 144*B^4*Q^5 + 60*B^2*Q^4)/(1-B^4)
  dBB <- (-24*B*Q^3+96*B^2*Q^3+144*B*Q^5+72*B^2*Q^4-432*B^2*Q^5-12*Q^4-144*B*Q^6+288*B^2*Q^6+24*Q^5)/(1-B^4) + (-144*B^5*Q^3 + 352*B^6*Q^3 + 864*B^5*Q^5 + 264*B^6*Q^4 - 1584*B^6*Q^5 - 168*B^4*Q^4 - 80*B^3*Q^3 -864*B^5*Q^6 + 1056*B^6*Q^6 + 336*B^4*Q^5)/(1-B^4)^2 + 8*B^3*(-16*B^6*Q^3 + 32*B^7*Q^3 + 96*B^6*Q^5 + 24*B^7*Q^4 - 144*B^7*Q^5 - 24*B^5*Q^4 - 16*B^4*Q^3 - 96*B^6*Q^6 + 96*B^7*Q^6 + 48*B^5*Q^5)/(1-B^4)^3
  dQQ <- (-24*B^3*Q+48*B^4*Q+480*B^3*Q^3+72*B^4*Q^2-720*B^4*Q^3-72*B^2*Q^2-24*B*Q-720*B^3*Q^4+720*B^4*Q^4+240*B^2*Q^3)/(1-B^4)
  dBQ <- (-36*B^2*Q^2+96*B^3*Q^2+360*B^2*Q^4+96*B^3*Q^3-720*B^3*Q^4-48*B*Q^3-12*Q^2-432*B^2*Q^5+576*B^3*Q^5+120*B*Q^4)/(1-B^4) + 4*B^3*(-12*B^3*Q^2 + 24*B^4*Q^2 + 120*B^3*Q^4 + 24*B^4*Q^3 - 180*B^4*Q^4 - 24*B^2*Q^3 - 12*B*Q^2 - 144*B^3*Q^5 + 144*B^4*Q^5 + 60*B^2*Q^4)/(1-B^4)^2
  return(list(dB=dB,dQ=dQ,dBB=dBB,dQQ=dQQ,dBQ=dBQ))
}
#
dm55z <- function(B,Q) {
  dB <- (-20*B^3*Q^4+50*B^4*Q^4-30*B^2*Q^6 + 100*B^4*Q^6+160*B^3*Q^7-300*B^4*Q^7-20*B*Q^6 + 120*B^3*Q^8-450*B^4*Q^8-720*B^3*Q^9 + 1200*B^4*Q^9 + 180*B^2*Q^8 - 5*Q^4 +40*B*Q^7 + 480*B^3*Q^10 - 600*B^4*Q^10 - 180*B^2*Q^9)/(1-B^5) + 5*B^4*(-5*B^4*Q^4 + 10*B^5*Q^4 - 10*B^3*Q^6 + 20*B^5*Q^6 + 40*B^4*Q^7 - 60*B^5*Q^7 - 10*B^2*Q^6 + 30*B^4*Q^8 - 90*B^5*Q^8 - 180*B^4*Q^9 + 240*B^5*Q^9 + 60*B^3*Q^8 - 5*B*Q^4 + 20*B^2*Q^7 + 120*B^4*Q^10 - 120*B^5*Q^10 -60*B^3*Q^9)/(1-B^5)^2
  dQ <- (-20*B^4*Q^3 + 40*B^5*Q^3 - 60*B^3*Q^5 + 120*B^5*Q^5 + 280*B^4*Q^6 - 420*B^5*Q^6 - 60*B^2*Q^5 + 240*B^4*Q^7 - 720*B^5*Q^7 - 1620*B^4*Q^8 + 2160*B^5*Q^8 + 480*B^3*Q^7 - 20*B*Q^3 + 140*B^2*Q^6 + 1200*B^4*Q^9 - 1200*B^5*Q^9- 540*B^3*Q^8)/(1-B^5)
  dBB <- (-60*B^2*Q^4+200*B^3*Q^4-60*B*Q^6+400*B^3*Q^6+480*B^2*Q^7-1200*B^3*Q^7-20*Q^6+360*B^2*Q^8-1800*B^3*Q^8-2160*B^2*Q^9+4800*B^3*Q^9+360*B*Q^8-40*Q^7+1440*B^2*Q^10-2400*B^3*Q^10-360*B*Q^9)/(1-B^5) + (-300*B^7*Q^4 + 700*B^8*Q^4 - 500*B^6*Q^6 + 1400*B^8*Q^6 + 960*B^7*Q^7 - 4200*B^8*Q^7 - 400*B^5*Q^6 + 1800*B^7*Q^8 - 6300*B^8*Q^8 - 10800*B^7*Q^9 + 16800*B^8*Q^9 + 3000*B^6*Q^8 - 150*B^4*Q^4 + 800*B^5*Q^7 + 7050*B^7*Q^10 -8400*B^8*Q^10 - 3000*B^6*Q^9)/(1-B^5)^2 +10*B^4*(-25*B^8*Q^4 + 50*B^9*Q^4 - 50*B^7*Q^6 + 100*B^9*Q^6 + 20*B^8*Q^7 - 300*B^9*Q^7 - 50*B^6*Q^6 + 150*B^8*Q^8 - 450*B^9*Q^8 - 900*B^8*Q^9 + 1200*B^9*Q^9 + 300*B^7*Q^8 - 25*B^5*Q^4 + 100*B^6*Q^7 + 600*B^8*Q^10 - 600*B^9*Q^10 - 300*B^7*Q^9)/(1-B^5)^3
  dQQ <- (-60*B^4*Q^2+120*B^5*Q^2-300*B^3*Q^4+600*B^5*Q^4+1680*B^4*Q^5-2520*B^5*Q^5-300*B^2*Q^4+1680*B^4*Q^6-5040*B^5*Q^6 -12960*B^4*Q^7 +17280*B^5*Q^9+3360*B^3*Q^6-60*B*Q^2+840*B^2*Q^5+10800*B^4*Q^8 - 10800*B^5*Q^8-4320*B^3*Q^7)/(1-B^5)
  dBQ <- (-80*B^3*Q^3+200*B^4*Q^3-180*B^2*Q^5+600*B^4*Q^5+1120*B^3*Q^6-2100*B^4*Q^6-120*B*Q^5+960*B^3*Q^7-3600*B^4*Q^7-6480*B^3*Q^8+10800*B^4*Q^8+1440*B^2*Q^7-20*Q^3+280*B*Q^6+4800*B^3*Q^9-6000*B^4*Q^9-1620*B^2*Q^8)/(1-B^5)+5*B^4*(-20*B^4*Q^3+40*B^5*Q^3-60*B^3*Q^5+120*B^5*Q^5+280*B^4*Q^6-420*B^5*Q^6-60*B^2*Q^5+240*B^4*Q^7-720*B^5*Q^7-1620*B^4*Q^8 + 2160*B^5*Q^8+480*B^3*Q^7-20*B*Q^3+140*B^2*Q^6+1200*B^4*Q^9-1200*B^5*Q^9-540*B^3*Q^8)/(1-B^5)^2
  return(list(dB=dB,dQ=dQ,dBB=dBB,dQQ=dQQ,dBQ=dBQ))
}
#
#
NBg <- function(B,Q,ajk){
  #browser()
  mjk <- estmjk(B,Q)
  if (all(is.na(ajk[1,]))) {
    mjk <- apply(mjk,2,adjtrunc)
  }
  a0k <- adjajk(ajk=ajk,B=B)
  #browser()
  while(nrow(ajk)<6) ajk <- rbind(ajk,rep(NA,ncol(ajk)))
  LLB <- LLQ <- 0
  if (all(is.na(ajk[1,]))) {
    for (k in 1:ncol(ajk)){
      if (all(is.na(ajk[,k]))) next
      LLtempQ <- sum(ifelse(1:6>(k+1),rep(0,6),c(
          0,#ajk[1,k]*(1/mjk[1,1]*dm00z(B,Q)$dQ+(0*(k-0)/Q)),
          ajk[2,k]*(1/mjk[2,2]*dm11z(B,Q)$dQ+(1*(k-1)/Q)),
          ajk[3,k]*(1/mjk[3,3]*dm22z(B,Q)$dQ+(2*(k-2)/Q)),
          ajk[4,k]*(1/mjk[4,4]*dm33z(B,Q)$dQ+(3*(k-3)/Q)),
          ajk[5,k]*(1/mjk[5,5]*dm44z(B,Q)$dQ+(4*(k-4)/Q)),
          ajk[6,k]*(1/mjk[6,6]*dm55z(B,Q)$dQ+(5*(k-5)/Q)))))
      LLtempB <- sum(ifelse(1:6>(k+1),rep(0,6),c(
          0,#ajk[1,k]*(1/mjk[1,1]*dm00z(B,Q)$dB+(k-0)/B),
          ajk[2,k]*(1/mjk[2,2]*dm11z(B,Q)$dB+(k-1)/B),
          ajk[3,k]*(1/mjk[3,3]*dm22z(B,Q)$dB+(k-2)/B),
          ajk[4,k]*(1/mjk[4,4]*dm33z(B,Q)$dB+(k-3)/B),
          ajk[5,k]*(1/mjk[5,5]*dm44z(B,Q)$dB+(k-4)/B),
          ajk[6,k]*(1/mjk[6,6]*dm55z(B,Q)$dB+(k-5)/B))))
        #
        LLB <- LLB + LLtempB
        LLQ <- LLQ + LLtempQ
    }
  } else {
      for (k in 1:ncol(ajk)){
        #browser()
        if (is.na(ajk[1,k])) next
        LLtempQ <- sum(ifelse(1:6>(k+1),rep(0,6),c(
          ajk[1,k]*(1/mjk[1,1]*dm00(B,Q)$dQ+(0*(k-0)/Q)),
          ajk[2,k]*(1/mjk[2,2]*dm11(B,Q)$dQ+(1*(k-1)/Q)),
          ajk[3,k]*(1/mjk[3,3]*dm22(B,Q)$dQ+(2*(k-2)/Q)),
          ajk[4,k]*(1/mjk[4,4]*dm33(B,Q)$dQ+(3*(k-3)/Q)),
          ajk[5,k]*(1/mjk[5,5]*dm44(B,Q)$dQ+(4*(k-4)/Q)),
          ajk[6,k]*(1/mjk[6,6]*dm55(B,Q)$dQ+(5*(k-5)/Q)))))
        LLtempB <- sum(ifelse(1:6>(k+1),rep(0,6),c(
          ajk[1,k]*(1/mjk[1,1]*dm00(B,Q)$dB+(k-0)/B),
          ajk[2,k]*(1/mjk[2,2]*dm11(B,Q)$dB+(k-1)/B),
          ajk[3,k]*(1/mjk[3,3]*dm22(B,Q)$dB+(k-2)/B),
          ajk[4,k]*(1/mjk[4,4]*dm33(B,Q)$dB+(k-3)/B),
          ajk[5,k]*(1/mjk[5,5]*dm44(B,Q)$dB+(k-4)/B),
          ajk[6,k]*(1/mjk[6,6]*dm55(B,Q)$dB+(k-5)/B))))
        #
        LLB <- LLB + LLtempB
        LLQ <- LLQ + LLtempQ
      }
  }
  return(c(LLB,LLQ))
}
#
#
NBgg <- function(B,Q,ajk){
  #
  #browser()
  #
  mjk <- estmjk(B,Q)
  if (all(is.na(ajk[1,]))) {
    mjk <- apply(mjk,2,adjtrunc)
  }
  while(nrow(ajk)<6) ajk <- rbind(ajk,rep(NA,ncol(ajk)))
  #
  #
  dQQL <- dBQL <- dBBL <- 0
  if (all(is.na(ajk[1,]))) {
    for (k in 1:ncol(ajk)){
      nk <- sum(ajk[,k],na.rm=TRUE)
      dQQL <- dQQL + sum(ifelse(is.na(ajk[0:5+1,k]),rep(0,6),c(
      nk*mjk[0+1,k+1]*(1/mjk[0+1,0+1]*(1/mjk[0+1,0+1]*dm00z(B,Q)$dQ^2-dm00(B,Q)$dQQ)+(0*(k-0))/Q^2),
      nk*mjk[1+1,k+1]*(1/mjk[1+1,1+1]*(1/mjk[1+1,1+1]*dm11z(B,Q)$dQ^2-dm11(B,Q)$dQQ)+(1*(k-1))/Q^2),
      nk*mjk[2+1,k+1]*(1/mjk[2+1,2+1]*(1/mjk[2+1,2+1]*dm22z(B,Q)$dQ^2-dm22(B,Q)$dQQ)+(2*(k-2))/Q^2),
      nk*mjk[3+1,k+1]*(1/mjk[3+1,3+1]*(1/mjk[3+1,3+1]*dm33z(B,Q)$dQ^2-dm33(B,Q)$dQQ)+(3*(k-3))/Q^2),
      nk*mjk[4+1,k+1]*(1/mjk[4+1,4+1]*(1/mjk[4+1,4+1]*dm44z(B,Q)$dQ^2-dm44(B,Q)$dQQ)+(4*(k-4))/Q^2),
      nk*mjk[5+1,k+1]*(1/mjk[5+1,5+1]*(1/mjk[5+1,5+1]*dm55z(B,Q)$dQ^2-dm55(B,Q)$dQQ)+(5*(k-5))/Q^2))))
      dBQL <- dBQL + sum(ifelse(is.na(ajk[0:5+1,k]),rep(0,6),c(
      nk*(mjk[0+1,k+1]/mjk[0+1,0+1])*(1/mjk[0+1,0+1]*dm00z(B,Q)$dQ*dm00(B,Q)$dB-dm00(B,Q)$dBQ),
      nk*(mjk[1+1,k+1]/mjk[1+1,1+1])*(1/mjk[1+1,1+1]*dm11z(B,Q)$dQ*dm11(B,Q)$dB-dm11(B,Q)$dBQ),
      nk*(mjk[2+1,k+1]/mjk[2+1,2+1])*(1/mjk[2+1,2+1]*dm22z(B,Q)$dQ*dm22(B,Q)$dB-dm22(B,Q)$dBQ),
      nk*(mjk[3+1,k+1]/mjk[3+1,3+1])*(1/mjk[3+1,3+1]*dm33z(B,Q)$dQ*dm33(B,Q)$dB-dm33(B,Q)$dBQ),
      nk*(mjk[4+1,k+1]/mjk[4+1,4+1])*(1/mjk[4+1,4+1]*dm44z(B,Q)$dQ*dm44(B,Q)$dB-dm44(B,Q)$dBQ),
      nk*(mjk[5+1,k+1]/mjk[5+1,5+1])*(1/mjk[5+1,5+1]*dm55z(B,Q)$dQ*dm55(B,Q)$dB-dm55(B,Q)$dBQ))))
      dBBL <- dBBL + sum(ifelse(is.na(ajk[0:5+1,k]),rep(0,6),c(
      nk*mjk[0+1,k+1]*(1/mjk[0+1,0+1]*(1/mjk[0+1,0+1]*dm00z(B,Q)$dB^2-dm00(B,Q)$dBB)+(k-0)/B^2),
      nk*mjk[1+1,k+1]*(1/mjk[1+1,1+1]*(1/mjk[1+1,1+1]*dm11z(B,Q)$dB^2-dm11(B,Q)$dBB)+(k-1)/B^2),
      nk*mjk[2+1,k+1]*(1/mjk[2+1,2+1]*(1/mjk[2+1,2+1]*dm22z(B,Q)$dB^2-dm22(B,Q)$dBB)+(k-2)/B^2),
      nk*mjk[3+1,k+1]*(1/mjk[3+1,3+1]*(1/mjk[3+1,3+1]*dm33z(B,Q)$dB^2-dm33(B,Q)$dBB)+(k-3)/B^2),
      nk*mjk[4+1,k+1]*(1/mjk[4+1,4+1]*(1/mjk[4+1,4+1]*dm44z(B,Q)$dB^2-dm44(B,Q)$dBB)+(k-4)/B^2),
      nk*mjk[5+1,k+1]*(1/mjk[5+1,5+1]*(1/mjk[5+1,5+1]*dm55z(B,Q)$dB^2-dm55(B,Q)$dBB)+(k-5)/B^2))))
    }
  }else{
    for (k in 1:ncol(ajk)){
      nk <- sum(ajk[,k],na.rm=TRUE)
      dQQL <- dQQL + sum(ifelse(is.na(ajk[0:5+1,k]),rep(0,6),c(
        nk*mjk[0+1,k+1]*(1/mjk[0+1,0+1]*(1/mjk[0+1,0+1]*dm00(B,Q)$dQ^2-dm00(B,Q)$dQQ)+(0*(k-0))/Q^2),
        nk*mjk[1+1,k+1]*(1/mjk[1+1,1+1]*(1/mjk[1+1,1+1]*dm11(B,Q)$dQ^2-dm11(B,Q)$dQQ)+(1*(k-1))/Q^2),
        nk*mjk[2+1,k+1]*(1/mjk[2+1,2+1]*(1/mjk[2+1,2+1]*dm22(B,Q)$dQ^2-dm22(B,Q)$dQQ)+(2*(k-2))/Q^2),
        nk*mjk[3+1,k+1]*(1/mjk[3+1,3+1]*(1/mjk[3+1,3+1]*dm33(B,Q)$dQ^2-dm33(B,Q)$dQQ)+(3*(k-3))/Q^2),
        nk*mjk[4+1,k+1]*(1/mjk[4+1,4+1]*(1/mjk[4+1,4+1]*dm44(B,Q)$dQ^2-dm44(B,Q)$dQQ)+(4*(k-4))/Q^2),
        nk*mjk[5+1,k+1]*(1/mjk[5+1,5+1]*(1/mjk[5+1,5+1]*dm55(B,Q)$dQ^2-dm55(B,Q)$dQQ)+(5*(k-5))/Q^2))))
      dBQL <- dBQL + sum(ifelse(is.na(ajk[0:5+1,k]),rep(0,6),c(
        nk*(mjk[0+1,k+1]/mjk[0+1,0+1])*(1/mjk[0+1,0+1]*dm00(B,Q)$dQ*dm00(B,Q)$dB-dm00(B,Q)$dBQ),
        nk*(mjk[1+1,k+1]/mjk[1+1,1+1])*(1/mjk[1+1,1+1]*dm11(B,Q)$dQ*dm11(B,Q)$dB-dm11(B,Q)$dBQ),
        nk*(mjk[2+1,k+1]/mjk[2+1,2+1])*(1/mjk[2+1,2+1]*dm22(B,Q)$dQ*dm22(B,Q)$dB-dm22(B,Q)$dBQ),
        nk*(mjk[3+1,k+1]/mjk[3+1,3+1])*(1/mjk[3+1,3+1]*dm33(B,Q)$dQ*dm33(B,Q)$dB-dm33(B,Q)$dBQ),
        nk*(mjk[4+1,k+1]/mjk[4+1,4+1])*(1/mjk[4+1,4+1]*dm44(B,Q)$dQ*dm44(B,Q)$dB-dm44(B,Q)$dBQ),
        nk*(mjk[5+1,k+1]/mjk[5+1,5+1])*(1/mjk[5+1,5+1]*dm55(B,Q)$dQ*dm55(B,Q)$dB-dm55(B,Q)$dBQ))))
      dBBL <- dBBL + sum(ifelse(is.na(ajk[0:5+1,k]),rep(0,6),c(
        nk*mjk[0+1,k+1]*(1/mjk[0+1,0+1]*(1/mjk[0+1,0+1]*dm00(B,Q)$dB^2-dm00(B,Q)$dBB)+(k-0)/B^2),
        nk*mjk[1+1,k+1]*(1/mjk[1+1,1+1]*(1/mjk[1+1,1+1]*dm11(B,Q)$dB^2-dm11(B,Q)$dBB)+(k-1)/B^2),
        nk*mjk[2+1,k+1]*(1/mjk[2+1,2+1]*(1/mjk[2+1,2+1]*dm22(B,Q)$dB^2-dm22(B,Q)$dBB)+(k-2)/B^2),
        nk*mjk[3+1,k+1]*(1/mjk[3+1,3+1]*(1/mjk[3+1,3+1]*dm33(B,Q)$dB^2-dm33(B,Q)$dBB)+(k-3)/B^2),
        nk*mjk[4+1,k+1]*(1/mjk[4+1,4+1]*(1/mjk[4+1,4+1]*dm44(B,Q)$dB^2-dm44(B,Q)$dBB)+(k-4)/B^2),
        nk*mjk[5+1,k+1]*(1/mjk[5+1,5+1]*(1/mjk[5+1,5+1]*dm55(B,Q)$dB^2-dm55(B,Q)$dBB)+(k-5)/B^2))))
    }
  }
  #
  return(list(dQQL=dQQL,dBBL=dBBL,dBQL=dBQL))
}
#
initBQ <- function(ajk){
  #browser()
  nk <- colSums(ajk)
  kmax <- ncol(ajk)
  if(all(is.na(ajk[1,]))){
    B0 <- 0.7
    Q0 <- 0.8
  } else {
    n <- sum(ajk,na.rm=TRUE)
    k <- length(nk[!is.na(nk)])
    a0k <- ajk[1,]
    a1k <- ajk[2,]
    B0 <- sum(nk*(a0k/nk)^(1/k),na.rm=TRUE)/n
    Q0 <- sum(a1k[2:ncol(ajk)]/(2:ncol(ajk)*nk[2:ncol(ajk)]*(1-B0)*B0^(2:ncol(ajk)-1))^(1/(2:ncol(ajk)-1)),na.rm=TRUE)/length(nk[2:ncol(ajk)])
  }
  #phi <- sum(ajk*(1:nrow(ajk)-1),na.rm=TRUE)/n
  #theta <- sum(t(t(ajk*(1:nrow(ajk)-1))/1:ncol(ajk)),na.rm=TRUE)/n
  #Q1 <- ((1-theta)/B0)^(1/theta)
  return(list(B0=B0,Q0=Q0))
}
#
#############################################################
# Main
#############################################################
#
#
LKmodel.test <- function(x=NULL,y=NULL,method="SC"){
  #browser()
  initBx <- initBQ(x)$B0
  initQx <- initBQ(x)$Q0
  if(!is.null(y)){
    initBy <- initBQ(y)$B0
    initQy <- initBQ(y)$Q0
  }
  add3 <- function(a,b){
    if (is.na(a) & is.na(b)) res <- NA
    else res <- sum(c(a,b),na.rm=TRUE)
    return(res)
  }
  while(nrow(x)<6) x <- rbind(x,rep(NA,ncol(x)))
  while(ncol(x)<5) x <- cbind(x,rep(NA,nrow(x)))
  xy <- NULL
  if (!is.null(y)) {
    while(nrow(y)<6) y <- rbind(y,rep(NA,ncol(y)))
    while(ncol(y)<5) y <- cbind(y,rep(NA,nrow(y)))
    xy <- matrix(mapply(add3,x,y),6,5,byrow=FALSE)
    initBxy <- initBQ(xy)$B0
    initQxy <- initBQ(xy)$Q0
  }
  #
  LLf1 <- function (X) return(NB(X[1],X[2],ajk=x))
  LLf2 <- function (X) return(NB(X[1],X[2],ajk=y))
  LLf12 <- function (X) return(NB(X[1],X[2],ajk=xy))
  #
  LLgf1 <- function (X) return(NBg(X[1],X[2],ajk=x))
  LLgf2 <- function (X) return(NBg(X[1],X[2],ajk=y))
  #
  LLgf12 <- function (X) return(NBg(X[1],X[2],ajk=xy))
  #

  res1 <- optim(c(initBx,initQx), fn=LLf1, gr=LLgf1, method="L-BFGS-B", control=list(fnscale=-1,parscale=c(1,1),trace=0),lower=c(0.01,0.01),upper=c(0.999,0.999))
  res1g <- NBgg(B=res1$par[1],Q=res1$par[2],ajk=x)
  V1 <- ginv(matrix(c(res1g$dQQ,res1g$dBQ,res1g$dBQ,res1g$dBB),2,2,byrow=TRUE))
  #
  #browser()
  nfamily <- ncol(x)
  mjkest <- estmjk(res1$par[1],res1$par[2])[,2:(nfamily+1)]
  if (all(is.na(x[1,]))) {
    mjkest <- apply(mjkest,2,adjtrunc)
  }
  mjkest[is.na(mjkest)] <- 0
  ajkest <- mjkest%*%diag(colSums(x,na.rm=TRUE)[1:nfamily])
  chi1 <- sum((x[1:(nfamily+1),1:nfamily] -ajkest[1:(nfamily+1),1:nfamily])^2 / ajkest[1:(nfamily+1),1:nfamily],na.rm=TRUE)
  p.value1 <- pchisq(chi1,df=1,lower.tail=FALSE)
  #
  V2 <- chi2 <- p.value2 <- NA
  if(!is.null(y)){
    res2 <- optim(c(initBy,initQy), fn=LLf2, gr=LLgf2, method="L-BFGS-B", control=list(fnscale=-1,parscale=c(1,1),trace=0),lower=c(0.01,0.01),upper=c(0.999,0.999))
    res2g <- NBgg(B=res2$par[1],Q=res2$par[2],ajk=y)
    V2 <- ginv(matrix(c(res2g$dQQ,res2g$dBQ,res2g$dBQ,res2g$dBB),2,2,byrow=TRUE))
    #
    ajkest <- mjkest%*%diag(colSums(y,na.rm=TRUE)[1:nfamily])
    chi2 <- sum((y[1:(nfamily+1),1:nfamily] -ajkest[1:(nfamily+1),1:nfamily])^2 / ajkest[1:(nfamily+1),1:nfamily],na.rm=TRUE)
    p.value2 <- pchisq(chi2,df=1,lower.tail=FALSE)
  }
  #
  V3 <- NA
  score <- NA
  p.value <- stat <- NA
  if(!is.null(y)){
    res3 <- optim(c(initBxy,initQxy), fn=LLf12, gr=LLgf12, method="L-BFGS-B", control=list(fnscale=-1,parscale=c(1,1),trace=0),lower=c(0.01,0.01),upper=c(0.999,0.999))
    res3g <- NBgg(B=res3$par[1],Q=res3$par[2],ajk=x+y)
    V3 <- ginv(matrix(c(res3g$dQQ,res3g$dBQ,res3g$dBQ,res3g$dBB),2,2,byrow=TRUE))
    #
    LL <- res3$value
    U <- c(NBg(B=res3$par[1],Q=res3$par[2],ajk=x)[2],NBg(B=res3$par[1],Q=res3$par[2],ajk=y)[2],NBg(B=res3$par[1],Q=res3$par[2],ajk=x+y)[1])
    #
    If1 <- matrix(c(NBgg(B=res1$par[1],Q=res1$par[2],ajk=x)$dQQL ,
                    0,
                    NBgg(B=res1$par[1],Q=res1$par[2],ajk=x)$dBQL ,
                    0,
                    NBgg(B=res2$par[1],Q=res2$par[2],ajk=y)$dQQL,
                    NBgg(B=res2$par[1],Q=res2$par[2],ajk=y)$dBQL,
                    NBgg(B=res1$par[1],Q=res1$par[2],ajk=x)$dBQL,
                    NBgg(B=res2$par[1],Q=res2$par[2],ajk=y)$dBQL,
                    NBgg(B=res1$par[1],Q=res1$par[2],ajk=x)$dBBL + NBgg(B=res2$par[1],Q=res2$par[2],ajk=y)$dBBL),
                  3,3,byrow=TRUE)
    #
    #If1 <- matrix(c(NBgg(B=res3$par[1],Q=res3$par[2],ajk=x)$dQQL,
    #                NBgg(B=res3$par[1],Q=res3$par[2],ajk=x)$dQQL+NBgg(B=res3$par[1],Q=res3$par[2],ajk=y)$dQQL,
    #                NBgg(B=res3$par[1],Q=res3$par[2],ajk=x)$dBQL,
    #                NBgg(B=res3$par[1],Q=res3$par[2],ajk=x)$dQQL+NBgg(B=res3$par[1],Q=res3$par[2],ajk=y)$dQQL,
    #                NBgg(B=res3$par[1],Q=res3$par[2],ajk=y)$dQQL,
    #                NBgg(B=res3$par[1],Q=res3$par[2],ajk=y)$dBQL,
    #                NBgg(B=res3$par[1],Q=res3$par[2],ajk=x)$dBQL,
    #                NBgg(B=res3$par[1],Q=res3$par[2],ajk=y)$dBQL,
    #                NBgg(B=res3$par[1],Q=res3$par[2],ajk=xy)$dBBL),3,3,byrow=TRUE)
    #browser()
    #
    C <- matrix(c(1,-1,0),3,1)
    theta <- c(res1$par[2],res2$par[2],res3$par[1])
    #browser()
    if (method=="SC")  stat <- U%*%ginv(If1)%*%U
    else if (method=="LR") stat <- (-1)*((-res1$value -res2$value) - (-res3$value)) # ???
    else if (method=="W") stat <- t(t(C)%*%theta)%*%(t(C)%*%ginv(If1)%*%C)^(-1)%*%t(C)%*%theta
    p.value <- pchisq(stat,df=1,lower.tail=FALSE)
  }
  #
  if (method=="W") methodlong <- "Wald test"
  else if (method=="SC") methodlong <- "Score test"
  else if (method=="LR") methodlong <- "Likelihood ratio test"
  #
  return(list(Q1=res1$par[2],B1=res1$par[1],V1=V1,chi1=chi1,p.value1=p.value1,Q2=ifelse(is.null(y),NA,res2$par[2]),B2=ifelse(is.null(y),NA,res2$par[1]),V2=V2,chi2=chi2,p.value2=p.value2,Qmix=ifelse(is.null(y),NA,res3$par[2]),Bmix=ifelse(is.null(y),NA,res3$par[1]),Vmix=V3,stat=stat,p.value=p.value,method=methodlong))
}
#
print.summary.LKmodel <- function(fm){
  cat(sprintf("Qhat.x; %.3f\t",fm$Q1))
  cat(sprintf("Bhat.x; %.3f\n",fm$B1))
  cat(sprintf("Var(Qhat.x); %.4f\t",fm$V1[2,2]))
  cat(sprintf("Var(Bhat.x); %.4f\n",fm$V1[1,1]))
  cat(sprintf("Goodness of fit chi^2; %.3f\t",fm$chi1))
  cat(sprintf("P-value; %.3f\n",fm$p.value1))
  cat(sprintf("Test method; %s\n",fm$method))
  cat(sprintf("Test statistics; %.3f\t",fm$stat))
  cat(sprintf("P-value; %.3f\n",fm$p.value))
}
########################### EMD OF PROGRAM #######################################

LKrct <- function(houseN,b0,q0,b1,q1){
  if (houseN==2) {
    placebo <- (1-b0)*b0*q0 + 1-b0^2-(1-b0)*q0*b0 - b0*q0*(1-b0)
    active <- (1-b1)*b0*q0 + 1-b0*b1-(1-b1)*q0*b0 - b1*q1*(1-b0)
  }
  if (houseN==3) {
    placebo <- 1-(
      b0^3 + 2*(1-b0)*b0^2*q0^2 + (1-b0)^2*b0*q0
      + 2*(1-b0)*b0*(1-q0)*b0*q0
    )
    active <- 1-(
      b1*b0^2 + 2*b1*q1*(1-b0)*b0*q0
      + b1*q1*(1-b0)^2 + 2*b1*q1*(1-b0)*b0*(1-q0)
    )
  }
  return(list(placebo=placebo,active=active))
}
#
#
