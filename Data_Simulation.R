'---------------------------------------------------------------------------'
'
                  CODE FOR THE DATA ANALYSIS IN THE M.Sc. Thesis:
                  

            
            Risk Preferences and Narratives under Ex-ante Inequalities
            in Decision-Making Opportunities: Evidence from Germany
            
            
                  
                  AUTHOR: Valeria Alejandra Quispe Villalobos
                        last update: 25.09.2023

'
'---------------------------------------------------------------------------'

##############################
#
#       PACKAGES
#
##############################


packages <- c("ggplot2", "sn")
installed.packages <- packages %in% rownames(installed.packages())
if (any(installed.packages==F)) {install.packages(packages[!installed.packages])}

invisible(lapply(packages, library, character.only=T))

####################################################
#
#           FUNCTIONS
#
####################################################


'PROBABILITY WEIGHT FUNCTION'  

w <- function(p, w_param){
  
  w1 <- (p^w_param)/
    ((p^w_param)+((1-p)^w_param))^(1/w_param)
  
  w2 <- ((1-p)^w_param)/
    (((1-p)^w_param)+(p^w_param))^(1/w_param)
  
  w_vals <- c(w1, w2)
  
  return(w_vals)
}


'UTILITY SAFE OPTION GAIN'

v_g <- function(x, alpha){
  x^alpha
}


'UTILITY SAFE OPTION LOSS'

v_l <- function(x, beta, lambda){
  (-lambda)*(x^beta)
}

          '---------------------------------------'
          '---------------------------------------'
          '         CPT UTILITY FUNCTION          '
          '---------------------------------------' 
          '---------------------------------------'

'
Function that calculates the CPT utility for a lottery with
up to two potential outcomes (x1, x2). It is only required 
to indicate the realization probability (p) of outcome x1.
The function automatically generates the realization 
probability (1-p) of outcome x2. The parameter domain accepts 
only the values 0 or 1. 

  Gain: domain == 1 
  Loss: domain == 0
  
'

CPT <- function(x1, p, x2, domain){
  
  if (!(is.na(domain) || domain %in% c(0, 1))){
    stop("Incorrect input. The parameter 'domain' can only take the values 0 or 1.
         To calculate in the GAIN please insert domain = 1.
         To calculate in the LOSS please insert domain = 0.
         ")
  }
  
  w_param <- ifelse(domain == 1, gamma, delta)
  
  w1 <- w(p, w_param)[1]
  w2 <- w(p, w_param)[2]
  
  x <- x1
  v1 <- ifelse(domain == 1, v_g(x, alpha), v_l(x, beta, lambda))
  
  x <- x2
  v2 <- ifelse(domain == 1, v_g(x, alpha), v_l(x, beta, lambda))
  
  u <- w1*v1 + w2*v2
  
}

            '---------------------------------------'
            '---------------------------------------'
            '         CERTAINTY EQUIVALENT          '
            '---------------------------------------' 
            '---------------------------------------'

# CERTAINTY EQUIVALENT

ce_g <- function(CPT_lot, alpha){
  CPT_lot^(1/alpha)
}


# CERTAINTY EQUIVALENT LOSS

ce_l <- function(CPT_lot, beta, lambda){
  (CPT_lot/(-lambda))^(1/beta)
}



####################################################
#
#           GENERATE DATA
#
####################################################



'---------------------------------------'
'---------------------------------------'
'  TK PARAMETERS FOR THE MEDIAN PLAYER  '
'---------------------------------------' 
'---------------------------------------'


alpha_m <- 0.88       #risk attitudes related to gains
beta_m  <- 0.88       #risk attitudes related to losses

lambda_m  <- 2.25     #risk aversion parameter; lambda >1 represents loss aversion
gamma_m   <- 0.61
delta_m   <- 0.69



'---------------------------------------'
'---------------------------------------'
'       COMPUTE CPT UTILITES FOR        '
'     CAPPELEN ET AL. (2013a) AND       '
' EXPECTED CPT UTILITES FOR THE MEDIAN  '
'      PLAYER IN MY EXPERIMENT          '
'---------------------------------------' 
'---------------------------------------'

alpha <- alpha_m
beta  <- beta_m

lambda <- lambda_m
gamma  <- gamma_m
delta  <- delta_m

SAFE_payoffs <- c(25, 200, 300, 400)


    GEN_SAFE_CPT_CAP <- function(x){
      return(CPT(x, 1,0,1))
    }


LOT_CPT_CAP <- CPT(800, 0.5, 0, 1)
LOT_CE_CAP <- ce_g(LOT_CPT_CAP, alpha)
SAFE_CPT_CAP_VALS <- sapply(SAFE_payoffs, GEN_SAFE_CPT_CAP)


'---------------------------------------'
'---------------------------------------'
'             CREATE TABLE 1            '
'---------------------------------------' 
'---------------------------------------'

LOT_FREQ_CAP <- c(78, 73, 50, 7)
SAFE_FREQ_CAP <- c(0, 5, 28, 71)
SIGNS <- c("<", "<", ">", ">")

table1 <- data.frame(SAFE_payoffs, LOT_FREQ_CAP, SAFE_FREQ_CAP,
                     SAFE_CPT_CAP_VALS, SIGNS , LOT_CPT_CAP)


'---------------------------------------'
'---------------------------------------'
'             CREATE TABLE 2            '
'---------------------------------------' 
'---------------------------------------'

TYPES <- c("Risk-averse", "Median", "Moderate risk-seeking", "(Strict) Risk-seeking")


# 5 people risk averse

gamma <- 0.2
LOT_CPT_RA_l <- CPT(800, 0.5, 0, 1)
LOT_CE_RA_l <- ce_g(LOT_CPT_RA_l, alpha)

gamma <- 0.42
LOT_CPT_RA_u <- CPT(800, 0.5, 0, 1)
LOT_CE_RA_u <- ce_g(LOT_CPT_RA_u, alpha)


# 43 people MODERATE risk seeking

gamma <- 0.62
LOT_CPT_MRS_l <- CPT(800, 0.5, 0, 1)
LOT_CE_MRS_l <- ce_g(LOT_CPT_MRS_l, alpha)

gamma <- 1
LOT_CPT_MRS_u <- CPT(800, 0.5, 0, 1)
LOT_CE_MRS_u <- ce_g(LOT_CPT_MRS_u, alpha)


# 7 people are risk seeking

gamma <- gamma_m
alpha <- 1.25
LOT_CPT_RS_l <- CPT(800, 0.5, 0, 1)

LOT_CPT_TYPES <- c(LOT_CPT_RA_u, LOT_CPT_CAP, LOT_CPT_MRS_l, LOT_CPT_RS_l)
LOT_CE_TYPES <- c(LOT_CE_RA_u, LOT_CE_CAP, LOT_CE_MRS_l, "-")

table2 <- data.frame(TYPES, LOT_CPT_TYPES, LOT_CE_TYPES)


'---------------------------------------'
'---------------------------------------'
'             CREATE TABLE 3            '
'---------------------------------------' 
'---------------------------------------'
alpha <- alpha_m

    GEN_SAFE_CPT_VQ <- function(x){
      return(abs(CPT(x, 1,0,0)))
    }

LOT_CPT_VQ <- CPT(800, 0.5, 0, 0)
LOT_CE_VQ <- ce_l(LOT_CPT_VQ, beta,lambda)
SAFE_CPT_VQ_VALS <- sapply(SAFE_payoffs, GEN_SAFE_CPT_VQ)

SIGNS_VQ <- c("<", "<", "<", ">")
  
table3 <- data.frame(SAFE_payoffs, SAFE_CPT_CAP_VALS, SIGNS, LOT_CPT_CAP,
                     SAFE_CPT_VQ_VALS, SIGNS_VQ, abs(LOT_CPT_VQ))




####################################
#
# PLOTS
#
####################################


#####################
#
# MY STYLES
#
#####################

style_green <- "#62E573"
style_blue  <- "#006AB3"
style_red   <- "#CC0000"
style_black <- "#404040"

style_theme <-  theme(axis.title.x=element_text(color = style_black), 
                      axis.title.y=element_text(color = style_black),
                      legend.title=element_blank(),
                      legend.text =element_text(size = 12, color = style_black))



'---------------------------------------'
'---------------------------------------'
'               FIGURE 7                '
'---------------------------------------' 
'---------------------------------------'

'cpt utlitiy median player and hypothetical 
risk-seeking player in the gain domain
'

x <- seq(0, 410)
y <- v_g(x, alpha_m)
y_rs <- v_g(x, 1.25)



df_fig7 <- data.frame(x = x, y = y, y_rs = y_rs, 
                 LOT_CE_CAP = LOT_CE_CAP, LOT_CPT_CAP = LOT_CPT_CAP, 
                 LOT_CE_RA_u = LOT_CE_RA_u, LOT_CPT_RA_u = LOT_CPT_RA_u, 
                 LOT_CE_MRS_l = LOT_CE_MRS_l, LOT_CPT_MRS_l = LOT_CPT_MRS_l)

ggplot(df_fig7, aes(x = x, y = y)) + 
  geom_line(aes(color = "alpha0.88")) + 
  geom_line(aes(y = x), linetype = "dashed") + 
  geom_line(aes(y = y_rs, color = "alpha1.25")) +
  geom_point(aes(x = LOT_CE_CAP, y = LOT_CPT_CAP), shape = 17, color=style_black, size = 2.5) +
  geom_point(aes(x = LOT_CE_RA_u, y = LOT_CPT_RA_u), shape = 15, color=style_black , size = 2.5) +
  geom_point(aes(x = LOT_CE_MRS_l, y = LOT_CPT_MRS_l), shape = 16, color=style_black , size = 2.5) +
  scale_y_continuous(limits = c(0, 440)) +
  xlab("Payoff") +
  ylab("CPT Utility") +
  scale_color_manual(
    values = c("alpha0.88" = style_green, "alpha1.25" = style_blue),
    labels = c(expression(paste(" ",alpha, " = 0.88")), expression(paste(" ", alpha, " = 1.25"))))+
  style_theme
  

'---------------------------------------'
'---------------------------------------'
'               FIGURE 8                '
'---------------------------------------' 
'---------------------------------------'
      
'cpt utiliy of the median player in the gain and loss 
domain' 


y_l <- abs(v_l(x, beta, lambda))

df_fig8 <- data.frame(x = x, y = y, y_l = y_l)
df_fig8_gain <- data.frame(LOT_CE = LOT_CE_CAP, LOT_CPT = LOT_CPT_CAP, SAFE_payoffs = SAFE_payoffs, SAFE_CPT_VALS = SAFE_CPT_CAP_VALS)
df_fig8_loss <- data.frame(LOT_CE = abs(LOT_CE_VQ), LOT_CPT = abs(LOT_CPT_VQ), SAFE_payoffs = SAFE_payoffs, SAFE_CPT_VALS = SAFE_CPT_VQ_VALS)


ggplot() +
  geom_line(data = df_fig8, aes(x = x, y = y , color = "gain")) +
  geom_line(data = df_fig8, aes(x = x, y = x), linetype = "dashed") +
  geom_line(data = df_fig8, aes(x = x, y = y_l , color = "loss")) +
  
  # Gain Points
  geom_point(data = df_fig8_gain, aes(x = LOT_CE, y = LOT_CPT), shape = 15, color = style_black, size = 2.5) +
  geom_point(data = df_fig8_gain, aes(x = SAFE_payoffs, y = SAFE_CPT_VALS), shape = 17, color = style_green, size = 2.5) +
  
  # Loss Points
  geom_point(data = df_fig8_loss, aes(x = LOT_CE, y = LOT_CPT), shape = 15, color = style_black, size = 2.5) +
  geom_point(data = df_fig8_loss, aes(x = SAFE_payoffs, y = SAFE_CPT_VALS), shape = 17, color = style_red, size = 2.5) +
  
  scale_y_continuous(limits = c(0, 450)) +
  xlab("Payoff") +
  ylab("CPT Utility")+
  scale_color_manual(
    values = c("gain" = style_green, "loss" = style_red),
    labels = c(expression(paste(" Gain domain: ",alpha, " = 0.88 ")), expression(paste("    Loss domain ",beta," = 0.88, ",lambda," = 2.25"))))+
  style_theme


'---------------------------------------'
'---------------------------------------'
'               FIGURE 10               '
'---------------------------------------' 
'---------------------------------------'
      

# UTILITY LOTTERY VQ


w_ineq <- function(p, delta, theta){
  theta*(p^delta)/((p^delta)+((1-p)^delta))^(1/delta)
}

thetas <- c(0.3, 0.6, 0.8)
betas <- c(beta_m, 0.85, 0.92)
y_b2 <- abs(v_l(x, betas[2], lambda_m))
y_b3 <- abs(v_l(x, betas[3], lambda_m))

  
create_vl_data_frame <- function(betas, thetas, lambda, delta) {
    result_df <- data.frame()
    
    for (beta in betas) {
      LOT_VQ_U <- w_ineq(0.5, delta_m, thetas[1]) * v_l(800, beta, lambda)
      LOT_VQ_CE <- ce_l(LOT_VQ_U, beta, lambda)
      
      result_df <- rbind(result_df, data.frame(
                        beta = beta, theta = thetas[1],
                        LOT_VQ_U = LOT_VQ_U, LOT_VQ_CE = LOT_VQ_CE))
      
      for (theta in thetas[-1]) {
        LOT_VQ_U_t <- w_ineq(0.5, delta, theta) * v_l(800, beta, lambda)
        LOT_VQ_CE_t <- ce_l(LOT_VQ_U_t, beta, lambda)
        
        result_df <- rbind(result_df, data.frame(
                        beta = beta, theta = theta,
                        LOT_VQ_U = LOT_VQ_U_t, LOT_VQ_CE = LOT_VQ_CE_t))
      }
    }
    
    return(result_df)
}
  
result <- create_vl_data_frame(betas, thetas, lambda, delta) 

df_fig10 <- data.frame(x = x, y_l = y_l, y_b2 = y_b2, y_b3 = y_b3)
color_group <- rep(c(style_red, style_blue, style_green), each = 3)


ggplot() +
  geom_line(data = df_fig10, aes(x = x, y = y_l, color = "b0.88")) +
  geom_line(data = df_fig10, aes(x = x, y = x), linetype = "dashed") +
  geom_line(data = df_fig10, aes(x = x, y = y_b2, color = "b0.85")) +
  geom_line(data = df_fig10, aes(x = x, y = y_b3, color = "b0.92")) +

  geom_point(data = result, aes(x = abs(LOT_VQ_CE), y = abs(LOT_VQ_U), shape = as.factor(theta)), 
             color = color_group, size = 2.5) +
  
  scale_y_continuous(limits = c(0, 500)) +
  
  xlab("Payoff") +
  ylab(expression(paste("CPT"["ineq"], " Utility")))+
  scale_color_manual(
    values = c("b0.88" = style_red, "b0.85" = style_blue, "b0.92" = style_green),
    # Caution with the order of the labels: NOT the same as above!
    labels = c(expression(paste(" ",beta," = 0.85")), 
               expression(paste(" ",beta," = 0.88")),
               expression(paste(" ",beta," = 0.92"))))+
  scale_shape_manual(
    values = c("0.3"=16, "0.6"=17, "0.8"=15),
    labels = c(expression(paste(" ",vartheta," = 0.3")), 
               expression(paste(" ",vartheta," = 0.6")),
               expression(paste(" ",vartheta," = 0.8"))))+
  style_theme


  
'---------------------------------------'
'---------------------------------------'
'               FIGURE 14               '
'---------------------------------------' 
'---------------------------------------'
  

norm_data <- rnorm(10000, mean = 0, sd = 1)
exp_data <- rexp(1000, rate = 0.7)
data_JW <- c(norm_data, 0 - (exp_data))

data_E <- rsn(10000, xi = 0, omega = 1, alpha = 1.14)
data_NE <- rsn(10000, xi = 0, omega = 1, alpha = -1.8)

hist(data_JW, breaks = 150, main = "Locus of Control across roles",
     xlab = "LoC", freq=F, 
     xlim = c(-3,2), ylim=c(0, 0.6))

lines(density(data_JW))
lines(density(data_E), col=style_green)
lines(density(data_NE), col=style_blue)

legend("topright", legend = c("Just World", "Elite", "Non-Elite"),
       col = c(style_black, style_green, style_blue), lty = 1, cex = 0.73)


  
####################################
#
# APPENDIX TABLES/ DATA FOR FIGURES
#
####################################
  
  
'---------------------------------------'
'---------------------------------------'
'        APPENDIX TABLE 1 - FIG 9       '
'---------------------------------------' 
'---------------------------------------'

'(moderate) risk-seeking in Cappelen et al. 2013a/VQ i=JW, T=0, t=1
                    AND
(moderate) risk-averse in in Cappelen et al. 2013a/VQ i=JW, T=0, t=1'
  
'---   FREQUENCIES  ---' 
BEHAVIOUR <- c("Strict risk-averse", "Risk-averse",
                "Moderate risk-averse",
                "Moderate-risk-seeking", "(Strict) Risk-seeking")
BEH_FREQ_CAP <- c(0, 5, 23, 43, 7)
BEH_FREQ_VQ_T0_JW <- c(0, 5, 0, 23+43, 7)

app_table1.1 <- data.frame(BEHAVIOUR, BEH_FREQ_CAP, BEH_FREQ_VQ_T0_JW)
  
  
'---   PERCENTAGES  ---' 
calculate_mrs_perc <- function(data) {
  mrs <- (data[4]+data[5])/sum(data)*100
    
  return (mrs)
}
  
calculate_beh_perc <- function(data1, data2) {
  mrs1 <- calculate_mrs_perc(data1)
  mrs2 <- calculate_mrs_perc(data2)
    
  mra1 <- (data1[2]+data1[3])/sum(data1)*100
  mra2 <- (data2[2]+data2[3])/sum(data2)*100
    
  return (data.frame(c(mrs1, mrs2), c(mra1, mra2)))
}
  
  
app_table1.2 <- calculate_beh_perc(BEH_FREQ_CAP, BEH_FREQ_VQ_T0_JW)
  
names(app_table1.2)[1] <- "(moderate) risk-seeking"
names(app_table1.2)[2] <- "(moderate) risk-averse"
row.names(app_table1.2) <- c("Cappelen et al. (2013a)", "Just World - Control Group")
  
  
  
'---------------------------------------'
'---------------------------------------'
'       APPENDIX TABLE 2 - FIG 11       '
'---------------------------------------' 
'---------------------------------------'
  
'(moderate) risk-seeking for i=JW, T=0, t=1/t=2
                  AND
(moderate) risk-seeking for i=E, T=0, t=1/t=2
                  AND
(moderate) risk-seeking for i=NE, T=0, t=1/t=2'
  
          #     JW      #
  
# Per definition participants in i=JW, T=0 do not present any changes in behaviour --> rep(., 2)
app_table2 <- data.frame(c(rep(app_table1.2$`(moderate) risk-seeking`[2], 2)))
names(app_table2)[1] <- "JW"
row.names(app_table2) <- c("Part 1", "Part 2")
  
  
'
Function that calculates the frequencies of risk preferences
given an "ineqA_perc" % of the sample experiencing inequality 
aversion. For the sake of simplicity assumption IV indicates 
that all individuals are characterized with theta = 0.8
'

calculate_ineq_freq <- function(data, ineqA_perc) {
  ineqA <- c(data[1], 
             data[2], data[4]+data[3], 0,       # since theta = 0.8
             data[5])*ineqA_perc
  ineqN <- data*(1-ineqA_perc)
    
  BEH_FREQ <- rowSums(data.frame(ineqN, ineqA))
    
  return (BEH_FREQ)
}
  
          #     E      #
  
# Assumption I --> ineqA_perc = 0.5
BEH_FREQ_VQ_T0_E <- calculate_ineq_freq(BEH_FREQ_VQ_T0_JW, 0.5)
  
  
  
# Assumption III --> rep(., 2)
app_table2$E <- c(rep(calculate_mrs_perc(BEH_FREQ_VQ_T0_E), 2))
  
  
          #     NE      #
  
# Assumption II --> ineqA_perc = 0.8
BEH_FREQ_VQ_T0_NE <- calculate_ineq_freq(BEH_FREQ_VQ_T0_JW, 0.8)
  
  
# Due to uncertainty of role in t=1, E and NE have the same expected behaviour in t=1
app_table2$NE <- c(calculate_mrs_perc(BEH_FREQ_VQ_T0_E), calculate_mrs_perc(BEH_FREQ_VQ_T0_NE))
  
  
ATE_NE <- app_table2$NE[2] - app_table2$NE[1]
  
  
  
  
'---------------------------------------'
'---------------------------------------'
'       APPENDIX TABLE 3 - FIG 12       '
'---------------------------------------' 
'---------------------------------------'
  
'(moderate) risk-seeking for i=JW, t=2, T=0/ T=1,2
                  AND
(moderate) risk-seeking for i=E, t=2, T=0/ T=1,2
                  AND
(moderate) risk-seeking for i=NE, t=2, T=0/ T=1,2'
  
app_table3 <- data.frame(c(app_table2$JW[2], 
                           app_table2$E[2], 
                           app_table2$NE[2]))
names(app_table3)[1] <- "Control Group"
row.names(app_table3) <- c("Just World", "Elite", "Non-Elite")

  
            #     JW      #
  
# Assumption V --> ineqA_perc = 0.6
BEH_FREQ_VQ_T12_JW <- calculate_ineq_freq(BEH_FREQ_VQ_T0_JW, 0.6)
  
  
            #     E      #
  
# Assumption V --> ineqA_perc = 0.6
BEH_FREQ_VQ_T12_E <- calculate_ineq_freq(BEH_FREQ_VQ_T0_E, 0.6)
  
  
            #     NE      #
  
# Assumption V --> ineqA_perc = 0.6
BEH_FREQ_VQ_T12_NE <- calculate_ineq_freq(BEH_FREQ_VQ_T0_NE, 0.6)
  
  
  
app_table3$"Treatment groups (T=1, T=2)" <- c(calculate_mrs_perc(BEH_FREQ_VQ_T12_JW), 
                                              calculate_mrs_perc(BEH_FREQ_VQ_T12_E),
                                              calculate_mrs_perc(BEH_FREQ_VQ_T12_NE))
  
app_table3$"ATE (in PP)" <- app_table3$"Treatment groups (T=1, T=2)"- app_table3$"Control Group"
app_table3$"ATE (in %)" <- app_table3$"ATE (in PP)"/app_table3$"Control Group"*100
  
  
  
  
'---------------------------------------'
'---------------------------------------'
'       APPENDIX TABLE 4 - FIG 13       '
'---------------------------------------' 
'---------------------------------------'
  
'(moderate) risk-seeking for i=JW, t=2, T=0/ T=1/ T=2
                  AND
(moderate) risk-seeking for i=E, t=2, T=0/ T=1/ T=2
                  AND
(moderate) risk-seeking for i=NE, t=2, T=0/ T=1/ T=2'
  
  
app_table4 <- data.frame(c(app_table3$`Control Group`))
names(app_table4)[1] <- "Control Group"
row.names(app_table4) <- c("Just World", "Elite", "Non-Elite")
  
  
'--- T = 1 ---'
  
# Assumption VI(.1) --> ineqA_perc = 0.8
BEH_FREQ_VQ_T1_JW <- calculate_ineq_freq(BEH_FREQ_VQ_T0_JW, 0.8)
  
  
BEH_FREQ_VQ_T1_E <- calculate_ineq_freq(BEH_FREQ_VQ_T0_E, 0.8)
  
  
BEH_FREQ_VQ_T1_NE <- calculate_ineq_freq(BEH_FREQ_VQ_T0_NE, 0.8)
  
  
  
app_table4$"Treatment Group 1" <- c(calculate_mrs_perc(BEH_FREQ_VQ_T1_JW), 
                                    calculate_mrs_perc(BEH_FREQ_VQ_T1_E),
                                    calculate_mrs_perc(BEH_FREQ_VQ_T1_NE))
  
  
'--- T = 2 ---'
  
# Assumption VI(.1) --> ineqA_perc = 0.4
BEH_FREQ_VQ_T2_JW <- calculate_ineq_freq(BEH_FREQ_VQ_T0_JW, 0.4)
  
  
BEH_FREQ_VQ_T2_E <- calculate_ineq_freq(BEH_FREQ_VQ_T0_E, 0.4)
  
  
BEH_FREQ_VQ_T2_NE <- calculate_ineq_freq(BEH_FREQ_VQ_T0_NE, 0.4)
  
  
  
app_table4$"Treatment Group 2" <- c(calculate_mrs_perc(BEH_FREQ_VQ_T2_JW), 
                                    calculate_mrs_perc(BEH_FREQ_VQ_T2_E),
                                    calculate_mrs_perc(BEH_FREQ_VQ_T2_NE))
  
  
'--- ATEs ---'
  
app_table4$"ATE 1 (in PP)" <- app_table4$"Treatment Group 1"- app_table4$"Control Group"
app_table4$"ATE 1 (in %)" <- app_table4$"ATE 1 (in PP)"/app_table4$"Control Group"*100

  
app_table4$"ATE 2 (in PP)" <- app_table4$"Treatment Group 2"- app_table4$"Control Group"
app_table4$"ATE 2 (in %)" <- app_table4$"ATE 2 (in PP)"/app_table4$"Control Group"*100

