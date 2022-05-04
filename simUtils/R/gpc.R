#' Perform Hypothesis Test
#'
#' GPC tests assuming larger values are preferred
#'
#' @param data data.table with the simulation data
#' @param target name of the target variable
#' @param type of the GPC ("univariate", (multivariate) "prioritized", (multivariate) "non-prioritized")
#' @param repeated vector of (prioritized order of) repeated measures (provided in a column named Time in dataset)
#' @param matching "matched" or "unmatched" GPC
#' @param side 1 or 2 for one- or two-sided test
#' @param best "higher" ("lower") if higher (lower) values are the preferred outcome
#'
#' @return the test result (TRUE if H0 is rejected, FALSE otherwise)
#' @export
gpc <- function(data,
                target,
                type,
                repeated,
                matching,
                side,
                best) {
  
  # Define univariate and multivariate scoring functions
  # Univariate score function for pairwise comparisons (here we assume larger 
  # values are preferred
  Score_fct <- function(Value_i, Value_j) {
    if (Value_i > Value_j) {
      Score <- -1
    } else if (Value_i < Value_j) {
      Score <- 1
    } else {
      Score <- 0
    }
    if (best == "higher") {
      return(Score)
    } else if (best == "lower") {
      return(-Score)
    }
  }
  
  # Multivariate score function for pairwise comparisons (here we assume larger 
  # values are preferred
  ScoreV <- function(Outcome, Trt) {
    n <- length(Trt)
    Score <- matrix(nrow = n, ncol = n)
    for (i in 1:n) {
      for (j in 1:n) {
        if (Outcome[i] > Outcome[j]) {
          Score[j, i] <- -1
        } else if (Outcome[i] < Outcome[j]) {
          Score[j, i] <- 1
        } else {
          Score[j, i] <- 0
        }
      }
    }
    if (best == "higher") {
      return(Score)
    } else if (best == "lower") {
      return(-Score)
    }
  }
  
  target <- as.symbol(target)
  
  if (type == "univariate") {
    
    data_sum <- data %>%
      group_by(Id, Group) %>%
      summarise(Sum = sum(!!target)) %>%
      ungroup()
    
    if (matching == "matched") {
      
      data_sum$Sum <- ifelse(
        data_sum$Group == "P",
        -data_sum$Sum,
        data_sum$Sum
      )
      data_sum <- data_sum %>%
        group_by(Id) %>%
        summarise(SumTx = sum(Sum)) %>%
        ungroup()
      
      score_positive <- ifelse(data_sum$SumTx > 0, 1, 0)
      score_negative <- ifelse(data_sum$SumTx < 0, 1, 0)
      if (best == "higher") {
        data_sum$ScoreT <- score_positive
        data_sum$ScoreC <- score_negative
      } else if (best == "lower") {
        data_sum$ScoreT <- score_negative
        data_sum$ScoreC <- score_positive
      }
      
      data_sumf <- data_sum %>%
        summarise(SumT = sum(ScoreT), SumC = sum(ScoreC)) %>%
        ungroup()
      
      # Perform two-sided and one-sided test
      # @Johan: what to do if SumT == SumC == 0?
      data_sumf$Z <- (data_sumf$SumT - data_sumf$SumC) / 
        sqrt(data_sumf$SumT + data_sumf$SumC)
      
      p_greater <- pnorm(as.numeric(data_sumf$Z), lower.tail = F)
      if (side == 1) {
        p_value <- p_greater
      } else if (side == 2) {
        p_less <- pnorm(as.numeric(data_sumf$Z), lower.tail = T)
        p_value <- ifelse(
          as.numeric(data_sumf$Z) > 0, 2 * p_greater, 2 * p_less
        )
      }
    } else if (matching == "unmatched") {
      
      # define number of subjects in each treatment arm
      Id_v <- as.data.frame(filter(data_sum, Group == "V"))
      nTest <- length(Id_v[, 2])  # @Johan: why 2nd column? Use nrow() instead?
      Id_p <- as.data.frame(filter(data_sum, Group == "P"))
      nControl <- length(Id_p[, 2])
      nPatients <- nTest + nControl
      
      # perform pairwise comparisons
      U_Gehan <- matrix(NA, nrow = nTest, ncol = nControl)
      
      for (i in 1:nTest) {
        for (j in 1:nControl) {
          U <- Score_fct(
            data_sum$Sum[data_sum$Group == "V"][i],
            data_sum$Sum[data_sum$Group == "P"][j]
          )
          U_Gehan[i, j] <- U
        }
      }
      
      Gehan <- mean(U_Gehan)
      
      # variance function
      U_Gehan_v <- matrix(NA, nrow = nPatients, ncol = nPatients)
      
      for (i in 1:nPatients) {
        for (j in 1:nPatients) {
          U_Gehan_v[i, j] <- Score_fct(data_sum$Sum[i], data_sum$Sum[j])
        }
      }
      
      Var_Gehan_P <- sum(rowSums(U_Gehan_v)^2) /
        (nTest * nControl * nPatients * (nPatients - 1))
      
      # Perform two-sided and one-sided test
      
      if (side == 1) {
        p_value <- pnorm(-(Gehan / sqrt(Var_Gehan_P)))
      } else if (side == 2) {
        p_value <- 2 * pnorm(-abs(Gehan / sqrt(Var_Gehan_P)))
      }
    }
  } else {
    
    Outcome <- split(select(data, !!target), data$Time)
    # @Johan: do we really only need the first entry?
    # If yes, the repeated parameter doesn't need to be a vector
    db_trt <- filter(data, Time == repeated[1])
    Trt <- ifelse(db_trt$Group == "V", 1, 0)
    nTest <- length(Trt[Trt == 1])
    nControl <- length(Trt[Trt == 0])
    nPatients <- length(Trt)
    
    list_D <- numeric()
    listD_cumulative <- numeric()
    list_V <- numeric()
    listV_cumulative <- numeric()
    Score_prev <- 0
    
    list_npD <- numeric()
    listnpD_cumulative <- numeric()
    list_npV <- numeric()
    listnpV_cumulative <- numeric()
    Score_npprev <- 0
    
    list_mpD <- numeric()
    listmpD_cumulative <- numeric()
    list_mpV <- numeric()
    listmpV_cumulative <- numeric()
    Score_mpprev <- 0
    
    for (i in 1:length(Outcome)) {
      Score_V <- ScoreV(unlist(Outcome[[i]]), Trt)
      
      if (type == "prioritized") {
        
        if (matching == "unmatched") {
          
          Score_pV <- Score_V * (1 - abs(Score_prev))
          Score_D <- Score_pV[which(Trt == 1), which(Trt == 0)]
          Score_prev <- Score_prev + Score_pV
          list_D[i] <- mean(Score_D)
          listD_cumulative[i] <- sum(list_D[1:i])
          
          list_V[i] <- sum(rowSums(Score_pV)^2) / 
            (nTest * nControl * nPatients * (nPatients - 1))
          listV_cumulative[i] <- sum(rowSums(Score_prev)^2) / 
            (nTest * nControl * nPatients * (nPatients - 1))
          
          pNB <- listD_cumulative[length(Outcome)]
          pNB_var <- listV_cumulative[length(Outcome)]
          if (side == 1) {
            p_value <- pnorm((-pNB / sqrt(pNB_var)))
          } else if (side == 2) {
            p_value <- 2 * pnorm(-abs(pNB / sqrt(pNB_var)))
          }
        }
        
        if (matching == "matched") {
          
          Score_mpV <- Score_V * (1 - abs(Score_mpprev))
          ID_b <- c(rep(1:(length(Trt) / 2), each = 2))
          Score_mD <- numeric()
          for (j in 1:(length(ID_b) / 2)) {
            Score_mD[j] <- Score_V[
              which(ID_b == j & Trt == 1), which(ID_b == j & Trt == 0)]
          }
          Score_mpprev <- Score_mpprev + Score_mpV
          
          list_mpD[i] <- sum(Score_mD)
          listmpD_cumulative[i] <- sum(list_mpD[1:i])
          list_mpV[i] <- sum(abs(Score_mD))
          listmpV_cumulative[i] <- sum(list_mpV[1:i])
          
          mpNB <- listmpD_cumulative[length(Outcome)]
          mpNB_var <- listmpV_cumulative[length(Outcome)]
          if (side == 1) {
            p_value <- pnorm((-mpNB / sqrt(mpNB_var)))
          } else if (side == 2) {
            p_value <- 2 * pnorm(-abs(mpNB / sqrt(mpNB_var)))
          }
        }
      } else if (type == "non-prioritized") {
        
        if (matching == "matched") {
          stop("Error: cannot perform matched non-prioritized GPC")
          
        } else if (matching == "unmatched") {
          
          Score_npD <- Score_V[which(Trt == 1), which(Trt == 0)]
          Score_npprev <- Score_npprev + Score_V
          list_npD[i] <- mean(Score_npD)
          listnpD_cumulative[i] <- sum(list_npD[1:i])
          list_npV[i] <- sum(rowSums(Score_V)^2) / 
            (nTest * nControl * nPatients * (nPatients - 1))
          listnpV_cumulative[i] <- sum(rowSums(Score_npprev)^2) / 
            (nTest * nControl * nPatients * (nPatients - 1))
          
          npNB <- listnpD_cumulative[length(Outcome)] / length(Outcome)
          npNB_var <- listnpV_cumulative[length(Outcome)] / length(Outcome)^2
          if (side == 1) {
            p_value <- pnorm((-npNB / sqrt(npNB_var)))
          } else if (side == 2) {
            p_value <- 2 * pnorm(-abs(npNB / sqrt(npNB_var)))
          }
        }
      }
    }
  }
  return(p_value)
}
