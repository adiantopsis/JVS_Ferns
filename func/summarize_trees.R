# Function to get cwm.sig organize results
# You must give 3 matrix: community, trait, environmental data,
# as well as, a formula of the model, number of iterations, if the model include
# or not, Moran Eigenvector Maps (MEM) to control spatial correlation.
get_model <- function(mod, mpd = F) {
  require(vegan)
  fited <- mod

  summ_c <- summary(fited)$coefficients
  r2_adj <- RsquareAdj(fited)[2]
  if (mpd == T){
  formula_chr <- paste(as.character(rownames(summ_c)[-1]), collapse = "+")
  } else {
  formula_chr <- as.character(summary(mod)$call[2])
  }
  # Pega os nomes das variáveis do modelo
  var_names <- rownames(summ_c)

  # Cria um data.frame inicial com estatísticas principais
  res <- data.frame(
    model = formula_chr,
    r2 = r2_adj,
    stringsAsFactors = FALSE
  )

  # Para cada variável, adiciona colunas de Estimate e p-valor
  for (i in seq_along(var_names)) {
    est_name <- var_names[i]
    res[[est_name]] <- summ_c[i, "Estimate"]
    res[[paste0("P-", est_name)]] <- summ_c[i, "Pr(>|t|)"]
  }
  res
}

get_cwm <- function(comm, trait, env, formula, runs, model = T) {
  # First we run cwm.sig
  # If mem is included on environmental data then we skip this part and
  # reformulate to include the mem
  mem <- grep(pattern = "MEM", x = colnames(env), value = T)
  if (length(mem) == 0) {
    mem <- NULL
  }

  if (is.null(mem)) {
    sig <- cwm.sig(
      comm = comm,
      traits = trait,
      envir = env,
      formula = formula,
      runs = runs,
      PGLS = F
    )
  } else {
    # If you included MEM in environmental data, then, set mem
    lhs <- all.vars(formula[[2]])
    rhs <- attr(terms(formula), "term.labels")
    rhs_new <- c(rhs, mem)
    new_formula <- reformulate(rhs_new, response = lhs)

    sig <- cwm.sig(
      comm = comm,
      traits = trait,
      envir = env,
      formula = new_formula,
      runs = runs,
      PGLS = F
    )
  }

  fited <- sig$model
  summ_c <- summary(fited)$coefficients
  summ_coef <- summ_c
  coefs <- coef(fited)
  r2_adj <- RsquareAdj(fited)[2]
  formula_chr <- paste(as.character(sig$formula)[-1], collapse = "~")

  # Pega os nomes das variáveis do modelo
  var_names <- rownames(summ_c)

  # Cria um data.frame inicial com estatísticas principais
  res <- data.frame(
    ss = sig$prob.site.shuffle,
    ts = sig$prob.trait.shuffle,
    Fstat = sig$statistic.obs,
    r2 = r2_adj,
    model = formula_chr,
    stringsAsFactors = FALSE
  )

  # Para cada variável, adiciona colunas de Estimate e p-valor
  for (i in seq_along(var_names)) {
    est_name <- var_names[i]
    res[[est_name]] <- summ_c[i, "Estimate"]
    res[[paste0("P-", est_name)]] <- summ_c[i, "Pr(>|t|)"]
  }
  if (model == T) {
    a <- list(cwm.sig = res, model = fited)
    return(a)
  } else {
    return(res)
  }
}

table_cwm <- function(result) {
  x <- sapply(result, function(col) {
    if (is.numeric(col)) {
      mean_1 <- mean(col, na.rm = TRUE)
      sd_1 <- sd(col, na.rm = TRUE)
      paste0(round(mean_1, 3), " [±", round(sd_1, 3), "]")
    } else {
      unique(col) # mantém o valor original (por exemplo, modelo)
    }
  })

  # adiciona t+s no início
  x <- c("t+s" = sum(result$ss <= 0.05 & result$ts <= 0.05, na.rm = TRUE), x)
  
  y <- c(count_ss= sum(result$ss <= 0.05),
    count_ts = sum(result$ts <= 0.05), x)

  as.data.frame(t(y), stringsAsFactors = FALSE)
}
