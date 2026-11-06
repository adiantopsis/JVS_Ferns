ses_hurry <- function(data, strata, complete = F) {
        library(gawdis)
        library(tidyverse)
        library(SYNCSA)
        library(picante)
        set.seed(811) # Set seed for reproducibility
        options(scipen = 999) # Avoid scientific notation in numeric output

   if (!is.null(strata) && complete == FALSE) {

     threshold <- quantile(data$environmental$samp_size, probs = strata, na.rm = TRUE)

     keep_sites <- rownames(data$environmental)[data$environmental$samp_size > threshold]

     community <- data$community[rownames(data$community) %in% keep_sites, ]
     environmental <- data$environmental[rownames(data$environmental) %in% keep_sites, ]

        } else {
                community <- data$community
                environmental <- data$environmental
        }

org_data <- organize.syncsa(comm=community, envir= environmental, traits=data$traits)


        full_trait <- gawdis(
                org_data$traits,
                groups = c(1, 1, 1, 1 , 2, 2, 3, 3, 3, 3)
        )

        ses_mpd_full <- picante::ses.mpd(
                org_data$community,
                full_trait,
                abundance.weighted = TRUE,
                null.model = "richness"
        ) |>
                rownames_to_column(var = "Site")

        # Combine MPD results with environmental data
        ses <- cbind.data.frame(ses_mpd_full, org_data$environmental)

        ses_mpd_pa <- picante::ses.mpd(
                org_data$community,
                full_trait,
                abundance.weighted = FALSE,
                null.model = "richness"
        ) |>
                rownames_to_column(var = "Site")

        # Combine with environmental data
        ses_pa <- cbind.data.frame(ses_mpd_pa, org_data$environmental)

        parameters <- c(
                sample_size_threshold = ifelse(!is.null(strata), threshold, NA),
                total_rich = ncol(org_data$community),
                remaning_sites = nrow(org_data$community),
                mean_ses_ab = mean(ses$mpd.obs),
                mean_ses_pa = mean(ses_pa$mpd.obs),
                loss_sites = nrow(data$community) - nrow(org_data$community)
        )

        ses_result <- list(
                Data_AB = ses,
                Data_PA = ses_pa,
                Parameters = parameters
        )

        return(ses_result)
}
