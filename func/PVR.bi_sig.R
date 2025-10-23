# Function to Correct Phylogenetic Signal in Binomial Traits via PVR

# Required Packages: 
library (binomTools)
library (rsq)
library (PVR)

PVR.bi_sig <- function(tree, t) {
  dectree <- PVRdecomp(tree)
  
  trait <- t[which(tree$tip.label == names(t))]
  
  PVR_res <-  PVR::PVR(x = dectree,
                       #eigenvector selection
                       trait = trait,
                       method = "moran")
  
  glm_pvr <- glm(trait ~ PVR_res@Selection$Vectors, family = "binomial")
  
  S <- binomTools::Residuals(glm_pvr, type = "standard.pearson")
  
  pR2_T <- rsq::rsq.n(glm_pvr) ## require
  
  S_glm <- glm(S ~ PVR_res@Selection$Vectors)
  
  pR2_S <- rsq::rsq.n(S_glm)
  
  names (S) <- names (trait)
  
  PVR_R2 <- PVR_res@PVR$R2 #keep r2 (intensity of signal in PVR)
  
  x <- list(
    corrected_trait = S,
    phylo.sig.results = cbind.data.frame(
      R2_Original = PVR_R2,
      Sig.T_comp = pR2_T,
      Sig.S_comp = pR2_S)
    )
  print(x)
}

# Description
#>This function corrects the phylogenetic signal in binomial traits. This framework was first #proposed by Iserhard et al. (2018). It requires three packages: binomTools, PVR, and rsq.

## Usage
#> PVR.bi_sig (tree, t = trait)

## Arguments
#To execute, you must provide two arguments:
#> tree: A phylogenetic tree to be decomposed into a set of orthogonal eigenvectors.
#> t: A binomial trait (i.e., a vector with 0 or 1 values) with species names.

## Value
#This function returns an object of class list with two elements:
#> 1. corrected_trait: a vector with Pearson standard residuals from a GLM, using the PVR-selected eigenvector as a predictor and a binomial trait as response.
#> 2. phylo.sig.results: a vector with the R² from each step of the analyses:
#>> R2_Original: The original PVR's R².
#>> Sig.T_comp: Nagelkerke R², measured with the raw trait modeled by the  PVR-selected eigenvector in a binomial GLM.
#>> Sig.S_comp: Nagelkerke R², measured for the corrected phylogenetic trait (i.e., Pearson standard residuals) modeled by the PVR-selected eigenvector in a Gaussian GLM.

# Authors
#> Denilson Machado, Department of Botany, Universidade Federal do Rio Grande do Sul (UFRGS)

### Generic function to capture results after applying this function iteratively ###
TABLE_MEAN<-function(x,y){
PVR_R2<-capture.output(cat(round(mean(x$R2_Original),2),
                           "[", round(range(x$R2_Original),2), "]"))
T_comp_pR2<-capture.output(cat(round(mean(x$Sig.T_comp), 2),
                               "[", round(range(x$Sig.T_comp),2),"]"))
S_comp_pR2<-capture.output(cat(round(mean(x$Sig.S_comp),4),
                               "[", round(range(x$Sig.S_comp),4), "]"))
vec<-rbind.data.frame(PVR_R2, T_comp_pR2, S_comp_pR2)
colnames(vec)<-y
rownames(vec)<-c("R2_Original", "Sig.T_comp", "Sig.S_comp")
return(vec)
}
