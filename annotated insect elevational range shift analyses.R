setwd('/Users/michaelmoore/desktop/Working Directory')

library(car)
library(MASS)
library(lme4)
library(lmerTest)
library(emmeans)
library(visreg)
library(ggplot2)
library(viridis)

up <- read.csv('insect elevational shifts.csv')
head(up)
dim(up)

# basic model. not a good phylogeny for diversity at this scale, and no one else tries it either, so let's go with the simplest model formulation from Lenoir et al. 2020. Use start date to account for the fact that mean estimate will be smaller if we started measuring back in the early 1900's when the rate of climate change wasn't as fast
mod01 <- lmer(ShiftR ~ volant * Position + Start + (1|Species) + (1|Genus) + (1|Family), data = up)
summary(mod01)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: ShiftR ~ volant * Position + Start + (1 | Species) + (1 | Genus) +      (1 | Family)
   # Data: up

# REML criterion at convergence: 9751.9

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -4.7024 -0.3501 -0.0293  0.3603  7.3788 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # Species  (Intercept)  4.1324  2.0328  
 # Genus    (Intercept)  0.6878  0.8293  
 # Family   (Intercept)  1.7648  1.3285  
 # Residual             51.8892  7.2034  
# Number of obs: 1418, groups:  Species, 807; Genus, 463; Family, 72

# Fixed effects:
                                # Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                   -7.465e+01  1.635e+01  2.846e+02  -4.565 7.43e-06 ***
# volanty                       -2.023e+00  1.441e+00  1.680e+02  -1.403 0.162406    
# PositionLeading edge           1.372e+00  1.304e+00  9.219e+02   1.052 0.293099    
# PositionTrailing edge         -3.980e+00  1.417e+00  1.023e+03  -2.809 0.005062 ** 
# Start                          4.006e-02  8.334e-03  2.532e+02   4.807 2.63e-06 ***
# volanty:PositionLeading edge   9.352e-02  1.451e+00  7.769e+02   0.064 0.948637    
# volanty:PositionTrailing edge  5.850e+00  1.583e+00  1.070e+03   3.695 0.000231 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
            # (Intr) volnty PstnLe PstnTe Start  vl:PLe
# volanty     -0.136                                   
# PstnLdngedg -0.339  0.794                            
# PstnTrlnged -0.034  0.543  0.597                     
# Start       -0.997  0.068  0.271 -0.014              
# vlnty:PstLe  0.291 -0.819 -0.895 -0.537 -0.230       
# vlnty:PstTe  0.075 -0.558 -0.546 -0.894 -0.033  0.574

anova(mod01, ddf = 'Kenward-Roger') # suggests a strong volant by range shift position interaction
# Type III Analysis of Variance Table with Kenward-Roger's method
                 # Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
# volant             0.09    0.09     1  31.64  0.0018  0.966885    
# Position         609.89  304.95     2 848.46  5.8763  0.002921 ** 
# Start           1116.70 1116.70     1 160.61 21.5209 7.220e-06 ***
# volant:Position 1023.68  511.84     2 656.58  9.8605 6.035e-05 ***

emmeans(mod01, pairwise~volant|Position, lmer.df = 'Kenward-Roger') # planned contrasts - completely volant vs non-completely volant at each range position. This is the most direct and interpretable thing to compare since rates of warming are different at diff parts of species' elevational range shifts. Use KR ddf approximation since it's most conservative. All results signif and in same direction with Satterthwaite ddf too. 

# $emmeans
# Position = Centroid:
 # volant emmean    SE    df lower.CL upper.CL
 # n       3.144 1.296 257.3    0.591     5.70
 # y       1.121 0.731  22.1   -0.395     2.64

# Position = Leading edge:
 # volant emmean    SE    df lower.CL upper.CL
 # n       4.516 0.576  47.3    3.357     5.67
 # y       2.587 0.695  26.2    1.159     4.01

# Position = Trailing edge:
 # volant emmean    SE    df lower.CL upper.CL
 # n      -0.836 1.208 261.0   -3.214     1.54
 # y       2.991 0.847  48.0    1.289     4.69

# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 

# $contrasts
# Position = Centroid:
 # contrast estimate    SE    df t.ratio p.value
 # n - y        2.02 1.488 106.7  1.359  0.1770 

# Position = Leading edge:
 # contrast estimate    SE    df t.ratio p.value
 # n - y        1.93 0.904  32.4  2.134  0.0405 

# Position = Trailing edge:
 # contrast estimate    SE    df t.ratio p.value
 # n - y       -3.83 1.469 117.2 -2.605  0.0104 

# Degrees-of-freedom method: kenward-roger 





#### so that's the most sensible model based on the data. 

#### In the Lenoir et al paper, they also included some random effects to control for data quality. They had many more rangeshift estimates in there than we do here because it's across so many diff species, but let's add in the random effects that they say they had and that we have the resolution for.
## tldr: throughout these analyses, effects of interest basically don't change more than a rounding error because these additional random effects have such a miniscule impact 

mod01b <- lmer(ShiftR ~ volant * Position + Start + (1|Species) + (1|Genus) + (1|Family) + (1|Quality) + (1|Sampling) + (1|PrAb) + (1|Grain), data = up)
summary(mod01b) # this model is way overfit

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: ShiftR ~ volant * Position + Start + (1 | Species) + (1 | Genus) +      (1 | Family) + (1 | Quality) + (1 | Sampling) + (1 | PrAb) +      (1 | Grain)
   # Data: up.clean

# REML criterion at convergence: 9751.9

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -4.7025 -0.3501 -0.0293  0.3603  7.3788 

# Random effects:
 # Groups   Name        Variance  Std.Dev. 
 # Species  (Intercept) 4.132e+00 2.0327943
 # Genus    (Intercept) 6.880e-01 0.8294624
 # Family   (Intercept) 1.765e+00 1.3284888
 # Quality  (Intercept) 0.000e+00 0.0000000
 # Sampling (Intercept) 2.591e-08 0.0001610
 # PrAb     (Intercept) 2.514e-08 0.0001586
 # Grain    (Intercept) 2.511e-04 0.0158462
 # Residual             5.189e+01 7.2034155
# Number of obs: 1418, groups:  Species, 807; Genus, 463; Family, 72; Quality, 4; Sampling, 3; PrAb, 2; Grain, 2

# Fixed effects:
                                # Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                   -7.465e+01  1.635e+01  2.845e+02  -4.565 7.43e-06 ***
# volanty                       -2.023e+00  1.441e+00  1.680e+02  -1.403 0.162417    
# PositionLeading edge           1.372e+00  1.304e+00  9.219e+02   1.052 0.293085    
# PositionTrailing edge         -3.980e+00  1.417e+00  1.023e+03  -2.809 0.005062 ** 
# Start                          4.006e-02  8.334e-03  2.532e+02   4.807 2.63e-06 ***
# volanty:PositionLeading edge   9.347e-02  1.451e+00  7.769e+02   0.064 0.948663    
# volanty:PositionTrailing edge  5.850e+00  1.583e+00  1.070e+03   3.695 0.000231 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
            # (Intr) volnty PstnLe PstnTe Start  vl:PLe
# volanty     -0.136                                   
# PstnLdngedg -0.339  0.794                            
# PstnTrlnged -0.034  0.543  0.597                     
# Start       -0.997  0.068  0.271 -0.014              
# vlnty:PstLe  0.291 -0.819 -0.895 -0.537 -0.230       
# vlnty:PstTe  0.075 -0.558 -0.546 -0.894 -0.033  0.574
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see ?isSingular



emmeans(mod01b, pairwise~volant|Position, lmer.df = 'Kenward-Roger') # even in this extremely overfit model (e.g. look at those CIs), still get sig contrasts

# $emmeans
# Position = Centroid:
 # volant emmean   SE   df   lower.CL  upper.CL
 # n       3.144 5.58 0.03  -2.98e+44  2.98e+44
 # y       1.121 5.40 0.00       -Inf       Inf

# Position = Leading edge:
 # volant emmean   SE   df   lower.CL  upper.CL
 # n       4.516 5.43 0.00       -Inf       Inf
 # y       2.587 5.28 0.00       -Inf       Inf

# Position = Trailing edge:
 # volant emmean   SE   df   lower.CL  upper.CL
 # n      -0.836 5.57 0.02  -7.06e+58  7.06e+58
 # y       2.991 5.38 0.01 -1.65e+256 1.65e+256

# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 

# $contrasts
# Position = Centroid:
 # contrast estimate    SE  df t.ratio p.value
 # n - y        2.02 1.504 106  1.345  0.1816 

# Position = Leading edge:
 # contrast estimate    SE  df t.ratio p.value
 # n - y        1.93 0.947  32  2.037  0.0500 

# Position = Trailing edge:
 # contrast estimate    SE  df t.ratio p.value
 # n - y       -3.83 1.526 114 -2.508  0.0135 

# Degrees-of-freedom method: kenward-roger 

#### when we subsetted the data just to insects, probably some of the random effects in Lenoir et al. 2020 don't make a ton of sense to use any more. let's remove the random effects where the data are distributed non-sensically - looks like its sampling and grain

table(up.clean$Quality)
  # BALANCED        LOW    MODELED RESURVEYED 
        # 46        689         22        661 
        
table(up.clean$PrAb)
# ABUND OCCUR 
  # 705   713 

table(up.clean$Grain)
  # FINE MEDIUM 
  # 1414      4 
  
table(up.clean$Sampling)
# CONT  IRR  TWO 
   # 5    1 1412 

# run model without sampling and grain
mod01c <- lmer(ShiftR ~ volant * Position + Start + (1|Species) + (1|Genus) + (1|Family) + (1|Quality) + (1|PrAb), data = up)
summary(mod01c)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: ShiftR ~ volant * Position + Start + (1 | Species) + (1 | Genus) +      (1 | Family) + (1 | Quality) + (1 | PrAb)
   # Data: up.clean

# REML criterion at convergence: 9751.9

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -4.7025 -0.3501 -0.0293  0.3603  7.3788 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # Species  (Intercept)  4.1323  2.0328  
 # Genus    (Intercept)  0.6879  0.8294  
 # Family   (Intercept)  1.7648  1.3285  
 # Quality  (Intercept)  0.0000  0.0000  
 # PrAb     (Intercept)  0.0000  0.0000  
 # Residual             51.8892  7.2034  
# Number of obs: 1418, groups:  Species, 807; Genus, 463; Family, 72; Quality, 4; PrAb, 2

# Fixed effects:
                                # Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                   -7.465e+01  1.635e+01  2.846e+02  -4.565 7.43e-06 ***
# volanty                       -2.023e+00  1.441e+00  1.680e+02  -1.403 0.162411    
# PositionLeading edge           1.372e+00  1.304e+00  9.219e+02   1.052 0.293094    
# PositionTrailing edge         -3.980e+00  1.417e+00  1.023e+03  -2.809 0.005062 ** 
# Start                          4.006e-02  8.334e-03  2.532e+02   4.807 2.63e-06 ***
# volanty:PositionLeading edge   9.350e-02  1.451e+00  7.769e+02   0.064 0.948647    
# volanty:PositionTrailing edge  5.850e+00  1.583e+00  1.070e+03   3.695 0.000231 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
            # (Intr) volnty PstnLe PstnTe Start  vl:PLe
# volanty     -0.136                                   
# PstnLdngedg -0.339  0.794                            
# PstnTrlnged -0.034  0.543  0.597                     
# Start       -0.997  0.068  0.271 -0.014              
# vlnty:PstLe  0.291 -0.819 -0.895 -0.537 -0.230       
# vlnty:PstTe  0.075 -0.558 -0.546 -0.894 -0.033  0.574
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see ?isSingular

emmeans(mod01c, pairwise~volant|Position, lmer.df = 'Kenward-Roger') # judging by the giant estimated CIs, this model is probably also a touch overfit too. there are still two random effects estimated at 0, so it's probably having trouble with that. Nonetheless, effects are qualtitatively the same
# $emmeans
# Position = Centroid:
 # volant emmean    SE    df lower.CL upper.CL
 # n       3.144 1.485 11.71   -0.101     6.39
 # y       1.121 0.964  1.13   -8.210    10.45

# Position = Leading edge:
 # volant emmean    SE    df lower.CL upper.CL
 # n       4.516 0.926  0.44 -337.330   346.36
 # y       2.587 0.905  1.04   -7.811    12.98

# Position = Trailing edge:
 # volant emmean    SE    df lower.CL upper.CL
 # n      -0.836 1.465  8.19   -4.200     2.53
 # y       2.991 1.003  2.38   -0.730     6.71

# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 

# $contrasts
# Position = Centroid:
 # contrast estimate    SE    df t.ratio p.value
 # n - y        2.02 1.503 106.2  1.346  0.1812 

# Position = Leading edge:
 # contrast estimate    SE    df t.ratio p.value
 # n - y        1.93 0.933  32.1  2.068  0.0468 

# Position = Trailing edge:
 # contrast estimate    SE    df t.ratio p.value
 # n - y       -3.83 1.523 113.6 -2.513  0.0134 

# Degrees-of-freedom method: kenward-roger 

### since "quality" isn't having a big effect, let's go ahead and drop that one just to see what happens
mod01d <- lmer(ShiftR ~ volant * Position + Start + (1|Species) + (1|Genus) + (1|Family) + (1|PrAb), data = up)
summary(mod01d)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: ShiftR ~ volant * Position + Start + (1 | Species) + (1 | Genus) +      (1 | Family) + (1 | PrAb)
   # Data: up.clean

# REML criterion at convergence: 9751.9

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -4.7024 -0.3501 -0.0293  0.3603  7.3788 

# Random effects:
 # Groups   Name        Variance  Std.Dev. 
 # Species  (Intercept) 4.132e+00 2.033e+00
 # Genus    (Intercept) 6.877e-01 8.293e-01
 # Family   (Intercept) 1.765e+00 1.328e+00
 # PrAb     (Intercept) 9.068e-09 9.523e-05
 # Residual             5.189e+01 7.203e+00
# Number of obs: 1418, groups:  Species, 807; Genus, 463; Family, 72; PrAb, 2

# Fixed effects:
                                # Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                   -7.465e+01  1.635e+01  2.846e+02  -4.565 7.43e-06 ***
# volanty                       -2.023e+00  1.441e+00  1.680e+02  -1.403 0.162407    
# PositionLeading edge           1.372e+00  1.304e+00  9.219e+02   1.052 0.293101    
# PositionTrailing edge         -3.980e+00  1.417e+00  1.023e+03  -2.809 0.005062 ** 
# Start                          4.006e-02  8.334e-03  2.532e+02   4.807 2.63e-06 ***
# volanty:PositionLeading edge   9.352e-02  1.451e+00  7.769e+02   0.064 0.948636    
# volanty:PositionTrailing edge  5.850e+00  1.583e+00  1.070e+03   3.695 0.000231 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
            # (Intr) volnty PstnLe PstnTe Start  vl:PLe
# volanty     -0.136                                   
# PstnLdngedg -0.339  0.794                            
# PstnTrlnged -0.034  0.543  0.597                     
# Start       -0.997  0.068  0.271 -0.014              
# vlnty:PstLe  0.291 -0.819 -0.895 -0.537 -0.230       
# vlnty:PstTe  0.075 -0.558 -0.546 -0.894 -0.033  0.574
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see ?isSingular

emmeans(mod01d, pairwise~volant|Position, lmer.df = 'Kenward-Roger') # still not great in the fitting category, same results here. So the result seems robust to all the random effects that Lenoir et al. 2020 used
# $emmeans
# Position = Centroid:
 # volant emmean    SE    df lower.CL upper.CL
 # n       3.144 1.310 17.95   0.3906     5.90
 # y       1.121 0.732  1.60  -2.9298     5.17

# Position = Leading edge:
 # volant emmean    SE    df lower.CL upper.CL
 # n       4.516 0.619  0.69 -19.7522    28.78
 # y       2.587 0.759  1.28  -3.2408     8.41

# Position = Trailing edge:
 # volant emmean    SE    df lower.CL upper.CL
 # n      -0.836 1.268 11.95  -3.6008     1.93
 # y       2.991 0.901  2.82   0.0164     5.97

# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 

# $contrasts
# Position = Centroid:
 # contrast estimate    SE    df t.ratio p.value
 # n - y        2.02 1.496 106.5  1.352  0.1792 

# Position = Leading edge:
 # contrast estimate    SE    df t.ratio p.value
 # n - y        1.93 0.908  32.3  2.126  0.0413 

# Position = Trailing edge:
 # contrast estimate    SE    df t.ratio p.value
 # n - y       -3.83 1.471 116.7 -2.601  0.0105 

# Degrees-of-freedom method: kenward-roger 

mod01e <- lmer(ShiftR ~ volant * Position + Start + (1|Species) + (1|Genus) + (1|Family) + (1|Quality), data = up)
summary(mod01e)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: ShiftR ~ volant * Position + Start + (1 | Species) + (1 | Genus) +      (1 | Family) + (1 | Quality)
   # Data: up

# REML criterion at convergence: 9751.9

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -4.7024 -0.3501 -0.0293  0.3603  7.3788 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # Species  (Intercept)  4.1333  2.0331  
 # Genus    (Intercept)  0.6867  0.8287  
 # Family   (Intercept)  1.7647  1.3284  
 # Quality  (Intercept)  0.0000  0.0000  
 # Residual             51.8892  7.2034  
# Number of obs: 1418, groups:  Species, 807; Genus, 463; Family, 72; Quality, 4

# Fixed effects:
                                # Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                   -7.465e+01  1.635e+01  2.846e+02  -4.565 7.43e-06 ***
# volanty                       -2.023e+00  1.441e+00  1.680e+02  -1.403 0.162388    
# PositionLeading edge           1.372e+00  1.304e+00  9.219e+02   1.052 0.293133    
# PositionTrailing edge         -3.980e+00  1.417e+00  1.023e+03  -2.809 0.005062 ** 
# Start                          4.006e-02  8.334e-03  2.532e+02   4.807 2.63e-06 ***
# volanty:PositionLeading edge   9.361e-02  1.451e+00  7.769e+02   0.065 0.948583    
# volanty:PositionTrailing edge  5.850e+00  1.583e+00  1.070e+03   3.695 0.000231 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
            # (Intr) volnty PstnLe PstnTe Start  vl:PLe
# volanty     -0.136                                   
# PstnLdngedg -0.339  0.794                            
# PstnTrlnged -0.034  0.543  0.597                     
# Start       -0.997  0.068  0.271 -0.014              
# vlnty:PstLe  0.291 -0.819 -0.895 -0.537 -0.230       
# vlnty:PstTe  0.075 -0.558 -0.546 -0.894 -0.033  0.574
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

emmeans(mod01e, pairwise~volant|Position, lmer.df = 'Kenward-Roger') 
# $emmeans
# Position = Centroid:
 # volant emmean    SE    df lower.CL upper.CL
 # n       3.144 1.454 19.21    0.102     6.19
 # y       1.121 0.950  2.05   -2.881     5.12

# Position = Leading edge:
 # volant emmean    SE    df lower.CL upper.CL
 # n       4.516 0.868  0.72  -23.542    32.57
 # y       2.587 0.829  2.47   -0.405     5.58

# Position = Trailing edge:
 # volant emmean    SE    df lower.CL upper.CL
 # n      -0.836 1.379 14.52   -3.784     2.11
 # y       2.991 0.938  6.01    0.699     5.28

# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 

# $contrasts
# Position = Centroid:
 # contrast estimate   SE    df t.ratio p.value
 # n - y        2.02 1.49 106.2   1.353  0.1788

# Position = Leading edge:
 # contrast estimate   SE    df t.ratio p.value
 # n - y        1.93 0.93  32.2   2.074  0.0461

# Position = Trailing edge:
 # contrast estimate   SE    df t.ratio p.value
 # n - y       -3.83 1.52 114.6  -2.526  0.0129

# Degrees-of-freedom method: kenward-roger 