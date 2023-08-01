Manuscript Title: Upslope migration is slower in insects that depend on metabolically demanding flight

Year: 2023

File Name: insect elevation shifts.csv
Description: Rates of upslope migration (meters per year) among insects at the highest, middle, and lowest portions of the species’ elevational range. This is a subset of the larger BioShifts dataset (Lenoir et al. 2020, Nature Ecol Evol). The R scripts that use this dataset are named “annotated insect elevational shift analyses.R”. 
Rows: 1418, excluding header. Each row corresponds to a species’ elevational shift rate at the highest, middle, or lowest portion of its elevation range.
Columns: 11. Family, Genus, Species, volant, Position, ShiftR, PrAb, Sampling, Grain, Quality, Start   

Family: Taxonomic family name of each species

Genus: Taxonomic family name of each genus

Species: Taxonomic species name of each species

Volant: whether the species relies entirely on flight for locomotion (y) or can use other methods of locomotion (n) 

Position: whether the range shift measurement was at the highest portion (Leading edge), middle (Centroid), and lowest portion of the of the species’ range (Trailing edge). 

ShiftR: The meters per year that a species has shifted its elevational range at each range position.

PrAb: How the range shift was calculated in the original study. See Comte et al. 2020 (FigShare) & Lenoir et al. (2020, Nat Ecol Evol) for full description: ABUND = range shift calculated using abundance data; OCCUR = range shift calculated using occurrence data

Sampling: How many times the range shift was sampled in the original study. See Comte et al. 2020 (FigShare) & Lenoir et al. (2020, Nat Ecol Evol) for full description: CONT = yearly sampling or more frequently; IRR = multiple sampling periods irregularly distributed; TWO = two sampling periods

Grain: The grain at which the range shifts were sample. See Comte et al. 2020 (FigShare) & Lenoir et al. (2020, Nat Ecol Evol) for full description. Following their descriptions: FINE = spatial resolution of sampling less than 10 km; MEDIUM = spatial resolution of sampling between 10 km and 100 km.

Quality: The “quality” of the sampling of the range shifts. See Comte et al. 2020 (FigShare) & Lenoir et al. (2020, Nat Ecol Evol) for full description. Following their descriptions: LOW = no pre-processing of raw data; BALANCED = data cleaning or resampling procedures were carried out; MODELED = range shifts were quantified species ranges modeled using species distribution models; RESURVEYED = range shifts were quantified from paired designs such as permanent plots

Start: The starting year from the original study over which the mean annual elevational shift was estimated.
