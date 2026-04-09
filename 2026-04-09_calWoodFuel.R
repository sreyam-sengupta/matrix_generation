library(magclass)
library(gdx2)
library(dplyr)
library(tidyr)


# From Florian Humpenöder
# Forestry residues feed into woodfuel, reporting woodfuel in EJ would capture both without double-counting.

# define the MAgPIE output gdx
gdx <- "C:/Model/magpie/output/default_2026-02-26_00.04.05_50pc/fulldata.gdx"

# gdx <- "C:/Model/magpie/output/default_2026-02-26_08.15.59_10pc/fulldata.gdx"
# 
# gdx <- "C:/Model/magpie/output/default_2026-02-25_20.35.10/fulldata.gdx"


# read in iso-level woodfuel output
# demandWoodFuelIso  <- gdx2::readGDX(gdx, "p73_forestry_demand_prod_specific")[, , "wood_fuel"] # mil m3 
# volConWood         <- gdx2::readGDX(gdx, "f73_volumetric_conversion")[, , "woodfuel"] # mil ton per m3

# read in region-level woodfuel output
demandWoodFuelReg  <- gdx2::readGDX(gdx, "pm_demand_forestry")[, , "woodfuel"] # mil ton
attr(demandWoodFuelReg, "description")

# get the mass to energy unit conversion coefficient
attrWoodFuel       <- gdx2::readGDX(gdx, "fm_attributes")[, , "woodfuel"][, , "ge"] # GJ per tDM
attrWoodFuel[]     <- 18 # fix for the moment
attr(attrWoodFuel, "description")

# EJ = 10^9 GJ
demandWoodFuelEJ <- demandWoodFuelReg * 10^6 * attrWoodFuel / 10^9
getNames(demandWoodFuelEJ) <- "woodfuel"
getSets(demandWoodFuelEJ) <- c("Region", "year", "data")

# filter for year <= 2110
demandWoodFuelEJ <- demandWoodFuelEJ[, getItems(demandWoodFuelEJ, "year") <= "y2110", ]

# check Region names
unique(dimnames(demandWoodFuelEJ)[[1]])
unique(dimnames(demandWoodFuelEJ)[[2]])

df <- as.data.frame(demandWoodFuelEJ)

df %>% group_by(Year) %>% 
  summarise(woodfuel = sum(Value, na.rm = T)) ->
  sum.reg



# iso sum up
# df.iso <- as.data.frame(demandWoodFuelPJ)
# 
# df.iso %>% 
#   group_by(Year) %>% 
#   summarise(Value = sum(Value, na.rm = T)) ->
#   sum.iso
