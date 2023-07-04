library(tidyverse)
library(dplyr)

# Retrieve Short Term Mortality Fluctuation Database from Human Mortality Database

{
  STMFRaw <- read.csv("https://www.mortality.org/File/GetDocument/Public/STMF/Outputs/stmf.csv",
    sep = ",", skip=2, strip.white = TRUE, stringsAsFactors = FALSE)
}

## Tidy Up Short Term Mortality Fluctuation Database from Human Mortality Database
{
  STMF <- STMFRaw[!(STMFRaw$Year<2015
                    | STMFRaw$Year>2020
                    | STMFRaw$Week==53
                    | STMFRaw$CountryCode == "AUS2"
                    | STMFRaw$CountryCode == "CAN"
                    | STMFRaw$CountryCode == "CHL"
                    | STMFRaw$CountryCode == "DEUTNP"
                    | STMFRaw$CountryCode == "GRC"
                    | STMFRaw$CountryCode == "ISL"
                    | STMFRaw$CountryCode == "LUX"
                    | STMFRaw$CountryCode == "RUS"
                    | STMFRaw$CountryCode == "TWN"),]
  
  STMF <- STMF %>%
    dplyr::rename (
      country = CountryCode,
      year = Year,
      week = Week,
      sex = Sex,
      abs14m = D0_14,
      abs1564 = D15_64,
      abs6574 = D65_74,
      abs7584 = D75_84,
      abs85p = D85p,
      abstotal = DTotal,
      rate14m = R0_14,
      rate1564 = R15_64,
      rate6574 = R65_74,
      rate7584 = R75_84,
      rate85p = R85p,
      ratetotal = RTotal)
  STMF$Split <- NULL
  STMF$SplitSex <- NULL
  STMF$Forecast <- NULL
  
  STMF$country[which(STMF$country == "AUT")] <- "Austria"
  STMF$country[which(STMF$country == "BEL")] <- "Belgium"
  STMF$country[which(STMF$country == "BGR")] <- "Bulgaria"
  STMF$country[which(STMF$country == "CHE")] <- "Switzerland"
  STMF$country[which(STMF$country == "CZE")] <- "Czechia"
  STMF$country[which(STMF$country == "DNK")] <- "Denmark"
  STMF$country[which(STMF$country == "ESP")] <- "Spain"
  STMF$country[which(STMF$country == "EST")] <- "Estonia"
  STMF$country[which(STMF$country == "FIN")] <- "Finland"
  STMF$country[which(STMF$country == "FRATNP")] <- "France"
  STMF$country[which(STMF$country == "GBR_NIR")] <- "Northern Ireland (GB)"
  STMF$country[which(STMF$country == "GBR_SCO")] <- "Scotland (GB)"
  STMF$country[which(STMF$country == "GBRTENW")] <- "England and Wales (GB)"
  STMF$country[which(STMF$country == "HRV")] <- "Croatia"
  STMF$country[which(STMF$country == "HUN")] <- "Hungary"
  STMF$country[which(STMF$country == "ISR")] <- "Israel"
  STMF$country[which(STMF$country == "ITA")] <- "Italy"
  STMF$country[which(STMF$country == "KOR")] <- "South Korea"
  STMF$country[which(STMF$country == "LTU")] <- "Lithuania"
  STMF$country[which(STMF$country == "LVA")] <- "Latvia"
  STMF$country[which(STMF$country == "NLD")] <- "Netherlands"
  STMF$country[which(STMF$country == "NOR")] <- "Norway"
  STMF$country[which(STMF$country == "NZL_NP")] <- "New Zealand"
  STMF$country[which(STMF$country == "POL")] <- "Poland"
  STMF$country[which(STMF$country == "PRT")] <- "Portugal"
  STMF$country[which(STMF$country == "SVK")] <- "Slovakia"
  STMF$country[which(STMF$country == "SVN")] <- "Slovenia"
  STMF$country[which(STMF$country == "SWE")] <- "Sweden"
  STMF$country[which(STMF$country == "USA")] <- "United States"
  STMF$sex[which(STMF$sex == "b")] <- "both"
  STMF$sex[which(STMF$sex == "f")] <- "female"
  STMF$sex[which(STMF$sex == "m")] <- "male"
}  

# Difference with Means
meansTable <- STMF %>%
  select(country, year, week, sex, abstotal, rate14m, rate1564, rate6574, rate7584, rate85p, ratetotal) %>%
  group_by(country, week, sex) %>%
  mutate(ratetotaladj = (rate14m * (1000 + 4000 + 5500 + 5500) + 
           rate1564 * (5500 + 6000 + 6000 + 6500 + 7000  + 7000 + 7000 + 7000 + 6500 + 6000) +
           rate6574 * (5500 + 5000) +
           rate7584 * (4000 + 2500) +
           rate85p * (1500 + 800 + 200))/100000,
         meanabs = mean(abstotal[year!=2020], na.rm = T),
         meanrate = mean(ratetotal[year!=2020], na.rm = T),
         meanrateadj = mean(ratetotaladj[year!=2020], na.rm = T),
         absdif = mean(abstotal[year==2020], na.rm =T) - meanabs,
         ratedif = mean(ratetotal[year==2020], na.rm =T) - meanrate,
         ratedifadj = mean(ratetotaladj[year==2020], na.rm =T) - meanrateadj) %>%
  ungroup ()

## Standard Population is taken from: 
## https://ec.europa.eu/eurostat/documents/3859598/5926869/KS-RA-13-028-EN.PDF.pdf/e713fa79-1add-44e8-b23d-5e8fa09b3f8f?t=1414782757000

meansTableUni <- meansTable %>%
  distinct(country, week, sex, meanabs, meanrate, meanrateadj, absdif, ratedif, ratedifadj)

# Linear Model method

regModelAbs <- meansTable %>%
  select(year, country, week, sex, abstotal) %>%
  group_by (country, week, sex) %>%
  nest () %>%
  mutate (model = map (data, ~ lm (abstotal[year!=2020] ~ year[year!=2020], data = .x)),
          tidied = map (model, broom::tidy)) %>%
  unnest (tidied) %>%
  ungroup ()

regModelCru <- meansTable %>%
  select(year, country, week, sex, ratetotal) %>%
  group_by (country, week, sex) %>%
  nest () %>%
  mutate (model = map (data, ~ lm (ratetotal[year!=2020] ~ year[year!=2020], data = .x)),
          tidied = map (model, broom::tidy)) %>%
  unnest (tidied) %>%
  ungroup ()

regModelAdj <- meansTable %>%
  select(year, country, week, sex, ratetotaladj) %>%
  group_by (country, week, sex) %>%
  nest () %>%
  mutate (model = map (data, ~ lm (ratetotaladj[year!=2020] ~ year[year!=2020], data = .x)),
          tidied = map (model, broom::tidy)) %>%
  unnest (tidied) %>%
  ungroup ()

regEstAbs <- regModelAbs %>%
  select(country, week, sex, term, estimate) %>%
  group_by(country, week, sex) %>%
  mutate(est20 = estimate[term == "(Intercept)"] + 2020 * estimate[term == "year[year != 2020]"]) %>%
  ungroup()
regEstAbs <- regEstAbs %>%
  distinct(country, week, sex, est20)

regEstCru <- regModelCru %>%
  select(country, week, sex, term, estimate) %>%
  group_by(country, week, sex) %>%
  mutate(est20 = estimate[term == "(Intercept)"] + 2020 * estimate[term == "year[year != 2020]"]) %>%
  ungroup()
regEstCru <- regEstCru %>%
  distinct(country, week, sex, est20)

regEstAdj <- regModelAdj %>%
  select(country, week, sex, term, estimate) %>%
  group_by(country, week, sex) %>%
  mutate(est20 = estimate[term == "(Intercept)"] + 2020 * estimate[term == "year[year != 2020]"]) %>%
  ungroup()
regEstAdj <- regEstAdj %>%
  distinct(country, week, sex, est20)

regEst <- meansTable %>%
  select(country, week, sex, year, abstotal, ratetotal, ratetotaladj) %>%
  group_by(country, week, sex) %>%
  mutate(act20abs = abstotal[year==2020],
         act20cru = ratetotal[year==2020],
         act20adj = ratetotaladj[year==2020]) %>%
  ungroup()

regEst <- regEst %>%
  distinct(country, week, sex, act20abs, act20cru, act20adj)

regEst$est20abs <- regEstAbs$est20
regEst$est20cru <- regEstCru$est20
regEst$est20adj <- regEstAdj$est20
regEst$difAbs <- regEst$act20abs - regEst$est20abs
regEst$difCru <- regEst$act20cru - regEst$est20cru
regEst$difAdj <- regEst$act20adj - regEst$est20adj

results <- meansTableUni %>%
  select(country, week, sex, meanabs, meanrate, meanrateadj, absdif, ratedif, ratedifadj) %>%
  mutate(country = country,
         week = week,
         sex = sex,
         avAbNumber = meanabs,
         avCrRate = meanrate,
         avStRate = meanrateadj,
         avDifAbs = absdif,
         avDifCrRate = ratedif,
         avDifStRate = ratedifadj,
         )
results <- results %>%
  distinct(country, week, sex, avAbNumber, avCrRate, avStRate, avDifAbs, avDifCrRate, avDifStRate)

results$regCrRate <- regEst$est20abs
results$regCrRate <- regEst$est20cru
results$regStRate <- regEst$est20adj
results$actCrRate <- regEst$act20abs
results$actCrRate <- regEst$act20cru
results$actStRate <- regEst$act20adj
results$regDifAbs <- regEst$difAbs
results$regDifCrRate <- regEst$difCru
results$regDifStRate <- regEst$difAdj

write.csv(results,"results.csv", row.names = TRUE)