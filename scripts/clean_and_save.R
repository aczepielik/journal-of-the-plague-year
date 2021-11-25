library(dplyr)

weeks <- readr::read_delim(
  'sources/mortality-bills/WeeklyBillsMortality1644to1849/WeekDict.txt',
  delim = '|') %>% 
  mutate(begindate = lubridate::dmy(begindate)) %>% 
  filter(year < 1680)

#weeks$begindate[15] <- as.Date("1644-03-21")
#weeks$begindate[183] <- as.Date("1647-06-15")
weeks$begindate2 <- weeks$begindate[length(weeks$begindate)]

for (i in rev(seq_along(weeks$weekID))[2:length(weeks$weekID)]) {
  if (is.na(weeks$begindate[i]) | 
      abs(as.numeric(difftime(weeks$begindate2[i+1],weeks$begindate[i], units = 'days'))) > 21) {
    #print(as.numeric(difftime(weeks$begindate2[i+1], weeks$begindate[i], 'days')))
    weeks$begindate2[i] <- as.Date(weeks$begindate2[i+1] - lubridate::days(7))
  } else {
    weeks$begindate2[i] <- as.Date(weeks$begindate[i])
  }
}


burials <- readr::read_delim(
  'sources/mortality-bills/WeeklyBillsMortality1644to1849/counts.txt',
  delim = "|")

parishes <- readr::read_delim(
  'sources/mortality-bills/WeeklyBillsMortality1644to1849/ParcodeDict.txt',
  delim = "|")

bread <- readr::read_delim(
  'sources/mortality-bills/WeeklyBillsMortality1644to1849/bread.txt',
  delim = "|")

cods <- readr::read_delim(
  'sources/mortality-bills/WeeklyBillsMortality1644to1849/cods.txt',
  delim = "|")

weeks %>% saveRDS("sources/mortality-bills/cleaned/weeks.rds")
burials %>% saveRDS("sources/mortality-bills/cleaned/burials.rds")
parishes %>% saveRDS("sources/mortality-bills/cleaned/parishes.rds")
bread %>% saveRDS("sources/mortality-bills/cleaned/bread.rds")
cods %>% saveRDS("sources/mortality-bills/cleaned/cods.rds")
