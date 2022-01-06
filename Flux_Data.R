
#import NEON GPP data  ------

library(neonUtilities)
neonToken <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJrZWxsZXJhYkB1bW4uZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzY2ODk2ODgwLCJpYXQiOjE2MDkyMTY4ODAsImVtYWlsIjoia2VsbGVyYWJAdW1uLmVkdSJ9.L2gHraOdcGLWe1dvJDPxpDymwMusPBCLqutgNP2V9bnV3Aqz0hgGJOqvvVjJgP1Qvjc-JV1GIr_cm-61YGl-0g"


sgs_flux <- zipsByProduct(dpID="DP4.00200.001", site="CPER", check.size = F,
                          token = neonToken)

sgs_flux<- stackEddy('filesToStack00200', level = "dp04", var = NA, avg = NA)

sgs_flux_1 <- data.frame(sgs_flux[1])
head(sgs_flux_1)
str(sgs_flux_1)
sgs_flux_1

sgs_flux_1 <- sgs_flux_1 %>%
  select(CPER.timeBgn, CPER.timeEnd, CPER.data.fluxCo2.nsae.flux) %>%
  filter(CPER.data.fluxCo2.nsae.flux > -500) %>%
  filter(CPER.data.fluxCo2.nsae.flux < 500) 

month(sgs_flux_1$CPER.timeEnd)
head(sgs_flux_1)

hist(sgs_flux_1$CPER.data.fluxCo2.nsae.flux)
summary(sgs_flux_1)

sgs_flux_1$year <- as.numeric(substr(sgs_flux_1$CPER.timeBgn,0,4))
sgs_flux_1$month <-as.numeric(substr(sgs_flux_1$CPER.timeBgn,6,7))
sgs_flux_1$dat <-as.numeric(substr(sgs_flux_1$CPER.timeBgn,9,10))

ggplot(sgs_flux_1,aes(month,CPER.data.fluxCo2.nsae.flux)) +
  facet_wrap(~year) +
  stat_summary(fun='min',geom='point')


#ameriflux ------

library(janitor)
ameriflux_sgs <- fread('./../../Data/GPP/Ecoregion/shortgrass_steppe/Ameriflux/AMF_US-xCP_BASE-BADM_4-5/AMF_US-xCP_BASE_HH_4-5.csv')
head(ameriflux_sgs,3)
names(ameriflux_sgs) <- as.character(ameriflux_sgs[3,])
ameriflux_sgs <- ameriflux_sgs[-c(1,2,3)]
str(ameriflux_sgs)
ameriflux_sgs$NEE_PI <- as.numeric(ameriflux_sgs$NEE_PI)
ameriflux_sgs <- ameriflux_sgs %>%
  select(TIMESTAMP_START, TIMESTAMP_END,NEE_PI) %>%
  filter(NEE_PI > -900)

head(ameriflux_sgs)

ameriflux_sgs$year <- substr(ameriflux_sgs$TIMESTAMP_START, 0, 4)
ameriflux_sgs$month <- substr(ameriflux_sgs$TIMESTAMP_START, 5, 6)
ameriflux_sgs$day <- substr(ameriflux_sgs$TIMESTAMP_START, 7, 8)

head(ameriflux_sgs)

ggplot(ameriflux_sgs,aes(day,NEE_PI)) +
  facet_wrap(~month) +
  geom_point()
  stat_summary(geom='line')


head(sgs_flux[1],1)

list2env(foliarCN, .GlobalEnv)