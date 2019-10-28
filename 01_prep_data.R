library(data.table)
library(ggplot2)
library(sf)

## CHANGE TO WHERE YOU'VE DOWNLOADED ALL COVARIATE DATASETS AND MIGRATION.
repo <- 'C:/Users/ngraetz/Documents/repos/stalled_transition/'

## Load migration data
mig <- fread(paste0(repo,'/raw_data/Data on the global flow of people_Version March2014.csv'))
mig <- mig[country_orig!=country_dest, ]
mig_countries <- unique(mig[, country_orig])

#########################################################################################################
###################### Load population data
#########################################################################################################
pops <- fread(paste0(repo,'/raw_data/WPP2019_POP_F07_1_POPULATION_BY_AGE_BOTH_SEXES.csv'))
names(pops) <- c('Index','Variant','name','Notes','country_code','type','parent','year',paste0('age',as.character(seq(0,100,5))))
pops <- melt(pops, id.vars=c('name','year'), measure.vars = grep('^age',names(pops)), value.name = 'pop', variable.name = 'age')
pops[, pop := gsub(' ','',pop)]
pops[, pop := as.numeric(pop)*1000]
pops[, age := as.numeric(gsub('age','',age))]
mig[!(country_orig %in% pops[, name]), unique(country_orig)]
pops[name=='North Macedonia', name := 'Macedonia']
pops[grep('Eswa',name), name := 'Swaziland']
pops[grep('Ivoire',name), name := 'Ivory Coast']
pops[grep('United States of America',name), name := 'United States']
pops[grep('Democratic Republic of the Congo',name), name := 'DR Congo']
pops[name=='Cabo Verde', name := 'Cape Verde']
pops[grep('United Republic of Tanzania',name), name := 'Tanzania']
pops[grep('Bosnia and Herzegovina',name), name := 'Bosnia & Herzegovina']
pops[grep('Czechia',name), name := 'Czech Republic']
pops[grep('TFYR Macedonia',name), name := 'Macedonia']
pops[grep('Republic of Moldova',name), name := 'Moldova']
pops[grep('Russian Federation',name), name := 'Russia']
pops[grep('Syrian Arab Republic',name), name := 'Syria']
pops[grep('Iran',name), name := 'Iran']
pops[name=='Republic of Korea', name := 'South Korea']
pops[name=="Dem. People's Republic of Korea", name := 'North Korea']
pops[grep('Brunei Darussalam',name), name := 'Brunei']
pops[name=="Lao People's Democratic Republic", name := 'Laos']
pops[grep('Viet Nam',name), name := 'Vietnam']
pops[grep('Bolivia',name), name := 'Bolivia']
pops[grep('Trinidad and Tobago',name), name := 'Trinidad & Tobago']
pops[name=="Saint Vincent & the Grenadines", name := 'Saint Vincent & the Grenadines']
pops[grep('Venezuela',name), name := 'Venezuela']
mig[!(country_orig %in% pops[, name]), unique(country_orig)]
setnames(pops, 'name', 'country')
newpops <- copy(pops)

## Make total pops
total_pops <- newpops[, list(total_pop=sum(pop)), by=c('year','country')]
total_pops <- total_pops[, c('country','year','total_pop')]
saveRDS(total_pops, paste0(repo, '/processed_data/total_pops.RDS'))

## Make all growth rates
growth <- copy(newpops)
growth[, l.pop := shift(pop), by=c('age','country')]
growth[, r := (log(pop/l.pop)/5)*100]
growth[, l.r := shift(r), by=c('age','country')]
growth <- dcast(growth, country + year ~ age, value.var = c('r','l.r'))
saveRDS(growth, paste0(repo, '/processed_data/growth.RDS'))

growth <- copy(newpops)
growth <- growth[age %in% c(15,20), list(pop=sum(pop)), by=c('country','year')]
growth[, l5.pop := shift(pop), by=c('country')]
growth[, l10.pop := shift(l5.pop), by=c('country')]
growth[, r.15.24 := (log(pop/l10.pop)/10)*100]
growth[, l.r.15.24 := shift(r.15.24), by=c('country')]
saveRDS(growth, paste0(repo, '/processed_data/growth_15_24.RDS'))

#########################################################################################################
###################### Load covariate data
#########################################################################################################

## NEW GDP (2019)
gdp <- fread(paste0(repo,'/raw_data/IHME_FGH2018_GDPpc.csv'))
setnames(gdp, 'mean_gdppc_2018usd', 'gdp')
setnames(gdp, 'location_name', 'name')
mig[!(country_orig %in% gdp[, name]), unique(country_orig)]
gdp[grep('Ivoire',name), name := 'Ivory Coast']
gdp[grep('Democratic Republic of the Congo',name), name := 'DR Congo']
gdp[grep('The Gambia',name), name := 'Gambia']
gdp[grep('Bosnia',name), name := 'Bosnia & Herzegovina']
gdp[grep('Russian Federation',name), name := 'Russia']
gdp[grep('Federated States of Micronesia',name), name := 'Micronesia']
mig[!(country_orig %in% gdp[, name]), unique(country_orig)]
gdp <- gdp[, c('name','year','gdp')]
setnames(gdp, 'name', 'country')
saveRDS(gdp, paste0(repo,'/processed_data/new_gdp.RDS'))

## Load urbanicity
urban <- fread(paste0(repo,'/raw_data/WUP2018-F16-Percentage_Total_in_Cities.csv'), header = TRUE)
setnames(urban, 'Country or area','name')
urban <- melt(urban, id.vars = c('name','City Code'), measure.vars = c("1990","1995","2000","2005"), variable.name = 'year')
urban <- urban[, list(urbanicity=sum(value)), by=c('name','year')]
urban[, year := as.numeric(as.character(year))]
mig[!(country_orig %in% urban[, name]), unique(country_orig)]
urban[grep('Eswa',name), unique(name)]
urban[grep('Ivoire',name), name := 'Ivory Coast']
urban[grep('Democratic Republic of the Congo',name), name := 'DR Congo']
urban[grep('The Gambia',name), name := 'Gambia']
urban[grep('Bosnia',name), name := 'Bosnia & Herzegovina']
urban[grep('Russian Federation',name), name := 'Russia']
urban[grep('Federated States of Micronesia',name), name := 'Micronesia']
urban[grep('Bolivia',name), name := 'Bolivia']
urban[grep('Tanzania',name), name := 'Tanzania']
urban[grep('Syria',name), name := 'Syria']
urban[grep('Iran',name), name := 'Iran']
urban[grep('Bhutan',name), name := 'Bhutan']
urban[grep('Republic of Korea',name), name := 'South Korea']
urban[grep('Veit',name), name := 'Vietnam']
urban[grep("Dem. People's Republic of Korea",name), name := 'North Korea']
urban[grep('Lao',name), name := 'Laos']
urban[grep('Brunei',name), name := 'Brunei']
urban[grep('Belize',name), name := 'Belize']
urban[grep('Bolivia',name), name := 'Bolivia']
urban[grep('Venezuela',name), name := 'Venezuela']
mig[!(country_orig %in% urban[, name]), unique(country_orig)]
setnames(urban, 'name', 'country')
saveRDS(urban, paste0(repo,'/processed_data/urban.RDS'))

## Merge EPR 
ilo_epr <- fread(paste0(repo,'/raw_data/API_SL.EMP.1524.SP.FE.ZS_DS2_en_csv_v2_45173.csv'), header = TRUE)
ilo_epr <- melt(ilo_epr, id.vars = 'Country Name', measure.vars = as.character(1990:2010), variable.name = 'year', value.name = 'epr_15_24')
setnames(ilo_epr, 'Country Name', 'name')
ilo_epr[, year := as.numeric(as.character(year))]
ilo_epr_1990 <- ilo_epr[year==1991, ]
ilo_epr_1990[, year := 1990]
ilo_epr <- rbind(ilo_epr[year!=1990, ], ilo_epr_1990)
mig[!(country_orig %in% ilo_epr[, name]), unique(country_orig)]
ilo_epr[grep('Korea',name), unique(name)]
ilo_epr[grep('Ivoire',name), name := 'Ivory Coast']
ilo_epr[grep('Congo, Dem. Rep.',name), name := 'DR Congo']
ilo_epr[grep('Congo, Rep.',name), name := 'Congo']
ilo_epr[grep('Cabo Verde',name), name := 'Cape Verde']
ilo_epr[grep('Egypt',name), name := 'Egypt']
ilo_epr[grep('Gambia',name), name := 'Gambia']
ilo_epr[grep('Eswa',name), name := 'Swaziland']
ilo_epr[grep('Bosnia',name), name := 'Bosnia & Herzegovina']
ilo_epr[grep('Yemen',name), name := 'Yemen']
ilo_epr[grep('Iran',name), name := 'Iran']
ilo_epr[grep('Korea, Rep',name), name := 'South Korea']
ilo_epr[grep('Korea, Dem',name), name := 'North Korea'] ## Dem. People's Republic of Korea
ilo_epr[grep('Macedonia',name), name := 'Macedonia']
ilo_epr[grep('Slovakia',name), name := 'Slovakia']
ilo_epr[grep('Kyrgyzstan',name), name := 'Kyrgyzstan']
ilo_epr[grep('Russia',name), name := 'Russia']
ilo_epr[grep('Syria',name), name := 'Syria']
ilo_epr[grep('Brunei',name), name := 'Brunei']
ilo_epr[grep('Lao',name), name := 'Laos']
ilo_epr[grep('Venezuela',name), name := 'Venezuela']
mig[!(country_orig %in% ilo_epr[, name]), unique(country_orig)]
setnames(ilo_epr, 'name', 'country')
saveRDS(ilo_epr, paste0(repo,'/processed_data/epr.RDS'))

## Merge polity2
polity <- fread(paste0(repo,'/raw_data/polity2.csv'))
polity <- polity[year %in% c(1990,1995,2000,2005), c('country','year','polity2')]
mig[!(country_orig %in% polity[, country]), unique(country_orig)]
polity[grep('Belize',country), unique(country)]
polity[grep('Congo Kinshasa',country), country := 'DR Congo']
polity[grep('Congo Brazzaville',country), country := 'Congo']
polity[grep('Bosnia',country), country := 'Bosnia & Herzegovina']
polity[grep('Congo Kinshasa',country), country := 'Bosnia & Herzegovina']
polity[grep('Korea South',country), country := 'South Korea']
polity[grep('Korea North',country), country := 'North Korea']
polity[grep('Myanmar',country), country := 'Myanmar']
mig[!(country_orig %in% polity[, country]), unique(country_orig)]
saveRDS(polity, paste0(repo,'/processed_data/polity2.RDS'))

## GBD SHOCKS
shocks <- readRDS(paste0(repo,'/raw_data/cfr_gbd_cod.RDS'))
setnames(shocks, 'val', 'gbd_mx_shocks')
setnames(shocks, 'year_id', 'year')
setnames(shocks, 'location_id', 'loc_id')
setnames(shocks, 'location_name', 'name')
for(y in seq(1950,2015,5)) shocks[year>=y, five_year := y]
shocks <- shocks[, list(gbd_mx_shocks=mean(gbd_mx_shocks)), by=c('name','five_year','cause_name')]
shocks <- dcast(shocks, name + five_year ~ cause_name, value.var='gbd_mx_shocks')
setnames(shocks, c('five_year','Conflict and terrorism','Exposure to forces of nature'), c('year','mort_war','mort_nature'))
shocks <- shocks[, c('name','year','mort_war','mort_nature')]
shocks[, original_mort_war := mort_war]
shocks[, original_mort_nature := mort_nature]
shocks[, mort_war := log((mort_war * 1000) + 0.000001)]
shocks[, mort_nature := log((mort_nature * 1000) + 0.000001)]
mig[!(country_orig %in% shocks[, name]), unique(country_orig)]
shocks[grepl('Lao',name), unique(name)]
shocks[grep('Ivoire',name), name := 'Ivory Coast']
shocks[grep('Democratic Republic of the Congo',name), name := 'DR Congo']
shocks[grep('The Gambia',name), name := 'Gambia']
shocks[grep('Bosnia',name), name := 'Bosnia & Herzegovina']
shocks[grep('Russian Federation',name), name := 'Russia']
shocks[grep('Federated States of Micronesia',name), name := 'Micronesia']
mig[!(country_orig %in% shocks[, name]), unique(country_orig)]
setnames(shocks, 'name', 'country')
saveRDS(shocks, paste0(repo,'/processed_data/shocks.RDS'))

############################################################################
## FINAL MODEL INPUT DATA
############################################################################
## Load migration flows data
mig <- fread(paste0(repo,'/raw_data/Data on the global flow of people_Version March2014.csv'))
mig <- mig[country_orig!=country_dest, ]
## Collapse Hong Kong and Macao into "China".
mig[country_orig=='Hong Kong', country_orig:='China', ]
mig[country_dest=='Hong Kong', country_dest:='China', ]
mig[country_orig=='Macao', country_orig:='China', ]
mig[country_dest=='Macao', country_dest:='China', ]
mig <- mig[, lapply(.SD,sum), .SDcols=c('countryflow_1990','countryflow_1995','countryflow_2000','countryflow_2005'), by=c('country_orig','country_dest','region_orig','region_dest')]
mig_countries <- unique(mig[, country_orig])

## Create distance matrix
map <- readRDS(paste0(repo,'/raw_data/admin0_map.RDS'))
dist <- st_distance(st_centroid(map),st_centroid(map))
colnames(dist) <- map$ADM0_NAME
dist_dt <- as.data.table(dist)
dist_dt[, country_orig := map$ADM0_NAME]
dist_dt <- melt(dist_dt, id.vars = 'country_orig', variable.name = 'country_dest')
dist_dt[, distance := as.numeric(value) / 100000]
dist_dt <- dist_dt[country_orig != country_dest, ]
dist_dt[, value := NULL]
mig[!(country_orig %in% dist_dt[, country_orig]), unique(country_orig)]
dist_dt[grep('Ivoire',country_orig), country_orig := 'Ivory Coast']
dist_dt[grep('Ivoire',country_dest), country_dest := 'Ivory Coast']
dist_dt[grep('Democratic Republic of the Congo',country_orig), country_orig := 'DR Congo']
dist_dt[grep('Democratic Republic of the Congo',country_dest), country_dest := 'DR Congo']
dist_dt[grep('Republic of Congo',country_orig), country_orig := 'Congo']
dist_dt[grep('Republic of Congo',country_dest), country_dest := 'Congo']
dist_dt[grep('Bosnia and Herzegovina',country_orig), country_orig := 'Bosnia & Herzegovina']
dist_dt[grep('Bosnia and Herzegovina',country_dest), country_dest := 'Bosnia & Herzegovina']
dist_dt[grep('Trinidad and Tobago',country_orig), country_orig := 'Trinidad & Tobago']
dist_dt[grep('Trinidad and Tobago',country_dest), country_dest := 'Trinidad & Tobago']
dist_dt[grep('Saint Vincent and the Grenadines',country_orig), country_orig := 'Saint Vincent & Grenadines']
dist_dt[grep('Saint Vincent and the Grenadines',country_dest), country_dest := 'Saint Vincent & Grenadines']
mig[!(country_orig %in% dist_dt[, country_orig]), unique(country_orig)]

## Merge distance, melt
## Not including from migration file: Palestine, Virgin Islands, Sao Tome & Principe, Channel Islands
mig_merge <- merge(mig, dist_dt, by=c('country_orig','country_dest'), all.x=T)
mig_merge <- melt(mig_merge, id.vars=c('country_orig','country_dest','region_orig','region_dest',
                                       'distance'),
                  measure.vars = c('countryflow_1990','countryflow_1995','countryflow_2000','countryflow_2005'),
                  value.name = 'migrants')
mig_merge[, year := tstrsplit(variable,'_',keep=2)]
mig_merge[, year := as.numeric(year)]

## Merge covariates
total_pops <- readRDS(paste0(repo,'/processed_data/total_pops.RDS'))
mig_merge <- merge(mig_merge, total_pops, by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)
setnames(mig_merge, 'total_pop', 'total_pop_orig')
mig_merge <- merge(mig_merge, total_pops, by.x=c('country_dest','year'), by.y=c('country','year'), all.x=T)
setnames(mig_merge, 'total_pop', 'total_pop_dest')

growth <- readRDS(paste0(repo,'/processed_data/growth.RDS'))
mig_merge <- merge(mig_merge, growth, by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)

growth1524 <- readRDS(paste0(repo,'/processed_data/growth_15_24.RDS'))
mig_merge <- merge(mig_merge, growth1524[, c('country','year','l.r.15.24','r.15.24')], by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)

gdp <- readRDS(paste0(repo,'/processed_data/new_gdp.RDS'))
mig_merge <- merge(mig_merge, gdp, by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)
setnames(mig_merge, 'gdp', 'gdp_orig')
mig_merge <- merge(mig_merge, gdp, by.x=c('country_dest','year'), by.y=c('country','year'), all.x=T)
setnames(mig_merge, 'gdp', 'gdp_dest')
gdp_missing <- mig_merge[is.na(gdp_orig), unique(country_orig)]

shocks <- readRDS(paste0(repo,'/processed_data/shocks.RDS'))
mig_merge <- merge(mig_merge, shocks, by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)
shocks_missing <- mig_merge[is.na(mort_war) | is.na(mort_nature), unique(country_orig)]

epr <- readRDS(paste0(repo,'/processed_data/epr.RDS'))
mig_merge <- merge(mig_merge, epr, by.x=c('country_orig','year'), by.y=c('country','year'), all.x=T)
epr_missing <- mig_merge[is.na(epr_15_24), unique(country_orig)]

baseline_missing <- c('Channel Islands', 'Palestine', 'Reunion', 'Saint Vincent & Grenadines', 'Sao Tome & Principe', 'Virgin Islands', 'Western Sahara')
all_missing <- unique(c(shocks_missing, epr_missing, baseline_missing))
mig_merge <- mig_merge[!(country_orig %in% all_missing) & !(country_dest %in% all_missing), ]

## RESCALE COVARIATES
model_data <- copy(mig_merge)
model_data[, mort_nature := scale(mort_nature)]
model_data[, mort_war := scale(mort_war)]
model_data[, gdp_dest := scale(log(gdp_dest), scale=F)]
model_data[, gdp_original := gdp_orig]
model_data[, gdp_orig := scale(log(gdp_orig), scale=F)]
model_data[, epr_15_24 := scale(epr_15_24)]
model_data[, log_distance := scale(log(distance))]
model_data[, log_pop_orig := scale(log(total_pop_orig))]
model_data[, log_pop_dest := scale(log(total_pop_dest))]

## MERGE FULL REGION IDS
gbd_regions <- fread(paste0(repo,'/raw_data/gbd_regions.csv'))
setnames(gbd_regions, 'gbd_super_region', 'gbd_super_region')
setnames(gbd_regions, 'gbd_region', 'gbd_region')
setnames(gbd_regions, 'location_name', 'name')
mig[!(country_orig %in% gbd_regions[, name]), unique(country_orig)]
gbd_regions[grep('Ivoire',name), name := 'Ivory Coast']
gbd_regions[grep('Democratic Republic of the Congo',name), name := 'DR Congo']
gbd_regions[grep('The Gambia',name), name := 'Gambia']
gbd_regions[grep('Bosnia',name), name := 'Bosnia & Herzegovina']
gbd_regions[grep('Russian Federation',name), name := 'Russia']
gbd_regions[grep('Federated States of Micronesia',name), name := 'Micronesia']
gbd_regions[grep('Bolivia',name), name := 'Bolivia']
gbd_regions[grep('Tanzania',name), name := 'Tanzania']
gbd_regions[grep('Democratic Republic of Congo',name), name := 'DR Congo']
gbd_regions[grep('Cabo Verde',name), name := 'Cape Verde']
gbd_regions[grep('Czech',name), name := 'Czech Republic']
gbd_regions[grep('Syria',name), name := 'Syria']
gbd_regions[grep('Lao',name), name := 'Laos']
gbd_regions[grep('Viet',name), name := 'Vietnam']
gbd_regions[grep('Iran',name), name := 'Iran']
gbd_regions[grep('Venezuela',name), name := 'Venezuela']
gbd_regions[grep('United States',name), name := 'United States']
mig[!(country_orig %in% gbd_regions[, name]), unique(country_orig)]
gbd_regions <- gbd_regions[, c('name','gbd_region','gbd_super_region')]
setnames(gbd_regions, 'name', 'country_dest')
model_data <- merge(model_data, gbd_regions, by=c('country_dest'), all.x=T)
setnames(model_data, c('gbd_region','gbd_super_region'), c('gbd_region_dest','gbd_super_region_dest'))
setnames(gbd_regions, 'country_dest', 'country_orig')
model_data <- merge(model_data, gbd_regions, by=c('country_orig'), all.x=T)

## Merge urban and polity
urban_polity <- readRDS(paste0(repo,'/processed_data/urban_and_polity.RDS'))
model_data <- merge(model_data, urban_polity, by=c('country_orig','year'), all.x=T)

## SAVE
saveRDS(model_data, paste0(repo,'/processed_data/gravity_model_full_data.RDS'))
