library(data.table)
library(ggplot2)
library(sf)
library(MASS)
library(pscl)

repo <- 'C:/Users/ngraetz/Documents/repos/stalled_transition/'

model_data <- readRDS(paste0(repo,'/processed_data/gravity_model_full_data.RDS'))

## Options
iv <- 'l.r.15.24'
int_iv <- 'gdp_orig'
intervention_variable <- 'l.r.15.24'
intervention_value <- 0
ldi_inc <- 0.33
keep_regions <- paste0('ALLINT_',intervention_variable,'_',intervention_value)
use_quants <- FALSE
use_zero <- FALSE
make_plots <- FALSE

# ## REMOVE HIGH INCOME COUNTRIES
wb_ids <- readRDS(paste0(repo,'/processed_data/wb_ids.RDS'))
model_data <- merge(model_data, wb_ids, by.x='country_orig', by.y='country', all.x=T)
setnames(wb_ids, 'region_name', 'region_name_dest')
model_data <- merge(model_data, wb_ids, by.x='country_dest', by.y='country', all.x=T)

table_data <- copy(model_data)
table_data <- table_data[!is.na(polity2), ]
table_data <- table_data[!is.na(urbanicity), ]
table_data[, gdp_gap := gdp_dest - gdp_orig]

model_data <- model_data[!is.na(polity2), ]
model_data <- model_data[!is.na(urbanicity), ]
model_data[, gdp_gap := gdp_dest - gdp_orig]

## COLLAPSE TO REGION-DESTINATIONS
vars_to_collapse <- c('l.r.15.24','gdp_orig','epr_15_24','mort_nature','mort_war','polity2','urbanicity','gdp_gap','log_distance','log_pop_dest','log_pop_orig','total_pop_orig')
total_migrants <- model_data[, list(migrants=sum(migrants)), by=c('region_f','country_orig','year','gbd_super_region_dest','region_name','gbd_region')]
model_data <- model_data[, lapply(.SD, weighted.mean, w=total_pop_dest), .SDcols=vars_to_collapse, by=c('region_f','country_orig','year','gbd_super_region_dest','region_name','gbd_region')]
model_data <- merge(total_migrants, model_data, by=c('region_f','country_orig','year','gbd_super_region_dest','region_name','gbd_region'))

# for(keep_regions in c('ALL',as.character(unique(model_data[, region_f])),'ALLINT')) {
file_tag <- keep_regions
if(grepl('ALL',keep_regions)) keep_regions <- unique(model_data[, region_f])

## TEST OUTLIERS
outliers <- c('China','India', 
              'Bahrain','Kuwait','Oman','Qatar','Saudi Arabia','United Arab Emirates',
              'Lebanon') 

## FULL MODEL
grav_data <- model_data[!(country_orig %in% outliers) & region_f %in% keep_regions,]
grav_data[, ols_migrants := migrants + 1]
message(paste(unique(grav_data$country_orig), collapse=' '))

gravity_model <- glm(as.formula(paste0('migrants ~ ',ifelse(grepl('ALLINT',file_tag), paste0(iv,'*',int_iv), iv),' + epr_15_24 + mort_nature + mort_war + polity2 + urbanicity + gdp_gap + gdp_orig + log_distance + log_pop_dest + log_pop_orig + country_orig')), data = grav_data, family = poisson(link = "log"))
if(grepl('ALLINT',file_tag)) gravity_model <- glm(as.formula(paste0('migrants ~ ',ifelse(grepl('ALLINT',file_tag), paste0(iv,'*',int_iv), iv),' + epr_15_24 + mort_nature + mort_war + polity2 + urbanicity + gdp_gap + log_distance + log_pop_dest + log_pop_orig + country_orig + gbd_super_region_dest')), data = grav_data, family = poisson(link = "log"))

# grav_zero_poisson <- zeroinfl(as.formula(paste0('migrants ~ ',ifelse(grepl('ALLINT',file_tag), paste0(iv,'*',int_iv), iv),' + epr_15_24 + mort_nature + mort_war + polity2 + urbanicity + gdp_gap + log_distance + log_pop_dest + log_pop_orig + country_orig + gbd_super_region_dest | gdp_gap + log_distance + log_pop_dest + log_pop_orig')), data = grav_data, dist='poisson')

saveRDS(gravity_model, paste0(repo,'/processed_data/model_',file_tag,'.RDS'))
# }

## FIGURE 1: COEFFICIENTS ON GROWTH BY GDP
results <- copy(grav_data)
if(use_quants) {
  for(q in quants) {
    results[ldi_quantile==as.character(q),
            pred_coef := ifelse(q!=0,
                                gravity_model$coefficients[paste0(iv,':',int_iv,q)] + gravity_model$coefficients[iv],
                                gravity_model$coefficients[iv])]
  }
  results[, pred_coef := exp(pred_coef)]
}
if(!use_quants) results[, pred_coef := exp(get(int_iv) * gravity_model$coefficients[paste0(iv,':',int_iv)] + gravity_model$coefficients[iv])]
if(use_zero) results[, pred_coef := exp(get(paste0(int_iv)) * grav_zero$coefficients$count[paste0(iv,':',int_iv)] + grav_zero$coefficients$count[iv])]

jpeg(paste0(repo,'/results/Coefficient_Figure_', file_tag, '.jpeg'), width=1900, height=1100, res=100)
gg.coef <- ggplot() +
  geom_line(data=unique(results[, c(int_iv,'pred_coef','region_f'), with=F]),
            aes(x=get(int_iv),
                y=pred_coef),
            color='black',
            size=1) +
  geom_jitter(data=unique(results[, c(int_iv,'pred_coef','region_f'), with=F]),
              aes(x=get(int_iv),
                  y=pred_coef,
                  fill=region_f),
              shape=21,
              alpha=0.8,
              size=10, width=0.01, height=0.05) +
  geom_hline(yintercept = 1) + 
  geom_vline(xintercept = 0) + 
  # lims(x=c(-2.5,1),y=c(1,1.2)) + 
  scale_color_manual(name='', values=c('#253494','#2ca25f','#bdbdbd','#de2d26','#ff7f00','#ffff33'),guide=F) +
  scale_fill_manual(name='Region', values=c('#253494','#2ca25f','#ff7f00','#de2d26','#ff7f00','#ffff33')) +
  guides(fill = guide_legend(override.aes = list(size = 15))) + 
  labs(y='Coefficient of growth on out-migration',x='Income per capita') + 
  theme_bw() + 
  theme(axis.title.y = element_text(size = 25, margin = margin(r=10)),
        axis.title.x = element_text(size = 25, margin = margin(t=10)),
        axis.text = element_text(size = 16),
        legend.key.size = unit(3,'line'),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25))
print(gg.coef)
dev.off()

## MAKE PREDICTIONS
create_preds <- function(intervention, d, intervention_value) {
  coef_data <- copy(d)
  if(!is.null(intervention)) {
    coef_data[, (intervention) := intervention_value]
    if(intervention=='gdp_orig') {
      coef_data[, gdp1990 := ifelse(year==1990, gdp_orig, 0)]
      coef_data[, (intervention) := max(gdp1990), by=country_orig]
    }
  }
  for(c in unique(coef_data[, country_orig])) coef_data[, (paste0('country_orig',c)) := ifelse(country_orig==c,1,0)]
  for(c in unique(coef_data[, gbd_super_region_dest])) coef_data[, (paste0('gbd_super_region_dest',c)) := ifelse(gbd_super_region_dest==c,1,0)]
  if(use_quants) {
    for(q in unique(coef_data[, ldi_quantile])) {
      coef_data[, (paste0('ldi_quantile',q)) := ifelse(ldi_quantile==q,1,0)]
      coef_data[, (paste0('l.r.15.24:ldi_quantile',q)) := get(paste0('ldi_quantile',q)) * l.r.15.24]
    }
  }
  coef_data[, (paste0('l.r.15.24:',int_iv)) := l.r.15.24 * get(int_iv)]
  coef_data[, ('(Intercept)') := 1]
  betas <- MASS::mvrnorm(1000, mu = coef(gravity_model), Sigma = vcov(gravity_model))
  new_d <- coef_data[, colnames(betas), with=F]
  setcolorder(new_d, colnames(betas))
  ## 1000 predictions
  preds <- betas %*% t(as.matrix(new_d))
  preds <- as.data.table(t(preds))
  cols <- paste0('draw',1:1000)
  setnames(preds, cols)
  preds[, (cols) := lapply(.SD,exp), .SDcols=cols]
  return(preds)
}
cols <- paste0('draw',1:1000)
preds_growth <- create_preds(intervention=NULL, d=results, intervention_value=intervention_value)
preds_no_growth <- create_preds(intervention=intervention_variable, d=results, intervention_value=intervention_value)
pred_diffs <- preds_growth - preds_no_growth

results <- cbind(results, preds_growth)
setnames(results, cols, paste0('growth_',cols))
results <- cbind(results, preds_no_growth)
setnames(results, cols, paste0('nogrowth_',cols))
results <- cbind(results, pred_diffs)

preds_growth[, pred_mean := apply(.SD,1,median), .SDcols=cols]
preds_growth[, pred_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
preds_growth[, pred_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]

preds_no_growth[, pred_no_growth_mean := apply(.SD,1,median), .SDcols=cols]
preds_no_growth[, pred_no_growth_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
preds_no_growth[, pred_no_growth_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]

results <- cbind(results, preds_growth[, c('pred_mean'), with=F])
results <- cbind(results, preds_no_growth[, c('pred_no_growth_mean'), with=F])

## Get means with predict to compare
results[, pred := exp(predict(gravity_model, results))]
results[, (intervention_variable) := 0]
results[, pred_no_growth := exp(predict(gravity_model, results))]

## COUNTRY AGGREGATES
results[, diff := pred - pred_no_growth]
results[diff>0, direction := 'Positive contributions']
results[diff<=0, direction := 'Negative contributions']
get_aggregate_table <- function(levels) {
  ## Total observed migrants
  migrant_totals <- results[, list(migrants=sum(migrants)), by=c(levels)]
  ## Expected percent change in net migrants given no growth
  pred_totals <- results[, lapply(.SD,sum), .SDcols=paste0('growth_',cols), by=c(levels)]
  pred_no_growth_totals <- results[, lapply(.SD,sum), .SDcols=paste0('nogrowth_',cols), by=c(levels)]
  perc_cols <- paste0('perc_draw',1:1000)
  these_levels <- pred_totals[, levels, with=F]
  all <- as.matrix(pred_totals[, paste0('growth_',cols), with=F]) / as.matrix(pred_no_growth_totals[, paste0('nogrowth_',cols), with=F])
  all <- as.data.table(all)
  setnames(all, perc_cols)
  all <- cbind(these_levels, all)
  all[, perc_mean := apply(.SD,1,median), .SDcols=perc_cols]
  all[, perc_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=perc_cols]
  all[, perc_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=perc_cols]
  all <- all[, c(levels,'perc_mean','perc_lower','perc_upper'), with=F]
  ## Total expected net migrants given no growth
  pred_no_growth_totals <- results[, lapply(.SD,sum), .SDcols=cols, by=c(levels)]
  pred_no_growth_totals[, diff_mean := apply(.SD,1,median), .SDcols=cols]
  pred_no_growth_totals[, diff_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
  pred_no_growth_totals[, diff_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]
  pred_no_growth_totals <- pred_no_growth_totals[, c(levels,'diff_mean','diff_lower','diff_upper'), with=F]
  ## Total positive contribution
  positive_cont <- results[diff>0, list(positive_cont=sum(diff)), by=c(levels)]
  positive_cont <- results[diff>0, lapply(.SD,sum), .SDcols=cols, by=c(levels)]
  positive_cont[, positive_mean := apply(.SD,1,median), .SDcols=cols]
  positive_cont[, positive_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
  positive_cont[, positive_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]
  positive_cont <- positive_cont[, c(levels,'positive_mean','positive_lower','positive_upper'), with=F]
  ## Total negative contribution
  negative_cont <- results[diff<=0, list(negative_cont=sum(diff)), by=c(levels)]
  negative_cont <- results[diff<=0, lapply(.SD,sum), .SDcols=cols, by=c(levels)]
  negative_cont[, negative_mean := apply(.SD,1,median), .SDcols=cols]
  negative_cont[, negative_lower := apply(.SD,1,quantile,probs=0.025), .SDcols=cols]
  negative_cont[, negative_upper := apply(.SD,1,quantile,probs=0.975), .SDcols=cols]
  negative_cont <- negative_cont[, c(levels,'negative_mean','negative_lower','negative_upper'), with=F]
  ## Combine
  conts <- merge(positive_cont, negative_cont, all.x=T, all.y=T)
  conts[is.na(positive_mean), positive_mean := 0]
  conts[is.na(negative_mean), negative_mean := 0]
  totals <- Reduce(merge, list(migrant_totals, pred_no_growth_totals, conts, all))
  return(totals)
}
country_aggs <- get_aggregate_table(c('country_orig','region_f'))
country_aggs <- country_aggs[country_orig!='Equatorial Guinea',]
options(scipen=999)
country_aggs <- country_aggs[order(-positive_mean)]
country_aggs[, country_orig := factor(country_orig, levels=rev(country_orig))]

country_all <- melt(country_aggs, id.vars = c('region_f','country_orig'), measure.vars = c('perc_mean','positive_mean'))
country_all <- country_all[order(variable,-value)]
country_all[, country_orig_perc := factor(country_orig, levels=rev(country_all[variable=='perc_mean', country_orig]))]
country_all[, country_orig_abs := factor(country_orig, levels=rev(country_all[variable=='positive_mean', country_orig]))]
country_all[, rank := 1:.N, by='variable']
country_map <- copy(country_all[variable=='positive_mean',])

country_all <- country_all[rank <= 12, ]

if(make_plots) {
  country_all[, clean_region := region_f]
  country_all[clean_region == 'North Africa and Middle East', clean_region := 'NAME']
  country_all[, clean_region := factor(clean_region, levels=c('Sub-Saharan Africa','Asia','NAME','Latin America and Carribbean'))]
  gg1 <- ggplot() + 
    geom_bar(data=country_all[variable=='perc_mean',],
             aes(x=country_orig_perc,
                 y=value*100-100,
                 fill=clean_region),
             color='black',
             stat='identity') +
    theme_bw() + 
    lims(y=c(0,110)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(size = 15, margin = margin(r=10)),
          axis.title.x = element_text(size = 15, margin = margin(t=10)),
          axis.text = element_text(size = 12),
          legend.key.size = unit(3,'line'),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 12)) + 
    labs(y='Percent increase in total out-migrants',x='') + 
    scale_fill_manual(name='Region', values=c('Sub-Saharan Africa'='#440154FF',
                                              'Asia'='#21908CFF',
                                              'NAME'='#FDE725FF'))
  gg2 <- ggplot() + 
    geom_bar(data=country_all[variable=='positive_mean',],
             aes(x=country_orig_abs,
                 y=value/1000,
                 fill=clean_region),
             color='black',
             stat='identity') +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(size = 15, margin = margin(r=10)),
          axis.title.x = element_text(size = 15, margin = margin(t=10)),
          axis.text = element_text(size = 12),
          legend.key.size = unit(3,'line'),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 12)) + 
    labs(y='Increase in total out-migrants (thousands)',x='') + 
    scale_fill_viridis_d(name='Region')
  saveRDS(country_all, paste0(repo,'/results/top_countries.RDS'))
  write.csv(country_all, paste0(repo,'/results/top_countries.csv'))
  ## Try map of absolute additional migrants
  map_theme <- theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     strip.background = element_blank(),
                     strip.text.x = element_blank(),
                     legend.text = element_text(size=12),
                     legend.justification=c(0,0),
                     legend.position=c(0.8,0.3),
                     legend.key.width=unit(1,"line"),
                     legend.key.height=unit(1.5,"line"),
                     legend.background = element_rect(fill="white", 
                                                      size=0.5, linetype="solid", color='black'),
                     panel.grid.major = element_line(colour = "#ccd4e3"),
                     panel.grid.minor = element_line(colour = "#ccd4e3"),
                     axis.line = element_line(colour = "#ccd4e3"))
  map <- readRDS(paste0(repo,'/raw_data/lbd_standard_admin_0.rds'))
  map$ADM0_NAME <- as.character(map$ADM0_NAME)
  map$ADM0_NAME[grepl('Democratic Republic of the Congo',map$ADM0_NAME)] <- 'DR Congo'
  map$ADM0_NAME[grepl('Ivoire',map$ADM0_NAME)] <- 'Ivory Coast'
  map$ADM0_NAME[grepl('Republic of Congo',map$ADM0_NAME)] <- 'Congo'
  map <- map[map$ADM0_NAME!='Antarctica',]
  map_names <- data.table(map=unique(map$ADM0_NAME))
  country_map[!(country_orig_abs %in% map$ADM0_NAME), unique(country_orig_abs)]
  country_map[, ADM0_NAME := country_orig_abs]
  country_map_abs <- country_map[variable=='positive_mean',]
  country_map_abs[value>500000, value := 500000]
  country_map_abs[, value := value / 1000]
  map_merge <- merge(map, country_map_abs, by='ADM0_NAME')
  map_merge <- st_as_sf(map_merge)
  library(grid)
  library(gridExtra)
  map.gg <- ggplot(data=map_merge) + 
    geom_sf(aes(fill=value),
            color='black',
            lwd=0.1) +
    scale_fill_viridis_c(name='Additional\nmigrants\n(thousands)', na.value = 'grey') + 
    theme_minimal() + 
    map_theme

  gLegend<-function(a.plot){
    if ("ggplot" %in% class(a.plot)) {
      tmp <- ggplot_gtable(ggplot_build(a.plot))
    } else if ("grob" %in% class(a.plot)) {
      tmp <- .gplot
    }
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  p.legend1 <- gLegend(gg2)
  p.legend1$vp <- viewport(layout.pos.row = 8:14, layout.pos.col = 11:12)
  p.legend2 <- gLegend(map.gg)
  p.legend2$vp <- viewport(layout.pos.row = 1:7, layout.pos.col = 11:12)
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  pdf(paste0(repo,'/results/Figure_2_', Sys.Date(), '_',file_tag,'.pdf'),height=12,width=14)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(14, 12)))
  print(map.gg + theme(legend.position="none"), vp = vplayout(1:7, 1:10))
  print(gg1 + theme(legend.position="none"), vp = vplayout(8:14, 1:5))
  print(gg2 + theme(legend.position="none"), vp = vplayout(8:14, 6:10))
  grid.draw(p.legend1)
  grid.draw(p.legend2)
  dev.off()
}

## REGION AGGREGATES
wb_aggs <- get_aggregate_table('region_name')
setnames(wb_aggs, 'region_name', 'level')
region_aggs <- get_aggregate_table('gbd_region')
region_aggs[order(-diff_mean)]
setnames(region_aggs, 'gbd_region', 'level')
results[, global := 'global']
global_aggs <- get_aggregate_table('global')
setnames(global_aggs, 'global', 'level')
all_aggs <- rbindlist(list(global_aggs,wb_aggs,region_aggs),fill=T)
write.csv(all_aggs, paste0(repo,'/results/Table_2_', Sys.Date(), '_',file_tag,'.csv'), row.names = FALSE)

