## Load tankopedia
WG_Tankopedia <- mongo("Tankopedia", "BlitzStats", URI, options = ssl_opts)
tanks <- as.data.table(jsonlite::flatten(WG_Tankopedia$find("{}")), encoding = "UTF-8")
setkey(tanks, tank_id)
tanks[, nation := as.factor(nation)]
tanks[, type := as.factor(type)]
tanks[, name := as.factor(name)]
tanks[, type := factor(type, tank_types, Tank_Types, ordered = TRUE)]
tanks[, nation := factor(nation, nations, Nations, ordered = TRUE)]
# saveRDS(tanks, file.path(get_data_dir(), 'tanks.rda'))
