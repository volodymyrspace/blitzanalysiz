updates <- get_updates(since='9.0')

for (u in updates) {
	fn.totals <- filename_update_totals(u)
	fn.update <- filename_update(u)
	fn.career <- filename_cumulative(u)
	fn.career_src <- filename_cumulative_src(u)
	fns <- c(fn.totals, fn.update, fn.career, fn.career_src)

	for (fn in fns) {
		message("Update: ", u, " File: ", fn)
		transform_parts(fn, function(DT) { DT[tank_id == 6753, is_premium:=FALSE]; return(DT);})
		gc()
	}

}