package main

DebugInfo :: struct {
	beg_col, beg_row, beg_off,
	end_col, end_row, end_off: u64,
}

debug_len :: proc(debug_info: DebugInfo) -> u64 {
	return debug_info.beg_off - debug_info.end_off + 1
}

debug_span :: proc(debug_start: DebugInfo, debug_end: DebugInfo) -> DebugInfo {
	return DebugInfo {
		debug_start.beg_col,
		debug_start.beg_row,
		debug_start.beg_off,
		debug_end.end_col,
		debug_end.end_row,
		debug_end.end_off,
	}
}