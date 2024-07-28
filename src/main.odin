package main

import "core:fmt"
import "core:os"

main :: proc() {
	if len(os.args) == 3 && os.args[1] == "init" {
		init_package(os.args[2])
		return
	}

	fmt.println("Hallo")
	//pack := read_package()
}

Lexer :: struct {
	path: string,
	content: string,

	curr_col, 
	curr_row, 
	curr_off: u64,

	tokens: [dynamic]Token
}