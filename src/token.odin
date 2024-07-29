package main

TokenKind :: enum {
	Nil,
	Eof,			
	Module,			// module
	Struct,			// struct
	Enum,			// enum
	Fn,				// fn
	Type,			// type
	Const,			// const
	Proto,			// proto
	Self,			// self
	Pub,			// pub
	Try,			// try
	Catch,			// catch
	Extract,		// extract
	Unwrap,			// unwrap
	Defer,			// defer
	Let,			// let
	Mut,			// mut
	If,				// if
	Else,			// else
	Match,			// match
	For,			// for
	In, 			// in
	DoubleColon, 	// ::
	Colon,			// :
	SemiColon,		// ;
	Comma,			// ,
	OrElse,			// ??
	QMark,			// ?
	Bang,			// !
	Eq,				// ==
	NotEq,			// !=
	Greater,		// >
	Less,			// <
	GreaterEq,		// >=
	LessEq,			// <=
	And,			// &&
	Or,				// ||
	Add,			// +
	Sub,			// -
	Mul,			// *
	Div,			// /
	Mod,			// %
	Pow,			// ^
	AddAs,			// +=
	SubAs,			// -=
	MulAs,			// *=
	DivAs,			// /=
	ModAs,			// %=
	PowAs,			// ^=
	Assign,			// =
	Arrow,			// ->
	Dot,			// .
	DoubleDot,		// ..
	BrackL,			// [
	BrackR,			// ]
	BraceL,			// {
	BraceR,			// }
	ParenL,			// (
	ParenR,			// )
	RuntimeRef, 	// @
	BitAnd, 		// &
	BitOr, 			// |
	BitNot, 		// ~
	BitXor, 		// #
	BitAndAs,		// &=
	BitOrAs,		// |=
	BitNotAs,		// ~=
	BitXorAs,		// #=
	IntLit,			// e.g. -2
	FloatLit,		// e.g. 0.34
	RuneLit,		// e.g. 'a'
	StringLit,		// e.g. "ur mom"
	BoolLit,		// e.g. true
	Primitive,		// e.g. i32
	Ident,			// e.g. foo
	Break,			// break
	Continue,		// continue
	Return,			// return
	Fallthrough,		// fallthrough
	Tag
}

PrimitiveKind :: enum {
	U8,
	U16,
	U32,
	U64,
	Uint,
	Usize,
	I8,
	I16,
	I32,
	I64,
	Int,
	Isize,
	F16,
	F32,
	F64,
	Bool,
	Rune,
	String,
}

Ident :: distinct string
Tag :: distinct string

Token :: struct {
	debug_info: DebugInfo,
	kind:  TokenKind,
	value: union {
		f64,
		i128,
		rune,
		bool,
		string,
		Ident,
		Tag,
		PrimitiveKind,
	},
}

NIL_TOKEN :: Token{}

new_token :: proc {
	new_token_kind,
	new_token_int,
	new_token_float,
	new_token_bool,
	new_token_rune,
	new_token_string,
	new_token_primitive,
	new_token_ident,
	new_token_tag,
}

new_token_kind :: proc(kind: TokenKind, debug_info: DebugInfo) -> Token {
	return Token {
		debug_info,
		kind,
		nil
	}
}

new_token_int :: proc(value: i128,  debug_info: DebugInfo) -> Token {
	return Token {
		debug_info,
		.IntLit,
		value
	}
}

new_token_float :: proc(value: f64, debug_info: DebugInfo) -> Token {
	return Token {
		debug_info,
		.FloatLit,
		value
	}
}

new_token_bool :: proc(value: bool, debug_info: DebugInfo) -> Token {
	return Token {
		debug_info,
		.BoolLit,
		value
	}
}

new_token_rune :: proc(value: rune, debug_info: DebugInfo) -> Token {
	return Token {
		debug_info,
		.RuneLit,
		value
	}
}

new_token_string :: proc(value: string, debug_info: DebugInfo) -> Token {
	return Token {
		debug_info,
		.StringLit,
		value
	}
}

new_token_primitive :: proc(value: PrimitiveKind, debug_info: DebugInfo) -> Token {
	return Token {
		debug_info,
		.Primitive,
		value
	}
}

Test :: union {
	int,
	string,
}

new_token_ident :: proc(value: Ident, debug_info: DebugInfo) -> Token {
	return Token {
		debug_info,
		.Ident,
		value
	}
}

new_token_tag :: proc(value: Tag, debug_info: DebugInfo) -> Token {
	return Token {
		debug_info,
		.Tag,
		value
	}
}