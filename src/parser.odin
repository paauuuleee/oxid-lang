package main

import "core:slice"
import str "core:strings"
import "core:testing"

Parser :: struct {
    pack: ^Package,
    path: string,
    curr_mod: ^Module,

    content: []string,
    tokens: []Token,
    index: int,

    ast: [dynamic]Node,
}

new_parser :: proc(pack: ^Package, path: string, content: string, tokens: []Token) -> Parser {
    return Parser {
        pack = pack,
        path = path,
        curr_mod = nil,

        content = str.split_lines(content),
        tokens = tokens,
        index = 0,

        ast = make([dynamic]Node)
    }
}

peek_next_token :: proc(parser: ^Parser) -> Result(Token) {
    if parser.index >= len(parser.tokens) { return error("error: no tokens left, developer error") }
    return parser.tokens[parser.index]
}

is_next_token :: proc(parser: ^Parser, kind: TokenKind) -> bool {
    if parser.index >= len(parser.tokens) { return false }
    return parser.tokens[parser.index].kind == kind
}

return_next_token :: proc(parser: ^Parser) -> Result(Token) {
    if parser.index >= len(parser.tokens) { return error("error: no tokens left, developer error") }
    token := parser.tokens[parser.index]
    parser.index += 1
    return token
}

consume_next_token :: proc(parser: ^Parser) -> Error {
    if parser.index >= len(parser.tokens) { return error("error: no tokens left, developer error") }
    parser.index += 1
    return nil
}

return_next_token_if :: proc(parser: ^Parser, kind: TokenKind) -> Result(Token) {
    token := try(peek_next_token(parser)) or_return
    if token.kind != kind { return error("error: token is not of kind: ", kind) }
    parser.index += 1
    return token
}

consume_next_token_if :: proc(parser: ^Parser, kind: TokenKind) -> Error {
    token := try(peek_next_token(parser)) or_return
    if token.kind != kind { return error("error: token is not of kind: ", kind) }
    parser.index += 1
    return nil
}

scan_stmt :: proc(parser: ^Parser, kinds: []TokenKind, until: TokenKind) -> bool {
    i := parser.index
    for parser.tokens[i].kind != until {
        if slice.contains(kinds, parser.tokens[i].kind) { return true }
        i += 1
    }
    return false
}

parse :: proc(parser: ^Parser) -> Error {
    consume_next_token_if(parser, .Module) or_return
    mod_token := try(return_next_token_if(parser, .Ident)) or_return
    mod_name := string(mod_token.value.(Ident))

    if _, ok := parser.pack.modules[mod_name]; ok { 
        parser.curr_mod = &parser.pack.modules[mod_name]
    } else { return error("error: no module ", mod_name, " is defined in oxid.mod") }
    
    loop: for {
        debug_start := DebugInfo{}
        public := false

        if token, ok := unwrap_or(return_next_token_if(parser, .Pub), NIL_TOKEN); ok { 
            debug_start = token.debug_info; public = true 
        }

        node_token := try(peek_next_token(parser)) or_return
        if debug_start == NIL_TOKEN.debug_info { debug_start = node_token.debug_info }

        #partial switch node_token.kind {
            case .Struct: {
                parse_struct(parser, public, debug_start) or_return
                continue loop
            }
            case .Enum: {
                parse_enum(parser, public, debug_start) or_return
                continue loop
            }
            case .Proto: {
                parse_proto(parser, public, debug_start) or_return
                continue loop
            }
            case .Fn: {
                parse_fn(parser, public, debug_start) or_return
                continue loop
            }
            case .Type: {
                parse_type_defn(parser, public, debug_start) or_return
                continue loop
            }
            case .Const: {
                parse_constant(parser, public, debug_start) or_return
                continue loop
            }
            case .Eof: if !public { break }
        }
        return error("error: you can only define structures, enumarations, protocols, functions, types and constants in the file scope (no statements or expressions)")
    }

    asts := [][]Node{ parser.curr_mod.ast, parser.ast[:] }
    parser.curr_mod.ast = slice.concatenate(asts)
    return nil
}

parse_struct :: proc(parser: ^Parser, public: bool, debug_start: DebugInfo) -> Error {
    s := Struct { public = public }
    consume_next_token(parser) or_return

    ident := try(return_next_token_if(parser, .Ident)) or_return
    s.ident = ident.value.(Ident)
    s.generics = consume_next_token_if(parser, .BrackL) == nil ? (try(parse_generics(parser)) or_return) : []Generic{}

    consume_next_token_if(parser, .BraceL) or_return
    s.fields = try(parse_fields(parser)) or_return
    debug_end := (try(return_next_token_if(parser, .BraceR)) or_return).debug_info
    s.debug_info = debug_span(debug_start, debug_end)

    append(&parser.ast, Node{ s, s.debug_info })
    return nil
}

parse_enum :: proc(parser: ^Parser, public: bool, debug_start: DebugInfo) -> Error {
    e := Enum { public = public }
    consume_next_token(parser) or_return

    ident := try(return_next_token_if(parser, .Ident)) or_return
    e.ident = ident.value.(Ident)
    e.generics = consume_next_token_if(parser, .BrackL) == nil ? (try(parse_generics(parser)) or_return) : []Generic{}

    consume_next_token_if(parser, .BraceL) or_return
    e.variants = try(parse_varients(parser)) or_return
    debug_end := (try(return_next_token_if(parser, .BraceR)) or_return).debug_info
    e.debug_info = debug_span(debug_start, debug_end)

    append(&parser.ast, Node{ e, e.debug_info })
    return nil
}

parse_proto :: proc(parser: ^Parser, public: bool, debug_start: DebugInfo) -> Error {
    p := Proto { public = public }
    consume_next_token(parser) or_return

    ident := try(return_next_token_if(parser, .Ident)) or_return
    p.ident = ident.value.(Ident)
    p.generics = consume_next_token_if(parser, .BrackL) == nil ? (try(parse_generics(parser)) or_return) : []Generic{}

    consume_next_token_if(parser, .BraceL) or_return
    parse_proto_elements(parser, &p.fields, &p.funcs) or_return
    debug_end := (try(return_next_token_if(parser, .BraceR)) or_return).debug_info
    p.debug_info = debug_span(debug_start, debug_end)

    append(&parser.ast, Node{ p, p.debug_info })
    return nil
}

parse_proto_elements :: proc(parser: ^Parser, fields: ^[]Field, funcs: ^[]FnSig) -> Error {
    if is_next_token(parser, .BraceR) { return nil }

    fd := make([dynamic]Field)
    fn := make([dynamic]FnSig)
    loop: for {
        debug_start := DebugInfo{}
        public := false

        if token, ok := unwrap_or(return_next_token_if(parser, .Pub), NIL_TOKEN); ok { 
            debug_start = token.debug_info; public = true 
        }

        switch is_next_token(parser, .Fn) {
            case true: {
                fn_token := try(return_next_token(parser)) or_return
                if debug_start == NIL_TOKEN.debug_info { debug_start = fn_token.debug_info }

                ident := try(return_next_token_if(parser, .Ident)) or_return
                generics := consume_next_token_if(parser, .BrackL) == nil ? (try(parse_generics(parser)) or_return) : []Generic{}

                consume_next_token_if(parser, .ParenL) or_return
                params := consume_next_token_if(parser, .ParenR) != nil ? (try(parse_params(parser, false, true)) or_return) : []Param{}

                return_type: ^TypeExpr = nil
                if consume_next_token_if(parser, .Arrow) == nil {
                    return_type = try(parse_type_expr(parser)) or_return
                }

                fnsig := FnSig {
                    public = public,
                    ident = ident.value.(Ident),
                    generics = generics,
                    params = params,
                    return_type = return_type,
                    debug_info = debug_span(debug_start, return_type.debug_info)
                }

                append(&fn, fnsig)
                if consume_next_token_if(parser, .Comma) != nil || is_next_token(parser, .BraceR) { break loop } 
            }

            case false: {
                ident := try(return_next_token_if(parser, .Ident)) or_return
                if debug_start == NIL_TOKEN.debug_info { debug_start = ident.debug_info }
                type_expr := try(parse_type_expr(parser)) or_return

                field := Field {
                    public = public,
                    variable = ident.value.(Ident),
                    data_type = type_expr,
                    debug_info = debug_span(debug_start, type_expr.debug_info)
                }
                append(&fd, field)
                if consume_next_token_if(parser, .Comma) != nil || is_next_token(parser, .BraceR) { break loop }
            }
        }
    }

    fields^ = fd[:]
    funcs^ = fn[:]
    return nil
}

parse_type_defn :: proc(parser: ^Parser, public: bool, debug_start: DebugInfo) -> Error {
    t := TypeDefn { public = public }
    consume_next_token(parser) or_return

    ident := try(return_next_token_if(parser, .Ident)) or_return
    t.ident = ident.value.(Ident)
    t.generics = consume_next_token_if(parser, .BrackL) == nil ? try(parse_generics(parser)) or_return : []Generic{}

    consume_next_token_if(parser, .Assign) or_return
    t.type_expr = try(parse_type_expr(parser)) or_return
    t.debug_info = debug_span(debug_start, t.type_expr.debug_info)

    append(&parser.ast, Node { t, t.debug_info })
    return nil
}

parse_constant :: proc(parser: ^Parser, public: bool, debug_start: DebugInfo) -> Error {
    c := Constant { public = public }
    consume_next_token(parser) or_return

    ident := try(return_next_token_if(parser, .Ident)) or_return
    c.ident = ident.value.(Ident)

    consume_next_token_if(parser, .Assign) or_return
    c.val_expr = try(parse_expr(parser)) or_return
    c.debug_info = debug_span(debug_start, c.val_expr.debug_info)

    append(&parser.ast, Node { c, c.debug_info })
    return nil
}

parse_fn :: proc(parser: ^Parser, public: bool, debug_start: DebugInfo) -> Error {
    caller_type: ^TypeExpr = nil
    consume_next_token(parser) or_return

    if caller, err := try(parse_type_expr(parser)); err == nil {
        if _, ok := caller.kind.(DefinedTypeExpr); !ok { return error("error: methods can only apply to defined types")}
        caller_type = caller
        consume_next_token_if(parser, .DoubleColon) or_return
    }
    
    ident := try(return_next_token_if(parser, .Ident)) or_return

    if is_next_token(parser, .BraceL) && caller_type == nil { 
        parse_fn_union(parser, public, debug_start, ident.value.(Ident)) or_return
        return nil
    }

    generics := consume_next_token_if(parser, .BrackL) == nil ? (try(parse_generics(parser)) or_return) : []Generic{}
    
    consume_next_token_if(parser, .ParenL) or_return
    params := consume_next_token_if(parser, .ParenR) != nil ? (try(parse_params(parser, true, caller_type == nil ? false : true)) or_return) : []Param{}

    return_type: ^TypeExpr = nil
    if consume_next_token_if(parser, .Arrow) == nil {
        return_type = try(parse_type_expr(parser)) or_return
    } 

    consume_next_token_if(parser, .BraceL) or_return
    body := try(parse_body(parser)) or_return

    end_scope := try(return_next_token_if(parser, .BraceR)) or_return

    fn := Fn {
        public = public,
        ident = ident.value.(Ident),
        generics = generics,
        params = params,
        return_type = return_type,
        body = body,
        debug_info = debug_span(debug_start, end_scope.debug_info)
    }

    switch caller_type {
        case nil: append(&parser.ast, Node { fn, fn.debug_info })
        case: append(&parser.ast, Node { Methode{ caller_type, fn }, fn.debug_info })
    }

    return nil
}

parse_fn_union :: proc(parser: ^Parser, public: bool, debug_start: DebugInfo, ident: Ident) -> Error {
    consume_next_token(parser) or_return

    funcs := make([dynamic]Ident)
    loop: for {
        ident := try(return_next_token_if(parser, .Ident)) or_return
        append(&funcs, ident.value.(Ident))
        if consume_next_token_if(parser, .Comma) != nil || is_next_token(parser, .BraceR) { break loop }
    }
    debug_end := try(return_next_token_if(parser, .BraceR)) or_return

    fn_union := FnUnion {
        public = public,
        ident = ident,
        funcs = funcs[:],
        debug_info = debug_span(debug_start, debug_end.debug_info)
    }
    append(&parser.ast, Node { fn_union, fn_union.debug_info })
    return nil
}

parse_generics :: proc(parser: ^Parser) -> Result([]Generic) {
    generics := make([dynamic]Generic)
    loop: for {
        ident := try(return_next_token_if(parser, .Ident)) or_return
        type_expr := try(parse_type_expr(parser)) or_return
        generic := Generic {
            ident = ident.value.(Ident),
            type_expr = type_expr,
            debug_info = debug_span(ident.debug_info, type_expr.debug_info)
        }
        append(&generics, generic)
        if consume_next_token_if(parser, .Comma) != nil { break loop }
    }

    consume_next_token_if(parser, .BrackR) or_return
    return generics[:]
}

parse_fields :: proc(parser: ^Parser) -> Result([]Field) {
    if is_next_token(parser, .BraceR) { return []Field{} }

    fields := make([dynamic]Field)
    loop: for {
        debug_start := DebugInfo{}
        public := false

        if token, ok := unwrap_or(return_next_token_if(parser, .Pub), NIL_TOKEN); ok { 
            debug_start = token.debug_info; public = true 
        }
        
        ident := try(return_next_token_if(parser, .Ident)) or_return
        if debug_start == NIL_TOKEN.debug_info { debug_start = ident.debug_info }
        type_expr := try(parse_type_expr(parser)) or_return

        field := Field {
            public = public,
            variable = ident.value.(Ident),
            data_type = type_expr,
            debug_info = debug_span(debug_start, type_expr.debug_info)
        }
        append(&fields, field)
        if consume_next_token_if(parser, .Comma) != nil || is_next_token(parser, .BraceR) { break loop }
    }
    return fields[:]
}

parse_varients :: proc(parser: ^Parser) -> Result([]Variant) {
    if is_next_token(parser, .BraceR) { return []Variant{} }

    variants := make([dynamic]Variant)
    value := i128(0)
    loop: for {
        debug_end := DebugInfo{}
        ident := try(return_next_token_if(parser, .Ident)) or_return

        capture_type: ^TypeExpr = nil
        if err := consume_next_token_if(parser, .BitOr); err == nil || is_next_token(parser, .ParenL) {
            capture_type := try(parse_type_expr(parser)) or_return
            debug_end = capture_type.debug_info
            if err == nil { 
                dash := try(return_next_token_if(parser, .BitOr)) or_return
                debug_end = dash.debug_info
            }
        }

        if consume_next_token_if(parser, .Assign) == nil {
            value_token := try(return_next_token_if(parser, .IntLit)) or_return
            debug_end = value_token.debug_info
            value = value_token.value.(i128)
            for var in variants {
                if value == var.value { return error("error: cannot same enum value for multiple varients") }
            }
            if !is_next_token(parser, .Comma) { return error("error: no expressions as enum values are allowed") }
        }

        variant := Variant {
            ident = ident.value.(Ident),
            value = value,
            capture_type = capture_type,
            debug_info = debug_span(ident.debug_info, debug_end)
        }
        append(&variants, variant)
        if consume_next_token_if(parser, .Comma) != nil || is_next_token(parser, .BraceR) { break loop }
        
        max_value := variants[0].value
        for var in variants[1:] {
            if max_value > var.value { max_value = var.value }
        }
        value = max_value + 1
    }
    return variants[:]
}

parse_params :: proc(parser: ^Parser, full_func, method: bool) -> Result([]Param) {
    params := make([dynamic]Param)

    if method {
        param := Param { mutable = false }

        debug_start := NIL_TOKEN.debug_info
        if token, err := try(return_next_token_if(parser, .Mut)); err == nil {
            debug_start := token.debug_info
            param.mutable = true
        }

        data_type := try(parse_type_expr(parser)) or_return
        if debug_start == NIL_TOKEN.debug_info { debug_start = data_type.debug_info }
        if _, ok := data_type.kind.(SelfTypeExpr); !ok { return error("error: methods have to take its caller as a parameter") }
        param.variable = Ident("self")
        param.data_type = data_type
        param.debug_info = debug_span(debug_start, data_type.debug_info)
        append(&params, param)
        if consume_next_token_if(parser, .Comma) != nil {
            if consume_next_token_if(parser, .ParenR) != nil {
                return error("error: illegal token") 
            }
            
            return params[:]
        }
    }

    loop: for {
        param := Param { mutable = false }
        if consume_next_token_if(parser, .Mut) == nil { param.mutable = true }
        switch full_func {
            case true: {
                ident := try(return_next_token_if(parser, .Ident)) or_return
                param.variable = ident.value.(Ident)
                param.data_type = try(parse_type_expr(parser)) or_return
                if _, ok := param.data_type.kind.(SelfTypeExpr); ok { return error("error: cannot take parameter of the caller at this position") }
            }

            case false: {
                ident := try(return_next_token_if(parser, .Ident)) or_else NIL_TOKEN
                if ident == NIL_TOKEN { param.variable = Ident("__none__") }
                param.data_type = try(parse_type_expr(parser)) or_return
                if _, ok := param.data_type.kind.(SelfTypeExpr); ok { return error("error: cannot take parameter of the caller at this position") }
            }
        }
        append(&params, param)
        if consume_next_token_if(parser, .Comma) != nil || is_next_token(parser, .ParenR) { break loop }
    }

    consume_next_token_if(parser, .ParenR) or_return
    return params[:]
}

parse_expr :: proc(parser: ^Parser) -> Result(^Expr) { // TODO: unimplemented
    unimplemented() 
}

parse_type_expr :: proc(parser: ^Parser) -> Result(^TypeExpr) {
    type_expr := new(TypeExpr)

    debug_start := DebugInfo{}
    init_token := try(return_next_token(parser)) or_return
    debug_start = init_token.debug_info

    #partial switch init_token.kind {
        case .BrackL: {
            #partial switch token := try(return_next_token(parser)) or_return; token.kind {
                case .BrackR: {
                    array_type := try(parse_type_expr(parser)) or_return
                    type_expr.kind = ArrayTypeExpr { array_type }
                    type_expr.debug_info = debug_span(debug_start, array_type.debug_info)
                }
                case .Comma: {
                    consume_next_token_if(parser, .BrackR) or_return
                    matrix_type := try(parse_type_expr(parser)) or_return
                    type_expr.kind = MatrixTypeExpr { matrix_type }
                    type_expr.debug_info = debug_span(debug_start, matrix_type.debug_info)
                }
                case .BitAnd: {
                    consume_next_token_if(parser, .BrackR) or_return
                    slice_type := try(parse_type_expr(parser)) or_return
                    type_expr.kind = SliceTypeExpr { slice_type }
                    type_expr.debug_info = debug_span(debug_start, slice_type.debug_info)
                }
                case .RuntimeRef: {
                    consume_next_token_if(parser, .BrackR) or_return
                    slice_type := try(parse_type_expr(parser)) or_return
                    type_expr.kind = RuntimeSliceTypeExpr { slice_type }
                    type_expr.debug_info = debug_span(debug_start, slice_type.debug_info)
                }
                case .DoubleDot: {
                    consume_next_token_if(parser, .BrackR) or_return
                    array_type := try(parse_type_expr(parser)) or_return
                    type_expr.kind = DynamicArrayTypeExpr { array_type }
                    type_expr.debug_info = debug_span(debug_start, array_type.debug_info)
                }
                case .Primitive: {
                    #partial switch token.value.(PrimitiveKind) {
                        case .Bool, .String, .F64, .F32, .F16:
                            return error("error: range of strings, booleans or floats is not allowed")
                    }
                    debug_end := (try(return_next_token_if(parser, .BrackR)) or_return).debug_info
                    type_expr.kind = RangeTypeExpr { token.value.(PrimitiveKind) }
                    type_expr.debug_info = debug_span(debug_start, debug_end)
                }

                case: {
                    key_type := try(parse_type_expr(parser)) or_return
                    consume_next_token_if(parser, .BrackR) or_return
                    cell_type := try(parse_type_expr(parser)) or_return
                    type_expr.kind = MapTypeExpr { key_type, cell_type }
                    type_expr.debug_info = debug_span(debug_start, cell_type.debug_info)
                }
            }
        }

        case .BitAnd: {
            ref_type := try(parse_type_expr(parser)) or_return
            if kind, ok := ref_type.kind.(SelfTypeExpr); ok {
                type_expr.kind = SelfTypeExpr { ref = true, self_type = kind.self_type }
            }
            else { type_expr.kind = RefTypeExpr { ref_type } }
            type_expr.debug_info = debug_span(debug_start, ref_type.debug_info)
        }

        case .RuntimeRef: {
            ref_type := try(parse_type_expr(parser)) or_return
            type_expr.kind = RuntimeRefTypeExpr { ref_type }
            type_expr.debug_info = debug_span(debug_start, ref_type.debug_info)
        }

        case .Mul: {
            ptr_type := try(parse_type_expr(parser)) or_return
            type_expr.kind = SinglePointerTypeExpr { ptr_type }
            type_expr.debug_info = debug_span(debug_start, ptr_type.debug_info)
        }

        case .Pow: {
            ptr_type := try(parse_type_expr(parser)) or_return
            type_expr.kind = SharedPointerTypeExpr { ptr_type }
            type_expr.debug_info = debug_span(debug_start, ptr_type.debug_info)
        }

        case .ParenL: {
            element_types := make([dynamic]TypeExpr)
            tupel_loop: for {
                elem_type := try(parse_type_expr(parser)) or_return
                defer free(elem_type)
                append(&element_types, elem_type^)
                if consume_next_token_if(parser, .Comma) != nil { break tupel_loop }
            }
            paren := try(return_next_token_if(parser, .ParenR)) or_return
            type_expr.kind = TupelTypeExpr { element_types[:] }
            type_expr.debug_info = debug_span(debug_start, paren.debug_info)
        }

        case .QMark: {
            some_type := try(parse_type_expr(parser)) or_return
            type_expr.kind = OptionTypeExpr { some_type }
            type_expr.debug_info = debug_span(debug_start, some_type.debug_info)
        }

        case .Bang: {
            ok_type := try(parse_type_expr(parser)) or_return
            type_expr.kind = ResultTypeExpr { ok_type }
            type_expr.debug_info = debug_span(debug_start, ok_type.debug_info)
        }

        case .Self: {
            type_expr.kind = SelfTypeExpr { ref = false, self_type = nil }
            type_expr.debug_info = init_token.debug_info
        }

        case .Primitive: {
            type_expr.kind = PrimitiveTypeExpr { init_token.value.(PrimitiveKind) }
            type_expr.debug_info = init_token.debug_info
        }

        case .Fn: {
            generics := consume_next_token_if(parser, .BrackL) == nil ? (try(parse_generics(parser)) or_return) : []Generic{}

            consume_next_token_if(parser, .ParenL) or_return
            params := consume_next_token_if(parser, .ParenR) != nil ? (try(parse_params(parser, false, false)) or_return) : []Param{}

            return_type: ^TypeExpr = nil
            if consume_next_token_if(parser, .Arrow) == nil {
                return_type = try(parse_type_expr(parser)) or_return
            }
            
            type_expr.kind = FnTypeExpr {
                generics = generics, 
                params = params,
                return_type = return_type
            }
            type_expr.debug_info = debug_span(debug_start, return_type.debug_info)
        }

        case .Ident: {
            debug_end := init_token.debug_info

            generic_args := make([dynamic]Expr)
            if err := consume_next_token_if(parser, .BrackL); err == nil {
                def_loop: for {
                    generic_arg := try(parse_expr(parser)) or_return
                    defer free(generic_arg)
                    append(&generic_args, generic_arg^)
                    if consume_next_token_if(parser, .Comma) != nil { break def_loop }
                }
                bracket := try(return_next_token_if(parser, .BrackR)) or_return
                debug_end = bracket.debug_info
            }

            type_expr.kind = DefinedTypeExpr { init_token.value.(Ident), generic_args[:] }
            type_expr.debug_info = debug_span(debug_start, debug_end)
        }

        case: return error("error: illegal token")
    }
    
    return type_expr
}

parse_match_expr :: proc(praser: ^Parser) -> Result(^MatchExpr) { // TODO: unimplemente
    unimplemented()
}

parse_branch_expr :: proc(praser: ^Parser) -> Result(^BranchExpr) { // TODO: unimplemente
    unimplemented()
}

parse_call_expr :: proc(parser: ^Parser) -> Result(^CallExpr) { // TODO: unimplemente
    unimplemented()
}

parse_condition_expr :: proc(parser: ^Parser) -> Result(^CondExpr) { // TODO: unimplemente
    unimplemented()
}

parse_range_expr :: proc(parser: ^Parser) -> Result(^RangeExpr) { // TODO: unimplemente
    unimplemented()
}

parse_body :: proc(parser: ^Parser) -> Result([]Stmt) {
    if is_next_token(parser, .BraceR) { return []Stmt{} }
    stmts := make([dynamic]Stmt)
    for !is_next_token(parser, .BraceR) {
        stmt := try(parse_stmt(parser)) or_return
        append(&stmts, stmt^)
    }
    return stmts[:]
}

parse_stmt :: proc(parser: ^Parser) -> Result(^Stmt) {
    stmt := new(Stmt)
    #partial switch (try(peek_next_token(parser)) or_return).kind {
        case .Let: {
            declare_stmt := try(parse_declare_stmt(parser)) or_return
            stmt.stmt = declare_stmt
            stmt.debug_info = declare_stmt.debug_info
        }
        case .Defer: {
            defer_stmt := try(parse_defer_stmt(parser)) or_return
            stmt.stmt = defer_stmt
            stmt.debug_info = defer_stmt.debug_info
        }
        case .For, .Tag: {
            loop_stmt := try(parse_loop_stmt(parser)) or_return
            stmt.stmt = loop_stmt
            stmt.debug_info = loop_stmt.debug_info
        }
        case .Match: {
            match_stmt := try(parse_match_stmt(parser)) or_return
            stmt.stmt = match_stmt
            stmt.debug_info = match_stmt.debug_info
        }
        case .If: { 
            branch_stmt := try(parse_branch_stmt(parser)) or_return 
            stmt.stmt = branch_stmt
            stmt.debug_info = branch_stmt.debug_info
        }
        case .Return: {
            return_stmt := try(parse_return_stmt(parser)) or_return
            stmt.stmt = return_stmt
            stmt.debug_info = return_stmt.debug_info
        }
        case .Break: {
            break_stmt := try(parse_break_stmt(parser)) or_return
            stmt.stmt = break_stmt
            stmt.debug_info = break_stmt.debug_info
        }
        case .Continue: {
            continue_stmt := try(parse_continue_stmt(parser)) or_return
            stmt.stmt = continue_stmt
            stmt.debug_info = continue_stmt.debug_info
        }
        case .Fallthrough: {
            fallthrough_stmt := try(parse_fallthrough_stmt(parser)) or_return
            stmt.stmt = fallthrough_stmt
            stmt.debug_info = fallthrough_stmt.debug_info
        }
        case: {
            if scan_stmt(parser, {.Assign, .AddAs, .SubAs, .MulAs, .DivAs, .PowAs, .ModAs, .BitAndAs, .BitOrAs, .BitNotAs, .BitXorAs}, .SemiColon) {
                assign_stmt := try(parse_assign_stmt(parser)) or_return
                stmt.stmt = assign_stmt
                stmt.debug_info = assign_stmt.debug_info
            } else {
                expr_stmt := try(parse_expr_stmt(parser)) or_return
                stmt.stmt = expr_stmt
                stmt.debug_info = expr_stmt.debug_info
            }
        }
    }
    return stmt
}

parse_branch_stmt :: proc(parser: ^Parser) -> Result(BranchStmt) {
    branch_stmt := BranchStmt{ try(parse_branch_expr(parser)) or_return }
    return branch_stmt
}

parse_match_stmt :: proc(parser: ^Parser) -> Result(MatchStmt) {
    match_stmt := MatchStmt{ try(parse_match_expr(parser)) or_return }
    return match_stmt
}

parse_return_stmt :: proc(parser: ^Parser) -> Result(ReturnStmt) {
    debug_start := (try(return_next_token_if(parser, .Return)) or_return).debug_info
    debug_end := debug_start
    if consume_next_token_if(parser, .SemiColon) != nil {
        expr := try(parse_expr(parser)) or_return
        consume_next_token_if(parser, .SemiColon)
        return ReturnStmt{ expr, debug_span(debug_start, expr.debug_info) }
    }
    return ReturnStmt { nil, debug_span(debug_start, debug_end) }
}

parse_break_stmt :: proc(parser: ^Parser) -> Result(BreakStmt) {
    debug_start := (try(return_next_token_if(parser, .Break)) or_return).debug_info
    debug_end := debug_start
    if consume_next_token_if(parser, .SemiColon) != nil {
        tag := try(return_next_token_if(parser, .Tag)) or_return
        consume_next_token_if(parser, .SemiColon) or_return
        return BreakStmt{ tag.value.(Tag), debug_span(debug_start, tag.debug_info) }
    }
    return BreakStmt { nil, debug_span(debug_start, debug_end) }
}

parse_continue_stmt :: proc(parser: ^Parser) -> Result(ContinueStmt) {
    debug_start := (try(return_next_token_if(parser, .Continue)) or_return).debug_info
    debug_end := debug_start
    if consume_next_token_if(parser, .SemiColon) != nil {
        tag := try(return_next_token_if(parser, .Tag)) or_return
        consume_next_token_if(parser, .SemiColon) or_return
        return ContinueStmt{ tag.value.(Tag), debug_span(debug_start, tag.debug_info) }
    }
    return ContinueStmt { nil, debug_span(debug_start, debug_end) }
}

parse_fallthrough_stmt :: proc(parser: ^Parser) -> Result(FallthroughStmt) {
    debug_info := (try(return_next_token_if(parser, .Fallthrough)) or_return).debug_info
    consume_next_token_if(parser, .SemiColon) or_return
    return FallthroughStmt { debug_info }
}

parse_expr_stmt :: proc(parser: ^Parser) -> Result(ExprStmt) {
    expr := try(parse_expr(parser)) or_return
    return ExprStmt { expr }
}

parse_assign_stmt :: proc(parser: ^Parser) -> Result(AssignStmt) {
    call_expr := try(parse_call_expr(parser)) or_return
    op := try(return_next_token(parser)) or_return
    #partial switch op.kind {
        case .Assign: {}
        case .AddAs: {}
        case .SubAs: {}
        case .MulAs: {}
        case .DivAs: {}
        case .PowAs: {}
        case .ModAs: {}
        case .BitAndAs: {}
        case .BitXorAs: {}
        case .BitNotAs: {}
        case .BitOrAs: {}
        case: {}
    }
    assign_expr := try(parse_expr(parser)) or_return
    consume_next_token_if(parser, .SemiColon) or_return
    return AssignStmt { call_expr, assign_expr, op, debug_span(call_expr.debug_info, assign_expr.debug_info)}
}

parse_defer_stmt :: proc(parser: ^Parser) -> Result(DeferStmt) {
    debug_start := (try(return_next_token_if(parser, .Defer)) or_return).debug_info
    defer_stmt := try(parse_stmt(parser)) or_return
    consume_next_token_if(parser, .SemiColon) or_return
    return DeferStmt { defer_stmt, debug_span(debug_start, defer_stmt.debug_info) }
}

parse_declare_stmt :: proc(parser: ^Parser) -> Result(DeclareStmt) {
    debug_start := (try(return_next_token_if(parser, .Let)) or_return).debug_info
    debug_end := NIL_TOKEN.debug_info

    mutables := make([dynamic]bool, 1)
    variables := make([dynamic]Ident, 1)
    types := make([dynamic]TypeExpr, 1)
    if consume_next_token_if(parser, .ParenL) == nil {
        loop: for {
            mutable := consume_next_token_if(parser, .Mut) == nil ? (true) : false
            append(&mutables, mutable)
            ident := try(return_next_token_if(parser, .Ident)) or_return
            append(&variables, ident.value.(Ident))
            if consume_next_token_if(parser, .Comma) == nil { continue loop }
            type_expr := try(parse_type_expr(parser)) or_return
            append(&types, type_expr^)
            if consume_next_token_if(parser, .Comma) != nil { 
                consume_next_token_if(parser, .ParenL) or_return
                break loop 
            }
        }
        if !is_next_token(parser, .Assign) { 
            return error("error: cannot declare multiple variables without an assign expression which results in a tupel structure") 
        }
    } else {
        mutable := consume_next_token_if(parser, .Mut) == nil ? (true) : false
        append(&mutables, mutable)
        ident := try(return_next_token_if(parser, .Ident)) or_return
        append(&variables, ident.value.(Ident))
        if !is_next_token(parser, .Assign) {
            type_expr := try(parse_type_expr(parser)) or_return
            append(&types, type_expr^)
            debug_end = type_expr.debug_info
        }
    }

    assign_expr: ^Expr = nil
    if consume_next_token_if(parser, .Assign) == nil {
        assign_expr = try(parse_expr(parser)) or_return
        debug_end = assign_expr.debug_info
    }

    consume_next_token_if(parser, .SemiColon) or_return
    return DeclareStmt { mutables[:], variables[:], types[:], assign_expr, debug_span(debug_start, debug_end) }
}

parse_loop_stmt :: proc(parser: ^Parser) -> Result(LoopStmt) {
    tag: Maybe(Tag) = nil
    debug_start := NIL_TOKEN.debug_info
    if tag_tok, err := try(return_next_token_if(parser, .Tag)); err == nil { tag = tag_tok.value.(Tag) }
    debug_start = debug_start == NIL_TOKEN.debug_info ? (try(return_next_token_if(parser, .For)) or_return).debug_info : debug_start
    
    inscope_declare: ^InscopeDeclareExpr = nil
    if is_next_token(parser, .Let) {
        declare := try(parse_declare_stmt(parser)) or_return
        inscope_declare = new(InscopeDeclareExpr)
        inscope_declare.declare = declare 
    }

    loop_kind: LoopStmtKind
    if scan_stmt(parser, {.In}, .BraceL) {
        if consume_next_token_if(parser, .ParenL) == nil {
            ref_var := try(return_next_token_if(parser, .Ident)) or_return
            consume_next_token_if(parser, .Comma) or_return
            index_var := try(return_next_token_if(parser, .Ident)) or_return
            consume_next_token_if(parser, .ParenR) or_return
            consume_next_token_if(parser, .In) or_return
            iter_call := try(parse_call_expr(parser)) or_return
            debug_end := (try(return_next_token_if(parser, .BraceL)) or_return).debug_info
            loop_kind = IteratorLoop { ref_var.value.(Ident), index_var.value.(Ident), iter_call, debug_span(debug_start, debug_end) }
        } else {
            variable := try(return_next_token_if(parser, .Ident)) or_return
            consume_next_token_if(parser, .In) or_return
            range_expr := try(parse_range_expr(parser)) or_return
            debug_end := (try(return_next_token_if(parser, .BraceL)) or_return).debug_info
            loop_kind = RangeLoop {variable.value.(Ident), range_expr, debug_span(debug_start, debug_end)}
        }
    } else {
        condition := try(parse_condition_expr(parser)) or_return
        debug_end := (try(return_next_token_if(parser, .BraceL)) or_return).debug_info
        loop_kind = ConditionLoop { condition, debug_span(debug_start, debug_end) }
    }

    body := try(parse_body(parser)) or_return
    debug_end := (try(return_next_token_if(parser, .BraceR)) or_return).debug_info
    return LoopStmt { tag, inscope_declare, loop_kind, body, debug_span(debug_start, debug_end) }
}