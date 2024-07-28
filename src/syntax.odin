package main

import "core:fmt"

// Defines

Node :: struct {
    node: AnyNode,
    debug_info: DebugInfo
}

AnyNode :: union {
    Proto,
    Struct,
    Enum,
    Fn,
    Methode,
    FnUnion,
    TypeDefn,
    Constant
}

Proto :: struct {
    public: bool,
    ident: Ident,
    generics: []Generic,
    fields: []Field,
    funcs: []FnSig,
    debug_info: DebugInfo
}

Struct :: struct {
    public: bool,
    ident: Ident,
    // protocols: []TypeExpr,
    generics: []Generic,
    fields: []Field,
    debug_info: DebugInfo
}

Enum :: struct {
    public: bool,
    ident: Ident,
    // protocols: []TypeExpr,
    generics: []Generic,
    variants: []Variant,
    debug_info: DebugInfo
}

Fn :: struct {
    public: bool,
    ident: Ident,
    generics: []Generic,
    params: []Param,
    return_type: ^TypeExpr,
    body: []Stmt,
    debug_info: DebugInfo
}

TypeDefn :: struct {
    public: bool,
    ident: Ident,
    generics: []Generic,
    type_expr: ^TypeExpr,
    debug_info: DebugInfo
}

Methode :: struct {
    caller_type: ^TypeExpr,
    using func: Fn
}

Constant :: struct {
    public: bool,
    ident: Ident,
    val_expr: ^Expr,
    debug_info: DebugInfo
}

FnUnion :: struct {
    public: bool,
    ident: Ident,
    funcs: []Ident,
    debug_info: DebugInfo
}

Generic :: struct {
    ident: Ident,
    type_expr: ^TypeExpr,
    debug_info: DebugInfo
}

Variant :: struct {
    ident: Ident,
    value: i128,
    capture_type: ^TypeExpr,
    debug_info: DebugInfo
}

Field :: struct {
    public: bool,
    variable: Ident,
    data_type: ^TypeExpr,
    debug_info: DebugInfo
}

Param :: struct {
    mutable: bool,
    variable: Ident,
    data_type: ^TypeExpr,
    debug_info: DebugInfo
}

FnSig :: struct {
    public: bool,
    ident: Ident,
    generics: []Generic,
    params: []Param,
    return_type: ^TypeExpr,
    debug_info: DebugInfo
}

// Expr

Expr :: struct {
    expr: AnyExpr,
    debug_info: DebugInfo
}

AnyExpr :: union {
    StringLitExpr,
    RuneLitExpr,
    FloatLitExpr,
    IntLitExpr,
    BoolLitExpr,
    VarientExpr,
    InitExpr,
    InitArrayExpr,
    InitMapExpr,
    MathExpr,
    CondExpr,
    BitExpr,
    RangeExpr,
    CallExpr,
    MatchExpr,
    UnwrapExpr,
    ExtractExpr,
    TryExpr,
    CatchExpr,
    DealOptionExpr,
    BranchExpr,
    TypeExpr,
}

StringLitExpr :: struct {
    val: string,
    debug_info: DebugInfo,
}

RuneLitExpr :: struct {
    val: rune,
    debug_info: DebugInfo
}

FloatLitExpr :: struct {
    val: f64,
    debug_info: DebugInfo
}

IntLitExpr :: struct {
    val: i128,
    debug_info: DebugInfo
}

BoolLitExpr :: struct {
    val: bool,
    debug_info: DebugInfo
}

VarientExpr :: struct {
    enum_type: ^TypeExpr,
    varient: Ident,
    caputure_expr: ^Expr,
    debug_info: DebugInfo
}

InitTupelExpr :: struct {
    elems: []Expr,
    debug_info: DebugInfo
}

InitExpr :: struct {
    init_type: ^TypeExpr,
    fields: []Ident,
    field_args: []Expr,
    debug_info: DebugInfo
}

InitArrayExpr :: struct {
    init_type: ^TypeExpr,
    elems: []Expr,
    debug_info: DebugInfo
}

InitMapExpr :: struct {
    init_type: ^TypeExpr,
    keys: []Expr,
    cells: []Expr,
    debug_info: DebugInfo
}

MathExpr :: struct {
    left, right: ^Expr,
    op: Token,
    debug_info: DebugInfo
}

CondExpr :: struct {
    left, right: ^Expr,
    op: Token,
    debug_info: DebugInfo
}

BitExpr :: struct {
    left, right: ^Expr,
    op: Token,
    debug_info: DebugInfo
}

RangeExpr :: struct {
    upper, lower: i128,
    debug_info: DebugInfo
}

CallExpr :: struct {
    kind: CallExprKind,
    debug_info: DebugInfo,
}

CallExprKind :: union {
    FnCallExpr,
    FieldCallExpr,
    VarCallExpr,
    SliceCallExpr,
    IndexCallExpr,
    MatrixCallExpr,
    MapCallExpr,
}

FnCallExpr :: struct {
    ident: Token,
    args: []Expr,
    debug_info: DebugInfo
}

FieldCallExpr :: struct {
    parent, field: ^CallExpr,
    debug_info: DebugInfo
}

VarCallExpr :: struct {
    ident: Ident,
    debug_info: DebugInfo
}

SliceCallExpr :: struct {
    field: ^CallExpr,
    range: ^RangeExpr,
    debug_info: DebugInfo
}

IndexCallExpr :: struct {
    field: ^CallExpr,
    index: u64,
    debug_info: DebugInfo
}

MatrixCallExpr :: struct {
    field: ^CallExpr,
    row, col: u64,
    debug_info: DebugInfo
}

MapCallExpr :: struct {
    field: ^CallExpr,
    key_expr: ^Expr,
    debug_info: DebugInfo
}

UnwrapExpr :: struct {
    expr: ^CallExpr,
    debug_info: DebugInfo
}

ExtractExpr :: struct {
    expr: ^CallExpr,
    debug_info: DebugInfo
}

TryExpr :: struct {
    expr: ^CallExpr,
    body: []Stmt,
    debug_info: DebugInfo
}

CatchExpr :: struct {
    err_var: ^VarCallExpr,
    body: []Stmt,
    debug_info: DebugInfo
}

DealOptionExpr :: struct {
    expr: ^CallExpr,
    none_case: ^Expr,
    debug_info: DebugInfo
}

InscopeDeclareExpr :: struct {
    using declare: DeclareStmt
}

MatchExpr :: struct {
    inscope_declare: ^InscopeDeclareExpr,
    expr: ^Expr,
    cases: []MatchCase,
    else_case: []MatchCase,
    debug_info: DebugInfo
}

MatchCase :: struct {
    pattern: MatchPattern,
    body: []Stmt,
    debug_info: DebugInfo
}

MatchPatternLiteral :: union {
    FloatLitExpr,
    IntLitExpr,
    BoolLitExpr,
    StringLitExpr,
    RuneLitExpr,
    RangeExpr,
    VarientExpr,
    InitExpr,
    InitMapExpr,
    InitArrayExpr,
    InitTupelExpr,
    InscopeDeclareExpr,
}

MatchPattern :: struct {
    pattern_extracts: []^InscopeDeclareExpr, // point to pattern_expractions in the match expressions
    match_exprs: []MatchPatternLiteral,
    debug_info: DebugInfo
}

BranchExpr :: struct {
    inscope_declare: ^InscopeDeclareExpr,
    condition: ^Expr,
    body: []Stmt,
    else_branches: []BranchExpr,
    debug_info: DebugInfo
}

TypeExpr :: struct {
    kind: TypeExprKind,
    debug_info: DebugInfo
}

TypeExprKind :: union {
    PrimitiveTypeExpr,
    DefinedTypeExpr,
    OptionTypeExpr,
    ResultTypeExpr,
    SelfTypeExpr,
    RefTypeExpr,
    RuntimeRefTypeExpr,
    SinglePointerTypeExpr,
    SharedPointerTypeExpr,
    ArrayTypeExpr,
    DynamicArrayTypeExpr,
    SliceTypeExpr,
    RuntimeSliceTypeExpr,
    MatrixTypeExpr,
    RangeTypeExpr,
    MapTypeExpr,
    FnTypeExpr,
    TupelTypeExpr,
}

OptionTypeExpr :: struct {
    some_type: ^TypeExpr,
}

ResultTypeExpr :: struct {
    ok_type: ^TypeExpr,
}

PrimitiveTypeExpr :: struct {
    kind: PrimitiveKind,
}

DefinedTypeExpr :: struct {
    ident: Ident,
    generic_args: []Expr,
}

SinglePointerTypeExpr :: struct {
    type_expr: ^TypeExpr,
}

SharedPointerTypeExpr :: struct {
    type_expr: ^TypeExpr,
}

SelfTypeExpr :: struct {
    ref: bool,
    self_type: ^TypeExpr,
}

RefTypeExpr :: struct {
    type_expr: ^TypeExpr,
}

RuntimeRefTypeExpr :: struct {
    type_expr: ^TypeExpr,
}

ArrayTypeExpr :: struct {
    type_expr: ^TypeExpr,
}

DynamicArrayTypeExpr :: struct {
    type_expr: ^TypeExpr,
}

SliceTypeExpr :: struct {
    type_expr: ^TypeExpr,
}

RuntimeSliceTypeExpr :: struct {
    type_expr: ^TypeExpr,
}

MatrixTypeExpr :: struct {
    type_expr: ^TypeExpr,
}

RangeTypeExpr :: struct {
    range_kind: PrimitiveKind
}

MapTypeExpr :: struct {
    key_type_expr: ^TypeExpr,
    cell_type_expr: ^TypeExpr,
}

FnTypeExpr :: struct {
    generics: []Generic,
    params: []Param,
    return_type: ^TypeExpr
}

TupelTypeExpr :: struct {
    element_types: []TypeExpr,
}

// Stmts

Stmt :: struct {
    stmt: AnyStmt,
    debug_info: DebugInfo
}

AnyStmt :: union {
    BreakStmt,
    ContinueStmt,
    FallthroughStmt,
    ReturnStmt,
    DeclareStmt,
    AssignStmt,
    LoopStmt,
    DeferStmt,
    ExprStmt,
    MatchStmt,
    BranchStmt
}

BreakStmt :: struct { 
    tag: Maybe(Tag),
    debug_info: DebugInfo 
}

ContinueStmt :: struct { 
    tag: Maybe(Tag),
    debug_info: DebugInfo,
}

FallthroughStmt :: struct { 
    debug_info: DebugInfo
}

ReturnStmt :: struct { 
    return_expr: ^Expr,
    debug_info: DebugInfo
}

DeclareStmt :: struct {
    mutable: []bool,
    variables: []Ident,
    types: []TypeExpr,
    assign_expr: ^Expr,
    debug_info: DebugInfo
}

AssignStmt :: struct {
    variable: ^CallExpr,
    assign_exprs: ^Expr,
    op: Token,
    debug_info: DebugInfo
}

LoopStmt :: struct {
    tag: Maybe(Tag),
    inscope_declare: ^InscopeDeclareExpr,
    kind: LoopStmtKind,
    body: []Stmt,
    debug_info: DebugInfo
}

LoopStmtKind :: union {
    ConditionLoop,
    RangeLoop,
    IteratorLoop
}

ConditionLoop :: struct {
    condition: ^CondExpr,
    debug_info: DebugInfo
}

RangeLoop :: struct {
    variable: Ident,
    range: ^RangeExpr,
    debug_info: DebugInfo
}

IteratorLoop :: struct {
    ref_var, index_var: Ident,
    iterator: ^CallExpr,
    debug_info: DebugInfo
}

DeferStmt :: struct {
    defer_stmt: ^Stmt,
    debug_info: DebugInfo
}

BranchStmt :: struct {
    using branch_expr: ^BranchExpr
}

MatchStmt :: struct {
    using match_expr: ^MatchExpr
}

ExprStmt :: struct {
    using expr: ^Expr,
}