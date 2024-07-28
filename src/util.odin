package main

import "core:fmt"

Error :: Maybe(string)
Result :: union($T: typeid) #no_nil { T, Error }

error :: proc(args: ..any) -> Error {
    return Error(fmt.tprint(args))
}

unwrap_or :: proc(res: Result($T), default: T) -> (T, bool) #optional_ok {
    switch t in res {
        case T: return res.(T), true
        case Error: return default, false
    }
    unreachable()
}

unwrap_or_else :: proc(res: Result($T), op: proc(err: Error) -> T) -> (T, bool) #optional_ok {
    switch t in res {
        case T: return res.(T), true
        case Error: return op(res.(Error)), false
    }
    unreachable()
}

unwrap :: proc(res: Result($T)) -> T {
    switch t in res {
        case T: return res.(T)
        case Error: panic(res.(Error).(string))
    }
    unreachable()
}

try :: proc(res: Result($T)) -> (T, Error) {
    switch t in res {
        case T: return res.(T), nil
        case Error: return T{}, res.(Error)
    }
    unreachable()
}
