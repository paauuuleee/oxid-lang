package main

import "core:os"
import str "core:strings"
import "core:fmt"

Package :: struct {
    name: string,
    version: u32,
    modules: map[string]Module,
    dependecies: map[string]Dependency
}

Dependency :: struct {
    name: string,
    fetch_path: string,
    pack: Package,
}

Module :: struct {
    name: string,
    ast: []Node
}

read_package :: proc() -> Package {
    if fd, errno := os.open("myproj/main.oxid", 0); errno != 0 {
        fmt.panicf("error: cannot find entry point with errno: %d", errno)
    } else { os.close(fd) }

    fd, errno := os.open("oxid.mod", os.O_RDONLY);
    if errno != 0 { fmt.panicf("error: cannot find mod file with errno: %d", errno) }
    defer os.close(fd)

    size, seek_err := os.file_size(fd)
    if seek_err != 0 { fmt.panicf("error: cannot determine file size with errno: %d", seek_err) }

    buf := make([]u8, size)
    _, read_err := os.read(fd, buf)
    if read_err != 0 { fmt.panicf("error: cannot read mod file with errno: %d", read_err) }

    content := str.clone_from_bytes(buf)
    fmt.println(content)

    return Package {
        name = "myproj",
        version = 0x000100,
        modules = nil,
        dependecies = nil
    }
}

init_package :: proc(pack_name: string) {
    if errno := os.make_directory(pack_name); errno != 0 {
        fmt.eprintln("error: cannot create package directory with errno: ", errno)
        return
    }

    main_fd, main_open_err := os.open(str.concatenate({pack_name, "/main.oxid"}), os.O_CREATE | os.O_RDWR, 0o0644);
    if main_open_err != 0 {
        fmt.eprintln("error: cannot open file with errno: ", main_open_err)
        return
    }
    defer os.close(main_fd)

    main_content := fmt.tprint("mod main\n\nfn main() {}\n")

    _, main_write_err := os.write(main_fd, transmute([]u8)main_content)
    if main_write_err != 0 { 
        fmt.eprintln("error: cannot write to file with errno: ", main_write_err)
        return
    }

    mod_fd, mod_open_err := os.open("oxid.mod", os.O_CREATE | os.O_RDWR, 0o0644);
    if mod_open_err != 0 {
        fmt.eprintln("error: cannot open file with errno: ", mod_open_err)
        return
    }
    defer os.close(mod_fd)

    mod_content := fmt.tprint("package ", pack_name, "\nversion 0.1.0\nmodules [main]\ndependencies []\n")

    _, mod_write_err := os.write(mod_fd, transmute([]u8)mod_content)
    if mod_write_err != 0 { 
        fmt.eprintln("error: cannot write to file with errno: ", mod_write_err)
        return
    }
}