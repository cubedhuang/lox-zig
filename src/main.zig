const std = @import("std");
const lox = @import("root.zig");
const VM = lox.vm.VM;

const LoxError = error{ArgumentError};

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const check = gpa.deinit();
        std.debug.assert(check == .ok);
    }

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    switch (args.len) {
        1 => {
            try repl(allocator);
        },
        2 => {
            try runFile(allocator, args[1]);
        },
        else => {
            _ = try std.fs.File.stderr().write("Usage: lox [usage]\n");
            return LoxError.ArgumentError;
        },
    }
}

fn repl(allocator: std.mem.Allocator) !void {
    var vm = try VM.init(allocator);
    defer vm.deinit();

    var buffer: [256]u8 = undefined;

    while (true) {
        _ = try std.fs.File.stdout().write("> ");

        const len = try std.fs.File.stdin().read(&buffer);
        if (len == buffer.len) {
            std.debug.print("Input too long", .{});
            continue;
        }
        const line = buffer[0..len];

        vm.interpret(line) catch |err| {
            std.debug.print("{any}\n", .{err});
        };
    }
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    const source = try readFile(allocator, path);
    defer allocator.free(source);

    var vm = try VM.init(allocator);
    defer vm.deinit();

    vm.interpret(source) catch |err| {
        std.debug.print("{any}\n", .{err});
    };
}

fn readFile(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const length = try file.getEndPos();

    const buffer = try allocator.alloc(u8, length);
    var reader = file.reader(buffer);

    try reader.interface.readSliceAll(buffer);

    return buffer;
}
