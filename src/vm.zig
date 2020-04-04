const std = @import("std");
const Allocator = std.mem.Allocator;
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const printValue = @import("./value.zig").printValue;

const verbose = true;

pub const InterpretResult = enum {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const VM = struct {
    chunk: *Chunk,
    ip: usize, // NOTE, book uses a byte pointer for this

    pub fn init(allocator: *Allocator) VM {
        return VM{
            .chunk = undefined,
            .ip = undefined,
        };
    }

    pub fn deinit(self: *VM) void {}

    pub fn interpret(self: *VM, chunk: *Chunk) InterpretResult {
        self.chunk = chunk;
        self.ip = 0;
        return self.run();
    }

    fn run(self: *VM) InterpretResult {
        var out = InterpretResult.RUNTIME_ERROR;
        blk: while (true) {
            if (verbose) {
                // Print debugging information
                const offset = self.chunk.disassembleInstruction(self.ip);
            }

            const instruction = self.readByte();

            switch (@intToEnum(OpCode, instruction)) {
                .OP_RETURN => {
                    out = InterpretResult.OK;
                    break :blk;
                },
                .OP_CONSTANT => {
                    // TODO, abstract out reading a byte
                    const constant = self.readByte();
                    const value = self.chunk.constants.at(constant);
                    printValue(value);
                    std.debug.warn("\n");
                },
            }
        }
        return out;
    }

    fn readByte(self: *VM) u8 {
        const byte = self.chunk.code.at(self.ip);
        self.ip += 1;
        return byte;
    }
};
