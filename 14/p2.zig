const std = @import("std");
const input = @embedFile("input");

const W = 1200;
const H = 200;
const O = 0;
const Point = struct {
    x: i32,
    y: i32,

    pub fn below(self: Point) Point {
        return Point { .x = self.x, .y = self.y + 1 };
    }
    pub fn belowLeft(self: Point) Point {
        return Point { .x = self.x - 1, .y = self.y + 1 };
    }
    pub fn belowRight(self: Point) Point {
        return Point { .x = self.x + 1, .y = self.y + 1 };
    }
};
const INPUT = Point { .x = 500 - O, .y = 0 };

fn signum(n: i32) i32 {
    if (n > 0) return 1;
    if (n < 0) return -1;
    return 0;
}

fn simulate(grid: *[H][W]bool, position: Point) void {
    if (grid[@intCast(usize, position.y)][@intCast(usize, position.x)]) return;
    simulate(grid, position.below());
    simulate(grid, position.belowLeft());
    simulate(grid, position.belowRight());
    grid[@intCast(usize, position.y)][@intCast(usize, position.x)] = true;
}

fn countFilled(grid: [H][W]bool) usize {
    var filled: usize = 0;
    for (grid) |row| {
        for (row) |col| {
            if (col) { filled += 1; }
        }
    }
    return filled;
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var lines = std.mem.split(u8, input, "\n");

    var grid: [H][W]bool = .{};
    var maxY: usize = 0;

    while (lines.next()) |line| {
        if (line.len == 0) break;
        var points = std.mem.split(u8, line, " -> ");
        var prev: ?Point = null;
        while (points.next()) |pointstr| {
            var coords = std.mem.split(u8, pointstr, ",");
            const point = Point {
                .x = try std.fmt.parseInt(i32, coords.next().?, 10) - O,
                .y = try std.fmt.parseInt(i32, coords.next().?, 10),
            };
            if (prev == null) {
                prev = point;
                grid[@intCast(usize, prev.?.y)][@intCast(usize, prev.?.x)] = true;
            }
            maxY = std.math.max(maxY, @intCast(usize, point.y));
            const dx = signum(point.x - prev.?.x);
            const dy = signum(point.y - prev.?.y);
            while (!std.meta.eql(prev, point)) {
                prev.?.x += dx;
                prev.?.y += dy;
                grid[@intCast(usize, prev.?.y)][@intCast(usize, prev.?.x)] = true;
            }
        }
    }

    for ([_]u0{0} ** W) |_, i| {
        grid[maxY + 2][i] = true;
    }

    const blocks = countFilled(grid);
    simulate(&grid, INPUT);
    const sand = countFilled(grid) - blocks;
    try stdout.print("{d}\n", .{sand});
}
