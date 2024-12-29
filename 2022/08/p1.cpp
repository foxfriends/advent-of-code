#include <iostream>
#include <algorithm>
#include <string>
#include <vector>
#include <climits>

int main() {
    std::vector<std::vector<int>> grid;
    std::string input;
    while (std::cin >> input) {
        std::vector<int> row(input.length());
        std::transform(input.begin(), input.end(), row.begin(), [] (unsigned char ch) { return ch - '0'; });
        grid.emplace_back(row);
    }

    std::vector<std::vector<int>> visible_heights(grid.size(), std::vector<int>(grid[0].size(), INT_MAX));
    for (int i = 0; i < grid.size(); ++i) {
        int current_height = 0;
        for (int j = 0; j < grid.size(); ++j) {
            visible_heights[i][j] = std::min(current_height, visible_heights[i][j]);
            current_height = std::max(grid[i][j] + 1, current_height);
        }

        current_height = 0;
        for (int j = grid[i].size() - 1; j >= 0; --j) {
            visible_heights[i][j] = std::min(current_height, visible_heights[i][j]);
            current_height = std::max(grid[i][j] + 1, current_height);
        }
    }

    for (int j = 0; j < grid[0].size(); ++j) {
        int current_height = 0;
        for (int i = 0; i < grid.size(); ++i) {
            visible_heights[i][j] = std::min(current_height, visible_heights[i][j]);
            current_height = std::max(grid[i][j] + 1, current_height);
        }

        current_height = 0;
        for (int i = grid.size() - 1; i >= 0; --i) {
            visible_heights[i][j] = std::min(current_height, visible_heights[i][j]);
            current_height = std::max(grid[i][j] + 1, current_height);
        }
    }

    int visible = 0;
    for (int i = 0; i < grid.size(); ++i) {
        for (int j = 0; j < grid[i].size(); ++j) {
            bool is_visible = grid[i][j] >= visible_heights[i][j];
            if (is_visible) { ++visible; }
        }
    }
    std::cout << visible << std::endl;
}
