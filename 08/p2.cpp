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

    std::vector<std::vector<int>> scores(grid.size(), std::vector<int>(grid[0].size(), 1));
    for (int i = 0; i < grid.size(); ++i) {
        int height_positions[10] = {0};
        for (int j = 0; j < grid.size(); ++j) {
            int height = grid[i][j];
            scores[i][j] *= j - height_positions[height];
            for (int k = 0; k <= height; ++k) {
                height_positions[k] = j;
            }
        }

        for (int k = 0; k < 10; ++k) height_positions[k] = grid[k].size() - 1;
        for (int j = grid[i].size() - 1; j >= 0; --j) {
            int height = grid[i][j];
            scores[i][j] *= height_positions[height] - j;
            for (int k = 0; k <= height; ++k) {
                height_positions[k] = j;
            }
        }
    }

    for (int j = 0; j < grid[0].size(); ++j) {
        int height_positions[10] = {0};
        for (int i = 0; i < grid.size(); ++i) {
            int height = grid[i][j];
            scores[i][j] *= i - height_positions[height];
            for (int k = 0; k <= height; ++k) {
                height_positions[k] = i;
            }
        }

        for (int k = 0; k < 10; ++k) height_positions[k] = grid.size() - 1;
        for (int i = grid.size() - 1; i >= 0; --i) {
            int height = grid[i][j];
            scores[i][j] *= height_positions[height] - i;
            for (int k = 0; k <= height; ++k) {
                height_positions[k] = i;
            }
        }
    }

    int max_score = 0;
    for (int i = 0; i < scores.size(); ++i) {
        for (int j = 0; j < scores[i].size(); ++j) {
            max_score = std::max(scores[i][j], max_score);
        }
    }
    std::cout << max_score << std::endl;
}
