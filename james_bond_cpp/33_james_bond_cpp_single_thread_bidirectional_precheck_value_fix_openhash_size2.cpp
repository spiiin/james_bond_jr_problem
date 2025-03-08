#include <iostream>
#include <vector>
#include <deque>
#include <array>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <queue>
#include <functional>
#include <chrono>
#include <bitset>

#include "flat_hash_map.hpp"

using Matrix = std::array<char, 16>;
using MatrixHashT = uint32_t;

struct MatrixHash {
    MatrixHashT operator()(const Matrix& m) const {
        MatrixHashT res = 0;
        for (int e : m) {
            res <<= 2;
            res |= ((e-1) & 3);
        }
        return res;
    }
};

Matrix decodeMatrixFromHash(MatrixHashT hash) {
    Matrix m{};
    for (int i = 15; i >= 0; --i) {
        m[i] = (hash & 3) + 1;
        hash >>= 2;
    }
    return m;
}

struct Node {
    Matrix matrix;
    uint8_t rate;
    uint8_t path_length;

    bool operator>(const Node& other) const {
        //return rate > other.rate;

        if (path_length == other.path_length) {
            return rate > other.rate;
        }
        return path_length > other.path_length;
    }
};

using ClosedList = ska::flat_hash_map<MatrixHashT, MatrixHashT>;
using OpenHash = ska::flat_hash_set<MatrixHashT>;
using OpenList = std::priority_queue<Node, std::vector<Node>, std::greater<Node>>;

static uint8_t rate(const Matrix& v, const Matrix& target) {
    uint8_t res = 0;
    for (size_t i = 0; i < v.size(); ++i) {
        if (v[i] == target[i]) {
            ++res;
        }
    }
    return uint8_t(16) - res;
}

Matrix Right(const Matrix& m, int line) {
    Matrix n = m;
    int plus = line * 4;
    n[0 + plus] = m[3 + plus];
    n[1 + plus] = m[0 + plus];
    n[2 + plus] = m[1 + plus];
    n[3 + plus] = m[2 + plus];
    return n;
}

Matrix Left(const Matrix& m, int line) {
    Matrix n = m;
    int plus = line * 4;
    n[0 + plus] = m[1 + plus];
    n[1 + plus] = m[2 + plus];
    n[2 + plus] = m[3 + plus];
    n[3 + plus] = m[0 + plus];
    return n;
}

Matrix Up(const Matrix& m, int line) {
    Matrix n = m;
    n[0 + line] = m[4 + line];
    n[4 + line] = m[8 + line];
    n[8 + line] = m[12 + line];
    n[12 + line] = m[0 + line];
    return n;
}

Matrix Down(const Matrix& m, int line) {
    Matrix n = m;
    n[0 + line] = m[12 + line];
    n[4 + line] = m[0 + line];
    n[8 + line] = m[4 + line];
    n[12 + line] = m[8 + line];
    return n;
}

using TransformFunc = Matrix(*)(const Matrix&, int);

std::vector<std::pair<TransformFunc, int>> generateFunctions() {
    std::vector<std::pair<TransformFunc, int>> fun;
    for (int i = 0; i < 4; ++i) {
        fun.emplace_back(Left, i);
        fun.emplace_back(Right, i);
        fun.emplace_back(Up, i);
        fun.emplace_back(Down, i);
    }
    return fun;
}

std::vector<MatrixHashT> extractPath(const ClosedList& closedForward, const ClosedList& closedBackward, const MatrixHashT& meetingPoint) {
    std::vector<MatrixHashT> pathForward = { meetingPoint };
    MatrixHashT current = meetingPoint;
    while (closedForward.find(current) != closedForward.end()) {
        current = closedForward.at(current);
        pathForward.push_back(current);
    }
    std::reverse(pathForward.begin(), pathForward.end());

    std::vector<MatrixHashT> pathBackward;
    current = meetingPoint;
    while (closedBackward.find(current) != closedBackward.end()) {
        current = closedBackward.at(current);
        pathBackward.push_back(current);
    }

    pathForward.insert(pathForward.end(), pathBackward.begin(), pathBackward.end());
    return pathForward;
}

std::vector<MatrixHashT> bidirectionalSearch(const Matrix& source, const Matrix& target) {
    static auto fun = generateFunctions();

    OpenList openForward, openBackward;
    OpenHash openHashForward = { MatrixHash{}(source) };
    OpenHash openHashBackward = { MatrixHash{}(target) };

    ClosedList closedForward, closedBackward;

    openForward.push({ source, rate(source, target), 0 });
    openBackward.push({ target, rate(target, source), 0 });

    int step = 0;

    while (!openForward.empty() && !openBackward.empty()) {
        auto expand = [&](OpenList& open, OpenHash& openHash, ClosedList& closed, ClosedList& oppositeClosed) -> std::optional<MatrixHashT> {
            auto head = open.top();
            open.pop();
            auto head_hash = MatrixHash{}(head.matrix);

            //if (++step % 5000 == 0) {
            //    std::cout << "step:" << step << " head:" << int(head.rate) << " " << int(head.path_length) << "\n";
            //}

            for (const auto& [f, line] : fun) {
                Matrix val = f(head.matrix, line);
                uint8_t newPathLength = head.path_length + 1;
                auto valHash = MatrixHash{}(val);
                if (auto it = oppositeClosed.find(valHash); it != oppositeClosed.end()) {
                    closed[valHash] = head_hash;
                    return valHash;
                }

                if (auto it = openHash.emplace(valHash); it.second) {
                    open.push({ val, rate(val, target), newPathLength });
                    closed[valHash] = head_hash;
                }
            }
            return std::nullopt;
        };

        if (auto meetingPoint = expand(openForward, openHashForward, closedForward, closedBackward)) {
            return extractPath(closedForward, closedBackward, *meetingPoint);
        }

        if (auto meetingPoint = expand(openBackward, openHashBackward, closedBackward, closedForward)) {
            return extractPath(closedForward, closedBackward, *meetingPoint);
        }
    }

    return {};
}

int main() {

    //Matrix source = { 11, 12, 13, 14, 21, 22, 23, 24, 31, 32, 33, 34, 41, 42, 43, 44 };
    Matrix source = { 1, 2, 2, 1, 3, 4, 4, 3, 3, 4, 4, 3, 2, 4, 4, 2 };
    Matrix target = { 4, 3, 4, 2, 3, 1, 2, 4, 4, 2, 4, 3, 2, 4, 3, 1 };

    auto start = std::chrono::high_resolution_clock::now();
    auto path = bidirectionalSearch(source, target);
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> duration = end - start;
    std::cout << "Elapsed: " << duration.count() << " sec" << std::endl;

    std::cout << "Steps: " << path.size() - 1 << std::endl;
    for (const auto& state_hash : path) {
        auto state = decodeMatrixFromHash(state_hash);
        //std::cout << std::bitset<sizeof(MatrixHashT) * 8>(state_hash) << "\n";
        for (size_t i = 0; i < state.size(); ++i) {
            std::cout << int(state[i]) << (i % 4 == 3 ? "\n" : " ");
        }
        std::cout << "\n";
    }

    return 0;
}
