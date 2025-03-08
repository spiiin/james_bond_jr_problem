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

//#include "flat_hash_map.hpp"
#include <boost/unordered/concurrent_flat_map.hpp>

using Matrix = std::array<char, 16>;

struct MatrixHash {
    uint32_t operator()(const Matrix& m) const {
        uint32_t res = 0;
        for (int e : m) {
            res += e - 1;
            res <<= 2;
        }
        return res;
    }
};

struct Node {
    Matrix matrix;
    uint8_t rate;
    uint8_t path_length;

    bool operator>(const Node& other) const {
        if (path_length == other.path_length) {
            return rate > other.rate;
        }
        return path_length > other.path_length;
    }
};

//using ClosedList = ska::flat_hash_map<Matrix, Matrix, MatrixHash>;
//using OpenHash = ska::flat_hash_map<size_t, uint8_t>;
using ClosedList = boost::unordered::concurrent_flat_map<Matrix, Matrix, MatrixHash>;
using OpenHash = boost::unordered::concurrent_flat_map<size_t, uint8_t>;
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


std::vector<Matrix> extractPath(const ClosedList& closed, const Matrix& target) {
    Matrix last = target;
    std::vector<Matrix> result = { target };

    closed.cvisit(last, [&](const auto& entry) {
        Matrix current = entry.second;
        while (true) {
            result.push_back(current);
            if (!closed.cvisit(current, [&](const auto& e) { current = e.second; })) {
                break;
            }
        }
    });
    std::reverse(result.begin(), result.end());
    return result;
}

static ClosedList search(const Matrix& source, const Matrix& target) {
    static auto fun = generateFunctions();
    OpenList open;
    OpenHash open_hash = { {MatrixHash{}(source), 0} };
    ClosedList closed;

    open_hash.reserve(1u << 24u);

    open.push({ source, rate(source, target), 0 });

    int step = 0;
    while (!open.empty()) {
        auto head = open.top();
        open.pop();
        
        if (head.matrix == target) {
            std::cout << "head:" << int(head.rate) << " " << int(head.path_length) << "\n";
            return closed;
        }

        if (++step % 50000 == 0) {
            std::cout << "head:" << int(head.rate) << " " << int(head.path_length) << "\n";
        }

        std::vector<std::optional<Node>> temp_nodes(fun.size());
        std::transform(fun.begin(), fun.end(), temp_nodes.begin(), [&](const auto& entry) -> std::optional<Node> {
            const auto& [f, line] = entry;

            Matrix val = f(head.matrix, line);
            uint8_t new_path_length = head.path_length + 1;
            size_t val_hash = MatrixHash{}(val);
            if (!open_hash.emplace(val_hash, new_path_length)) {
                return std::nullopt;
            }
            closed.emplace(val, head.matrix);

            return Node{ val, rate(val, target), new_path_length };
        });


        for (auto& opt_node : temp_nodes) {
            if (opt_node) {
                open.push(std::move(*opt_node));
            }
        }
    }
    return {};
}

int main() {

    //Matrix source = { 11, 12, 13, 14, 21, 22, 23, 24, 31, 32, 33, 34, 41, 42, 43, 44 };
    Matrix source = { 1, 2, 2, 1, 3, 4, 4, 3, 3, 4, 4, 3, 2, 4, 4, 2 };
    Matrix target = { 4, 3, 4, 2, 3, 1, 2, 4, 4, 2, 4, 3, 2, 4, 3, 1 };

    auto start = std::chrono::high_resolution_clock::now();
    auto closed = search(source, target);
    auto path = extractPath(closed, target);
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> duration = end - start;
    std::cout << "Elapsed: " << duration.count() << " sec" << std::endl;

    std::cout << "Steps: " << path.size() - 1 << std::endl;
    for (const auto& state : path) {
        for (size_t i = 0; i < state.size(); ++i) {
            std::cout << int(state[i]) << (i % 4 == 3 ? "\n" : " ");
        }
        std::cout << "\n";
    }

    return 0;
}
