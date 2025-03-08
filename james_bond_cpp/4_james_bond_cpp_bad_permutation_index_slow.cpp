#include <iostream>
#include <vector>
#include <deque>
#include <array>
#include <map>
#include <unordered_set>
#include <unordered_map>
#include <algorithm>
#include <queue>
#include <functional>
#include <chrono>

#include "flat_hash_map.hpp"

using Matrix = std::array<char, 16>;
using PackedMatrix = uint32_t;

//Matrix source = { 11, 12, 13, 14, 21, 22, 23, 24, 31, 32, 33, 34, 41, 42, 43, 44 };
//constexpr Matrix source = { 1, 2, 2, 1, 3, 4, 4, 3, 3, 4, 4, 3, 2, 4, 4, 2 };
constexpr Matrix source   = { 0, 1, 1, 0, 2, 3, 3, 2, 2, 3, 3, 2, 1, 3, 3, 1 };
//constexpr Matrix target = { 4, 3, 4, 2, 3, 1, 2, 4, 4, 2, 4, 3, 2, 4, 3, 1 };
constexpr Matrix target   = { 3, 2, 3, 1, 2, 0, 1, 3, 3, 1, 3, 2, 1, 3, 2, 0 };

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

constexpr unsigned long long factorial[] = { 1ULL, 1ULL, 2ULL, 6ULL, 24ULL, 120ULL, 720ULL, 5040ULL, 40320ULL, 362880ULL, 3628800ULL, 39916800ULL, 479001600ULL, 6227020800ULL, 87178291200ULL, 1307674368000ULL, 20922789888000ULL };

constexpr unsigned long long permutationsCount(const Matrix& arr) {
    std::array<int, 4> counter{};
    for (int num : arr) {
        counter[num]++;
    }

    unsigned long long result = factorial[arr.size()];
    for (int freq : counter) {
        if (freq > 1) {
            result /= factorial[freq];
        }
    }
    return result;
}

constexpr PackedMatrix permutationIndex(const Matrix& arr) {
    unsigned long long rank = 0;
    std::array<char, 4> freqs{ 2, 4, 4, 6 };

    /*for (int num : arr) {
        freqs[num]++;
    }*/

    for (std::size_t n = 0; n < arr.size(); ++n) {
        unsigned long long fsum = 0;
        int arrn = arr[n];
        for (int i = 0; i < arrn; ++i) {
            fsum += freqs[i];
        }

        unsigned long long fprod = 1;
        for (int freqIndex = 0u; freqIndex < freqs.size(); freqIndex++) {
            int freq = freqs[freqIndex];
            if (freq > 1) {
                fprod *= factorial[freq];
            }
        }

        rank += (fsum * factorial[arr.size() - n - 1]) / fprod;

        --freqs[arrn];
    }

    return static_cast<PackedMatrix>(rank);
}

//using ClosedList = ska::flat_hash_map<Matrix, Matrix, MatrixHash>;
constexpr auto perm_count = permutationsCount(source);
using ClosedList = std::array<uint32_t, perm_count>;
using OpenHash = std::array<uint8_t, perm_count>;
using OpenList = std::priority_queue<Node, std::vector<Node>, std::greater<Node>>;

static OpenHash open_hash;
static ClosedList closed;

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
    //while (closed.find(last) != closed.end()) {
    //    last = closed.at(last);
    //    result.push_back(last);
    //}
    result.pop_back();
    std::reverse(result.begin(), result.end());
    return result;
}

void search(const Matrix& source, const Matrix& target, ClosedList& closed) {
    static auto fun = generateFunctions();
    OpenList open;
    open_hash[permutationIndex(source)] = 0;

    open.push({ source, rate(source, target), 0 });

    int step = 0;
    while (!open.empty()) {
        auto head = open.top();
        open.pop();
        
        if (head.matrix == target) {
            return;
        }

        if (++step % 50000 == 0) {
            std::cout << "head:" << int(head.rate) << " " << int(head.path_length) << "\n";
        }

        for (uint16_t funcIdx = 0; const auto& [f, line] : fun) {

            Matrix val = f(head.matrix, line);
            uint8_t new_path_length = head.path_length + 1;
            size_t val_hash = permutationIndex(val);

            auto path_len = open_hash[val_hash];
            if (path_len == 0u) {
                open.push({ val, rate(val, target), new_path_length });
                closed[val_hash] = permutationIndex(head.matrix);
            }
            else {
                //replace element if it path shorter
                if (new_path_length < path_len) {
                    open_hash[val_hash] = new_path_length;
                    closed[val_hash] = permutationIndex(head.matrix);
                }
            }
        }
    }
    return;
}

int main() {
    auto start = std::chrono::high_resolution_clock::now();
    search(source, target, closed);
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
