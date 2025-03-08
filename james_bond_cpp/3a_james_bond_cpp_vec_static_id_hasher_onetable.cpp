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
#include <immintrin.h>

#include "flat_hash_map.hpp"
#include <cassert>

using Matrix = std::array<char, 16>;
using MatrixHashT = uint32_t;
const size_t BUCKETS_COUNT = 2 << 14;

struct MatrixHash {
    MatrixHashT operator()(const Matrix& m) const {
        MatrixHashT res = 0;
        for (int e : m) {
            res <<= 2;
            res |= ((e - 1) & 3);
        }
        return res;
    }
};

struct IdHasher {
    MatrixHashT operator() (MatrixHashT value) const { return value; }
};

Matrix decodeMatrixFromHash(MatrixHashT hash) {
    Matrix m{};
    for (int i = 15; i >= 0; --i) {
        m[i] = (hash & 3) + 1;
        hash >>= 2;
    }
    return m;
}

struct QueueVector {
    std::vector<MatrixHashT> vec = std::vector<MatrixHashT>(BUCKETS_COUNT);
    mutable size_t open_pos = 0u;
    mutable size_t last_pos = 0u;

    inline MatrixHashT pop() const { return vec[open_pos++]; }
    inline void push(MatrixHashT e) { vec[last_pos++] = e; }
    inline bool empty() const { return !vec[open_pos]; }
};

using ClosedList = ska::flat_hash_map<MatrixHashT, MatrixHashT, IdHasher>;
using OpenHash = ska::flat_hash_set<MatrixHashT, IdHasher>;
using OpenList = QueueVector;

static uint8_t rate(MatrixHashT v, MatrixHashT target) {
        MatrixHashT diff = v ^ target;
        uint32_t mask = 0x55555555;
        diff = ~diff & mask;
        return 16 - _mm_popcnt_u32(diff) / 2;
}

MatrixHashT Right(MatrixHashT hash, int line) {
    int shift = line * 8;
    MatrixHashT row = (hash >> shift) & 0xFF;
    row = ((row & 0b00000011) << 6) | ((row & 0b11111100) >> 2);
    return (hash & ~(0xFF << shift)) | (row << shift);
}

MatrixHashT Left(MatrixHashT hash, int line) {
    int shift = line * 8;
    MatrixHashT row = (hash >> shift) & 0xFF;
    row = ((row & 0b11000000) >> 6) | ((row & 0b00111111) << 2);
    return (hash & ~(0xFF << shift)) | (row << shift);
}

MatrixHashT Up(MatrixHashT hash, int col) {
    MatrixHashT mask = 0b11;
    MatrixHashT col0 = (hash >> (col * 2)) & mask;
    MatrixHashT col1 = (hash >> (8 + col * 2)) & mask;
    MatrixHashT col2 = (hash >> (16 + col * 2)) & mask;
    MatrixHashT col3 = (hash >> (24 + col * 2)) & mask;

    MatrixHashT newHash = hash & ~(mask << (col * 2)) & ~(mask << (8 + col * 2)) &
        ~(mask << (16 + col * 2)) & ~(mask << (24 + col * 2));

    newHash |= (col1 << (col * 2));
    newHash |= (col2 << (8 + col * 2));
    newHash |= (col3 << (16 + col * 2));
    newHash |= (col0 << (24 + col * 2));

    return newHash;
}

MatrixHashT Down(MatrixHashT hash, int col) {
    MatrixHashT mask = 0b11;
    MatrixHashT col0 = (hash >> (col * 2)) & mask;
    MatrixHashT col1 = (hash >> (8 + col * 2)) & mask;
    MatrixHashT col2 = (hash >> (16 + col * 2)) & mask;
    MatrixHashT col3 = (hash >> (24 + col * 2)) & mask;

    MatrixHashT newHash = hash & ~(mask << (col * 2)) & ~(mask << (8 + col * 2)) &
        ~(mask << (16 + col * 2)) & ~(mask << (24 + col * 2));

    newHash |= (col3 << (col * 2));
    newHash |= (col0 << (8 + col * 2));
    newHash |= (col1 << (16 + col * 2));
    newHash |= (col2 << (24 + col * 2));

    return newHash;
}


using TransformFunc = MatrixHashT(*)(MatrixHashT, int);

auto generateFunctions() {
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

static auto fun = generateFunctions();

static OpenList openForward, openBackward;
static ClosedList closedForward(BUCKETS_COUNT), closedBackward(BUCKETS_COUNT);

std::vector<MatrixHashT> bidirectionalSearch(const Matrix& source, const Matrix& target) {
    static auto source_hash = MatrixHash{}(source);
    static auto target_hash = MatrixHash{}(target);
    openForward.push(source_hash);
    openBackward.push(target_hash);
    closedForward[source_hash] = 0; //sentinel value
    closedBackward[target_hash] = 0;
    while (!openForward.empty() && !openBackward.empty()) {
        auto expand = [&](OpenList& open, ClosedList& closed, ClosedList& oppositeClosed) -> std::optional<MatrixHashT> {
            auto head = open.pop();

            for (const auto& [f, line] : fun) {
                MatrixHashT valHash = f(head, line);
                if (auto it = oppositeClosed.find(valHash); it != oppositeClosed.end()) {
                    closed[valHash] = head;
                    return valHash;
                }
                if (auto it = closed.emplace(valHash, head); it.second) {
                    open.push(valHash);
                }
            }
            return std::nullopt;
        };

        if (auto meetingPoint = expand(openForward, closedForward, closedBackward)) {
            return extractPath(closedForward, closedBackward, *meetingPoint);
        }

        if (auto meetingPoint = expand(openBackward, closedBackward, closedForward)) {
            return extractPath(closedForward, closedBackward, *meetingPoint);
        }
    }

    return {};
}

int main() {

    //Matrix source = { 11, 12, 13, 14, 21, 22, 23, 24, 31, 32, 33, 34, 41, 42, 43, 44 };
    Matrix source = { 1, 2, 2, 1, 3, 4, 4, 3, 3, 4, 4, 3, 2, 4, 4, 2 };
    Matrix target = { 4, 3, 4, 2, 3, 1, 2, 4, 4, 2, 4, 3, 2, 4, 3, 1 };

    //std::cout << "TTT: " << std::bitset<sizeof(MatrixHashT) * 8>(MatrixHash{}(source)) << "\n";

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
