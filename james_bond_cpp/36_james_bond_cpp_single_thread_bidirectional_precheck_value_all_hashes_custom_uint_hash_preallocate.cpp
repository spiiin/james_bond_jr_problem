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

using Matrix = std::array<char, 16>;
using MatrixHashT = uint32_t;

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

struct MurMurHasher {
    MatrixHashT operator() (MatrixHashT value) const {
        value ^= value >> 15;
        value *= 0x85ebca6b;
        value ^= value >> 13;
        value *= 0xc2b2ae35;
        value ^= value >> 16;
        return value;
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
    MatrixHashT matrix;
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

using ClosedList = ska::flat_hash_map<MatrixHashT, MatrixHashT, MurMurHasher>;
using OpenHash = ska::flat_hash_set<MatrixHashT, MurMurHasher>;
using OpenList = std::priority_queue<Node, std::vector<Node>, std::greater<Node>>;

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

std::vector<MatrixHashT> bidirectionalSearch(const Matrix& source, const Matrix& target) {
    static auto fun = generateFunctions();
    static auto source_hash = MatrixHash{}(source);
    static auto target_hash = MatrixHash{}(target);

    const size_t BUCKETS_COUNT = 2 << 14;
    OpenList openForward, openBackward;
    OpenHash openHashForward({ source_hash }, BUCKETS_COUNT);
    OpenHash openHashBackward({ target_hash }, BUCKETS_COUNT);

    ClosedList closedForward(BUCKETS_COUNT), closedBackward(BUCKETS_COUNT);

    openForward.push({ source_hash, rate(source_hash, target_hash), 0 });
    openBackward.push({ target_hash, rate(target_hash, source_hash), 0 });

    int step = 0;

    while (!openForward.empty() && !openBackward.empty()) {
        auto expand = [&](OpenList& open, OpenHash& openHash, ClosedList& closed, ClosedList& oppositeClosed) -> std::optional<MatrixHashT> {
            auto head = open.top();
            open.pop();

            //if (++step % 5000 == 0) {
            //    std::cout << "step:" << step << " head:" << int(head.rate) << " " << int(head.path_length) << "\n";
            //}

            for (const auto& [f, line] : fun) {
                MatrixHashT valHash = f(head.matrix, line);
                uint8_t newPathLength = head.path_length + 1;
                if (auto it = oppositeClosed.find(valHash); it != oppositeClosed.end()) {
                    closed[valHash] = head.matrix;
                    return valHash;
                }

                if (auto it = openHash.emplace(valHash); it.second) {
                    open.push({ valHash, rate(valHash, target_hash), newPathLength });
                    closed[valHash] = head.matrix;
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
