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

#include <thread>
#include <mutex>

#include "tracy/Tracy.hpp"

#include "flat_hash_map.hpp"
//#include "bytell_hash_map.hpp"
#include "sparsehash/dense_hash_map"

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
    typedef ska::fibonacci_hash_policy hash_policy;
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
    typedef ska::power_of_two_hash_policy hash_policy;
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

    inline MatrixHashT pop() const { 
        ZoneScopedN("open_pop");
        return vec[open_pos++];
    }
    inline void push(MatrixHashT e) {
        ZoneScopedN("open_push");
        vec[last_pos++] = e;
    }
    inline bool empty() const {
        ZoneScopedN("open_empty");
        return !vec[open_pos];
    }
};

using ClosedList = ska::flat_hash_map<MatrixHashT, MatrixHashT, IdHasher>;
//using ClosedList = google::dense_hash_map<MatrixHashT, MatrixHashT, MurMurHasher>;
using OpenList = QueueVector;

static uint8_t rate(MatrixHashT v, MatrixHashT target) {
        MatrixHashT diff = v ^ target;
        uint32_t mask = 0x55555555;
        diff = ~diff & mask;
        return 16 - _mm_popcnt_u32(diff) / 2;
}

std::vector<MatrixHashT> extractPath(ClosedList& closedForward, ClosedList& closedBackward, const MatrixHashT& meetingPoint) {
    ZoneScopedN("extract");
    std::vector<MatrixHashT> pathForward = { meetingPoint };
    MatrixHashT current = meetingPoint;
    while (closedForward.find(current) != closedForward.end()) {
        current = closedForward[current];
        pathForward.push_back(current);
    }
    std::reverse(pathForward.begin(), pathForward.end());

    std::vector<MatrixHashT> pathBackward;
    current = meetingPoint;
    while (closedBackward.find(current) != closedBackward.end()) {
        current = closedBackward[current];
        pathBackward.push_back(current);
    }

    pathForward.insert(pathForward.end(), pathBackward.begin(), pathBackward.end());
    return pathForward;
}

static OpenList openForward, openBackward;
static ClosedList closedForward(BUCKETS_COUNT), closedBackward(BUCKETS_COUNT);
std::thread forwardThread, backwardThread;

static std::array<MatrixHashT, 16> generateNextMoves(MatrixHashT head) {
    ZoneScopedN("generateNextMoves");
    MatrixHashT shiftedRightAll = (head >> 2u) & 0x3F3F3F3Fu | (head << 6u) & 0xC0C0C0C0u;
    MatrixHashT r0 = head & 0x00FFFFFF | shiftedRightAll & ~0x00FFFFFF;
    MatrixHashT r1 = head & 0xFF00FFFF | shiftedRightAll & ~0xFF00FFFF;
    MatrixHashT r2 = head & 0xFFFF00FF | shiftedRightAll & ~0xFFFF00FF;
    MatrixHashT r3 = head & 0xFFFFFF00 | shiftedRightAll & ~0xFFFFFF00;

    MatrixHashT shiftedLeftAll = (head << 2u) & 0xFCFCFCFCu | (head >> 6u) & 0x03030303u;
    MatrixHashT l0 = head & 0x00FFFFFF | shiftedLeftAll & ~0x00FFFFFF;
    MatrixHashT l1 = head & 0xFF00FFFF | shiftedLeftAll & ~0xFF00FFFF;
    MatrixHashT l2 = head & 0xFFFF00FF | shiftedLeftAll & ~0xFFFF00FF;
    MatrixHashT l3 = head & 0xFFFFFF00 | shiftedLeftAll & ~0xFFFFFF00;

    MatrixHashT shiftedUpAll = (head >> 8u) & 0x00FFFFFFu | (head << 24u) & 0xFF000000;
    MatrixHashT u0 = head & 0x3F3F3F3F | shiftedUpAll & ~0x3F3F3F3F;
    MatrixHashT u1 = head & 0xCFCFCFCF | shiftedUpAll & ~0xCFCFCFCF;
    MatrixHashT u2 = head & 0xF3F3F3F3 | shiftedUpAll & ~0xF3F3F3F3;
    MatrixHashT u3 = head & 0xFCFCFCFC | shiftedUpAll & ~0xFCFCFCFC;

    MatrixHashT shiftedDownAll = (head << 8u) & 0xFFFFFF00u | (head >> 24u) & 0x000000FFu;
    MatrixHashT d0 = head & 0x3F3F3F3F | shiftedDownAll & ~0x3F3F3F3F;
    MatrixHashT d1 = head & 0xCFCFCFCF | shiftedDownAll & ~0xCFCFCFCF;
    MatrixHashT d2 = head & 0xF3F3F3F3 | shiftedDownAll & ~0xF3F3F3F3;
    MatrixHashT d3 = head & 0xFCFCFCFC | shiftedDownAll & ~0xFCFCFCFC;

    return {
        l0, r0, u0, d0,
        l1, r1, u1, d1,
        l2, r2, u2, d2,
        l3, r3, u3, d3,
    };
}

std::vector<MatrixHashT> bidirectionalSearch(const Matrix& source, const Matrix& target) {
    FrameMark;
    ZoneScoped;

    static auto source_hash = MatrixHash{}(source);
    static auto target_hash = MatrixHash{}(target);

    OpenList openForward, openBackward;
    openForward.push(source_hash);
    openBackward.push(target_hash);

    ClosedList closedForward, closedBackward;
    closedForward[source_hash] = 0; // sentinel value
    closedBackward[target_hash] = 0;

    TracyLockable(std::mutex, mutexForward);
        TracyLockable(std::mutex, mutexBackward);
    std::atomic<bool> found = false;
    std::optional<MatrixHashT> meetingPoint;

    auto expand = [&](OpenList& open, ClosedList& closed, ClosedList& oppositeClosed, tracy::Lockable<std::mutex>& mutexClosed, tracy::Lockable<std::mutex>& mutexOpposite) -> void {
        ZoneScopedN("expand");
        while (!open.empty() && !found) {
            auto head = open.pop();
            for (auto valHash : generateNextMoves(head)) {
                bool foundInOpposite = false;
                {
                    ZoneScopedN("lock_opposite");
                    std::lock_guard<LockableBase(std::mutex)> lockOpposite((mutexOpposite));
                    foundInOpposite = oppositeClosed.find(valHash) != oppositeClosed.end();
                }
                if (foundInOpposite) {
                    {
                        std::lock_guard<LockableBase(std::mutex)> lockClosed(mutexClosed);
                        closed[valHash] = head;
                    }
                    meetingPoint = valHash;
                    found = true;
                    return;
                }

                {
                    bool emplaced = false;
                    {
                        ZoneScopedN("lock_self");
                        std::lock_guard<LockableBase(std::mutex)> lockClosed(mutexClosed);
                        emplaced = closed.emplace(valHash, head).second;
                    }
                    if (emplaced) {
                        open.push(valHash);
                    }
                }
            }
        }
    };

    forwardThread = std::thread(expand, std::ref(openForward), std::ref(closedForward), std::ref(closedBackward), std::ref(mutexForward), std::ref(mutexBackward));
    backwardThread = std::thread(expand, std::ref(openBackward), std::ref(closedBackward), std::ref(closedForward), std::ref(mutexBackward), std::ref(mutexForward));

    forwardThread.join();
    backwardThread.join();

    if (meetingPoint) {
        return extractPath(closedForward, closedBackward, *meetingPoint);
    }

    return {};
}

int main() {

#ifdef TRACY_ENABLE
    while (!TracyIsConnected) {
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }
#endif

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
