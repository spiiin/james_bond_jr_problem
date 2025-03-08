// cpp_test_zmeya.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include <algorithm>
#include <array>
#include <unordered_map>
#include <memory>
#include <fstream>

#define ZMEYA_ENABLE_SERIALIZE_SUPPORT
#include "Zmeya.h"

using Matrix = std::array<char, 16>;
using PackedMatrix = uint32_t;

PackedMatrix packMatrix(const Matrix& matrix) {
    PackedMatrix packed = 0;

    for (size_t i = 0; i < 16; ++i) {
        uint32_t bits = (static_cast<uint8_t>(matrix[i]) & 0b11);
        packed |= (bits << (i * 2));
    }

    return packed;
}

Matrix unpackMatrix(PackedMatrix packed) {
    Matrix matrix = {};

    for (size_t i = 0; i < 16; ++i) {
        matrix[i] = static_cast<char>((packed >> (i * 2)) & 0b11);
    }

    return matrix;
}

struct HashMapRoot
{
    zm::HashMap<PackedMatrix, uint32_t> map;
};

int main()
{
    std::cout << "--- generate\n";
    Matrix ar{1,1, 2,2,2,2, 3,3,3,3, 4,4,4,4,4,4};
    std::unordered_map<PackedMatrix, uint32_t> permutationIndexes;
    int i = 0;
    do
    {
        permutationIndexes[packMatrix(ar)] = i++;
        if (i % 50000 == 0) {
            std::cout << i << '\n';
        }
    } while (std::next_permutation(ar.begin(), ar.end()));
    std::cout << i << '\n';

    std::cout << "--- serialize\n";
    std::shared_ptr<zm::BlobBuilder> blobBuilder = zm::BlobBuilder::create(1);
    zm::BlobPtr<HashMapRoot> root = blobBuilder->allocate<HashMapRoot>();
    blobBuilder->copyTo(root->map, permutationIndexes);
    zm::Span<char> bytes = blobBuilder->finalize();
    
    std::cout << "--- save\n";
    std::ofstream file("permutations_indexes_1122223333444444.zm", std::ios::binary);
    file.write(bytes.data, bytes.size);
    std::cout << "--- done\n";
}
