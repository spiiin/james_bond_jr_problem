#include <iostream>
#include <fstream>

#include "../Zmeya.h"

using Matrix = std::array<char, 16>;
using PackedMatrix = uint32_t;
struct HashMapRoot
{
    zm::HashMap<PackedMatrix, uint32_t> map;
};

PackedMatrix packMatrix(const Matrix& matrix) {
    PackedMatrix packed = 0;

    for (size_t i = 0; i < 16; ++i) {
        uint32_t bits = (static_cast<uint8_t>(matrix[i]) & 0b11);
        packed |= (bits << (i * 2));
    }

    return packed;
}

std::vector<char> readFromFile(const std::string& filename) {
    std::ifstream file(filename, std::ios::binary | std::ios::ate);
    std::streamsize size = file.tellg();
    file.seekg(0, std::ios::beg);
    std::vector<char> buffer(size);
    if (!file.read(buffer.data(), size)) {
        throw std::runtime_error("Error reading file");
    }
    return buffer;
}

int main()
{
    auto buffer = readFromFile("../permutations_indexes_1122223333444444.zm");
    const HashMapRoot* rootCopy = (const HashMapRoot*)(buffer.data());
    Matrix ar{ 4, 3, 4, 2, 3, 1, 2, 4, 4, 2, 4, 3, 2, 4, 3, 1 };
    auto value = rootCopy->map.find(packMatrix(ar));
    std::cout << *value <<'\n';
}
