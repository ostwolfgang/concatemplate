#include <iostream>
#include <tuple>

#include "language.h"

template<typename T>
using fragment_type_2 = Push<Literal<int, 2>, T>;
template<typename T>
using fragment_type_3 = Push<Literal<int, 3>, T>;
using program_type = IfThenElse<Equals,
                                Add,
                                Dup,
                                Concatenate<fragment_type_2,
                                            Concatenate<fragment_type_3,
                                                        Init>>>;

int main() {
    auto program = program_type();
    auto value = program.value();
    std::cout << typeid(decltype(value)).name() << std::endl;

    std::cout << std::boolalpha << std::get<0>(value) << std::endl;

}