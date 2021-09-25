#include <iostream>

#include "language.h"

int main() {

    auto program = Equals<Push<Literal<int, 5>,
                               Add<Push<Literal<int, 4>,
                                        Push<Literal<int, 3>,
                                             Init
                                            >
                                       >
                                  >
                              >
                         >();
    std::cout << typeid(decltype(program)::value_type).name() << std::endl;

    std::cout << std::get<0>(program.value()) << std::endl;

}