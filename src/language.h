#include <tuple>
#include <type_traits>

namespace tuple_detail {

    template<typename>
    struct is_tuple: std::false_type {};

    template<typename... T>
    struct is_tuple<std::tuple<T...>>: std::true_type {};

    template<typename S, typename... A>
    requires is_tuple<S>::value &&
             is_tuple<A...>::value &&
             std::is_default_constructible<S>::value &&
             std::is_default_constructible<A...>::value
    struct tuple_cat_helper {
        auto operator()() { return std::tuple_cat(A()..., S()); }
        using type = std::invoke_result<tuple_cat_helper<S, A...>>::type;
    };

    template<typename T, size_t N>
    requires is_tuple<T>::value
    struct remove {
    private:
        template<size_t M, typename X>
        requires (M < std::tuple_size<X>::value - 1)
        struct getfrom_helper {
            auto operator()() {
                return getfrom<M+1, X>(X());
            }
            using type = std::invoke_result<getfrom_helper<M, X>>::type;
        };

        template<size_t M, typename X>
        std::enable_if<M < std::tuple_size<X>::value - 1,
                       tuple_cat_helper<typename getfrom_helper<M+1, X>::type,
                                        std::tuple<typename std::tuple_element<M, X>::type>
                                       >
                      >::type
        static getfrom(X x) {
            return std::tuple_cat(std::tuple(std::get<M>(x)), getfrom<M+1, X>(x));
        }

        template<size_t M, typename X>
        std::enable_if<M == std::tuple_size<X>::value - 1,
                       std::tuple<typename std::tuple_element<M, X>::type>>::type
        static getfrom(X x) {
            return std::tuple<typename std::tuple_element<M, X>::type>(std::get<M>(x));
        }

        template<size_t M, typename X>
        requires (M < std::tuple_size<X>::value)
        struct helper {
            auto operator()() {
                return getfrom<M, X>(X());
            }
            using type = std::invoke_result<helper<M, X>>::type;
        };

    public:
        using type = helper<N, T>::type;
        auto operator()(T t) {
            return getfrom<N, T>(t);
        }
    };

}

struct BottomType {
    using value_type = std::tuple<int>;
    static constexpr std::tuple<int> value() {
        return std::tuple<int>(0);
    }
};
struct Init {
    using value_type = std::tuple<BottomType>;
    static constexpr value_type value() {
        return std::tuple(BottomType());
    }
};

template<typename T, T val>
struct Literal {
    using value_type = std::tuple<T>;
    static constexpr value_type value() {
        return value_type(val);
    }
};

template<typename T, typename Program>
requires tuple_detail::is_tuple<typename Program::value_type>::value
struct Push {
    using value_type = tuple_detail::tuple_cat_helper<typename Program::value_type, typename T::value_type>::type;
    static constexpr value_type value() {
        return std::tuple_cat(T::value(), Program::value());
    }
};

template<typename Program>
requires tuple_detail::is_tuple<typename Program::value_type>::value
struct Pop {
    using value_type = tuple_detail::remove<typename Program::value_type, 1>::type;
    static constexpr value_type value() {
        return tuple_detail::remove<typename Program::value_type, 1>()(Program().value());
    }
};


// Binary operations (a b --> c)
namespace binop_detail {

    // Generic binary operation instruction
    template<typename Op, typename Program>
    requires tuple_detail::is_tuple<typename Program::value_type>::value &&
             (std::tuple_size<typename Program::value_type>::value >= 3)
    struct BinOp {
        using op_result_type = Op::result_type;
        using value_type = tuple_detail::tuple_cat_helper<typename tuple_detail::remove<typename Program::value_type, 2>::type,
                                                          std::tuple<op_result_type>>::type;
        static constexpr value_type value() {
            auto prog = Program();
            auto val = prog.value();
            return std::tuple_cat(
                std::tuple(Op()(std::get<0>(val), std::get<1>(val))),
                tuple_detail::remove<typename Program::value_type, 2>()(val)
            );
        }
    };

    template<typename A, typename B>
    struct Sum {
        auto operator()(A a, B b) const {
            return a + b;
        }
        using result_type = std::invoke_result<Sum<A, B>, A, B>::type;
    };

    template<typename A, typename B>
    struct Equals {
        auto operator()(A a, B b) const {
            return a == b;
        }
        using result_type = std::invoke_result<Equals<A, B>, A, B>::type;
    };

}

template<typename Program>
requires tuple_detail::is_tuple<typename Program::value_type>::value &&
         (std::tuple_size<typename Program::value_type>::value >= 3)
struct Add : binop_detail::BinOp<binop_detail::Sum<typename std::tuple_element<0, typename Program::value_type>::type,
                                                   typename std::tuple_element<1, typename Program::value_type>::type>,
                                 Program> {};

template<typename Program>
requires tuple_detail::is_tuple<typename Program::value_type>::value &&
         (std::tuple_size<typename Program::value_type>::value >= 3)
struct Equals : binop_detail::BinOp<binop_detail::Equals<typename std::tuple_element<0, typename Program::value_type>::type,
                                                         typename std::tuple_element<1, typename Program::value_type>::type>,
                                    Program>{};