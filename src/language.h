#include <tuple>
#include <type_traits>

namespace detail {

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
requires detail::is_tuple<typename Program::value_type>::value
struct Push {
    using value_type = detail::tuple_cat_helper<typename Program::value_type, typename T::value_type>::type;
    static constexpr value_type value() {
        return std::tuple_cat(T::value(), Program::value());
    }
};

template<typename Program>
requires detail::is_tuple<typename Program::value_type>::value
struct Pop {
    using value_type = detail::remove<typename Program::value_type, 1>::type;
    static constexpr value_type value() {
        return detail::remove<typename Program::value_type, 1>()(Program().value());
    }
};

template<typename Program>
requires detail::is_tuple<typename Program::value_type>::value &&
         (std::tuple_size<typename Program::value_type>::value >= 3)
struct Add {
    using value_type = detail::tuple_cat_helper<typename detail::remove<typename Program::value_type, 2>::type,
                                                std::tuple<decltype(std::get<0>(Program().value()) +
                                                                    std::get<1>(Program().value()))>>::type;
    static constexpr value_type value() {
        auto prog = Program();
        return std::tuple_cat(
            std::tuple(std::get<0>(prog.value()) + std::get<1>(prog.value())),
            detail::remove<typename Program::value_type, 2>()(prog.value())
        );
    }
};

template<typename Program>
requires detail::is_tuple<typename Program::value_type>::value &&
         (std::tuple_size<typename Program::value_type>::value >= 3)
struct Equals {
    using value_type = detail::tuple_cat_helper<typename detail::remove<typename Program::value_type, 2>::type,
                                                std::tuple<bool>>::type;
    static constexpr value_type value() {
        auto prog = Program();
        return std::tuple_cat(
            std::tuple(std::get<0>(prog.value()) == std::get<1>(prog.value())),
            detail::remove<typename Program::value_type, 2>()(prog.value())
        );
    }
};