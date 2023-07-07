/* Example of usage. Finds sum of a segment of an array. */

#include <iostream>
#include <utility>
#include "treap.h"


struct TreapProperty {
    long long sum = 0;

    TreapProperty() = default;

    template <typename node_type>
    void update(const node_type& node) {
        sum = (long long)node.key.second * node.amount;
        if (node.left) {
            sum += node.left->property.sum;
        }
        if (node.right) {
            sum += node.right->property.sum;
        }
    }
};


int main() {
    int length;
    std::cin >> length;
    // std::pair for strict ordering
    using treap_type = Treap<std::pair<int, int>, TreapProperty>;
    treap_type treap;
    for (int i = 0; i < length; ++i) {
        int number;
        std::cin >> number;
        treap.insert(std::make_pair(i, number));
    }
    int number_of_questions;
    std::cin >> number_of_questions;
    while (--number_of_questions != -1) {
        int left, right; // bounds
        std::cin >> left >> right;
        auto triple = treap.segment(left, right);
        std::cout << triple.middle.get_property().sum << '\n';
        treap = treap_type::Unite(std::move(triple));
    }
}
