#pragma once
#include <iostream>
#include <algorithm>
#include <cstdlib>
#include <type_traits>
#include <cstdint>


/* treap default iterator */
template <typename Treap>
class TreapIterator {
  public:
    // element
    using value_type = typename Treap::value_type;
    using const_value_type = typename Treap::const_value_type;
    using pointer = typename Treap::pointer;
    using const_pointer = typename Treap::const_pointer;
    using reference = typename Treap::reference;
    using const_reference = typename Treap::const_reference;
    // size
    using size_type = typename Treap::size_type;
    using difference_type = typename Treap::difference_type;
    // category
    using iterator_category = std::bidirectional_iterator_tag;
    // node
    using node_type = typename Treap::node_type;

  private:
    node_type* ptr = nullptr;
    bool is_end = true;

  public:
    TreapIterator(node_type* ptr, bool is_end = false)
        : ptr(ptr), is_end(is_end) {}
    TreapIterator(const typename Treap::iterator& other)
        : ptr(other.ptr), is_end(other.is_end) {}

    reference operator*() const noexcept {
        return ptr->key;
    }
    pointer operator->() const noexcept {
        return &(operator*());
    }

    TreapIterator& operator+=(difference_type offset) noexcept {
        while (offset > 0) {
            ++(*this);
            --offset;
        }
        while (offset < 0) {
            --(*this);
            ++offset;
        }
        return *this;
    }
    TreapIterator& operator-=(difference_type offset) noexcept {
        *this += -offset;
        return *this;
    }

    TreapIterator& operator++() noexcept {
        if (!ptr || is_end) { return *this; } // UB
        if (ptr->right) {
            ptr = ptr->right;
            while (ptr->left) {
                ptr = ptr->left;
            }
            return *this;
        }
        node_type* node = ptr;
        while (node->parent && node == node->parent->right) {
            node = node->parent;
        }
        if (!node->parent) {
            is_end = true;
        } else {
            ptr = node->parent;
        }
        return *this;
    }
    TreapIterator& operator--() noexcept {
        if (!ptr) { return *this; } // UB
        if (is_end) {
            is_end = false;
            return *this;
        }
        if (ptr->left) {
            ptr = ptr->left;
            while (ptr->right) {
                ptr = ptr->right;
            }
            return *this;
        }
        node_type* node = ptr;
        while (node->parent && node == node->parent->left) {
            node = node->parent;
        }
        if (!node->parent) {
            // UB
        } else {
            ptr = node->parent;
        }
        return *this;
    }

    TreapIterator operator++(int) noexcept {
        TreapIterator temp = *this;
        ++*this;
        return temp;
    }
    TreapIterator operator--(int) noexcept {
        TreapIterator temp = *this;
        --*this;
        return temp;
    }

  private:
    friend Treap;
    // friend TreapIterator<Treap>;
    template <typename TreapStructure>
    friend bool operator==(const TreapIterator<TreapStructure>& it1, const TreapIterator<TreapStructure>& it2) noexcept;
    // friend int main();
};

template <typename Treap>
TreapIterator<Treap> operator+(
    TreapIterator<Treap> it,
    typename TreapIterator<Treap>::difference_type offset) {
    it += offset;
    return it;
}

template <typename Treap>
TreapIterator<Treap> operator-(
    TreapIterator<Treap> it,
    typename TreapIterator<Treap>::difference_type offset) {
    it -= offset;
    return it;
}

template <typename Treap>
TreapIterator<Treap> operator+(
    typename TreapIterator<Treap>::difference_type offset,
    TreapIterator<Treap> it) {
    it += offset;
    return it;
}

template <typename Treap>
bool operator==(const TreapIterator<Treap>& it1, const TreapIterator<Treap>& it2) noexcept {
    return it1.ptr == it2.ptr && it1.is_end == it2.is_end;
}
template <typename Treap>
bool operator!=(const TreapIterator<Treap>& it1, const TreapIterator<Treap>& it2) noexcept {
    return !(it1 == it2);
}


/* The default randomizer, that provides random integers */
template <typename Priority>
class DefaultTreapRandomizer {
    DefaultTreapRandomizer() = delete;
};

template <>
class DefaultTreapRandomizer<uint32_t> {
    static const uint32_t kMod = 32768;

    uint32_t get_random() const {
        return rand() % kMod;
    }

  public:
    DefaultTreapRandomizer() = default;

    // random number from the range [0, 2^30 - 1]
    uint32_t operator()() const {
        return get_random() * kMod + get_random();
    }
};


/* The default property structure */
class DefaultTreapProperty {
  public:
    template <typename node_type>
    void update(const node_type& node) {}
};


/*
// Treap property for 'sum' should look like this:

struct TreapProperty {
    int sum = 0;

    TreapProperty() = default;

    template <typename node_type>
    void update(const node_type& node) {
        sum = node.key * node.amount;
        if (node.left) {
            sum += node.left->property.sum;
        }
        if (node.right) {
            sum += node.right->property.sum;
        }
    }
};
*/


template <typename T1, typename T2 = T1, typename T3 = T2,
    typename std::enable_if_t<std::conjunction_v<
    std::is_same<
        std::decay_t<T1>, std::decay_t<T2>
    >,
    std::is_same<
        std::decay_t<T2>, std::decay_t<T3>
    >>>* = nullptr>
struct Triple {
    T1 left;
    T2 middle;
    T3 right;

    // constructors
    constexpr Triple() = default;

    template <typename V1, typename V2, typename V3,
        typename std::enable_if_t<std::conjunction_v<
    std::is_same<std::decay_t<V1>, std::decay_t<T1>>,
    std::is_same<std::decay_t<V2>, std::decay_t<T2>>,
    std::is_same<std::decay_t<V3>, std::decay_t<T3>>>>* = nullptr>
    Triple(V1&& left, V2&& middle, V3&& right) : 
        left(std::forward<V1>(left)),
        middle(std::forward<V2>(middle)),
        right(std::forward<V3>(right)) {}

    // copy-constructors
    Triple(const Triple& other) :
        left(other.left),
        middle(other.middle),
        right(other.right) {}
    Triple(Triple&& other) :
        left(std::move(other.left)),
        middle(std::move(other.middle)),
        right(std::move(other.right)) {}

    // assignment-operators
    Triple& operator=(const Triple& other) {
        left = other.left;
        middle = other.middle;
        right = other.right;
    }
    Triple& operator=(Triple&& other) {
        left = std::move(other.left);
        middle = std::move(other.middle);
        right = std::move(other.right);
    }
};

template <typename T1, typename T2, typename T3>
Triple<std::decay_t<T1>, std::decay_t<T2>, std::decay_t<T3>> make_triple(T1&& t1, T2&& t2, T3&& t3) {
    return Triple<std::decay_t<T1>, std::decay_t<T2>, std::decay_t<T3>>(
        std::forward<T1>(t1),
        std::forward<T2>(t2),
        std::forward<T3>(t3)
    );
}


template <
    typename Key,
    typename Property = DefaultTreapProperty,
    typename KeyCompare = std::less<Key>,
    typename Allocator = std::allocator<Key>,
    typename Priority = uint32_t,
    typename PriorityCompare = std::less<Priority>,
    typename Randomizer = DefaultTreapRandomizer<Priority>
>
class Treap : private KeyCompare, private Allocator, private PriorityCompare, private Randomizer {
  public:
    using treap_type = Treap<Key, Property, KeyCompare, Allocator, Priority, PriorityCompare, Randomizer>;
    // element
    using value_type = Key;
    using const_value_type = const Key;
    using pointer = Key*;
    using const_pointer = const Key*;
    using reference = Key&;
    using const_reference = const Key&;
    // size
    using size_type = uint64_t;
    using difference_type = int64_t;
    // allocator types
    using allocator_type = Allocator;
    using allocator_traits = std::allocator_traits<allocator_type>;

  private:
    struct Node {
        // allocator
        using allocator_node_type = typename allocator_traits::template rebind_alloc<Node>;
        using allocator_node_traits = std::allocator_traits<allocator_node_type>;
        // core
        value_type key;
        Priority priority;
        size_type amount;
        size_type size = 1;
        Node* parent = nullptr;
        Node* left = nullptr;
        Node* right = nullptr;
        // user-defined
        mutable Property property;

        Node(const_reference key, Priority priority, size_type amount) : 
            key(key),
            priority(priority),
            amount(amount),
            size(amount) {}

        size_type get_size() const {
            return amount + (left ? left->get_size() : 0) + (right ? right->get_size() : 0);
        }

        void clear(allocator_node_type alloc) {
            if (left) { left->clear(alloc); }
            if (right) { right->clear(alloc); }
            Node* ptr = this;
            allocator_node_traits::destroy(alloc, ptr);
            allocator_node_traits::deallocate(alloc, ptr, 1);
        }

        void update_size() {
            size = amount + (left ? left->size : 0) + (right ? right->size : 0);
        }
        void update_property() const {
            property.template update<Node>(*this);
        }
        void update() {
            update_size();
            update_property();
        }
    };

  public:
    using node_type = Node;
    // iterators
    using iterator = TreapIterator<treap_type>;
    using const_iterator = iterator;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    // triple
    using triple_type = Triple<treap_type, treap_type, treap_type>;

  private:
    KeyCompare& get_key_comp() noexcept {
        return static_cast<KeyCompare&>(*this);
    }

    using allocator_node_type = typename Node::allocator_node_type;
    using allocator_node_traits = typename Node::allocator_node_traits;

    allocator_type& get_allocator() noexcept {
        return static_cast<allocator_type&>(*this);
    }

    allocator_node_type get_node_allocator() const noexcept {
        return allocator_node_type(get_allocator());
    }

    PriorityCompare& get_priority_comp() noexcept {
        return static_cast<PriorityCompare&>(*this);
    }

    Randomizer& get_randomizer() noexcept {
        return static_cast<Randomizer&>(*this);
    }

    node_type* root = nullptr;

  public:
    // constructors
    explicit Treap(
        const KeyCompare& key_comp = KeyCompare(),
        const allocator_type& alloc = allocator_type(),
        const PriorityCompare& priority_comp = PriorityCompare(),
        const Randomizer& randomizer = Randomizer()) : 
        KeyCompare(key_comp),
        allocator_type(alloc),
        PriorityCompare(priority_comp),
        Randomizer(randomizer) {}
    explicit Treap(const allocator_type& alloc) : allocator_type(alloc) {}

    template <typename InputIt>
    Treap(InputIt first, InputIt last,
        const KeyCompare& key_comp = KeyCompare(),
        const allocator_type& alloc = allocator_type(),
        const PriorityCompare& priority_comp = PriorityCompare(),
        const Randomizer& randomizer = Randomizer()) :
        Treap(key_comp, alloc, priority_comp, randomizer) {
        for (; first != last; ++first) {
            insert(*first);
        }
    }

  private:
    Treap(node_type* root,
        const KeyCompare& key_comp = KeyCompare(),
        const allocator_type& alloc = allocator_type(),
        const PriorityCompare& priority_comp = PriorityCompare(),
        const Randomizer& randomizer = Randomizer()) : 
        Treap(key_comp, alloc, priority_comp, randomizer) {
        this->root = root;
    }

    inline Treap new_treap_by_root(node_type* root = nullptr) const noexcept {
        Treap new_instance(
            root,
            static_cast<const KeyCompare&>(*this),
            static_cast<const allocator_type&>(*this),
            static_cast<const PriorityCompare&>(*this),
            static_cast<const Randomizer&>(*this)
        );
        return new_instance;
    }

    Triple<node_type*, node_type*, node_type*> disjoin(node_type* ptr) {
        if (ptr->left) {
            ptr->left->parent = nullptr;
        }
        if (ptr->right) {
            ptr->right->parent = nullptr;
        }
        node_type* left_ptr = ptr->left;
        node_type* right_ptr = ptr->right;
        ptr->left = nullptr;
        ptr->right = nullptr;
        return make_triple(left_ptr, ptr, right_ptr);
    }

  public:
    // copy-constructors
    Treap(const Treap& other) : Treap(other.begin(), other.end(),
        static_cast<const KeyCompare&>(other),
        allocator_traits::select_on_container_copy_construction(static_cast<const allocator_type&>(other)),
        static_cast<const PriorityCompare&>(other),
        static_cast<const Randomizer&>(other)
    ) {
        std::cout << "CAUTION!" << std::endl;
    }

    // move-constructors
    Treap(Treap&& other) : root(other.root),
        KeyCompare(static_cast<KeyCompare&&>(other)),
        allocator_type(static_cast<allocator_type&&>(other)),
        PriorityCompare(static_cast<PriorityCompare&&>(other)),
        Randomizer(static_cast<Randomizer&&>(other)) {
        other.root = nullptr;
    }

    // copy-assignment operators
    Treap& operator=(const Treap& other) {
        if (root != other.root) {
            if constexpr (allocator_traits::propagate_on_container_copy_assignment::value) {
                Treap(other).swap(*this);
            } else {
                clear();
                get_key_comp() = static_cast<const KeyCompare&>(other);
                get_priority_comp() = static_cast<const PriorityCompare&>(other);
                get_randomizer() = static_cast<const Randomizer&>(other);
                for (const_reference key : other) {
                    insert(key);
                }
            }
        }
        return *this;
    }

    // move-assignment operators
    Treap& operator=(Treap&& other) {
        if (root != other.root) {
            if constexpr (allocator_traits::propagate_on_container_move_assignment::value) {
                Treap(std::move(other)).swap(*this);
            } else {
                clear();
                get_key_comp() = static_cast<KeyCompare&&>(other);
                get_priority_comp() = static_cast<PriorityCompare&&>(other);
                get_randomizer() = static_cast<Randomizer&&>(other);
                root = other.root;
                other.root = nullptr;
            }
        }
        return *this;
    }

    // destructor
    ~Treap() {
        clear();
    }

    // methods
    Treap& merge(Treap&& other) { // 'other' has bigger keys, TESTED
        if (other.empty()) { return *this; }
        if (empty()) {
            swap(other);
            return *this;
        }
        if (get_priority_comp()(root->priority, other.root->priority)) {
            if (root->right) {
                root->right->parent = nullptr;
            }
            Treap dump = new_treap_by_root(root->right);
            dump.merge(std::move(other));
            root->right = dump.root;
            dump.root = nullptr;
            root->right->parent = root;
        } else {
            if (other.root->left) {
                other.root->left->parent = nullptr;
                merge(new_treap_by_root(other.root->left));
            }
            other.root->left = root;
            root = other.root;
            root->left->parent = root;
        }
        root->update();
        other.root = nullptr;
        return *this;
    }

    triple_type split(const_reference key) { // TESTED
        if (empty()) { return triple_type(new_treap_by_root(), new_treap_by_root(), new_treap_by_root()); }
        if (get_key_comp()(key, root->key)) {
            if (root->left) {
                root->left->parent = nullptr;
            }
            triple_type dump = new_treap_by_root(root->left).split(key);
            root->left = nullptr;
            root->update();
            dump.right.merge(std::move(*this));
            root = nullptr;
            return std::move(dump);
        } else if (get_key_comp()(root->key, key)) {
            if (root->right) {
                root->right->parent = nullptr;
            }
            triple_type dump = new_treap_by_root(root->right).split(key);
            root->right = nullptr;
            root->update();
            dump.left = std::move(merge(std::move(dump.left)));
            root = nullptr;
            return std::move(dump);
        } else {
            Triple<node_type*, node_type*, node_type*> disjoined = disjoin(root);
            triple_type result = make_triple(
                new_treap_by_root(disjoined.left),
                new_treap_by_root(disjoined.middle),
                new_treap_by_root(disjoined.right)
            );
            result.middle.root->update();
            root = nullptr;
            return result;
        }
    }

    static Treap Unite(triple_type&& splitted) { // TESTED
        return std::move(splitted.left.merge(
            std::move(splitted.middle)
        ).merge(
            std::move(splitted.right)
        ));
    }

    void insert(const_reference key, size_type amount = 1) { // TESTED
        triple_type splitted = split(key);
        if (!splitted.middle.empty()) {
            splitted.middle.root->amount += amount;
            splitted.middle.root->update();
            // unite
            Unite(std::move(splitted)).swap(*this);
        } else {
            allocator_node_type alloc = get_node_allocator();
            node_type* ptr = nullptr;
            try {
                ptr = allocator_node_traits::allocate(alloc, 1);
                allocator_node_traits::construct(alloc, ptr, key, get_randomizer()(), amount);
            } catch (...) {
                if (ptr) {
                    allocator_node_traits::deallocate(alloc, ptr, 1);
                }
                // unite
                Unite(std::move(splitted)).swap(*this);
                throw;
            }
            ptr->update();
            // unite
            Unite(make_triple(
                std::move(splitted.left),
                new_treap_by_root(ptr),
                std::move(splitted.right)
            )).swap(*this);
        }
    }

    void print() const {
        if (empty()) { return; }
        Treap left(root->left);
        Treap right(root->right);
        left.print();
        std::cout << "Key: " << root->key.first << " " << root->key.second << ", Parent: " << root->parent << ", Left: " << root->left << ", Right: " << root->right << std::endl;
        right.print();
        left.root = nullptr;
        right.root = nullptr;
    }
    void print_keys() const {
        for (const_reference el : *this) {
            std::cout << el.first << " " << el.second << " ";
        }
    }

    void erase(const_reference key, size_type amount = 1) { // 0 means to erase completely
        triple_type splitted = split(key);
        if (!splitted.middle.empty()) {
            if (amount == 0) {
                splitted.middle = new_treap_by_root();
            } else {
                splitted.middle.root->amount = std::max(splitted.middle.root->amount - amount, size_type(0));
                if (splitted.middle.root->amount == 0) {
                    splitted.middle = new_treap_by_root();
                } else {
                    splitted.middle.root->update();
                }
            }
        }
        Unite(std::move(splitted)).swap(*this);
    }

    void erase(iterator it, size_type amount = 1) {
        erase(*it, amount);
    }

    size_type count(const_reference key) const {
        auto it = find(key);
        if (it == end()) { return 0; }
        return it.ptr->amount;
    }

    bool contains(const_reference key) const { return count(key) != 0; }

    iterator find(const_reference key) const {
        if (empty()) { return end(); }
        iterator result;
        if (get_key_comp()(key, root->key)) {
            Treap dump(root->left);
            result = dump.find(key);
            dump.root = nullptr;
        } else if (get_key_comp()(root->key, key)) {
            Treap dump(root->right);
            result = dump.find(key);
            dump.root = nullptr;
        } else {
            result = iterator(root);
        }
        return result;
    }

    iterator kth(size_type pos) const { // TESTED
        if (pos > size()) { return end(); }
        if (root->left) {
            if (pos <= root->left->size) {
                Treap dump = new_treap_by_root(root->left);
                iterator result = dump.kth(pos);
                dump.root = nullptr;
                return result;
            }
            pos -= root->left->size;
        }
        if (pos <= root->amount) {
            return iterator(root);
        }
        pos -= root->amount;
        Treap dump = new_treap_by_root(root->right);
        iterator result = dump.kth(pos);
        dump.root = nullptr;
        return result;
    }

    void clear() {
        if (root) {
            root->clear(get_node_allocator());
        }
        root = nullptr;
    }

    void swap(Treap& other) {
        std::swap(root, other.root);
        std::swap(static_cast<KeyCompare&>(*this), static_cast<KeyCompare&>(other));
        if constexpr (allocator_traits::propagate_on_container_swap::value) {
            std::swap(static_cast<allocator_type&>(*this), static_cast<allocator_type&>(other));
        }
        std::swap(static_cast<PriorityCompare&>(*this), static_cast<PriorityCompare&>(other));
        std::swap(static_cast<Randomizer&>(*this), static_cast<Randomizer&>(other));
    }

    bool empty() const { return root == nullptr; }

    size_type size() const noexcept { // TESTED
        return empty() ? 0 : root->size;
    }

    // property
    Property& get_property() const {
        return root->property;
    }

    // allocator
    allocator_type get_allocator() const noexcept {
        return static_cast<allocator_type>(*this);
    }

    // iterators
    iterator begin() const noexcept {
        if (!root) {
            return iterator(nullptr, true);
        }
        node_type* node = root;
        while (node->left) {
            node = node->left;
        }
        return iterator(node);
    }
    const_iterator cbegin() const noexcept {
        return begin();
    }

    iterator end() const noexcept {
        if (!root) {
            return iterator(nullptr, true);
        }
        node_type* node = root;
        while (node->right) {
            node = node->right;
        }
        return iterator(node, true);
    }
    const_iterator cend() const noexcept {
        return end();
    }

    auto rbegin() const noexcept {
        return std::reverse_iterator<iterator>(end());
    }
    auto crbegin() const noexcept {
        return std::reverse_iterator<const_iterator>(end());
    }

    auto rend() const noexcept {
        return std::reverse_iterator<iterator>(begin());
    }
    auto crend() const noexcept {
        return std::reverse_iterator<const_iterator>(begin());
    }

    template <typename TreapStructure>
    friend class TreapIterator;

    template <typename ...Args>
    friend bool operator==(const Treap<Args...>& t1, const Treap<Args...>& t2);
};

template <typename ...Args>
bool operator==(const Treap<Args...>& t1, const Treap<Args...>& t2) {
    if (t1.size() == t2.size()) {
        for (auto it1 = t1.begin(), it2 = t2.begin(); it1 != t1.end() && it2 != t2.end(); ++it1, ++it2) {
            if (*it1 != *it2) {
                return false;
            }
        }
        return true;
    }
    return false;
}

template <typename ...Args>
bool operator!=(const Treap<Args...>& t1, const Treap<Args...>& t2) {
    return !(t1 == t2);
}
