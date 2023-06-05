#ifndef ADS_SET_H
#define ADS_SET_H

#include <functional>
#include <algorithm>
#include <iostream>
#include <stdexcept>

template <typename Key, size_t N = 7>
class ADS_set {
public:
  class Iterator;
  using value_type = Key;
  using key_type = Key;
  using reference = value_type &;
  using const_reference = const value_type &;
  using size_type = size_t;
  using difference_type = std::ptrdiff_t;
  using const_iterator = Iterator;
  using iterator = const_iterator;
//  using key_compare = std::less<key_type>;                         // B+-Tree
  using key_equal = std::equal_to<key_type>;                       // Hashing
  using hasher = std::hash<key_type>;                              // Hashing

private:
  struct Element {
    key_type key;
    Element *next {nullptr};

    ~Element() { delete next; }
  };
  Element *table {nullptr};
  size_type table_size {0};
  size_type current_size {0};
  double max_lf {0.7};

  Element *add(const key_type&);
  Element *search(const key_type&) const;
  size_type h(const key_type &key) const { return hasher{}(key) % table_size; }
  void reserve(const size_type);
  void rehash(const size_type);

public:
  ADS_set() : table{new Element[N]}, table_size{N}, current_size{0} {}
  ADS_set(std::initializer_list<key_type> ilist) : ADS_set() { insert(ilist); }
  template<typename InputIt> ADS_set(InputIt first, InputIt last) : ADS_set() { insert(first, last); }
  ADS_set(const ADS_set &other);

  ~ADS_set() {delete[] table;}

  ADS_set &operator=(const ADS_set &other);
  ADS_set &operator=(std::initializer_list<key_type> ilist);

  size_type size() const { return current_size; }
  bool empty() const { return current_size == 0; }

  void insert(std::initializer_list<key_type> ilist) { insert(ilist.begin(), ilist.end()); }
  std::pair<iterator,bool> insert(const key_type &key) {
    if(auto e{search(key)})
      return {iterator{table, e, h(key), table_size, current_size},false};

    reserve(current_size + 1);

    return {iterator{table, add(key), h(key), table_size, current_size},true};
  }
  template<typename InputIt> void insert(InputIt first, InputIt last);

  void clear() { ADS_set tmp; swap(tmp); }
  size_type erase(const key_type &key);

  size_type count(const key_type &key) const { return search(key) != nullptr; }
  iterator find(const key_type &key) const;

  void swap(ADS_set &other);

  const_iterator begin() const { return iterator{table, table -> next, 0, table_size, current_size}; }
  const_iterator end() const { return iterator{}; }

  void dump(std::ostream &o = std::cerr) const;

  friend bool operator==(const ADS_set &lhs, const ADS_set &rhs) {
    if(lhs.current_size != rhs.current_size)
      return false;

    for(const auto& e: lhs)
      if(!rhs.count(e))
        return false;

    return true;
  }
  friend bool operator!=(const ADS_set &lhs, const ADS_set &rhs) { return !(lhs==rhs); }
};

template <typename Key, size_t N>
typename ADS_set<Key, N>::Element *ADS_set<Key, N>::add(const key_type& key) {
  Element *elem = new Element();
  elem -> key = key;
  elem -> next = table[h(key)].next;
  table[h(key)].next = elem;
  ++current_size;

  return elem;
}

template <typename Key, size_t N>
typename ADS_set<Key,N>::Element *ADS_set<Key,N>::search(const key_type& key) const {
  for(Element *pointer {table[h(key)].next}; pointer != nullptr;) {
    if(key_equal{}(key, pointer -> key))
      return pointer;
    else
      pointer = pointer -> next;
  }

  return nullptr;
}

template <typename Key, size_t N>
void ADS_set<Key,N>::reserve(const size_type n) {
  if(n < table_size * max_lf)
    return;

  size_type new_table_size {table_size};
  while(new_table_size * max_lf < n)
    ++(new_table_size *= 2);

  rehash(new_table_size);
}

template <typename Key, size_t N>
void ADS_set<Key,N>::rehash(const size_type n) {
  size_type new_table_size {std::max(N,std::max(static_cast<size_type>(current_size/max_lf),n))};
  Element *new_table {new Element[new_table_size]};
  Element *old_table {table};
  size_type old_table_size {table_size};

  current_size = 0;
  table = new_table;
  table_size = new_table_size;

  for(size_type i {0}; i < old_table_size; ++i) {
    for(Element *pointer {old_table[i].next}; pointer != nullptr; pointer = pointer -> next) {
      add(pointer -> key);
    }
  }

  delete[] old_table;
}

template <typename Key, size_t N>
ADS_set<Key,N>::ADS_set(const ADS_set &other) {
  rehash(other.table_size);
  for(const auto& e: other)
    add(e);
}

template <typename Key, size_t N>
ADS_set<Key,N> &ADS_set<Key,N>::operator=(const ADS_set &other) {
  ADS_set tmp{other};
  swap(tmp);

  return *this;
}

template <typename Key, size_t N>
ADS_set<Key,N> &ADS_set<Key,N>::operator=(std::initializer_list<key_type> ilist) {
  ADS_set tmp{ilist};
  swap(tmp);

  return *this;
}

template <typename Key, size_t N>
template<typename InputIt> void ADS_set<Key,N>::insert(InputIt first, InputIt last) {
  for(auto it{first}; it != last; ++it) {
    if(!count(*it)) {
      reserve(current_size + 1);
      add(*it);
    }
  }
}

template <typename Key, size_t N>
typename ADS_set<Key, N>::size_type ADS_set<Key, N>::erase(const key_type &key) {
  auto e{search(key)};
  Element *e_previous {&table[h(key)]};
  if(e == nullptr)
    return 0;

  for(; e_previous != nullptr; e_previous = e_previous -> next) {
    if(e_previous -> next == e) {
      e_previous -> next = e -> next;
      e -> next = nullptr;
      delete e;
      --current_size;
      break;
    }
  }

  return 1;
}

template <typename Key, size_t N>
typename ADS_set<Key,N>::iterator ADS_set<Key,N>::find(const key_type &key) const {
  if (auto e{search(key)}) {
    return iterator{table, e, h(key), table_size, current_size};
  }

  return end();
}

template <typename Key, size_t N>
void ADS_set<Key,N>::swap(ADS_set &other) {
  using std::swap;
  swap(table, other.table);
  swap(table_size, other.table_size);
  swap(current_size, other.current_size);
  swap(max_lf, other.max_lf);
}

template <typename Key, size_t N>
void ADS_set<Key,N>::dump(std::ostream &o) const {
  o << "table_size: " << table_size << ", current_size: " << current_size << "\n";

  for(size_type i{0}; i < table_size; ++i) {
    o << " Cell " << i << ":";

    Element *elem {table[i].next};
    bool first{true};

    while(elem != nullptr) {
      if(first)
        first = false;
      else
        o << " -> ";

      o << '(' << elem -> key << ')';

      elem = elem -> next;
    }

    o << "\n";
  }

  o << " Cell " << table_size << ": END" << "\n";
}


template <typename Key, size_t N>
class ADS_set<Key,N>::Iterator {
  Element *table;
  Element *e;
  size_type idx;
  size_type table_size;
  size_type current_size;

  void skip_idx() { while(idx < table_size && table[idx].next == nullptr) ++idx; };

public:
  using value_type = Key;
  using difference_type = std::ptrdiff_t;
  using reference = const value_type &;
  using pointer = const value_type *;
  using iterator_category = std::forward_iterator_tag;

  explicit Iterator(Element *table = nullptr, Element *e = nullptr) : table{table}, e{e}, idx{0}, table_size{0}, current_size{0} { if(e) skip_idx(); }
  explicit Iterator(Element *table, Element *e, size_type idx, size_type table_size, size_type current_size) :
    table{table}, e{e}, idx{idx}, table_size{table_size}, current_size{current_size} {
      if(this -> current_size != 0 && this -> e == nullptr) {
        skip_idx();
        this -> e = (this -> table)[this -> idx].next;
      }
    }
  reference operator*() const { return e -> key; }
  pointer operator->() const { return &e -> key; }
  Iterator &operator++() {
    if(e -> next == nullptr) {
      if(idx < table_size) {
        ++idx;
        skip_idx();
      }

      if(idx == table_size)
        e = nullptr;
      else
        e = table[idx].next;
    }
    else
      e = e -> next;

    return *this;
  }
  Iterator operator++(int) { auto i{*this}; ++*this; return i; }
  friend bool operator==(const Iterator &lhs, const Iterator &rhs) { return lhs.e == rhs.e; }
  friend bool operator!=(const Iterator &lhs, const Iterator &rhs) { return !(lhs==rhs); }
};

template <typename Key, size_t N>
void swap(ADS_set<Key,N> &lhs, ADS_set<Key,N> &rhs) { lhs.swap(rhs); }


#endif // ADS_SET_H
