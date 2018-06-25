#include <vector>
#include <utility>
#include <functional>
#include <iostream>
#include <iomanip>
using namespace std;

template < typename T >
using fun = function<T>;

template <typename T> class Stream {
  private:
    vector<T> value;

    template <typename F, typename U, typename B, typename E>
    auto fold(B iter, E end, F f, U u) -> decltype(f(u, *iter)) {
      if (iter != end) {
        u = f(u, *iter);
        iter++;
        for (; iter != end; iter++)
          u = f(u, *iter);
      }
      return u;
    }

    template <typename F, typename U, typename B, typename E>
    auto scan(B iter, E end, F f, U u) -> Stream<decltype(f(u, *iter))> {
      vector<T> v;
      v.push_back(u);
      if (iter != end) {
        u = f(u, *iter);
        v.push_back(u);
        iter++;
        for (; iter != end; iter++) {
          u = f(u, *iter);
          v.push_back(u);
        }
      }
      Stream<decltype(f(u, *iter))> s(v);
      return s;
    }

  public:
    Stream(initializer_list<T> &&v) {value = v;};
    Stream(vector<T> v) : value(v) {};
    Stream(int n) {value.resize(n);};
    Stream() {};
    
    Stream& operator= (initializer_list<T> &&v) {value = v;};
    T& operator[] (int n) {return value[n];};

    void push_back(T t) {
        value.push_back(t);
    }   

    size_t size() {
        return value.size();
    }   

    void reserve(size_t s) {
        value.reserve(s);
    }   

    void resize(size_t s) {
        value.resize(s);
    }

    vector<T> unwrap() {
      return value;
    }

    Stream<T> reverse() {
      vector<T> v = value;
      std::reverse(v.begin(), v.end());
      return Stream<T>(v);
    }

    template <typename U>
    Stream<pair<T, U>> zip(Stream<U> s) {
      Stream<pair<T, U>> z;
      z.reserve(min(value.size(), s.size()));
      for (size_t i=0; i<min(value.size(), s.size()); i++)
        z.push_back(make_pair(value[i], s[i]));
      return z;
    }

    template <typename F>
    auto map(F f) -> Stream<decltype(f(value[0]))> {
      vector<decltype(f(value[0]))> c;
      c.reserve(value.size());
      for (auto x: value) c.push_back(f(x));
      return Stream<decltype(f(value[0]))>(move(c));
    }

    template <typename F>
    auto foreach(F f) -> decltype(f(value[0])) {
      for (auto x: value) f(x);
    }

    Stream<T> filter(fun<bool(T)> f) {
      vector<T> c;
      for (auto x: value) if (f(x))
        c.push_back(x);
      return Stream<T>(move(c));
    }

    template <typename F, typename U>
    auto foldr(F f, U u) -> decltype(f(u, value[0])) {
      return fold(value.rbegin(), value.rend(), f, u);
    }

    template <typename F, typename U>
    auto foldl(F f, U u) -> decltype(f(u, value[0])) {
      return fold(value.begin(), value.end(), f, u);
    }

    template <typename F, typename U>
    auto scanr(F f, U u) -> Stream<decltype(f(u, value[0]))> {
      return scan(value.rbegin(), value.rend(), f, u);
    }

    template <typename F, typename U>
    auto scanl(F f, U u) -> Stream<decltype(f(u, value[0]))> {
      return scan(value.begin(), value.end(), f, u);
    }

    Stream<T> take(size_t n) {
      vector<T> v;
      for (size_t i=0; i<value.size() && i<n; i++)
        v.push_back(value[i]);
      return Stream<T>(v);
    }

    Stream<T> drop(size_t n) {
      vector<T> v;
      for (size_t i=n; i<value.size(); i++)
        v.push_back(value[i]);
      return Stream<T>(v);
    }

    void debug() {
      for (auto x: value) 
        cout << x << " ";
      cout << endl;
    }

    auto begin() -> decltype(value.begin()) { 
      return value.begin(); 
    }

    auto end() -> decltype(value.end()) { 
      return value.end(); 
    }
};

int f(int x) {
    return x * 2;
};

fun<char(int)> g = [](auto x) {
    return (char) x + 64;
};

fun<void(int)> h = [](auto x) {
  cout << x << " ";
};

fun<bool(int)> odd = [](auto x) {
    return x%2 == 1;
};

fun<int(int, int)> add = [](auto x, auto y) {
    return x + y;
};

fun<int(int, int)> mul = [](auto x, auto y) {
    return x * y;
};

int main() {
  Stream<int>  s = {1, 2, 3, 4, 5};

  // 一応中身の要素を出力できる。
  s.debug();

  // zip : Stream<a> -> Stream<b> -> Stream<pair<a,b>>
  s.zip(s)
   .foreach([](auto x){cout << "(" << x.first << ", " << x.second << ") ";});

  // map : Stream<a> -> fun<b(a)> -> Stream<b>
  s.map(g).debug();
  s.map(fun<int(int)>(f)).debug();
 
  // foreach : Stream<a> -> fun<void(a)>;
  s.foreach(h);
  cout << endl;
  
  // filter : Stream<a> -> fun<bool(a)> -> Stream<a>
  s.filter(odd).debug();

  // foldl, foldr : Stream<a> -> fun<b(b, a)> -> b -> b
  cout << s.foldl(mul, 1) << endl;
  cout << s.foldr(mul, 1) << endl;
  
  // scanl, foldr : Stream<a> -> fun<b(b, a)> -> b -> Stream<b>
  s.scanl(add, 0).debug();
  s.scanr(add, 0).debug();

  // reverse : Stream<a> -> Stream<a>
  s.reverse().debug();
  
  // take, drop : Stream<a> -> size_t -> Stream<a>
  s.take(3).debug();
  s.drop(3).debug();

  // range-based for
  for (auto x: s.map(g))
    cout << x << " ";
  cout << endl;
 
  // chain
  s.map([](auto x){return x * 2;})
   .zip(s)
   .map([](auto p){return make_pair(p.first, 10*p.second);})
   .foreach([](auto x){cout << "(" << x.first << ", " << x.second << ") ";});
  cout << endl;
};

