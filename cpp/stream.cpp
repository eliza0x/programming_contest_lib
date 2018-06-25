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

int main() {
  Stream<int>  s = {1, 2, 3, 4, 5};
  
  for (auto x: s)
    cout << x << " ";
  cout << endl;

  cout << s.map([](auto x){return x * 2;})
           .foldl([](auto x, auto y){return x + y;}, 0) << endl;

  s.map([](auto x){return x * 2;})
   .zip(s)
   .map([](auto p){return make_pair(p.first, 10*p.second);})
   .take(3)
   .reverse()
   .foreach([](auto x){cout << "(" << x.first << ", " << x.second << ") ";});
  cout << endl;
};

