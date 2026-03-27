// =============================================================
//  big_integer.cpp -- BigInteger class implementation
//
//  TASK: Implement all methods declared in big_integer.h
//  This stub file exists only so the project structure is clear.
//  Replace its contents with your implementation.
// =============================================================

#include "big_integer.h"
#include <string>
#include <vector>
#include <algorithm>
#include <climits>
#include <stdexcept>

// Вспомогательные функции
static void remove_zeros(std::vector<int>& a) {
    while (a.size() > 1 && a.back() == 0)
        a.pop_back();
}

static int cmp_abs(const std::vector<int>& a, const std::vector<int>& b) {
    if (a.size() != b.size())
        return a.size() < b.size() ? -1 : 1;
    for (int i = a.size() - 1; i >= 0; i--)
        if (a[i] != b[i])
            return a[i] < b[i] ? -1 : 1;
    return 0;
}

static std::vector<int> add_abs(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> res;
    int carry = 0;
    size_t i = 0;
    while (i < a.size() || i < b.size() || carry) {
        int sum = carry;
        if (i < a.size()) sum += a[i];
        if (i < b.size()) sum += b[i];
        res.push_back(sum % 10);
        carry = sum / 10;
        i++;
    }
    return res;
}

static std::vector<int> sub_abs(const std::vector<int>& a, const std::vector<int>& b) {
    // Предполагаем, что a >= b
    std::vector<int> res;
    int borrow = 0;
    for (size_t i = 0; i < a.size(); i++) {
        int x = a[i] - borrow;
        if (i < b.size()) x -= b[i];
        if (x < 0) {
            x += 10;
            borrow = 1;
        } else {
            borrow = 0;
        }
        res.push_back(x);
    }
    remove_zeros(res);
    return res;
}

// Конструкторы
BigInteger::BigInteger() : digits_{0}, negative_(false) {}

BigInteger::BigInteger(int value) : digits_(), negative_(value < 0) {
    long long x = value;
    if (x < 0) x = -x;
    if (x == 0) {
        digits_ = {0};
        negative_ = false;
        return;
    }
    while (x) {
        digits_.push_back(x % 10);
        x /= 10;
    }
}

BigInteger::BigInteger(long long value) : digits_(), negative_(value < 0) {
    unsigned long long x;
    if (value < 0) {
        if (value == LLONG_MIN) {
            x = static_cast<unsigned long long>(LLONG_MAX) + 1;
        } else {
            x = -value;
        }
    } else {
        x = value;
    }
    if (x == 0) {
        digits_ = {0};
        negative_ = false;
        return;
    }
    while (x) {
        digits_.push_back(x % 10);
        x /= 10;
    }
}

BigInteger::BigInteger(const std::string& s) : digits_(), negative_(false) {
    size_t start = 0;
    if (s[0] == '-') {
        negative_ = true;
        start = 1;
    }
    for (size_t i = s.length(); i > start; i--){
        if (!isdigit(s[i-1])) {
            throw std::invalid_argument("Invalid charecter in input");
        }
        digits_.push_back(s[i-1] - '0');
    }
    remove_zeros(digits_);
    if (digits_.size() == 1 && digits_[0] == 0)
        negative_ = false;
}

// Сравнениеi
bool BigInteger::operator==(const BigInteger& rhs) const {
    return negative_ == rhs.negative_ && digits_ == rhs.digits_;
}

bool BigInteger::operator!=(const BigInteger& rhs) const {
    return !(*this == rhs);
}

bool BigInteger::operator<(const BigInteger& rhs) const {
    if (negative_ != rhs.negative_) return negative_;
    int c = cmp_abs(digits_, rhs.digits_);
    return negative_ ? c > 0 : c < 0;
}

bool BigInteger::operator<=(const BigInteger& rhs) const {
    return *this < rhs || *this == rhs;
}

bool BigInteger::operator>(const BigInteger& rhs) const {
    return !(*this <= rhs);
}

bool BigInteger::operator>=(const BigInteger& rhs) const {
    return !(*this < rhs);
}

// Арифметика
BigInteger& BigInteger::operator+=(const BigInteger& rhs) {
    if (negative_ == rhs.negative_) {
        digits_ = add_abs(digits_, rhs.digits_);
    } else {
        int c = cmp_abs(digits_, rhs.digits_);
        if (c == 0) {
            digits_ = {0};
            negative_ = false;
        } else if (c > 0) {
            digits_ = sub_abs(digits_, rhs.digits_);
        } else {
            digits_ = sub_abs(rhs.digits_, digits_);
            negative_ = rhs.negative_;
        }
    }
    return *this;
}

BigInteger BigInteger::operator+(const BigInteger& rhs) const {
    BigInteger r = *this;
    r += rhs;
    return r;
}

BigInteger& BigInteger::operator-=(const BigInteger& rhs) {
    *this += (-rhs);
    return *this;
}

BigInteger BigInteger::operator-(const BigInteger& rhs) const {
    BigInteger r = *this;
    r -= rhs;
    return r;
}

BigInteger& BigInteger::operator*=(const BigInteger& rhs) {
    bool sign = negative_ != rhs.negative_;
    std::vector<int> res(digits_.size() + rhs.digits_.size(), 0);
    
    for (size_t i = 0; i < digits_.size(); i++) {
        int carry = 0;
        for (size_t j = 0; j < rhs.digits_.size() || carry; j++) {
            long long cur = res[i + j] + carry;
            if (j < rhs.digits_.size())
                cur += 1LL * digits_[i] * rhs.digits_[j];
            res[i + j] = cur % 10;
            carry = static_cast<int>(cur / 10);
        }
    }
    
    remove_zeros(res);
    digits_ = res;
    negative_ = sign;
    if (is_zero()) negative_ = false;
    return *this;
}

BigInteger BigInteger::operator*(const BigInteger& rhs) const {
    BigInteger r = *this;
    r *= rhs;
    return r;
}

BigInteger& BigInteger::operator/=(const BigInteger& rhs) {
    if (rhs.is_zero()) {
        throw std::runtime_error("Division by zero");
    }
    if (is_zero()) return *this;
    
    bool sign = negative_ != rhs.negative_;
    BigInteger a = *this, b = rhs;
    a.negative_ = b.negative_ = false;
    
    if (cmp_abs(a.digits_, b.digits_) < 0) {
        *this = 0;
        return *this;
    }
    
    std::vector<int> res;
    BigInteger cur;
    
    for (int i = static_cast<int>(a.digits_.size()) - 1; i >= 0; i--) {
        cur.digits_.insert(cur.digits_.begin(), a.digits_[i]);
        remove_zeros(cur.digits_);
        
        int x = 0;
        int low = 0, high = 9;
        while (low <= high) {
            int mid = (low + high) / 2;
            BigInteger test = b * mid;
            if (cmp_abs(cur.digits_, test.digits_) >= 0) {
                x = mid;
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }
        
        cur = cur - b * x;
        res.push_back(x);
    }
    
    std::reverse(res.begin(), res.end());
    remove_zeros(res);
    digits_ = res;
    negative_ = sign;
    if (is_zero()) negative_ = false;
    return *this;
}

BigInteger BigInteger::operator/(const BigInteger& rhs) const {
    BigInteger r = *this;
    r /= rhs;
    return r;
}

BigInteger& BigInteger::operator%=(const BigInteger& rhs) {
    if (rhs.is_zero()) {
        throw std::runtime_error("Division by zero");
    }
    if (is_zero()) return *this;
    
    BigInteger quotient = *this / rhs;
    *this = *this - quotient * rhs;
    return *this;
}

BigInteger BigInteger::operator%(const BigInteger& rhs) const {
    BigInteger r = *this;
    r %= rhs;
    return r;
}

// Унарные операции
BigInteger BigInteger::operator-() const {
    BigInteger r = *this;
    if (!r.is_zero()) r.negative_ = !r.negative_;
    return r;
}

BigInteger& BigInteger::operator++() {
    *this += 1;
    return *this;
}

BigInteger BigInteger::operator++(int) {
    BigInteger old = *this;
    ++(*this);
    return old;
}

BigInteger& BigInteger::operator--() {
    *this -= 1;
    return *this;
}

BigInteger BigInteger::operator--(int) {
    BigInteger old = *this;
    --(*this);
    return old;
}

// Вспомогательные методы
std::string BigInteger::to_string() const {
    if (is_zero()) return "0";
    std::string s;
    if (negative_) s += '-';
    for (int i = static_cast<int>(digits_.size()) - 1; i >= 0; i--)
        s += char('0' + digits_[i]);
    return s;
}

bool BigInteger::is_zero() const {
    return digits_.size() == 1 && digits_[0] == 0;
}

bool BigInteger::is_negative() const {
    return negative_;
}

BigInteger::operator bool() const {
    return !is_zero();
}

// Ввод/вывод
std::ostream& operator<<(std::ostream& os, const BigInteger& v) {
    return os << v.to_string();
}

std::istream& operator>>(std::istream& is, BigInteger& v) {
    std::string s;
    is >> s;
    v = BigInteger(s);
    return is;
}