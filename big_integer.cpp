#include "big_integer.h"
#include <algorithm>
#include <cctype>
#include <stdexcept>
#include <string>

static void remove_zeros(std::vector<int>& a) {
    while (a.size() > 1 && a.back() == 0) {
        a.pop_back();
    }
}

static int cmp(const std::vector<int>& a, const std::vector<int>& b) {
    if (a.size() != b.size()) return (a.size() < b.size() ? -1 : 1);
    for (int i = (int)a.size() - 1; i >= 0; i--) {
        if (a[i] != b[i]) return (a[i] < b[i] ? -1 : 1);
    }
    return 0;
}

static std::vector<int> add_abs(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> res;
    int carry = 0;
    
    for (size_t i = 0; i < std::max(a.size(), b.size()) || carry; i++) {
        int sum = carry;
        if (i < a.size()) sum += a[i];
        if (i < b.size()) sum += b[i];
        res.push_back(sum % 10);
        carry = sum / 10;
    }
    return res;
}

static std::vector<int> sub_abs(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> res;
    int borrow = 0;
    
    for (size_t i = 0; i < a.size(); i++) {
        int diff = a[i] - borrow;
        if (i < b.size()) diff -= b[i];
        
        if (diff < 0) {
            diff += 10;
            borrow = 1;
        } else {
            borrow = 0;
        }
        res.push_back(diff);
    }
    
    remove_zeros(res);
    return res;
}

BigInteger::BigInteger() {
    digits_.push_back(0);
    negative_ = false;
}

BigInteger::BigInteger(int value) {
    negative_ = (value < 0);
    long long abs_val = value;
    if (abs_val < 0) abs_val = -abs_val;
    
    if (abs_val == 0) {
        digits_.push_back(0);
    } else {
        while (abs_val > 0) {
            digits_.push_back(abs_val % 10);
            abs_val /= 10;
        }
    }
}

BigInteger::BigInteger(long long value) {
    negative_ = (value < 0);
    unsigned long long abs_val = value;
    if (value < 0) abs_val = -value;
    
    if (abs_val == 0) {
        digits_.push_back(0);
    } else {
        while (abs_val > 0) {
            digits_.push_back(abs_val % 10);
            abs_val /= 10;
        }
    }
}

BigInteger::BigInteger(const std::string& str) {
    digits_.clear();
    negative_ = false;
    
    int start = 0;
    if (!str.empty() && str[0] == '-') {
        negative_ = true;
        start = 1;
    } else if (!str.empty() && str[0] == '+') {
        start = 1;
    }
    
    for (int i = (int)str.size() - 1; i >= start; i--) {
        if (!std::isdigit(str[i])) {
            digits_.push_back(0);
            negative_ = false;
            return;
        }
        digits_.push_back(str[i] - '0');
    }
    
    if (digits_.empty()) digits_.push_back(0);
    remove_zeros(digits_);
    if (digits_.size() == 1 && digits_[0] == 0) negative_ = false;
}

BigInteger BigInteger::operator+(const BigInteger& rhs) const {
    BigInteger result;
    if (negative_ == rhs.negative_) {
        result.digits_ = add_abs(digits_, rhs.digits_);
        result.negative_ = negative_;
    } else {
        int c = cmp(digits_, rhs.digits_);
        if (c == 0) {
            result.digits_ = {0};
            result.negative_ = false;
        } else if (c > 0) {
            result.digits_ = sub_abs(digits_, rhs.digits_);
            result.negative_ = negative_;
        } else {
            result.digits_ = sub_abs(rhs.digits_, digits_);
            result.negative_ = rhs.negative_;
        }
    }
    return result;
}

BigInteger BigInteger::operator-(const BigInteger& rhs) const {
    BigInteger neg_rhs = rhs;
    if (!rhs.is_zero()) neg_rhs.negative_ = !rhs.negative_;
    return *this + neg_rhs;
}

BigInteger BigInteger::operator*(const BigInteger& rhs) const {
    std::vector<int> res(digits_.size() + rhs.digits_.size(), 0);
    
    for (size_t i = 0; i < digits_.size(); i++) {
        int carry = 0;
        for (size_t j = 0; j < rhs.digits_.size() || carry; j++) {
            long long cur = res[i + j] + carry;
            if (j < rhs.digits_.size()) {
                cur += 1LL * digits_[i] * rhs.digits_[j];
            }
            res[i + j] = cur % 10;
            carry = (int)(cur / 10);
        }
    }
    
    remove_zeros(res);
    BigInteger result;
    result.digits_ = res;
    result.negative_ = (negative_ != rhs.negative_);
    
    if (result.is_zero()) result.negative_ = false;
    return result;
}

BigInteger BigInteger::operator/(const BigInteger& rhs) const {
    if (rhs.is_zero()) {
        throw std::runtime_error("Division by zero");
    }
    
    if (cmp(digits_, rhs.digits_) < 0) {
        return BigInteger(0);
    }
    
    bool sign = (negative_ != rhs.negative_);
    
    BigInteger a = *this;
    BigInteger b = rhs;
    a.negative_ = false;
    b.negative_ = false;
    
    std::vector<int> quotient;
    BigInteger current;
    
    for (int i = (int)a.digits_.size() - 1; i >= 0; i--) {
        current.digits_.insert(current.digits_.begin(), a.digits_[i]);
        remove_zeros(current.digits_);
        
        int count = 0;
        while (cmp(current.digits_, b.digits_) >= 0) {
            current.digits_ = sub_abs(current.digits_, b.digits_);
            count++;
        }
        
        quotient.insert(quotient.begin(), count);
    }
    
    remove_zeros(quotient);
    BigInteger result;
    result.digits_ = quotient;
    result.negative_ = sign;
    
    if (result.is_zero()) result.negative_ = false;
    return result;
}

BigInteger BigInteger::operator%(const BigInteger& rhs) const {
    BigInteger quotient = *this / rhs;
    BigInteger product = quotient * rhs;
    return *this - product;
}

BigInteger& BigInteger::operator+=(const BigInteger& rhs) {
    *this = *this + rhs;
    return *this;
}

BigInteger& BigInteger::operator-=(const BigInteger& rhs) {
    *this = *this - rhs;
    return *this;
}

BigInteger& BigInteger::operator*=(const BigInteger& rhs) {
    *this = *this * rhs;
    return *this;
}

BigInteger& BigInteger::operator/=(const BigInteger& rhs) {
    *this = *this / rhs;
    return *this;
}

BigInteger& BigInteger::operator%=(const BigInteger& rhs) {
    *this = *this % rhs;
    return *this;
}

BigInteger BigInteger::operator-() const {
    BigInteger result = *this;
    if (!result.is_zero()) result.negative_ = !negative_;
    return result;
}

BigInteger& BigInteger::operator++() {
    *this += 1;
    return *this;
}

BigInteger BigInteger::operator++(int) {
    BigInteger temp = *this;
    ++(*this);
    return temp;
}

BigInteger& BigInteger::operator--() {
    *this -= 1;
    return *this;
}

BigInteger BigInteger::operator--(int) {
    BigInteger temp = *this;
    --(*this);
    return temp;
}

bool BigInteger::operator==(const BigInteger& rhs) const {
    return negative_ == rhs.negative_ && digits_ == rhs.digits_;
}

bool BigInteger::operator!=(const BigInteger& rhs) const {
    return !(*this == rhs);
}

bool BigInteger::operator<(const BigInteger& rhs) const {
    if (negative_ != rhs.negative_) return negative_;
    
    int c = cmp(digits_, rhs.digits_);
    if (!negative_) return c < 0;
    return c > 0;
}

bool BigInteger::operator>(const BigInteger& rhs) const {
    return rhs < *this;
}

bool BigInteger::operator<=(const BigInteger& rhs) const {
    return !(*this > rhs);
}

bool BigInteger::operator>=(const BigInteger& rhs) const {
    return !(*this < rhs);
}

std::string BigInteger::to_string() const {
    if (is_zero()) return "0";
    
    std::string result;
    if (negative_) result += '-';
    
    for (int i = (int)digits_.size() - 1; i >= 0; i--) {
        result += char('0' + digits_[i]);
    }
    return result;
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

std::ostream& operator<<(std::ostream& os, const BigInteger& value) {
    os << value.to_string();
    return os;
}

std::istream& operator>>(std::istream& is, BigInteger& value) {
    std::string s;
    is >> s;
    value = BigInteger(s);
    return is;
}