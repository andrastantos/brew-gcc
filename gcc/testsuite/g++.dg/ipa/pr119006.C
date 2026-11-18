// PR ipa/119006
// { dg-do run { target c++11 } }
// { dg-options "-O2 -fwhole-program -fdump-ipa-icf-details" }

struct FixedString10 {
    bool operator==(const char* rhs_) const { return rhs_ and not __builtin_strcmp(_str, rhs_); }
    char _str[11];
};
struct FixedString127 {
    bool operator==(const char* rhs_) const { return rhs_ and not __builtin_strcmp(_str, rhs_); }
    bool operator!=(const char* rhs_) const { return !(*this == rhs_); }
    char _str[128];
};
[[gnu::noinline,gnu::used]]
int fixedStringUser(const FixedString10& lhs, const char* rhs) {
    return lhs == rhs;
}
constexpr const char *t = "StrOverTenChars";
[[gnu::noinline,gnu::used]]
void checkString(FixedString127 reason_) {
    if (reason_ != t) __builtin_trap();
}

int main(int argc, char** argv) {
    FixedString127 fs;
    __builtin_strcpy(fs._str, t);
    checkString(fs);
}

// The two operator== bodies are identical but their _str members have
// different bounds, so ICF must not treat them as equal.
// { dg-final { scan-ipa-dump-not "Unified" "icf" } }
// { dg-final { scan-ipa-dump "Equal symbols: 0" "icf" } }
