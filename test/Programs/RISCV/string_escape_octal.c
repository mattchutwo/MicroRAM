// String with lots of escape sequences.  This will produce escape
// sequences in the asm file, so we can test that they're parsed
// correctly.  In particular, this test will produce escape sequences
// that need to be parsed in octal and not, as Haskell would do by
// defualt, in decimal.
// `/x80` produces `/200` and `/xff` produces `/255`, both must be
// parsed in octal.
const unsigned char ARR[20] = "GIF89a\b\0\b\0\x80\0\0\0\0\0\xff\xff\xff!";

void __cc_trace_exec(const char*, unsigned long, unsigned long, unsigned long, unsigned long);

int main() {
    int acc = 0;
    for (int i = 0; i < 20; ++i) {
        __cc_trace_exec("got", ARR[i], 0, 0, 0);
        acc += ARR[i];
    }
    __cc_trace_exec("answer", acc, 0, 0, 0);
    return acc == 1366;
}
