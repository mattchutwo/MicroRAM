// String with lots of escape sequences.  This will produce escape sequences in
// the asm file, so we can test that they're parsed correctly.
const char* S1 = "hello, world!\r\n\a\b\x03\004\"\\";

int main() {
    int len = 0;
    while (S1[len] != 0) {
        ++len;
    }
    return len == 21;
}
