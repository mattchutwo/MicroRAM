#include <stdint.h>
#include <stdlib.h>
#include <fromager.h>

// Allocate `size` bytes of memory.
char* __cc_malloc(size_t size);
// Free the allocation starting at `ptr`.
void __cc_free(char* ptr);

// Let the prover arbitrarily choose a word to poison in the range `start <=
// ptr < start + len`.  This function returns an offset within the range.  If
// `offset < len`, then the word starting at `start + offset` should be
// poisoned; otherwise, nothing should be poisoned.
uintptr_t __cc_advise_poison_offset(char* start, uintptr_t len);

// Write `val` to `*ptr` and poison `*ptr`.  If `*ptr` is already poisoned, the
// trace is invalid.
void __cc_write_and_poison(uintptr_t* ptr, uintptr_t val);

// Mark the range from `start` to `end` as valid to access.  This overrides the
// effect of any previous `__cc_mark_invalid` on the range.  All heap memory is
// invalid to access by default; it must be marked with `__cc_access_valid`
// when allocated.
void __cc_access_valid(char* start, char* end);

// Mark the range from `start` to `end` as invalid to access.
void __cc_access_invalid(char* start, char* end);

// Allocate a block of `size` bytes.
char* malloc_internal(size_t size) {
    char* ptr = __cc_malloc(size);

    // Compute and validate the size of the allocation provided by the prover.
    uintptr_t addr = (uintptr_t)ptr;
    size_t region_size = 1ull << ((addr >> 58) & 63);
    // The allocated region must have space for `size` bytes, plus an
    // additional word for metadata.
    __cc_valid_if(region_size >= size + sizeof(uintptr_t),
        "allocated region size is too small");
    __cc_valid_if(addr % region_size == 0,
        "allocated address is misaligned for its region size");
    // Note that `region_size` is always a power of two and is at least the
    // word size, so the address must be a multiple of the word size.

    // Write 1 (allocated) to the metadata field, and poison it to prevent
    // tampering.  This will make the trace invalid if the metadata word is
    // already poisoned (this happens if the prover tries to return the same
    // region for two separate allocations).
    uintptr_t* metadata = (uintptr_t*)(ptr + region_size - sizeof(uintptr_t));
    __cc_write_and_poison(metadata, 1);

    __cc_access_valid(ptr, ptr + size);

    // Choose a word to poison in the range `ptr .. metadata`.
    char* padding_start = ptr + size;
    uintptr_t padding_len = (char*)metadata - padding_start;
    uintptr_t poison_offset = __cc_advise_poison_offset(padding_start, padding_len);
    if (poison_offset < padding_len) {
        uintptr_t* poison = (uintptr_t*)(padding_start + poison_offset);
        // The poisoned address must be well-aligned.
        __cc_valid_if((uintptr_t)poison % sizeof(uintptr_t) == 0,
            "poison address is not word-aligned");
        // The poisoned address is guaranteed to be in the unused space at the
        // end of the region.
        __cc_write_and_poison(poison, 0);
    }

    return ptr;
}

void free_internal(char* ptr) {
    if (ptr == NULL) {
        return;
    }

    // Get the allocation size.
    uintptr_t log_region_size = (uintptr_t)ptr >> 58;
    uintptr_t region_size = 1ull << log_region_size;

    // Ensure `ptr` points to the start of a region.
    __cc_bug_if((uintptr_t)ptr % region_size != 0,
        "freed pointer not the start of a region");

    // Write to `*ptr`.  This memory access lets us catch double-free and
    // free-before-alloc by turning them into use-after-free and
    // use-before-alloc bugs, which we catch by other means.
    (*ptr) = 0;

    __cc_access_invalid(ptr, ptr + region_size);

    // Choose a word to poison within the freed region.  Note we forbid
    // choosing the metadata word, which is already poisoned.
    char* freed_start = ptr;
    uintptr_t freed_len = region_size - 2 * sizeof(uintptr_t);
    uintptr_t poison_offset = __cc_advise_poison_offset(freed_start, freed_len);
    if (poison_offset < freed_len) {
        uintptr_t* poison = (uintptr_t*)(freed_start + poison_offset);
        // The poisoned address must be well-aligned.
        __cc_valid_if((uintptr_t)poison % sizeof(uintptr_t) == 0,
            "poison address is not word-aligned");
        // The pointer is guaranteed to be somewhere within the freed region.
        __cc_write_and_poison(poison, 0);
    }
}

void __llvm__memcpy__p0i8__p0i8__i64(uint8_t *dest, const uint8_t *src, uint64_t len) {
    for (uint64_t i = 0; i < len; ++i) {
      dest[i] = src[i];
    }
}

void __llvm__memset__p0i8__i64(uint8_t *dest, uint8_t val, uint64_t len) {
    for (uint64_t i = 0; i < len; ++i) {
        dest[i] = val;
    }
}

void* malloc(size_t size) {
    return (void*)malloc_internal(size);
}

void free(void* ptr) {
    free_internal((char*)ptr);
}

int strcmp(const char *s1, const char *s2) {
    for (;;) {
        int a = *s1;
        int b = *s2;
        int diff = a - b;
        if (diff == 0) {
            if (a == 0) {
                return 0;
            } else {
                ++s1;
                ++s2;
            }
        } else {
            return diff;
        }
    }
}

size_t strlen(const char* s) {
    const char* t = s;
    while (*t) {
        ++t;
    }
    return t - s;
}

char *strcpy(char *dest, const char *src) {
    char* orig_dest = dest;
    while (*src) {
        *dest = *src;
        ++dest;
        ++src;
    }
    return orig_dest;
}

char *strdup(const char *s) {
    char* t = malloc(strlen(s) + 1);
    strcpy(t, s);
    return t;
}

