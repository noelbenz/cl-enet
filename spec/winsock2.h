
#if defined(_WIN64)
typedef unsigned __int64 SOCKET;
typedef struct fd_set {
    unsigned int fd_count;
    SOCKET fd_array[64];
} fd_set;
#else
typedef unsigned int SOCKET;
typedef struct fd_set {
    unsigned int fd_count;
    SOCKET fd_array[64];
} fd_set;
#endif

