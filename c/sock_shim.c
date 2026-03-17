/*
 * Unix domain socket command channel for tc
 * Accepts 2-char Cmd strings (e.g. "m+" = heat inc) from external tools.
 * Thread-safe: listener runs in pthread, commands stored in mutex-protected buffer.
 */
#include <lean/lean.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>

// Global command buffer: written by listener thread, read by main thread
volatile char g_sock_cmd[256] = {0};
static pthread_mutex_t g_sock_mutex = PTHREAD_MUTEX_INITIALIZER;

static int g_listen_fd = -1;
static char g_sock_path[256] = {0};
static pthread_t g_listener_thread;
static volatile int g_sock_running = 0;

// Listener loop: accept connections, read command, store in buffer
static void *sock_listener_loop(void *arg) {
    (void)arg;
    while (g_sock_running) {
        struct sockaddr_un client;
        socklen_t len = sizeof(client);
        int cfd = accept(g_listen_fd, (struct sockaddr*)&client, &len);
        if (cfd < 0) {
            if (!g_sock_running) break;
            usleep(10000);
            continue;
        }
        char buf[256] = {0};
        ssize_t n = read(cfd, buf, sizeof(buf) - 1);
        close(cfd);
        if (n <= 0) continue;
        // Strip trailing newline
        while (n > 0 && (buf[n-1] == '\n' || buf[n-1] == '\r')) buf[--n] = 0;
        if (n == 0) continue;
        pthread_mutex_lock(&g_sock_mutex);
        strncpy((char*)g_sock_cmd, buf, sizeof(g_sock_cmd) - 1);
        pthread_mutex_unlock(&g_sock_mutex);
    }
    return NULL;
}

// Start socket listener: bind + listen + spawn thread
lean_obj_res lean_sock_start(b_lean_obj_arg path, lean_obj_arg world) {
    const char *p = lean_string_cstr(path);
    if (strlen(p) >= sizeof(g_sock_path) - 1)
        return lean_io_result_mk_ok(lean_box(0));

    // Clean up stale socket
    unlink(p);

    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0)
        return lean_io_result_mk_ok(lean_box(0));

    struct sockaddr_un addr = { .sun_family = AF_UNIX };
    strncpy(addr.sun_path, p, sizeof(addr.sun_path) - 1);

    if (bind(fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        close(fd);
        return lean_io_result_mk_ok(lean_box(0));
    }
    if (listen(fd, 4) < 0) {
        close(fd);
        unlink(p);
        return lean_io_result_mk_ok(lean_box(0));
    }

    g_listen_fd = fd;
    strncpy(g_sock_path, p, sizeof(g_sock_path) - 1);
    g_sock_running = 1;
    pthread_create(&g_listener_thread, NULL, sock_listener_loop, NULL);
    return lean_io_result_mk_ok(lean_box(1));
}

// Poll for pending command: returns command string or empty
lean_obj_res lean_sock_poll_cmd(lean_obj_arg world) {
    char buf[256] = {0};
    pthread_mutex_lock(&g_sock_mutex);
    if (g_sock_cmd[0]) {
        strncpy(buf, (const char*)g_sock_cmd, sizeof(buf) - 1);
        g_sock_cmd[0] = 0;
    }
    pthread_mutex_unlock(&g_sock_mutex);
    return lean_io_result_mk_ok(lean_mk_string(buf));
}

// Set environment variable (for TC_SOCK)
lean_obj_res lean_setenv(b_lean_obj_arg name, b_lean_obj_arg val, lean_obj_arg world) {
    setenv(lean_string_cstr(name), lean_string_cstr(val), 1);
    return lean_io_result_mk_ok(lean_box(0));
}

// Get process ID
lean_obj_res lean_getpid(lean_obj_arg world) {
    return lean_io_result_mk_ok(lean_box((size_t)getpid()));
}

// Shutdown: close socket, join thread, unlink path
lean_obj_res lean_sock_close(lean_obj_arg world) {
    if (!g_sock_running) return lean_io_result_mk_ok(lean_box(0));
    g_sock_running = 0;
    if (g_listen_fd >= 0) {
        shutdown(g_listen_fd, SHUT_RDWR);
        close(g_listen_fd);
        g_listen_fd = -1;
    }
    pthread_join(g_listener_thread, NULL);
    if (g_sock_path[0]) {
        unlink(g_sock_path);
        g_sock_path[0] = 0;
    }
    return lean_io_result_mk_ok(lean_box(0));
}
