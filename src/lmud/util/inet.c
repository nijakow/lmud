
#include "inet.h"


bool LMud_Inet_OpenServerV4(const char* address, LMud_Port port, LMud_Socket* the_socket)
{
    struct sockaddr_in  addr;
    int                 sock;
    int                 reuse;

    sock = socket(AF_INET, SOCK_STREAM, 0);

    if (sock == -1)
    {
        return false;
    }

    addr.sin_family      = AF_INET;
    addr.sin_port        = htons(port);
    addr.sin_addr.s_addr = inet_addr(address);

    // Set reuse address
    {
        reuse = 1;
        setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse));
    }

    if (bind(sock, (struct sockaddr*) &addr, sizeof(addr)) == -1)
    {
        LMud_Inet_Close(sock);
        return false;
    }

    if (listen(sock, 4) == -1)
    {
        LMud_Inet_Close(sock);
        return false;
    }

    *the_socket = sock;

    return true;
}

bool LMud_Inet_OpenV6(const char* address, LMud_Port port, LMud_Socket* the_socket)
{
    struct sockaddr_in6  addr;
    int                  sock;
    int                  reuse;

    sock = socket(AF_INET6, SOCK_STREAM, 0);

    if (sock == -1)
    {
        return false;
    }

    addr.sin6_family = AF_INET6;
    addr.sin6_port   = htons(port);
    inet_pton(AF_INET6, address, &addr.sin6_addr);

    // Set reuse address
    {
        reuse = 1;
        setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse));
    }

    if (bind(sock, (struct sockaddr*) &addr, sizeof(addr)) == -1)
    {
        LMud_Inet_Close(sock);
        return false;
    }

    if (listen(sock, 4) == -1)
    {
        LMud_Inet_Close(sock);
        return false;
    }

    *the_socket = sock;

    return true;
}


void LMud_Inet_AcceptInfo_Create(struct LMud_Inet_AcceptInfo* self)
{
    self->socket  = -1;
    self->addrlen = sizeof(self->addr);
}

void LMud_Inet_AcceptInfo_Destroy(struct LMud_Inet_AcceptInfo* self)
{
    (void) self;
}

LMud_Socket LMud_Inet_AcceptInfo_GetSocket(struct LMud_Inet_AcceptInfo* self)
{
    return self->socket;
}

bool LMud_Inet_Accept(LMud_Socket socket, struct LMud_Inet_AcceptInfo* info)
{
    info->socket = accept(socket, (struct sockaddr*) &info->addr, &info->addrlen);

    return info->socket != -1;
}


void LMud_Inet_Close(LMud_Socket socket)
{
    close(socket);
}
