#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <dlfcn.h>
#include <string.h>
#include <arpa/inet.h>
#include <stdbool.h>
#include <netinet/in.h>

// TODO: check atoi returns
// TODO: Implement -DSILENT and -DDEBUG
// TODO: Use proper return codes
// TODO: don't fail if no SRV_MAP
// TODO: Clean up logs

int (*super_connect)(int sockfd, const struct sockaddr *addr, socklen_t addrlen);

ssize_t (*super_sendto)(int sockfd, const void *buf, size_t len, int flags,
               const struct sockaddr *dest_addr, socklen_t addrlen);

ssize_t (*super_sendmsg)(int sockfd, const struct msghdr *msg, int flags);

ssize_t (*super_recvfrom)(int sockfd, void *buf, size_t len, int flags,
                 struct sockaddr *src_addr, socklen_t *addrlen);

ssize_t (*super_recvmsg)(int sockfd, struct msghdr *msg, int flags);

typedef struct
{
    // Port to map from. If "0" or "*" is given, maps from all ports.
    in_port_t port_from;

    // Port to map to. If "0" or "*" is given, maps to the original port.
    in_port_t port_to;

    // Address to map from. If "*" is given, maps from all addresses.
    struct in_addr *addr_from;

    // Address to map to. If "*" is given, maps to the original address.
    struct in_addr *addr_to;

} srv_mapping;

srv_mapping *mappings;
unsigned short n_mappings;


// TODO: attribute hidden
srv_mapping * srv_find_mapping(const struct sockaddr_in *addr_in)
{
    for (int i = 0; i < n_mappings; i ++)
    {
        if (mappings[i].addr_from == NULL ||
            (*mappings[i].addr_from).s_addr == (addr_in->sin_addr).s_addr)
        {
            if(mappings[i].port_from == 0 ||
                mappings[i].port_from == addr_in->sin_port)
            {
                fprintf(stderr, "WE HAVE A MATCH!!!!!!\n");
                return mappings + i;
            }
        }
    }

    return NULL;
}

// TODO: attribute hidden
srv_mapping * srv_find_mapping_to(const struct sockaddr_in *addr_in)
{
    for (int i = 0; i < n_mappings; i ++)
    {
        if (mappings[i].addr_to == NULL ||
            (*mappings[i].addr_to).s_addr == (addr_in->sin_addr).s_addr)
        {
            if(mappings[i].port_to == 0 ||
                mappings[i].port_to == addr_in->sin_port)
            {
                fprintf(stderr, "WE HAVE A MATCH!!!!!!\n");
                return mappings + i;
            }
        }
    }

    return NULL;
}


void _fini(void)
{
    if (mappings != NULL)
    {
        srv_mapping mapping;
        for (int i = 0; i < n_mappings; i ++)
        {
            // Free all the inner elements
            mapping = mappings[i];

            free(mapping.addr_from);
            free(mapping.addr_to);
        }

        free(mappings);

        mappings = NULL;
    }
}

void _init(void)
{
    char *mappings_str;
    if (!(mappings_str = getenv("SRV_MAP"))) {
        fprintf(stderr, "getenv(SRV_MAP) failed\n");
        exit(42);
    }

    fprintf(stderr, "%s\n", mappings_str);

    unsigned short mapping_strs_capacity = 1;
    char **mappings_strs = malloc(mapping_strs_capacity * sizeof(char*));;

    char *pch;
    pch = strtok(mappings_str, " ");

    while(pch != NULL)
    {
        fprintf(stderr, "Found mapping: %s\n", pch);

        if (n_mappings >= mapping_strs_capacity)
        {
            fprintf(stderr, "Growing ...\n");

            mapping_strs_capacity = 2 * mapping_strs_capacity;
            mappings_strs = realloc(mappings_strs, mapping_strs_capacity * sizeof(char*));
        }
        fprintf(stderr, "Copying to ix %d\n", n_mappings);

        mappings_strs[n_mappings] = malloc(strlen(pch));
        strcpy(mappings_strs[n_mappings], pch);
        n_mappings++;

        fprintf(stderr, "Next!\n");
        pch = strtok(NULL, " ");
    }

    fprintf(stderr, "Found %d mappings\n", n_mappings);

    mappings = malloc(n_mappings * sizeof(srv_mapping));

    for(int i = 0; i < n_mappings; i ++)
    {
        int res;

        fprintf(stderr, "Parsing mapping %d \n", i+1);

        pch = strtok(mappings_strs[i], ":");
        if(pch == NULL){ exit(43); }
        fprintf(stderr, "Parsing addr_from: %s\n", pch);

        if(strcmp(pch, "*") == 0)
        {
            mappings[i].addr_from = NULL;
        } else
        {
            mappings[i].addr_from = malloc(sizeof(struct in_addr));
            res = inet_pton(AF_INET, pch, mappings[i].addr_from);
            if (res == 1) {}
            else if (res == 0) { exit(43); }
            else if (res == -1) { exit(44); }
            else { exit(45); }
        }

        pch = strtok(NULL, ":");
        if(pch == NULL){ exit(43); }
        fprintf(stderr, "Parsing port_from: %s\n", pch);

        if(strcmp(pch, "0") == 0 || strcmp(pch, "*") == 0)
        {
            mappings[i].port_from = htons(0);
        } else
        {
            mappings[i].port_from = htons(atoi(pch));
        }


        pch = strtok(NULL, ":");
        if(pch == NULL){ exit(43); }

        fprintf(stderr, "Parsing addr_to: %s\n", pch);

        if(strcmp(pch, "*") == 0)
        {
            mappings[i].addr_to = NULL;
        } else
        {
            mappings[i].addr_to = malloc(sizeof(struct in_addr));
            res = inet_pton(AF_INET, pch, mappings[i].addr_to);
            if (res == 1) {}
            else if (res == 0) { exit(43); }
            else if (res == -1) { exit(44); }
            else { exit(45); }
        }

        pch = strtok(NULL, ":");
        if(pch == NULL){ exit(43); }

        fprintf(stderr, "Parsing port_to: %s\n", pch);

        if(strcmp(pch, "0") == 0 || strcmp(pch, "*") == 0)
        {
            mappings[i].port_to = htons(0);
        } else
        {
            mappings[i].port_to = htons(atoi(pch));
        }


        pch = strtok(NULL, ":");
        if(pch != NULL){ exit(12); }
    }

    const char *err;

    super_connect = dlsym(RTLD_NEXT, "connect");
    if ((err = dlerror()) != NULL) {
        fprintf(stderr, "dlsym(connect) failed: %s\n", err);
        exit(42);
    }

    super_sendto = dlsym(RTLD_NEXT, "sendto");
    if ((err = dlerror()) != NULL) {
        fprintf(stderr, "dlsym(sendto) failed: %s\n", err);
        exit(42);
    }

    super_sendmsg = dlsym(RTLD_NEXT, "sendmsg");
    if ((err = dlerror()) != NULL) {
        fprintf(stderr, "dlsym(sendmsg) failed: %s\n", err);
        exit(42);
    }

    super_recvmsg = dlsym(RTLD_NEXT, "recvmsg");
    if ((err = dlerror()) != NULL) {
        fprintf(stderr, "dlsym(recvmsg) failed: %s\n", err);
        exit(42);
    }

    super_recvfrom = dlsym(RTLD_NEXT, "recvfrom");
    if ((err = dlerror()) != NULL) {
        fprintf(stderr, "dlsym(recvfrom) failed: %s\n", err);
        exit(42);
    }
}

int connect(int sockfd, const struct sockaddr *dest_addr, socklen_t dest_len)
{

    fprintf(stderr, "Entering connect(2)\n");

    if (dest_addr->sa_family != AF_INET) {
        fprintf(stderr, "No AF_INET, skipping\n");
        return super_connect(sockfd, dest_addr, dest_len);
    }

    static struct sockaddr_in *dest_addr_in;
    dest_addr_in = (struct sockaddr_in *)dest_addr;

    srv_mapping * mapping = srv_find_mapping(dest_addr_in);

    if(mapping)
    {
        static struct sockaddr_storage new_dest_addr;
        memcpy(&new_dest_addr, dest_addr, dest_len);

        static struct sockaddr_in *new_dest_addr_in;
        new_dest_addr_in = (struct sockaddr_in *)&new_dest_addr;
        if(mapping->port_to != 0)
        {
            new_dest_addr_in->sin_port = mapping->port_to;
        }

        if(mapping->addr_to != NULL)
        {
            new_dest_addr_in->sin_addr = *mapping->addr_to;
        }
        return super_connect(sockfd, (struct sockaddr *)&new_dest_addr, dest_len);

    } else
    {
        return super_connect(sockfd, dest_addr, dest_len);

    }
}

ssize_t sendto(int sockfd, const void *buf, size_t len, int flags,
               const struct sockaddr *dest_addr, socklen_t addrlen)
{
    fprintf(stderr, "Entering sendto(2)\n");

    if (dest_addr->sa_family != AF_INET) {
        fprintf(stderr, "No AF_INET, skipping\n");
        return super_sendto(sockfd, buf, len, flags, dest_addr, addrlen);
    }

    static struct sockaddr_in *dest_addr_in;
    dest_addr_in = (struct sockaddr_in *)dest_addr;

    srv_mapping * mapping = srv_find_mapping(dest_addr_in);

    if(mapping)
    {
        static struct sockaddr_storage new_dest_addr;
        memcpy(&new_dest_addr, dest_addr, addrlen);

        static struct sockaddr_in *new_dest_addr_in;
        new_dest_addr_in = (struct sockaddr_in *)&new_dest_addr;
        if(mapping->port_to != 0)
        {
            new_dest_addr_in->sin_port = mapping->port_to;
        }

        if(mapping->addr_to != NULL)
        {
            new_dest_addr_in->sin_addr = *mapping->addr_to;
        }
        return super_sendto(sockfd, buf, len, flags, (struct sockaddr *)&new_dest_addr, addrlen);

    } else
    {
        return super_sendto(sockfd, buf, len, flags, dest_addr, addrlen);

    }

}

ssize_t sendmsg(int sockfd, const struct msghdr *msg, int flags)
{
    fprintf(stderr, "Entering sendmsg(2)\n");

    if(msg->msg_name == NULL)
    {
        fprintf(stderr, "No msg_name, skipping\n");
        return super_sendmsg(sockfd, msg, flags);

    }

    static struct sockaddr_in *dest_addr_in;
    dest_addr_in = (struct sockaddr_in *)msg->msg_name;

    if(dest_addr_in->sin_family != AF_INET)
    {
        fprintf(stderr, "No AF_INET, skipping\n");
        return super_sendmsg(sockfd, msg, flags);
    }

    srv_mapping * mapping = srv_find_mapping(dest_addr_in);

    if(mapping)
    {

        static struct sockaddr_storage new_dest_addr;
        memcpy(&new_dest_addr, dest_addr_in, msg->msg_namelen);

        static struct sockaddr_in *new_dest_addr_in;
        new_dest_addr_in = (struct sockaddr_in *)&new_dest_addr;

        if(mapping->port_to != 0)
        {
            new_dest_addr_in->sin_port = mapping->port_to;
        }

        if(mapping->addr_to != NULL)
        {
            new_dest_addr_in->sin_addr = *mapping->addr_to;
        }

        static struct msghdr new_msg;
        memcpy(&new_msg, msg, sizeof(*msg));

        new_msg.msg_name = (void*) new_dest_addr_in;

        return super_sendmsg(sockfd, &new_msg, flags);


    } else
    {
        return super_sendmsg(sockfd, msg, flags);
    }
}


// TODO: warn about limitations: since all mappings are not 1 to 1 it's not
// always possible to find a mapping "to", which is needed for recvfrom and
// recvmsg.

ssize_t recvfrom(int sockfd, void *buf, size_t len, int flags,
                 struct sockaddr *src_addr, socklen_t *addrlen)
{
    static struct sockaddr_in *src_addr_in;
    src_addr_in = (struct sockaddr_in *)src_addr;

    fprintf(stderr, "Entering recvfrom(2)\n");

    if (src_addr->sa_family != AF_INET) {
        fprintf(stderr, "No AF_INET, skipping\n");
    } else
    {
        srv_mapping * mapping = srv_find_mapping(src_addr_in);
        if(!mapping)
        {
            fprintf(stderr, "No mapping\n");
        } else
        {
            fprintf(stderr, "Found mapping\n");

            if(mapping->port_to != 0)
            {
                src_addr_in->sin_port = mapping->port_to;
            }

            if(mapping->addr_to != NULL)
            {
                src_addr_in->sin_addr = *mapping->addr_to;
            }
        }
    }

    ssize_t res = super_recvfrom(sockfd, buf, len, flags, src_addr, addrlen);

    if(src_addr_in->sin_family != AF_INET)
    {
        fprintf(stderr, "AFTER: recvfrom: No AF_INET, skipping\n");
    } else
    {
        fprintf(stderr, "AFTER: Found AF_INET\n");

        srv_mapping * mapping = srv_find_mapping_to(src_addr_in);
        if(!mapping)
        {
            fprintf(stderr, "AFTER: No mapping\n");
        } else
        {
            fprintf(stderr, "AFTER: Found mapping\n");
            if(mapping->port_from != 0)
            {
                fprintf(stderr, "AFTER: Setting port\n");
                src_addr_in->sin_port = mapping->port_from;
            }
            if(mapping->addr_from != NULL)
            {
                fprintf(stderr, "AFTER: Setting addr\n");
                src_addr_in->sin_addr = *mapping->addr_from;
            }
        }
    }

    return res;
}

ssize_t recvmsg(int sockfd, struct msghdr *msg, int flags)
{
    fprintf(stderr, "Entering recvmsg(2)\n");

    if(msg->msg_name == NULL)
    {
        fprintf(stderr, "No msg_name, skipping\n");
    } else
    {
        fprintf(stderr, "Found msg_name\n");
        struct sockaddr_in *src_addr_in = msg->msg_name;

        if(src_addr_in->sin_family != AF_INET)
        {
            fprintf(stderr, "recvmsg: No AF_INET, skipping\n");
        } else
        {
            fprintf(stderr, "Found AF_INET\n");
            srv_mapping * mapping = srv_find_mapping(src_addr_in);
            if(!mapping)
            {
                fprintf(stderr, "No mapping\n");
            } else
            {
                fprintf(stderr, "Found mapping\n");

                if(mapping->port_to != 0)
                {
                    src_addr_in->sin_port = mapping->port_to;
                }

                if(mapping->addr_to != NULL)
                {
                    src_addr_in->sin_addr = *mapping->addr_to;
                }
            }
        }
    }

    ssize_t res = super_recvmsg(sockfd, msg, flags);

    if(msg->msg_name == NULL)
    {
        fprintf(stderr, "AFTER: No msg_name, skipping\n");
    } else
    {
        fprintf(stderr, "AFTER: Found msg_name\n");
        struct sockaddr_in *src_addr_in = msg->msg_name;

        if(src_addr_in->sin_family != AF_INET)
        {
            fprintf(stderr, "AFTER: recvmsg: No AF_INET, skipping\n");
        } else
        {
            fprintf(stderr, "AFTER: Found AF_INET\n");

            srv_mapping * mapping = srv_find_mapping_to(src_addr_in);
            if(!mapping)
            {
                fprintf(stderr, "AFTER: No mapping\n");
            } else
            {
                fprintf(stderr, "AFTER: Found mapping\n");
                if(mapping->port_from != 0)
                {
                    fprintf(stderr, "AFTER: Setting port\n");
                    src_addr_in->sin_port = mapping->port_from;
                }
                if(mapping->addr_from != NULL)
                {
                    fprintf(stderr, "AFTER: Setting addr\n");
                    src_addr_in->sin_addr = *mapping->addr_from;
                }
            }
        }
    }

    return res;
}
